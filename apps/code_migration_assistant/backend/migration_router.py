"""Migration UI API ルーター.

エンドポイント一覧:
  POST /api/migrate/upload              - COBOLファイル/zipアップロード
  GET  /api/migrate/{task_id}/stream    - SSE進捗ストリーム
  POST /api/migrate/{task_id}/hitl      - HITL応答送信
  GET  /api/migrate/{task_id}/status    - タスクステータス取得
  GET  /api/migrate/{task_id}/download  - 成果物zipダウンロード
"""

from __future__ import annotations

import asyncio
import json
import logging
import os
import shutil
import tempfile
from pathlib import Path
from typing import Annotated, Any

from apps.code_migration_assistant.backend.migration_execution_adapter import (
    CmaCliExecutionAdapter,
    ExecutionConfig,
)
from apps.code_migration_assistant.backend.migration_task_store import (
    HITLRequest as PendingHITLRequest,
)
from apps.code_migration_assistant.backend.migration_task_store import (
    TaskStatus,
    TaskStore,
    get_task_store,
)
from apps.code_migration_assistant.cobol_project import COBOLFile, COBOLProject
from fastapi import APIRouter, Depends, File, HTTPException, Query, Request, UploadFile
from fastapi.responses import FileResponse, StreamingResponse
from pydantic import BaseModel


logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/migrate", tags=["migration"])

# 出力ルートディレクトリ（リポジトリルートからの絶対パス）
_DEFAULT_OUTPUT_ROOT = Path(__file__).parent.parent.parent.parent / "migration_output"

# バックグラウンドタスクの強参照セット（GCによる早期キャンセル防止）
_background_tasks: set[asyncio.Task[Any]] = set()


class UploadResponse(BaseModel):
    """アップロードAPIレスポンス."""

    task_id: str
    stream_url: str
    file_count: int
    program_names: list[str]


class HITLSubmitRequest(BaseModel):
    """HITL応答リクエスト."""

    request_id: str
    approved: bool
    comment: str = ""
    modifications: dict[str, Any] = {}


class TaskStatusResponse(BaseModel):
    """タスクステータスレスポンス."""

    task_id: str
    status: str
    current_stage: str | None
    download_available: bool
    error: str | None


# ---------- ヘルパー関数 ----------

def _get_output_root() -> Path:
    """出力ルートディレクトリを返す（テスト時は上書き可能）."""

    return _DEFAULT_OUTPUT_ROOT


def _get_execution_backend() -> str:
    """実行バックエンド種別を返す."""

    backend = os.environ.get("MIGRATION_EXECUTION_BACKEND", "cma_cli").strip().lower()
    if backend in {"cma_cli", "legacy_engine"}:
        return backend
    return "cma_cli"


async def _run_legacy_pipeline(
    *,
    task_id: str,
    cobol_files: list[COBOLFile],
    output_root: Path,
    fast_mode: bool,
    model: str,
    store: TaskStore,
) -> None:
    """互換 MigrationEngine で実行する（legacy 経路）."""

    from apps.code_migration_assistant.output.packager import OutputPackager
    from apps.code_migration_assistant.pipeline.engine import MigrationEngine

    engine = MigrationEngine(
        output_root=output_root,
        fast_mode=fast_mode,
        model=model,
    )

    for cobol_file in cobol_files:
        async for event in engine.run_file(cobol_file):
            event_dict = {
                "type": event.event_type,
                "stage": event.stage,
                **event.data,
            }
            await store.push_event(task_id, event_dict)
            await store.update_status(
                task_id,
                TaskStatus.RUNNING,
                current_stage=event.stage,
            )

            if event.event_type == "hitl_required" and getattr(event, "_hitl_event", None) is not None:
                task_obj = await store.get(task_id)
                if task_obj is not None:
                    task_obj.pending_hitl = PendingHITLRequest(
                        request_id=event.data.get("request_id", ""),
                        stage=event.stage or "",
                        artifact=event.data.get("artifact", {}),
                        unknowns=event.data.get("unknowns", []),
                        question=event.data.get("question", ""),
                        response_event=event._hitl_event,  # type: ignore[attr-defined]
                    )

            if event.event_type == "complete":
                program_name_raw = event.data.get("program_name")
                if isinstance(program_name_raw, str) and program_name_raw:
                    try:
                        packager = OutputPackager(output_root)
                        zip_path = packager.create_download_package(program_name_raw)
                        await store.set_download_path(task_id, zip_path)
                    except Exception as exc:
                        logger.warning("legacy zip作成失敗: %s", exc)


async def _run_cma_cli_pipeline(
    *,
    task_id: str,
    source_path: Path,
    output_root: Path,
    fast_mode: bool,
    model: str,
    store: TaskStore,
) -> None:
    """CMA CLI 契約実行を行い、イベントを中継する."""

    adapter = CmaCliExecutionAdapter(output_root / "_runtime")
    await adapter.start(
        task_id,
        ExecutionConfig(
            source_path=source_path,
            output_root=output_root,
            fast_mode=fast_mode,
            model=model,
            options={},
        ),
    )

    async for event in adapter.stream_events(task_id):
        await store.push_event(task_id, event)
        stage = event.get("stage")
        if isinstance(stage, str):
            await store.update_status(
                task_id,
                TaskStatus.RUNNING,
                current_stage=stage,
            )

    result = await adapter.await_result(task_id)
    if result.output_dir is not None and result.output_dir.exists():
        zip_path = CmaCliExecutionAdapter.create_download_package(result.output_dir, task_id)
        await store.set_download_path(task_id, zip_path)

    if result.error and result.decision == "ENV_ISSUE":
        await store.update_status(task_id, TaskStatus.ERROR, error_message=result.error)
        await store.push_event(task_id, {"type": "error", "stage": None, "message": result.error})
        return

    await store.update_status(task_id, TaskStatus.COMPLETE)


async def _run_pipeline_background(
    task_id: str,
    source_path: Path,
    cobol_files: list[COBOLFile],
    tmp_dir: Path,
    output_root: Path,
    fast_mode: bool,
    model: str,
    store: TaskStore,
) -> None:
    """バックグラウンドでパイプラインを実行し、イベントをキューに投入する."""

    await store.update_status(task_id, TaskStatus.RUNNING)
    backend = _get_execution_backend()
    try:
        if backend == "legacy_engine":
            await _run_legacy_pipeline(
                task_id=task_id,
                cobol_files=cobol_files,
                output_root=output_root,
                fast_mode=fast_mode,
                model=model,
                store=store,
            )
            await store.update_status(task_id, TaskStatus.COMPLETE)
        else:
            await _run_cma_cli_pipeline(
                task_id=task_id,
                source_path=source_path,
                output_root=output_root,
                fast_mode=fast_mode,
                model=model,
                store=store,
            )
    except Exception as exc:
        logger.exception("パイプライン実行エラー: %s", exc)
        await store.update_status(task_id, TaskStatus.ERROR, error_message=str(exc))
        await store.push_event(task_id, {"type": "error", "stage": None, "message": str(exc)})
    finally:
        await store.close_events(task_id)
        shutil.rmtree(tmp_dir, ignore_errors=True)


async def _sse_generator(task_id: str, store: TaskStore, last_event_id: int = 0):
    """SSEイベントを生成するジェネレータ.

    再接続時は last_event_id より後のイベントをリプレイしてから
    キューをドレインする。各イベントには SSE id: フィールドを付与する。
    """

    task = await store.get(task_id)
    if task is None:
        yield "data: " + json.dumps({"type": "error", "message": "タスクが存在しません"}) + "\n\n"
        return

    if last_event_id > 0:
        missed = await store.get_events_since(task_id, last_event_id)
        for event_id, event_data in missed:
            yield f"id: {event_id}\ndata: " + json.dumps(event_data, ensure_ascii=False) + "\n\n"

    while True:
        try:
            item = await asyncio.wait_for(task.events.get(), timeout=30.0)
        except TimeoutError:
            yield ": ping\n\n"
            continue

        if item is None:  # sentinel
            break

        event_id, event_data = item
        yield f"id: {event_id}\ndata: " + json.dumps(event_data, ensure_ascii=False) + "\n\n"


# ---------- エンドポイント ----------

@router.post("/upload", response_model=UploadResponse)
async def upload_cobol(
    file: Annotated[UploadFile, File(description="COBOLファイルまたはzipアーカイブ")],
    fast: bool = Query(default=True, description="実行比較スキップ（Java/gnucobol不要）"),
    model: str = Query(default="claude-opus-4-6", description="使用するClaudeモデルID"),
    store: TaskStore = Depends(get_task_store),
) -> UploadResponse:
    """COBOLファイルをアップロードして移行タスクを開始する."""

    if not file.filename:
        raise HTTPException(status_code=400, detail="ファイル名が指定されていません")

    allowed_exts = {".cbl", ".cob", ".cobol", ".zip"}
    suffix = Path(file.filename).suffix.lower()
    if suffix not in allowed_exts:
        raise HTTPException(
            status_code=400,
            detail=f"対応外のファイル形式: {suffix}。{allowed_exts} のみ対応しています",
        )

    tmp_dir = Path(tempfile.mkdtemp(prefix="migration_upload_"))
    try:
        upload_path = tmp_dir / Path(file.filename).name
        content = await file.read()
        upload_path.write_bytes(content)

        project = COBOLProject(upload_path, tmp_dir)
        project.setup()
        cobol_files = project.get_cobol_files()
        if not cobol_files:
            shutil.rmtree(tmp_dir, ignore_errors=True)
            raise HTTPException(
                status_code=422,
                detail="COBOLファイルが見つかりません（.cbl/.cob/.cobol）",
            )

        task = await store.create()
        output_root = _get_output_root()
        output_root.mkdir(parents=True, exist_ok=True)

        bg_task = asyncio.create_task(
            _run_pipeline_background(
                task_id=task.task_id,
                source_path=upload_path,
                cobol_files=cobol_files,
                tmp_dir=tmp_dir,
                output_root=output_root,
                fast_mode=fast,
                model=model,
                store=store,
            )
        )
        _background_tasks.add(bg_task)
        bg_task.add_done_callback(_background_tasks.discard)

        return UploadResponse(
            task_id=task.task_id,
            stream_url=f"/api/migrate/{task.task_id}/stream",
            file_count=len(cobol_files),
            program_names=[entry.program_name for entry in cobol_files],
        )

    except HTTPException:
        shutil.rmtree(tmp_dir, ignore_errors=True)
        raise
    except Exception as exc:
        shutil.rmtree(tmp_dir, ignore_errors=True)
        logger.exception("アップロード処理エラー: %s", exc)
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.get("/{task_id}/stream")
async def stream_events(
    task_id: str,
    request: Request,
    store: TaskStore = Depends(get_task_store),
) -> StreamingResponse:
    """SSEでパイプライン進捗をストリームする."""

    task = await store.get(task_id)
    if task is None:
        raise HTTPException(status_code=404, detail=f"タスクが存在しません: {task_id}")

    try:
        last_event_id = int(request.headers.get("Last-Event-ID", "0"))
    except ValueError:
        last_event_id = 0

    return StreamingResponse(
        _sse_generator(task_id, store, last_event_id),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "X-Accel-Buffering": "no",
        },
    )


@router.post("/{task_id}/hitl")
async def submit_hitl(
    task_id: str,
    body: HITLSubmitRequest,
    store: TaskStore = Depends(get_task_store),
) -> dict[str, str]:
    """HITL（Human-In-The-Loop）確認結果を送信する."""

    if _get_execution_backend() == "cma_cli":
        raise HTTPException(
            status_code=501,
            detail="cma_cli backend では HITL API は未サポートです（Phase 2 で接続予定）",
        )

    task = await store.get(task_id)
    if task is None:
        raise HTTPException(status_code=404, detail=f"タスクが存在しません: {task_id}")

    success = await store.submit_hitl_response(
        task_id=task_id,
        request_id=body.request_id,
        response={
            "approved": body.approved,
            "comment": body.comment,
            "modifications": body.modifications,
        },
    )
    if not success:
        raise HTTPException(
            status_code=409,
            detail="対応するHITL要求が存在しないか、request_idが一致しません",
        )
    return {"status": "resumed"}


@router.get("/{task_id}/status", response_model=TaskStatusResponse)
async def get_status(
    task_id: str,
    store: TaskStore = Depends(get_task_store),
) -> TaskStatusResponse:
    """タスクの現在ステータスを取得する."""

    task = await store.get(task_id)
    if task is None:
        raise HTTPException(status_code=404, detail=f"タスクが存在しません: {task_id}")
    return TaskStatusResponse(**task.to_status_dict())


@router.get("/{task_id}/download")
async def download_result(
    task_id: str,
    store: TaskStore = Depends(get_task_store),
) -> FileResponse:
    """移行成果物のzipをダウンロードする."""

    task = await store.get(task_id)
    if task is None:
        raise HTTPException(status_code=404, detail=f"タスクが存在しません: {task_id}")

    if task.status != TaskStatus.COMPLETE:
        raise HTTPException(
            status_code=409,
            detail=f"タスクがまだ完了していません: {task.status.value}",
        )

    if task.download_path is None or not task.download_path.exists():
        raise HTTPException(status_code=404, detail="ダウンロードファイルが見つかりません")

    return FileResponse(
        path=str(task.download_path),
        media_type="application/zip",
        filename=task.download_path.name,
    )

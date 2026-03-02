"""Migration Studio バックエンドテスト.

TaskStore・HITL・ルーターエンドポイントのユニット／統合テスト。
APIキー不要（全モック使用）。

実行方法:
    pytest apps/migration_studio/tests/test_backend.py -v
"""

from __future__ import annotations

import asyncio
import io
from pathlib import Path
from typing import Any
from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from fastapi.testclient import TestClient

from apps.migration_studio.backend.task_store import (
    HITLRequest,
    MigrationTask,
    TaskStatus,
    TaskStore,
)
from apps.migration_studio.pipeline.engine import MigrationEngine


# ============================================================
# TaskStore テスト
# ============================================================

class TestTaskStore:
    """TaskStore のユニットテスト."""

    @pytest.fixture()
    def store(self) -> TaskStore:
        """テスト用 TaskStore インスタンスを生成する."""
        return TaskStore()

    @pytest.mark.asyncio()
    async def test_create_returns_task(self, store: TaskStore) -> None:
        """create() が MigrationTask を返す."""
        task = await store.create()
        assert isinstance(task, MigrationTask)
        assert task.task_id
        assert task.status == TaskStatus.PENDING

    @pytest.mark.asyncio()
    async def test_create_generates_unique_ids(self, store: TaskStore) -> None:
        """create() が重複しない task_id を生成する."""
        task_a = await store.create()
        task_b = await store.create()
        assert task_a.task_id != task_b.task_id

    @pytest.mark.asyncio()
    async def test_get_existing_task(self, store: TaskStore) -> None:
        """get() で存在するタスクを取得できる."""
        task = await store.create()
        found = await store.get(task.task_id)
        assert found is not None
        assert found.task_id == task.task_id

    @pytest.mark.asyncio()
    async def test_get_nonexistent_task_returns_none(self, store: TaskStore) -> None:
        """get() で存在しない task_id は None を返す."""
        result = await store.get("nonexistent-id")
        assert result is None

    @pytest.mark.asyncio()
    async def test_update_status(self, store: TaskStore) -> None:
        """update_status() でステータスが更新される."""
        task = await store.create()
        await store.update_status(task.task_id, TaskStatus.RUNNING, current_stage="analyzer")
        updated = await store.get(task.task_id)
        assert updated is not None
        assert updated.status == TaskStatus.RUNNING
        assert updated.current_stage == "analyzer"

    @pytest.mark.asyncio()
    async def test_push_event_assigns_sequential_ids(self, store: TaskStore) -> None:
        """push_event() がイベントに連番IDを付与する."""
        task = await store.create()
        await store.push_event(task.task_id, {"type": "stage_start", "stage": "analyzer"})
        await store.push_event(task.task_id, {"type": "stage_complete", "stage": "analyzer"})

        item1 = await task.events.get()
        item2 = await task.events.get()

        assert item1 is not None
        assert item2 is not None
        event_id_1, _ = item1
        event_id_2, _ = item2
        assert event_id_1 == 1
        assert event_id_2 == 2

    @pytest.mark.asyncio()
    async def test_push_event_stores_in_history(self, store: TaskStore) -> None:
        """push_event() がイベントを event_history に保存する."""
        task = await store.create()
        event_data: dict[str, Any] = {"type": "stage_start", "stage": "analyzer"}
        await store.push_event(task.task_id, event_data)

        assert len(task.event_history) == 1
        eid, ev = task.event_history[0]
        assert eid == 1
        assert ev["type"] == "stage_start"

    @pytest.mark.asyncio()
    async def test_get_events_since_returns_missed(self, store: TaskStore) -> None:
        """get_events_since() が指定ID以降のイベントを返す."""
        task = await store.create()
        await store.push_event(task.task_id, {"type": "ev1"})
        await store.push_event(task.task_id, {"type": "ev2"})
        await store.push_event(task.task_id, {"type": "ev3"})

        missed = await store.get_events_since(task.task_id, last_event_id=1)
        assert len(missed) == 2
        ids = [eid for eid, _ in missed]
        assert ids == [2, 3]

    @pytest.mark.asyncio()
    async def test_get_events_since_empty_when_all_seen(self, store: TaskStore) -> None:
        """get_events_since() で最新IDを渡すと空リストを返す."""
        task = await store.create()
        await store.push_event(task.task_id, {"type": "ev1"})
        missed = await store.get_events_since(task.task_id, last_event_id=1)
        assert missed == []

    @pytest.mark.asyncio()
    async def test_get_events_since_nonexistent_task(self, store: TaskStore) -> None:
        """get_events_since() で存在しない task_id は空リストを返す."""
        missed = await store.get_events_since("no-such-task", last_event_id=0)
        assert missed == []

    @pytest.mark.asyncio()
    async def test_close_events_puts_sentinel(self, store: TaskStore) -> None:
        """close_events() が None sentinel をキューに投入する."""
        task = await store.create()
        await store.close_events(task.task_id)
        sentinel = await task.events.get()
        assert sentinel is None

    @pytest.mark.asyncio()
    async def test_submit_hitl_response_success(self, store: TaskStore) -> None:
        """submit_hitl_response() が待機イベントをセットして True を返す."""
        task = await store.create()
        response_event = asyncio.Event()
        task.pending_hitl = HITLRequest(
            request_id="req-001",
            stage="analyzer",
            artifact={"programs": []},
            unknowns=["field_x"],
            question="確認してください",
            response_event=response_event,
        )

        success = await store.submit_hitl_response(
            task_id=task.task_id,
            request_id="req-001",
            response={"approved": True, "comment": "OK"},
        )

        assert success is True
        assert response_event.is_set()
        # 応答後は pending_hitl がクリアされる
        assert task.pending_hitl is None

    @pytest.mark.asyncio()
    async def test_submit_hitl_response_wrong_request_id(self, store: TaskStore) -> None:
        """request_id が一致しない場合 False を返す."""
        task = await store.create()
        task.pending_hitl = HITLRequest(
            request_id="req-001",
            stage="analyzer",
            artifact={},
            unknowns=[],
            question="test",
            response_event=asyncio.Event(),
        )

        success = await store.submit_hitl_response(
            task_id=task.task_id,
            request_id="req-999",
            response={"approved": True},
        )

        assert success is False
        # pending_hitl はそのまま残る
        assert task.pending_hitl is not None

    @pytest.mark.asyncio()
    async def test_submit_hitl_response_no_pending(self, store: TaskStore) -> None:
        """pending_hitl がない場合 False を返す."""
        task = await store.create()
        success = await store.submit_hitl_response(
            task_id=task.task_id,
            request_id="req-001",
            response={},
        )
        assert success is False

    @pytest.mark.asyncio()
    async def test_delete_removes_task(self, store: TaskStore) -> None:
        """delete() でタスクが削除される."""
        task = await store.create()
        await store.delete(task.task_id)
        result = await store.get(task.task_id)
        assert result is None

    @pytest.mark.asyncio()
    async def test_set_download_path(self, store: TaskStore, tmp_path: Path) -> None:
        """set_download_path() がダウンロードパスを設定する."""
        task = await store.create()
        zip_path = tmp_path / "output.zip"
        zip_path.write_bytes(b"dummy")
        await store.set_download_path(task.task_id, zip_path)
        updated = await store.get(task.task_id)
        assert updated is not None
        assert updated.download_path == zip_path


# ============================================================
# MigrationEngine._should_hitl テスト
# ============================================================

class TestShouldHITL:
    """MigrationEngine._should_hitl の条件テスト."""

    def test_false_when_no_conditions(self) -> None:
        """条件ゼロの場合は False."""
        result = MigrationEngine._should_hitl({
            "unknowns": [],
            "has_database_access": False,
            "has_external_calls": False,
        })
        assert result is False

    def test_false_when_few_unknowns(self) -> None:
        """unknowns が 4 件以下は False."""
        result = MigrationEngine._should_hitl({
            "unknowns": ["a", "b", "c", "d"],
        })
        assert result is False

    def test_true_when_five_unknowns(self) -> None:
        """unknowns が 5 件以上で True."""
        result = MigrationEngine._should_hitl({
            "unknowns": ["a", "b", "c", "d", "e"],
        })
        assert result is True

    def test_true_when_many_unknowns(self) -> None:
        """unknowns が多数でも True."""
        result = MigrationEngine._should_hitl({
            "unknowns": [f"field_{i}" for i in range(10)],
        })
        assert result is True

    def test_true_when_database_access(self) -> None:
        """has_database_access が True なら True."""
        result = MigrationEngine._should_hitl({
            "unknowns": [],
            "has_database_access": True,
        })
        assert result is True

    def test_true_when_external_calls(self) -> None:
        """has_external_calls が True なら True."""
        result = MigrationEngine._should_hitl({
            "unknowns": [],
            "has_external_calls": True,
        })
        assert result is True

    def test_true_when_combined(self) -> None:
        """複数条件が重なっても True（最初にマッチした条件で判定）."""
        result = MigrationEngine._should_hitl({
            "unknowns": ["a"],
            "has_database_access": True,
            "has_external_calls": True,
        })
        assert result is True

    def test_false_with_empty_dict(self) -> None:
        """空 dict は False."""
        result = MigrationEngine._should_hitl({})
        assert result is False


# ============================================================
# ルーターエンドポイント テスト（FastAPI TestClient）
# ============================================================

@pytest.fixture()
def test_client(tmp_path: Path) -> TestClient:
    """テスト用 FastAPI TestClient を生成する.

    パイプラインは実行しない（upload 後は pending 状態のまま）。
    """
    from apps.migration_studio.backend.app import create_app

    app = create_app()

    def _fake_create_task(coro: object) -> MagicMock:
        """バックグラウンド実行を抑止しつつ未await警告を防ぐ."""
        close = getattr(coro, "close", None)
        if callable(close):
            close()
        return MagicMock(add_done_callback=lambda _cb: None)

    # バックグラウンドタスクを起動しないようにパッチ
    with patch(
        "apps.migration_studio.backend.router.asyncio.create_task",
        side_effect=_fake_create_task,
    ):
        # 出力ルートを tmp_path に向ける
        with patch(
            "apps.migration_studio.backend.router._get_output_root",
            return_value=tmp_path / "output",
        ):
            with TestClient(app) as client:
                yield client


class TestRouterEndpoints:
    """ルーターエンドポイントの統合テスト."""

    FIXTURES_DIR = Path(__file__).parent / "fixtures"
    SAMPLE_CBL = FIXTURES_DIR / "sample.cbl"

    def test_upload_invalid_extension(self, test_client: TestClient) -> None:
        """対応外の拡張子ファイルは 400 を返す."""
        response = test_client.post(
            "/api/migrate/upload",
            files={"file": ("test.txt", b"dummy content", "text/plain")},
        )
        assert response.status_code == 400
        assert "対応外" in response.json()["detail"]

    def test_upload_empty_filename(self, test_client: TestClient) -> None:
        """ファイル名なしは 4xx（400 または 422）を返す."""
        response = test_client.post(
            "/api/migrate/upload",
            files={"file": ("", b"", "application/octet-stream")},
        )
        # FastAPI バリデーションが 422 を返す場合もある
        assert response.status_code in (400, 422)

    def test_upload_valid_cobol_file(self, test_client: TestClient) -> None:
        """有効な .cbl ファイルで task_id が返る."""
        cobol_content = self.SAMPLE_CBL.read_bytes()
        response = test_client.post(
            "/api/migrate/upload",
            files={"file": ("sample.cbl", cobol_content, "text/plain")},
            params={"fast": "true"},
        )
        assert response.status_code == 200
        data = response.json()
        assert "task_id" in data
        assert data["file_count"] == 1
        assert data["program_names"] == ["SAMPLE"]
        assert "/stream" in data["stream_url"]

    def test_status_unknown_task(self, test_client: TestClient) -> None:
        """存在しない task_id のステータスは 404."""
        response = test_client.get("/api/migrate/nonexistent-id/status")
        assert response.status_code == 404

    def test_status_after_upload(self, test_client: TestClient) -> None:
        """アップロード直後のタスクはステータスが返る."""
        cobol_content = self.SAMPLE_CBL.read_bytes()
        upload = test_client.post(
            "/api/migrate/upload",
            files={"file": ("sample.cbl", cobol_content, "text/plain")},
        )
        assert upload.status_code == 200
        task_id = upload.json()["task_id"]

        status_resp = test_client.get(f"/api/migrate/{task_id}/status")
        assert status_resp.status_code == 200
        body = status_resp.json()
        assert body["task_id"] == task_id
        assert body["status"] in ("pending", "running", "complete", "error")

    def test_download_unknown_task(self, test_client: TestClient) -> None:
        """存在しない task_id のダウンロードは 404."""
        response = test_client.get("/api/migrate/nonexistent-id/download")
        assert response.status_code == 404

    def test_download_incomplete_task(self, test_client: TestClient) -> None:
        """未完了タスクのダウンロードは 409."""
        cobol_content = self.SAMPLE_CBL.read_bytes()
        upload = test_client.post(
            "/api/migrate/upload",
            files={"file": ("sample.cbl", cobol_content, "text/plain")},
        )
        assert upload.status_code == 200
        task_id = upload.json()["task_id"]

        # pending 状態のままダウンロードを試みる
        download = test_client.get(f"/api/migrate/{task_id}/download")
        assert download.status_code == 409

    def test_hitl_unknown_task(self, test_client: TestClient) -> None:
        """存在しない task_id への HITL は 404 または 501."""
        response = test_client.post(
            "/api/migrate/nonexistent-id/hitl",
            json={"request_id": "req-1", "approved": True},
        )
        # cma_cli バックエンドでは 501、legacy では 404
        assert response.status_code in (404, 501)

    def test_hitl_no_pending_hitl(self, test_client: TestClient) -> None:
        """HITL 要求がないタスクへの HITL 応答は 409 または 501."""
        cobol_content = self.SAMPLE_CBL.read_bytes()
        upload = test_client.post(
            "/api/migrate/upload",
            files={"file": ("sample.cbl", cobol_content, "text/plain")},
        )
        task_id = upload.json()["task_id"]

        response = test_client.post(
            f"/api/migrate/{task_id}/hitl",
            json={"request_id": "req-001", "approved": True},
        )
        # cma_cli バックエンドでは 501（未サポート）
        assert response.status_code in (409, 501)


# ============================================================
# Evolution Manager 追加テスト
# ============================================================

class TestEvolutionManagerExtra:
    """EvolutionManager の追加テスト（Phase 4 改善分）."""

    def test_record_evolution_stores_program_name(self, tmp_path: Path) -> None:
        """record_evolution() が program_name を正しく保存する."""
        import json
        from apps.migration_studio.evolution.manager import EvolutionManager

        manager = EvolutionManager(tmp_path)
        program_dir = tmp_path / "CUSTPROC"
        program_dir.mkdir()

        evolution_info = {
            "iteration": 1,
            "stage": "designer",
            "decision": "DESIGN_ISSUE",
            "fix_summary": "テスト修正",
            "evolved_at": "2026-01-01T00:00:00+00:00",
        }
        manager.record_evolution(evolution_info, program_dir, program_name="CUSTPROC")

        data = json.loads((program_dir / "evolution.json").read_text(encoding="utf-8"))
        assert data["program_name"] == "CUSTPROC"
        assert data["total_iterations"] == 1

    def test_record_evolution_uses_dirname_as_fallback(self, tmp_path: Path) -> None:
        """program_name 省略時はディレクトリ名が使われる."""
        import json
        from apps.migration_studio.evolution.manager import EvolutionManager

        manager = EvolutionManager(tmp_path)
        program_dir = tmp_path / "SAMPLE_PROG"
        program_dir.mkdir()

        evolution_info = {
            "iteration": 1,
            "stage": "transformer",
            "decision": "TRANSFORM_ISSUE",
            "fix_summary": "変換修正",
            "evolved_at": "2026-01-01T00:00:00+00:00",
        }
        manager.record_evolution(evolution_info, program_dir)

        data = json.loads((program_dir / "evolution.json").read_text(encoding="utf-8"))
        assert data["program_name"] == "SAMPLE_PROG"

    def test_get_iteration_count_initial(self, tmp_path: Path) -> None:
        """初期反復数はゼロ."""
        from apps.migration_studio.evolution.manager import EvolutionManager

        manager = EvolutionManager(tmp_path)
        assert manager.get_iteration_count("SAMPLE", "DESIGN_ISSUE") == 0

    def test_get_iteration_count_after_evolve(self, tmp_path: Path) -> None:
        """evolve() 後は反復数が増加する."""
        from apps.migration_studio.evolution.manager import EvolutionManager

        manager = EvolutionManager(tmp_path)
        version_dir = tmp_path / "v1"
        version_dir.mkdir()

        manager.evolve("SAMPLE", "DESIGN_ISSUE", {"reason": "test"}, version_dir)
        assert manager.get_iteration_count("SAMPLE", "DESIGN_ISSUE") == 1

        manager.evolve("SAMPLE", "DESIGN_ISSUE", {"reason": "test2"}, version_dir)
        assert manager.get_iteration_count("SAMPLE", "DESIGN_ISSUE") == 2

    def test_reset_clears_iteration_count(self, tmp_path: Path) -> None:
        """reset() でプログラムの反復カウントがゼロに戻る."""
        from apps.migration_studio.evolution.manager import EvolutionManager

        manager = EvolutionManager(tmp_path)
        version_dir = tmp_path / "v1"
        version_dir.mkdir()

        manager.evolve("SAMPLE", "DESIGN_ISSUE", {"reason": "test"}, version_dir)
        assert manager.get_iteration_count("SAMPLE", "DESIGN_ISSUE") == 1

        manager.reset("SAMPLE")
        assert manager.get_iteration_count("SAMPLE", "DESIGN_ISSUE") == 0

    def test_accumulate_evolution_records(self, tmp_path: Path) -> None:
        """record_evolution() を複数回呼ぶと iterations が積み重なる."""
        import json
        from apps.migration_studio.evolution.manager import EvolutionManager

        manager = EvolutionManager(tmp_path)
        program_dir = tmp_path / "PROG"
        program_dir.mkdir()

        for i in range(1, 4):
            manager.record_evolution(
                {
                    "iteration": i,
                    "stage": "designer",
                    "decision": "DESIGN_ISSUE",
                    "fix_summary": f"fix {i}",
                    "evolved_at": "2026-01-01T00:00:00+00:00",
                },
                program_dir,
                program_name="PROG",
            )

        data = json.loads((program_dir / "evolution.json").read_text(encoding="utf-8"))
        assert data["total_iterations"] == 3
        assert len(data["iterations"]) == 3

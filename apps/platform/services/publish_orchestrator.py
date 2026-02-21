"""PublishOrchestrator - 一键发布オーケストレーター.

Validate → CodeGen → Deploy → Register のフローを統合。
3種モード（Studio/CLI/API）統一API。

使用例:
    >>> orchestrator = PublishOrchestrator()
    >>> # 一键发布
    >>> async for event in orchestrator.publish(request):
    ...     print(event)
"""

from __future__ import annotations

import asyncio
import logging
import uuid
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING, Any

from apps.platform.schemas.publish_schemas import (
    PublishEvent,
    PublishPhase,
    PublishRequest,
    PublishResponse,
    PublishStatus,
    PublishTarget,
)
from apps.platform.services.component_library import (
    ComponentEntry,
    ComponentLibrary,
    ComponentType,
    ComponentVisibility,
    get_component_library,
)

from agentflow.core.interfaces import (
    CodeOutputType,
    DeployTarget,
    GeneratedCode,
)
from agentflow.services.publish_service import PublishService


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


class PublishOrchestrator:
    """一键发布オーケストレーター.

    コンポーネントの検証、コード生成、デプロイ、登録を統合的に処理。
    """

    def __init__(
        self,
        publish_service: PublishService | None = None,
        component_library: ComponentLibrary | None = None,
    ) -> None:
        """初期化.

        Args:
            publish_service: 発布サービス
            component_library: コンポーネントライブラリ
        """
        self._publish_service = publish_service or PublishService()
        self._library = component_library or get_component_library()
        self._logger = logging.getLogger(__name__)

        # 進行中/完了済みの発布状態
        self._active_publishes: dict[str, PublishResponse] = {}

        # 発布イベント履歴（SSE再接続向け）
        self._event_history: dict[str, list[PublishEvent]] = {}

        # SSE購読キュー
        self._subscribers: dict[str, set[asyncio.Queue[PublishEvent | None]]] = {}

        # 実行中タスク
        self._publish_tasks: dict[str, asyncio.Task[None]] = {}

        # キャンセル要求
        self._cancel_requests: set[str] = set()

    async def publish(self, request: PublishRequest) -> AsyncIterator[PublishEvent]:
        """一键发布を実行してイベントを逐次返す.

        Args:
            request: 発布リクエスト

        Yields:
            発布イベント
        """
        publish_id = await self.start_publish(request)
        async for event in self.stream_events(publish_id):
            yield event

    async def start_publish(self, request: PublishRequest) -> str:
        """発布処理をバックグラウンド開始し publish_id を返す.

        Args:
            request: 発布リクエスト

        Returns:
            発布ID
        """
        publish_id = f"pub_{uuid.uuid4().hex[:12]}"
        start_time = datetime.now(UTC)

        response = PublishResponse(
            publish_id=publish_id,
            status=PublishStatus.PENDING,
            target=request.target,
            phases=[],
            started_at=start_time,
            progress=0.0,
            current_phase="",
        )
        self._active_publishes[publish_id] = response
        self._event_history[publish_id] = []
        self._subscribers.setdefault(publish_id, set())

        task = asyncio.create_task(
            self._run_publish_task(publish_id, request),
            name=f"platform-publish-{publish_id}",
        )
        self._publish_tasks[publish_id] = task
        task.add_done_callback(lambda _task, pid=publish_id: self._publish_tasks.pop(pid, None))

        return publish_id

    async def stream_events(
        self,
        publish_id: str,
        *,
        from_index: int = 0,
    ) -> AsyncIterator[PublishEvent]:
        """発布イベントを履歴+リアルタイムで購読.

        Args:
            publish_id: 発布ID
            from_index: 履歴の開始インデックス

        Yields:
            発布イベント

        Raises:
            KeyError: 発布ID が存在しない場合
        """
        if publish_id not in self._active_publishes:
            msg = f"Publish not found: {publish_id}"
            raise KeyError(msg)

        history = self._event_history.get(publish_id, [])
        safe_index = max(0, from_index)
        for event in history[safe_index:]:
            yield event

        current = self._active_publishes.get(publish_id)
        if current is None or self._is_terminal_status(current.status):
            return

        queue: asyncio.Queue[PublishEvent | None] = asyncio.Queue()
        self._subscribers.setdefault(publish_id, set()).add(queue)

        try:
            while True:
                item = await queue.get()
                if item is None:
                    break
                yield item
                if self._is_terminal_status(item.status):
                    break
        finally:
            subscribers = self._subscribers.get(publish_id)
            if subscribers is not None:
                subscribers.discard(queue)

    def get_publish_events(self, publish_id: str) -> list[PublishEvent]:
        """発布イベント履歴を取得.

        Args:
            publish_id: 発布ID

        Returns:
            イベント履歴
        """
        return list(self._event_history.get(publish_id, []))

    async def _run_publish_task(self, publish_id: str, request: PublishRequest) -> None:
        """発布タスク実行ラッパー.

        Args:
            publish_id: 発布ID
            request: 発布リクエスト
        """
        try:
            await self._execute_publish(publish_id, request)
        finally:
            self._notify_stream_done(publish_id)
            self._cancel_requests.discard(publish_id)

    async def _execute_publish(self, publish_id: str, request: PublishRequest) -> None:
        """発布の実処理.

        Args:
            publish_id: 発布ID
            request: 発布リクエスト
        """
        response = self._active_publishes[publish_id]

        try:
            if self._is_cancel_requested(publish_id):
                self._mark_cancelled(response, "Publish cancelled before start")
                return

            # Phase 1: Validate
            self._emit_event(
                response=response,
                event_type="phase_start",
                phase="validate",
                status=PublishStatus.VALIDATING,
                message="Validating component...",
                progress=0.0,
            )

            validation_result = await self._validate(request)
            if not validation_result["valid"]:
                self._mark_failed(
                    response=response,
                    phase="validate",
                    message=f"Validation failed: {validation_result['error']}",
                    details=validation_result,
                )
                return

            response.phases.append(
                PublishPhase(
                    name="validate",
                    status=PublishStatus.COMPLETED,
                    message="Validation passed",
                    progress=100.0,
                    completed_at=datetime.now(UTC),
                )
            )
            self._emit_event(
                response=response,
                event_type="phase_complete",
                phase="validate",
                status=PublishStatus.VALIDATING,
                message="Validation passed",
                progress=25.0,
            )

            if self._is_cancel_requested(publish_id):
                self._mark_cancelled(response, "Publish cancelled after validation")
                return

            # Phase 2: CodeGen (該当する場合)
            generated_code: GeneratedCode | None = None
            if self._needs_codegen(request):
                self._emit_event(
                    response=response,
                    event_type="phase_start",
                    phase="codegen",
                    status=PublishStatus.GENERATING,
                    message="Generating code...",
                    progress=25.0,
                )

                generated_code = await self._generate_code(request, validation_result)

                response.phases.append(
                    PublishPhase(
                        name="codegen",
                        status=PublishStatus.COMPLETED,
                        message=f"Generated {len(generated_code.files)} files",
                        progress=100.0,
                        completed_at=datetime.now(UTC),
                        details={
                            "entry_point": generated_code.entry_point,
                            "output_type": generated_code.output_type.value,
                        },
                    )
                )
                self._emit_event(
                    response=response,
                    event_type="phase_complete",
                    phase="codegen",
                    status=PublishStatus.GENERATING,
                    message="Code generation complete",
                    progress=50.0,
                )

                if self._is_cancel_requested(publish_id):
                    self._mark_cancelled(response, "Publish cancelled after code generation")
                    return

            # Phase 3: Deploy
            self._emit_event(
                response=response,
                event_type="phase_start",
                phase="deploy",
                status=PublishStatus.DEPLOYING,
                message="Deploying...",
                progress=50.0,
            )

            deploy_result = await self._deploy(request, generated_code)

            if not deploy_result["success"]:
                self._mark_failed(
                    response=response,
                    phase="deploy",
                    message=f"Deployment failed: {deploy_result.get('error', 'Unknown error')}",
                    details=deploy_result,
                )
                return

            response.deployment_id = deploy_result.get("deployment_id")
            response.deployment_url = deploy_result.get("url")
            response.phases.append(
                PublishPhase(
                    name="deploy",
                    status=PublishStatus.COMPLETED,
                    message="Deployment successful",
                    progress=100.0,
                    completed_at=datetime.now(UTC),
                    details=deploy_result,
                )
            )
            self._emit_event(
                response=response,
                event_type="phase_complete",
                phase="deploy",
                status=PublishStatus.DEPLOYING,
                message="Deployment successful",
                progress=75.0,
                data={"url": deploy_result.get("url")},
            )

            if self._is_cancel_requested(publish_id):
                self._mark_cancelled(response, "Publish cancelled after deployment")
                return

            # Phase 4: Register (Gallery に登録する場合)
            if request.publish_to_gallery:
                self._emit_event(
                    response=response,
                    event_type="phase_start",
                    phase="register",
                    status=PublishStatus.REGISTERING,
                    message="Registering to Gallery...",
                    progress=75.0,
                )

                register_result = await self._register_to_gallery(request, deploy_result)

                response.gallery_id = register_result.get("gallery_id")
                response.phases.append(
                    PublishPhase(
                        name="register",
                        status=PublishStatus.COMPLETED,
                        message="Registered to Gallery",
                        progress=100.0,
                        completed_at=datetime.now(UTC),
                    )
                )
                self._emit_event(
                    response=response,
                    event_type="phase_complete",
                    phase="register",
                    status=PublishStatus.REGISTERING,
                    message="Registered to Gallery",
                    progress=90.0,
                )

            # 完了
            response.completed_at = datetime.now(UTC)
            response.error = None
            response.error_details = None
            self._emit_event(
                response=response,
                event_type="complete",
                phase="",
                status=PublishStatus.COMPLETED,
                message="Publish completed successfully",
                progress=100.0,
                data={
                    "deployment_id": response.deployment_id,
                    "deployment_url": response.deployment_url,
                    "gallery_id": response.gallery_id,
                },
            )

        except asyncio.CancelledError:
            if not self._is_terminal_status(response.status):
                self._mark_cancelled(response, "Publish task cancelled")
            raise
        except Exception as exc:
            self._logger.exception("Publish failed: %s", exc)
            self._mark_failed(
                response=response,
                phase=response.current_phase,
                message=f"Publish failed: {exc}",
                details={"exception": str(exc)},
            )

    def _emit_event(
        self,
        *,
        response: PublishResponse,
        event_type: str,
        phase: str,
        status: PublishStatus,
        message: str,
        progress: float,
        data: dict[str, Any] | None = None,
    ) -> PublishEvent:
        """状態更新とイベント配信を一括実行.

        Args:
            response: 発布レスポンス
            event_type: イベントタイプ
            phase: フェーズ名
            status: ステータス
            message: メッセージ
            progress: 進捗率
            data: 追加データ

        Returns:
            生成したイベント
        """
        response.status = status
        response.current_phase = phase
        response.progress = progress
        response.logs.append(f"{datetime.now(UTC).isoformat()} [{phase or 'system'}] {message}")

        event = self._create_event(
            publish_id=response.publish_id,
            event_type=event_type,
            phase=phase,
            status=status,
            message=message,
            progress=progress,
            data=data,
        )

        history = self._event_history.setdefault(response.publish_id, [])
        history.append(event)

        subscribers = list(self._subscribers.get(response.publish_id, set()))
        for queue in subscribers:
            queue.put_nowait(event)

        return event

    def _mark_failed(
        self,
        *,
        response: PublishResponse,
        phase: str,
        message: str,
        details: dict[str, Any] | None = None,
    ) -> None:
        """失敗状態に更新してエラーイベント送信.

        Args:
            response: 発布レスポンス
            phase: フェーズ
            message: エラーメッセージ
            details: 詳細情報
        """
        response.completed_at = datetime.now(UTC)
        response.error = message
        response.error_details = details or {}
        self._emit_event(
            response=response,
            event_type="error",
            phase=phase,
            status=PublishStatus.FAILED,
            message=message,
            progress=response.progress,
            data=details,
        )

    def _mark_cancelled(self, response: PublishResponse, message: str) -> None:
        """キャンセル状態に更新してイベント送信.

        Args:
            response: 発布レスポンス
            message: メッセージ
        """
        if self._is_terminal_status(response.status):
            return

        response.completed_at = datetime.now(UTC)
        response.error = message
        self._emit_event(
            response=response,
            event_type="cancelled",
            phase=response.current_phase,
            status=PublishStatus.CANCELLED,
            message=message,
            progress=response.progress,
        )

    def _notify_stream_done(self, publish_id: str) -> None:
        """購読中キューに終了通知を送る.

        Args:
            publish_id: 発布ID
        """
        subscribers = self._subscribers.get(publish_id)
        if not subscribers:
            return

        for queue in list(subscribers):
            queue.put_nowait(None)

    def _is_cancel_requested(self, publish_id: str) -> bool:
        """キャンセル要求の有無を取得."""
        return publish_id in self._cancel_requests

    @staticmethod
    def _is_terminal_status(status: PublishStatus) -> bool:
        """終端ステータス判定."""
        return status in {
            PublishStatus.COMPLETED,
            PublishStatus.FAILED,
            PublishStatus.CANCELLED,
        }

    async def _validate(self, request: PublishRequest) -> dict[str, Any]:
        """コンポーネントを検証.

        Args:
            request: 発布リクエスト

        Returns:
            検証結果
        """
        # コンポーネントIDが指定されている場合
        if request.component_id:
            entry = self._library.get_component(request.component_id)
            if not entry:
                return {"valid": False, "error": f"Component not found: {request.component_id}"}

            # 依存関係チェック
            missing = self._library.validate_dependencies(entry)
            if missing:
                return {"valid": False, "error": f"Missing dependencies: {missing}"}

            return {"valid": True, "entry": entry.to_dict()}

        # ソースパスが指定されている場合
        if request.source_path:
            path = Path(request.source_path)
            if not path.exists():
                return {"valid": False, "error": f"Source path not found: {request.source_path}"}
            return {"valid": True, "source_path": str(path)}

        # ソースコードが指定されている場合
        if request.source_code:
            # 基本的な構文チェック（実際はより詳細な検証が必要）
            if not request.source_code.strip():
                return {"valid": False, "error": "Source code is empty"}
            return {"valid": True, "source_code": request.source_code}

        return {"valid": False, "error": "No source specified"}

    def _needs_codegen(self, request: PublishRequest) -> bool:
        """コード生成が必要かどうか判定.

        Args:
            request: 発布リクエスト

        Returns:
            コード生成が必要な場合 True
        """
        # Docker/Lambda/Vercel へのデプロイはコード生成が必要
        return request.target in {
            PublishTarget.DOCKER,
            PublishTarget.VERCEL,
            PublishTarget.AWS_LAMBDA,
        }

    async def _generate_code(
        self,
        request: PublishRequest,
        validation_result: dict[str, Any],
    ) -> GeneratedCode:
        """コードを生成.

        Args:
            request: 発布リクエスト
            validation_result: 検証結果

        Returns:
            生成結果
        """
        # WorkflowDefinition を構築
        workflow_data: dict[str, Any] = {
            "name": request.name or "generated-workflow",
            "description": request.description or "",
            "nodes": [],
            "edges": [],
        }

        # コンポーネントからワークフローを構築
        if "entry" in validation_result:
            entry_data = validation_result["entry"]
            workflow_data["name"] = entry_data.get("name", workflow_data["name"])
            workflow_data["description"] = entry_data.get("description", workflow_data["description"])

        # コード生成を実行
        output_type = self._get_output_type(request.target)
        return await self._publish_service.generate_code(
            workflow=workflow_data,
            output_type=output_type,
        )

    def _get_output_type(self, target: PublishTarget) -> CodeOutputType:
        """発布ターゲットから出力タイプを取得.

        Args:
            target: 発布ターゲット

        Returns:
            コード出力タイプ
        """
        mapping = {
            PublishTarget.DOCKER: CodeOutputType.FULLSTACK,
            PublishTarget.VERCEL: CodeOutputType.FULLSTACK,
            PublishTarget.AWS_LAMBDA: CodeOutputType.BACKEND,
            PublishTarget.GITHUB_ACTIONS: CodeOutputType.BACKEND,
            PublishTarget.LOCAL: CodeOutputType.BACKEND,
        }
        return mapping.get(target, CodeOutputType.BACKEND)

    def _get_deploy_target(self, target: PublishTarget) -> DeployTarget:
        """発布ターゲットからデプロイターゲットを取得.

        Args:
            target: 発布ターゲット

        Returns:
            デプロイターゲット
        """
        mapping = {
            PublishTarget.DOCKER: DeployTarget.DOCKER,
            PublishTarget.VERCEL: DeployTarget.VERCEL,
            PublishTarget.AWS_LAMBDA: DeployTarget.AWS_LAMBDA,
            PublishTarget.GITHUB_ACTIONS: DeployTarget.GITHUB_ACTIONS,
        }
        return mapping.get(target, DeployTarget.DOCKER)

    async def _deploy(
        self,
        request: PublishRequest,
        generated_code: GeneratedCode | None,
    ) -> dict[str, Any]:
        """デプロイを実行.

        Args:
            request: 発布リクエスト
            generated_code: 生成されたコード

        Returns:
            デプロイ結果
        """
        # Local の場合は特別処理
        if request.target == PublishTarget.LOCAL:
            return {
                "success": True,
                "deployment_id": f"local_{uuid.uuid4().hex[:8]}",
                "url": None,
            }

        # Gallery のみの場合
        if request.target == PublishTarget.GALLERY:
            return {
                "success": True,
                "deployment_id": f"gallery_{uuid.uuid4().hex[:8]}",
                "url": None,
            }

        source: GeneratedCode | Path
        if request.source_path:
            source = Path(request.source_path)
        elif generated_code is not None:
            source = generated_code
        else:
            return {
                "success": False,
                "error": "No deployment source available",
            }

        # 実際のデプロイ
        deploy_target = self._get_deploy_target(request.target)
        config = request.config.copy()
        config["env_vars"] = request.env_vars

        try:
            result = await self._publish_service.deploy_sync(
                source=source,
                target=deploy_target,
                config=config,
            )
            return {
                "success": result.success,
                "deployment_id": result.deployment_id,
                "url": result.url,
                "logs": result.logs,
                "error": result.error,
            }
        except Exception as exc:
            return {
                "success": False,
                "error": str(exc),
            }

    async def _register_to_gallery(
        self,
        request: PublishRequest,
        deploy_result: dict[str, Any],
    ) -> dict[str, Any]:
        """Galleryに登録.

        Args:
            request: 発布リクエスト
            deploy_result: デプロイ結果

        Returns:
            登録結果
        """
        # コンポーネントエントリを作成または更新
        visibility_map = {
            "private": ComponentVisibility.PRIVATE,
            "tenant": ComponentVisibility.TENANT,
            "public": ComponentVisibility.PUBLIC,
        }
        visibility = visibility_map.get(request.gallery_visibility, ComponentVisibility.PRIVATE)

        entry = ComponentEntry(
            id=request.component_id
            or self._library.generate_id(
                request.name or "published-component",
                ComponentType.AGENT,
            ),
            name=request.name or "Published Component",
            type=ComponentType.AGENT,
            version=request.version or "1.0.0",
            description=request.description or "",
            visibility=visibility,
            source_path=request.source_path,
            metadata={
                "deployment_url": deploy_result.get("url"),
                "deployment_id": deploy_result.get("deployment_id"),
                "published_at": datetime.now(UTC).isoformat(),
            },
        )

        self._library.register(entry, overwrite=True)

        return {"gallery_id": entry.id}

    def _create_event(
        self,
        publish_id: str,
        event_type: str,
        phase: str,
        status: PublishStatus,
        message: str,
        progress: float,
        data: dict[str, Any] | None = None,
    ) -> PublishEvent:
        """発布イベントを作成.

        Args:
            publish_id: 発布ID
            event_type: イベントタイプ
            phase: フェーズ名
            status: ステータス
            message: メッセージ
            progress: 進捗率
            data: 追加データ

        Returns:
            発布イベント
        """
        return PublishEvent(
            publish_id=publish_id,
            event_type=event_type,
            phase=phase,
            status=status,
            message=message,
            progress=progress,
            timestamp=datetime.now(UTC),
            data=data or {},
        )

    def get_publish_status(self, publish_id: str) -> PublishResponse | None:
        """発布ステータスを取得.

        Args:
            publish_id: 発布ID

        Returns:
            発布レスポンス（存在しない場合 None）
        """
        return self._active_publishes.get(publish_id)

    async def cancel_publish(self, publish_id: str) -> bool:
        """発布をキャンセル.

        Args:
            publish_id: 発布ID

        Returns:
            キャンセル成功の場合 True
        """
        response = self._active_publishes.get(publish_id)
        if response is None:
            return False

        if self._is_terminal_status(response.status):
            return False

        self._cancel_requests.add(publish_id)
        self._mark_cancelled(response, "Publish cancelled by user")

        task = self._publish_tasks.get(publish_id)
        if task and not task.done():
            task.cancel()

        return True


__all__ = ["PublishOrchestrator"]

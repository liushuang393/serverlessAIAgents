# -*- coding: utf-8 -*-
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

import logging
import time
import uuid
from collections.abc import AsyncIterator
from datetime import datetime, UTC
from pathlib import Path
from typing import Any

from agentflow.services.publish_service import PublishService
from agentflow.core.interfaces import (
    CodeOutputType,
    DeployTarget,
    WorkflowDefinition,
)
from apps.platform.services.component_library import (
    ComponentLibrary,
    ComponentEntry,
    ComponentType,
    ComponentVisibility,
    get_component_library,
)
from apps.platform.schemas.publish_schemas import (
    PublishRequest,
    PublishResponse,
    PublishEvent,
    PublishPhase,
    PublishStatus,
    PublishTarget,
)


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

        # 進行中の発布を追跡
        self._active_publishes: dict[str, PublishResponse] = {}

    async def publish(
        self,
        request: PublishRequest,
    ) -> AsyncIterator[PublishEvent]:
        """一键发布を実行.

        フロー: Validate → CodeGen → Deploy → Register

        Args:
            request: 発布リクエスト

        Yields:
            発布イベント
        """
        publish_id = f"pub_{uuid.uuid4().hex[:12]}"
        start_time = datetime.now(UTC)

        # 初期レスポンスを作成
        response = PublishResponse(
            publish_id=publish_id,
            status=PublishStatus.PENDING,
            target=request.target,
            phases=[],
            started_at=start_time,
        )
        self._active_publishes[publish_id] = response

        try:
            # Phase 1: Validate
            yield self._create_event(
                publish_id, "phase_start", "validate", PublishStatus.VALIDATING,
                "Validating component...", 0.0
            )

            validation_result = await self._validate(request)
            if not validation_result["valid"]:
                yield self._create_event(
                    publish_id, "error", "validate", PublishStatus.FAILED,
                    f"Validation failed: {validation_result['error']}", 0.0,
                    data=validation_result
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
            yield self._create_event(
                publish_id, "phase_complete", "validate", PublishStatus.VALIDATING,
                "Validation passed", 25.0
            )

            # Phase 2: CodeGen (該当する場合)
            generated_code = None
            if self._needs_codegen(request):
                yield self._create_event(
                    publish_id, "phase_start", "codegen", PublishStatus.GENERATING,
                    "Generating code...", 25.0
                )

                generated_code = await self._generate_code(request, validation_result)

                response.phases.append(
                    PublishPhase(
                        name="codegen",
                        status=PublishStatus.COMPLETED,
                        message=f"Generated {len(generated_code.get('files', []))} files",
                        progress=100.0,
                        completed_at=datetime.now(UTC),
                    )
                )
                yield self._create_event(
                    publish_id, "phase_complete", "codegen", PublishStatus.GENERATING,
                    "Code generation complete", 50.0
                )

            # Phase 3: Deploy
            yield self._create_event(
                publish_id, "phase_start", "deploy", PublishStatus.DEPLOYING,
                "Deploying...", 50.0
            )

            deploy_result = await self._deploy(request, generated_code)

            if not deploy_result["success"]:
                yield self._create_event(
                    publish_id, "error", "deploy", PublishStatus.FAILED,
                    f"Deployment failed: {deploy_result.get('error', 'Unknown error')}", 50.0,
                    data=deploy_result
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
            yield self._create_event(
                publish_id, "phase_complete", "deploy", PublishStatus.DEPLOYING,
                "Deployment successful", 75.0,
                data={"url": deploy_result.get("url")}
            )

            # Phase 4: Register (Gallery に登録する場合)
            if request.publish_to_gallery:
                yield self._create_event(
                    publish_id, "phase_start", "register", PublishStatus.REGISTERING,
                    "Registering to Gallery...", 75.0
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
                yield self._create_event(
                    publish_id, "phase_complete", "register", PublishStatus.REGISTERING,
                    "Registered to Gallery", 90.0
                )

            # 完了
            response.status = PublishStatus.COMPLETED
            response.completed_at = datetime.now(UTC)
            response.progress = 100.0

            yield self._create_event(
                publish_id, "complete", "", PublishStatus.COMPLETED,
                "Publish completed successfully", 100.0,
                data={
                    "deployment_id": response.deployment_id,
                    "deployment_url": response.deployment_url,
                    "gallery_id": response.gallery_id,
                }
            )

        except Exception as e:
            self._logger.exception(f"Publish failed: {e}")
            response.status = PublishStatus.FAILED
            response.error = str(e)
            response.completed_at = datetime.now(UTC)

            yield self._create_event(
                publish_id, "error", "", PublishStatus.FAILED,
                f"Publish failed: {e}", response.progress
            )

        finally:
            # クリーンアップ（一定時間後に削除）
            pass

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
        return request.target in [
            PublishTarget.DOCKER,
            PublishTarget.VERCEL,
            PublishTarget.AWS_LAMBDA,
        ]

    async def _generate_code(
        self,
        request: PublishRequest,
        validation_result: dict[str, Any],
    ) -> dict[str, Any]:
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
        code = await self._publish_service.generate_code(
            workflow=workflow_data,
            output_type=output_type,
        )

        return {
            "files": [{"path": f.path, "content": f.content} for f in code.files],
            "main_file": code.main_file,
        }

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
        generated_code: dict[str, Any] | None,
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

        # 実際のデプロイ
        deploy_target = self._get_deploy_target(request.target)
        config = request.config.copy()
        config["env_vars"] = request.env_vars

        try:
            result = await self._publish_service.deploy_sync(
                source=Path(request.source_path) if request.source_path else generated_code,
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
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
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
            id=request.component_id or self._library.generate_id(
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
        if not response:
            return False

        if response.status in [PublishStatus.COMPLETED, PublishStatus.FAILED, PublishStatus.CANCELLED]:
            return False

        response.status = PublishStatus.CANCELLED
        response.completed_at = datetime.now(UTC)
        return True


__all__ = ["PublishOrchestrator"]

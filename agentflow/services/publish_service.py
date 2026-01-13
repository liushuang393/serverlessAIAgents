# -*- coding: utf-8 -*-
"""PublishService - 統一発布サービス.

コード生成とデプロイを統合したサービス層。
Studio / CLI / API 全てが使用します。
"""

from __future__ import annotations

import logging
from collections.abc import AsyncIterator
from io import BytesIO
from pathlib import Path
from typing import Any

from agentflow.core.interfaces import (
    CodeGenOptions,
    CodeOutputType,
    ConfigField,
    DeployConfig,
    DeployEvent,
    DeployResult,
    DeployTarget,
    FilePreview,
    GeneratedCode,
    ValidationResult,
    WorkflowDefinition,
)
from agentflow.codegen import CodeGenerator
from agentflow.deploy.executor import DeployExecutor
from agentflow.deploy.config.manager import ConfigManager

logger = logging.getLogger(__name__)


class PublishService:
    """発布サービス.

    コード生成からデプロイまでの全フローを統合的に処理します。

    使用例:
        >>> service = PublishService()
        >>> # コード生成
        >>> code = await service.generate_code(workflow, CodeOutputType.FULLSTACK)
        >>> # ZIP エクスポート
        >>> zip_file = await service.export_zip(workflow, CodeOutputType.BACKEND)
        >>> # 設定フィールド取得（UI 用）
        >>> fields = await service.get_config_fields(DeployTarget.VERCEL)
        >>> # デプロイ
        >>> async for event in service.deploy(code, DeployTarget.VERCEL, config):
        ...     print(event.message)
    """

    def __init__(self) -> None:
        """初期化."""
        self._code_generator = CodeGenerator()
        self._deploy_executor = DeployExecutor()
        self._config_manager = ConfigManager()

    # =========================================================================
    # Code Generation
    # =========================================================================

    async def generate_code(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> GeneratedCode:
        """コードを生成.

        Args:
            workflow: ワークフロー定義
            output_type: 出力タイプ
            options: 生成オプション

        Returns:
            生成されたコード
        """
        if isinstance(workflow, dict):
            workflow = WorkflowDefinition.from_dict(workflow)

        return await self._code_generator.generate(workflow, output_type, options)

    async def preview_code(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
    ) -> dict[str, FilePreview]:
        """生成されるコードをプレビュー.

        Args:
            workflow: ワークフロー定義
            output_type: 出力タイプ

        Returns:
            ファイルプレビューのマップ
        """
        if isinstance(workflow, dict):
            workflow = WorkflowDefinition.from_dict(workflow)

        return await self._code_generator.preview(workflow, output_type)

    async def export_zip(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> BytesIO:
        """ZIP ファイルとしてエクスポート.

        Args:
            workflow: ワークフロー定義
            output_type: 出力タイプ
            options: 生成オプション

        Returns:
            ZIP ファイルの BytesIO
        """
        if isinstance(workflow, dict):
            workflow = WorkflowDefinition.from_dict(workflow)

        return await self._code_generator.export_zip(workflow, output_type, options)

    def get_supported_output_types(self) -> list[dict[str, Any]]:
        """サポートされている出力タイプを取得.

        Returns:
            出力タイプの情報リスト
        """
        return [
            {
                "id": CodeOutputType.FRONTEND.value,
                "name": "Frontend",
                "description": "React アプリケーション",
                "icon": "⚛️",
            },
            {
                "id": CodeOutputType.BACKEND.value,
                "name": "Backend",
                "description": "FastAPI アプリケーション",
                "icon": "🐍",
            },
            {
                "id": CodeOutputType.FULLSTACK.value,
                "name": "Fullstack",
                "description": "React + FastAPI 完全アプリ",
                "icon": "🚀",
            },
        ]

    # =========================================================================
    # Deploy Configuration
    # =========================================================================

    async def get_config_fields(
        self,
        target: DeployTarget,
        current_config: dict[str, Any] | None = None,
    ) -> list[ConfigField]:
        """ターゲットに必要な設定フィールドを取得.

        UI でフォームを動的に生成するために使用します。

        Args:
            target: デプロイターゲット
            current_config: 現在の設定

        Returns:
            設定フィールドのリスト
        """
        return await self._config_manager.get_required_fields(target, current_config)

    async def validate_config(
        self,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> ValidationResult:
        """設定を検証.

        Args:
            target: デプロイターゲット
            config: 検証する設定

        Returns:
            検証結果
        """
        return await self._config_manager.validate(target, config)

    def get_supported_targets(self) -> list[dict[str, Any]]:
        """サポートされているデプロイターゲットを取得.

        Returns:
            ターゲット情報のリスト
        """
        return [
            {
                "id": DeployTarget.VERCEL.value,
                "name": "Vercel",
                "description": "Serverless Functions にデプロイ",
                "icon": "▲",
                "supports_direct_deploy": True,
            },
            {
                "id": DeployTarget.DOCKER.value,
                "name": "Docker",
                "description": "Docker イメージをビルド＆プッシュ",
                "icon": "🐳",
                "supports_direct_deploy": True,
            },
            {
                "id": DeployTarget.AWS_LAMBDA.value,
                "name": "AWS Lambda",
                "description": "Lambda Functions にデプロイ",
                "icon": "λ",
                "supports_direct_deploy": True,
            },
            {
                "id": DeployTarget.GITHUB_ACTIONS.value,
                "name": "GitHub Actions",
                "description": "CI/CD ワークフローを生成",
                "icon": "🔧",
                "supports_direct_deploy": False,
            },
        ]

    # =========================================================================
    # Deployment
    # =========================================================================

    async def deploy(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> AsyncIterator[DeployEvent]:
        """デプロイを実行.

        Args:
            source: 生成されたコード、またはソースパス
            target: デプロイターゲット
            config: デプロイ設定

        Yields:
            デプロイイベント
        """
        # 設定を検証
        validation = await self.validate_config(target, config)
        if not validation.valid:
            yield DeployEvent(
                type="error",
                message=f"Invalid config: {validation.errors}",
            )
            return

        # DeployConfig を構築
        deploy_config = DeployConfig(
            target=target,
            credentials={
                k: v for k, v in config.items()
                if k in ("token", "username", "password", "aws_access_key_id", "aws_secret_access_key")
            },
            settings={
                k: v for k, v in config.items()
                if k not in ("token", "username", "password", "aws_access_key_id", "aws_secret_access_key")
            },
            env_vars=config.get("env_vars", {}),
        )

        async for event in self._deploy_executor.deploy(source, target, deploy_config):
            yield event

    async def deploy_sync(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> DeployResult:
        """デプロイを同期実行.

        Args:
            source: 生成されたコード、またはソースパス
            target: デプロイターゲット
            config: デプロイ設定

        Returns:
            デプロイ結果
        """
        logs: list[str] = []
        result = DeployResult(success=False)

        async for event in self.deploy(source, target, config):
            logs.append(event.message)

            if event.type == "success":
                result = DeployResult(
                    success=True,
                    deployment_id=event.data.get("deployment_id") if event.data else None,
                    url=event.data.get("url") if event.data else None,
                    logs=logs,
                )
            elif event.type == "error":
                result = DeployResult(
                    success=False,
                    logs=logs,
                    error=event.message,
                )

        return result

    # =========================================================================
    # Full Workflow: Generate + Deploy
    # =========================================================================

    async def publish(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
        target: DeployTarget,
        config: dict[str, Any],
        options: CodeGenOptions | None = None,
    ) -> AsyncIterator[DeployEvent]:
        """ワークフローをコード生成してデプロイ（フルフロー）.

        Args:
            workflow: ワークフロー定義
            output_type: 出力タイプ
            target: デプロイターゲット
            config: デプロイ設定
            options: コード生成オプション

        Yields:
            デプロイイベント
        """
        # Phase 1: コード生成
        yield DeployEvent(
            type="progress",
            message="Generating code...",
            progress=10,
            phase="codegen",
        )

        try:
            code = await self.generate_code(workflow, output_type, options)
            yield DeployEvent(
                type="progress",
                message=f"Generated {len(code.files)} files",
                progress=30,
                phase="codegen_complete",
            )
        except Exception as e:
            yield DeployEvent(
                type="error",
                message=f"Code generation failed: {e}",
            )
            return

        # Phase 2: デプロイ
        async for event in self.deploy(code, target, config):
            # 進捗を調整（30-100）
            if event.progress is not None:
                event.progress = 30 + (event.progress * 0.7)
            yield event


__all__ = ["PublishService"]

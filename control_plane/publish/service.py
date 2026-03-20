"""Control-plane publish services and orchestration primitives."""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from contracts.core import (
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


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from io import BytesIO
    from pathlib import Path

    from contracts.interfaces.publish import ICodeGenerator, IConfigManager, IDeployExecutor


logger = logging.getLogger(__name__)


@dataclass(frozen=True, slots=True)
class PublishRequest:
    """Minimal publish summary contract used by control-plane APIs."""

    app_name: str
    target: str
    metadata: dict[str, Any] = field(default_factory=dict)


class PublishService:
    """Canonical publish service owned by control_plane."""

    def __init__(
        self,
        code_generator: ICodeGenerator | None = None,
        deploy_executor: IDeployExecutor | None = None,
        config_manager: IConfigManager | None = None,
    ) -> None:
        self._code_generator = code_generator
        self._deploy_executor = deploy_executor
        self._config_manager = config_manager

    def build_summary(self, request: PublishRequest) -> dict[str, Any]:
        """Return a minimal summary for a publish request."""
        return {
            "app_name": request.app_name,
            "target": request.target,
            "metadata": request.metadata,
        }

    def _get_code_generator(self) -> ICodeGenerator:
        if self._code_generator is None:
            from apps.dev_studio.codegen import CodeGenerator

            self._code_generator = CodeGenerator()
        return self._code_generator

    def _get_deploy_executor(self) -> IDeployExecutor:
        if self._deploy_executor is None:
            from control_plane.deploy.executor import DeployExecutor

            self._deploy_executor = DeployExecutor()
        return self._deploy_executor

    def _get_config_manager(self) -> IConfigManager:
        if self._config_manager is None:
            from control_plane.deploy.config.manager import ConfigManager

            self._config_manager = ConfigManager()
        return self._config_manager

    async def generate_code(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> GeneratedCode:
        """Generate code from a workflow definition."""
        if isinstance(workflow, dict):
            workflow = WorkflowDefinition.from_dict(workflow)
        return await self._get_code_generator().generate(workflow, output_type, options)

    async def preview_code(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
    ) -> dict[str, FilePreview]:
        """Preview generated files."""
        if isinstance(workflow, dict):
            workflow = WorkflowDefinition.from_dict(workflow)
        return await self._get_code_generator().preview(workflow, output_type)

    async def export_zip(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> BytesIO:
        """Export generated code as a ZIP archive."""
        if isinstance(workflow, dict):
            workflow = WorkflowDefinition.from_dict(workflow)
        return await self._get_code_generator().export_zip(workflow, output_type, options)

    def get_supported_output_types(self) -> list[dict[str, Any]]:
        """List supported code output types."""
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

    async def get_config_fields(
        self,
        target: DeployTarget,
        current_config: dict[str, Any] | None = None,
    ) -> list[ConfigField]:
        """Return required config fields for a deploy target."""
        return await self._get_config_manager().get_required_fields(target, current_config)

    async def validate_config(
        self,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> ValidationResult:
        """Validate deploy configuration."""
        return await self._get_config_manager().validate(target, config)

    def get_supported_targets(self) -> list[dict[str, Any]]:
        """List supported deployment targets."""
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

    async def deploy(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> AsyncIterator[DeployEvent]:
        """Deploy generated code or a source path."""
        validation = await self.validate_config(target, config)
        if not validation.valid:
            yield DeployEvent(type="error", message=f"Invalid config: {validation.errors}")
            return

        deploy_config = DeployConfig(
            target=target,
            credentials={
                key: value
                for key, value in config.items()
                if key in ("token", "username", "password", "aws_access_key_id", "aws_secret_access_key")
            },
            settings={
                key: value
                for key, value in config.items()
                if key
                not in (
                    "token",
                    "username",
                    "password",
                    "aws_access_key_id",
                    "aws_secret_access_key",
                )
            },
            env_vars=config.get("env_vars", {}),
        )

        async for event in self._get_deploy_executor().deploy(source, target, deploy_config):
            yield event

    async def deploy_sync(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> DeployResult:
        """Collect streamed deploy events into a final result."""
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
                result = DeployResult(success=False, logs=logs, error=event.message)

        return result

    async def publish(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
        target: DeployTarget,
        config: dict[str, Any],
        options: CodeGenOptions | None = None,
    ) -> AsyncIterator[DeployEvent]:
        """Generate code and deploy it as one pipeline."""
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
        except Exception as exc:
            yield DeployEvent(type="error", message=f"Code generation failed: {exc}")
            return

        async for event in self.deploy(code, target, config):
            if event.progress is not None:
                event.progress = 30 + (event.progress * 0.7)
            yield event


__all__ = ["PublishRequest", "PublishService"]

"""Compatibility wrapper for the canonical control-plane publish service."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from io import BytesIO
    from pathlib import Path

    from contracts.core import (
        CodeGenOptions,
        CodeOutputType,
        ConfigField,
        DeployEvent,
        DeployResult,
        DeployTarget,
        FilePreview,
        GeneratedCode,
        ValidationResult,
        WorkflowDefinition,
    )
    from contracts.interfaces.publish import ICodeGenerator, IConfigManager, IDeployExecutor
    from control_plane.publish.service import PublishRequest
    from control_plane.publish.service import PublishService as ControlPlanePublishService


class PublishService:
    """Lazy compatibility adapter that forwards to control_plane.publish.service."""

    def __init__(
        self,
        code_generator: ICodeGenerator | None = None,
        deploy_executor: IDeployExecutor | None = None,
        config_manager: IConfigManager | None = None,
    ) -> None:
        self._code_generator = code_generator
        self._deploy_executor = deploy_executor
        self._config_manager = config_manager
        self._impl: ControlPlanePublishService | None = None

    def _get_impl(self) -> ControlPlanePublishService:
        if self._impl is None:
            from control_plane.publish.service import PublishService as ControlPlanePublishService

            self._impl = ControlPlanePublishService(
                code_generator=self._code_generator,
                deploy_executor=self._deploy_executor,
                config_manager=self._config_manager,
            )
        return self._impl

    def build_summary(self, request: PublishRequest) -> dict[str, Any]:
        """Forward summary building to the canonical publish service."""
        return self._get_impl().build_summary(request)

    async def generate_code(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> GeneratedCode:
        """Forward code generation to the canonical publish service."""
        return await self._get_impl().generate_code(workflow, output_type, options)

    async def preview_code(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
    ) -> dict[str, FilePreview]:
        """Forward preview generation to the canonical publish service."""
        return await self._get_impl().preview_code(workflow, output_type)

    async def export_zip(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> BytesIO:
        """Forward ZIP export to the canonical publish service."""
        return await self._get_impl().export_zip(workflow, output_type, options)

    def get_supported_output_types(self) -> list[dict[str, Any]]:
        """Forward output type discovery to the canonical publish service."""
        return self._get_impl().get_supported_output_types()

    async def get_config_fields(
        self,
        target: DeployTarget,
        current_config: dict[str, Any] | None = None,
    ) -> list[ConfigField]:
        """Forward config field resolution to the canonical publish service."""
        return await self._get_impl().get_config_fields(target, current_config)

    async def validate_config(
        self,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> ValidationResult:
        """Forward config validation to the canonical publish service."""
        return await self._get_impl().validate_config(target, config)

    def get_supported_targets(self) -> list[dict[str, Any]]:
        """Forward target discovery to the canonical publish service."""
        return self._get_impl().get_supported_targets()

    async def deploy(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> AsyncIterator[DeployEvent]:
        """Forward deployment to the canonical publish service."""
        async for event in self._get_impl().deploy(source, target, config):
            yield event

    async def deploy_sync(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> DeployResult:
        """Forward synchronous deployment to the canonical publish service."""
        return await self._get_impl().deploy_sync(source, target, config)

    async def publish(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        output_type: CodeOutputType,
        target: DeployTarget,
        config: dict[str, Any],
        options: CodeGenOptions | None = None,
    ) -> AsyncIterator[DeployEvent]:
        """Forward full publish workflow to the canonical publish service."""
        async for event in self._get_impl().publish(workflow, output_type, target, config, options):
            yield event


__all__ = ["PublishService"]

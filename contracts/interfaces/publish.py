"""発布サービス用 Protocol 定義.

publish_service が具象クラスに直接依存しないよう、
Protocol を contracts 層に配置する。

NOTE: 現在の正規定義は kernel/core/interfaces/ にある。
contracts 層に段階的に集約するため、ここから re-export する。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from io import BytesIO
    from pathlib import Path

    from contracts.core import (
        CodeGenOptions,
        CodeOutputType,
        ConfigField,
        DeployConfig,
        DeployEvent,
        DeployTarget,
        FilePreview,
        GeneratedCode,
        ValidationResult,
        WorkflowDefinition,
    )


@runtime_checkable
class ICodeGenerator(Protocol):
    """コード生成器インターフェース."""

    async def generate(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> GeneratedCode: ...

    async def preview(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
    ) -> dict[str, FilePreview]: ...

    async def export_zip(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> BytesIO: ...


@runtime_checkable
class IDeployExecutor(Protocol):
    """デプロイ執行器インターフェース."""

    async def deploy(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]: ...


@runtime_checkable
class IConfigManager(Protocol):
    """設定管理器インターフェース."""

    async def get_required_fields(
        self,
        target: DeployTarget,
        current_config: dict[str, Any] | None = None,
    ) -> list[ConfigField]: ...

    async def validate(
        self,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> ValidationResult: ...


__all__ = [
    "ICodeGenerator",
    "IConfigManager",
    "IDeployExecutor",
]

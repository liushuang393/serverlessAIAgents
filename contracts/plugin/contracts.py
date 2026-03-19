"""Plugin 契約."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Protocol, runtime_checkable

from pydantic import Field, field_validator

from contracts.base import ContractModel


class PluginBinding(ContractModel):
    """app_config.json plugin_bindings の canonical 契約."""

    id: str = Field(..., min_length=1, max_length=120, description="プラグイン ID")
    version: str = Field(..., min_length=1, max_length=50, description="要求バージョン")
    config: dict[str, Any] = Field(default_factory=dict, description="プラグイン設定")

    @field_validator("id", mode="before")
    @classmethod
    def normalize_plugin_id(cls, value: str) -> str:
        """plugin id を正規化する."""
        text = str(value).strip().lower()
        if not text:
            msg = "plugin id は空文字を許可しません"
            raise ValueError(msg)
        return text


class PluginDescriptor(ContractModel):
    """plugin manifest の canonical 契約."""

    id: str = Field(..., description="プラグイン ID")
    version: str = Field(..., description="プラグインバージョン")
    risk_tier: str = Field(..., description="リスク階層")
    compatibility_kernel: str = Field(default="", description="kernel 互換制約")
    compatibility_product_lines: list[str] = Field(default_factory=list, description="互換 product_line")
    manifest_path: Path | None = Field(default=None, description="manifest パス")
    signature_status: str | None = Field(default=None, description="署名検証ステータス")
    signature_reason: str | None = Field(default=None, description="署名検証メッセージ")
    required_permissions: list[str] = Field(default_factory=list, description="必要権限")
    raw: dict[str, Any] = Field(default_factory=dict, description="元 manifest")


class PluginRuntimeAssessment(ContractModel):
    """ツール実行時 plugin 評価結果の canonical 契約."""

    app_name: str | None = Field(default=None, description="App 名")
    product_line: str = Field(default="framework", description="product_line")
    strict_mode: bool = Field(default=False, description="strict 判定かどうか")
    is_side_effect_tool: bool = Field(default=False, description="副作用ツールかどうか")
    plugin_id: str | None = Field(default=None, description="plugin ID")
    plugin_version: str | None = Field(default=None, description="plugin バージョン")
    plugin_risk_tier: str | None = Field(default=None, description="plugin リスク階層")
    plugin_signature_status: str | None = Field(default=None, description="署名検証ステータス")
    plugin_signature_reason: str | None = Field(default=None, description="署名検証メッセージ")
    manifest_required_permissions: list[str] = Field(default_factory=list, description="manifest 必須権限")
    manifest: PluginDescriptor | None = Field(default=None, description="manifest 契約")
    binding: PluginBinding | None = Field(default=None, description="binding 契約")
    errors: list[str] = Field(default_factory=list, description="検出エラー")
    warnings: list[str] = Field(default_factory=list, description="検出警告")


@runtime_checkable
class PluginRegistryProtocol(Protocol):
    """plugin registry の duck-type 契約."""

    def refresh(self) -> None:
        """manifest と app snapshot のキャッシュを再読込する."""
        ...

    def get_manifest(self, plugin_id: str) -> PluginDescriptor | None:
        """manifest を取得する."""
        ...

    def evaluate_tool(
        self,
        tool: Any,
        context: Any,
    ) -> PluginRuntimeAssessment:
        """ツール実行時の plugin 評価を行う."""
        ...

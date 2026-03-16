"""App / Deploy 契約."""

from __future__ import annotations

from typing import Any

from pydantic import Field

from contracts.base import ContractModel


class AppManifest(ContractModel):
    """Control Plane と App が共有する manifest 契約."""

    name: str = Field(..., description="アプリ識別子")
    display_name: str = Field(..., description="表示名")
    product_line: str = Field(..., description="製品主線")
    surface_profile: str = Field(..., description="公開面プロファイル")
    audit_profile: str = Field(..., description="監査プロファイル")
    version: str = Field(default="1.0.0", description="バージョン")
    entry_points: dict[str, Any] = Field(default_factory=dict, description="起動エントリ")
    ports: dict[str, int | None] = Field(default_factory=dict, description="ポート設定")
    runtime: dict[str, Any] = Field(default_factory=dict, description="ランタイム設定")
    tags: list[str] = Field(default_factory=list, description="タグ")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")


class DeploymentSpec(ContractModel):
    """配備要求の標準契約."""

    deployment_id: str = Field(..., description="デプロイ識別子")
    app_name: str = Field(..., description="対象アプリ")
    target: str = Field(..., description="配備先")
    version: str = Field(..., description="配備バージョン")
    config: dict[str, Any] = Field(default_factory=dict, description="配備設定")

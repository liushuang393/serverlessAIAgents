"""六層分離で共有する基本契約モデル。"""

from __future__ import annotations

from enum import Enum
from typing import Any

from pydantic import BaseModel, ConfigDict, Field


class ContractModel(BaseModel):
    """後方互換を前提にした共通契約基底。"""

    contract_version: str = Field(default="v1", description="契約バージョン")
    model_config = ConfigDict(extra="allow", populate_by_name=True)

    def to_payload(self) -> dict[str, Any]:
        """JSON 送受信用の辞書へ変換する。"""
        return self.model_dump(mode="json", exclude_none=True)


# ---------------------------------------------------------------------------
# コンポーネント契約型（旧 contracts/runtime + shared/registry から統合）
# ---------------------------------------------------------------------------


class LayerName(str, Enum):
    """六層の論理名。"""

    CONTRACTS = "contracts"
    INFRASTRUCTURE = "infrastructure"
    SHARED = "shared"
    KERNEL = "kernel"
    HARNESS = "harness"
    PLATFORM = "platform"
    APPS = "apps"


class ComponentToggle(ContractModel):
    """個別コンポーネントの有効化設定。"""

    enabled: bool = Field(default=True, description="有効フラグ")
    implementation: str = Field(default="default", description="優先実装名")
    fallback_implementation: str | None = Field(
        default="noop",
        description="無効時または解決失敗時のフォールバック実装名",
    )
    config: dict[str, Any] = Field(default_factory=dict, description="実装固有設定")


class ComponentSpec(ContractModel):
    """登録対象コンポーネントのメタデータ。"""

    name: str = Field(..., description="論理コンポーネント名")
    layer: LayerName = Field(..., description="所属層")
    implementation: str = Field(default="default", description="実装識別子")
    description: str = Field(default="", description="説明")
    tags: list[str] = Field(default_factory=list, description="検索用タグ")
    toggle: ComponentToggle = Field(default_factory=ComponentToggle, description="既定トグル")

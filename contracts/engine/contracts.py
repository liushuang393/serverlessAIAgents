"""エンジンレジストリ契約 — プラガブルエンジンパターン用。"""

from __future__ import annotations

from pydantic import Field

from contracts.base import ContractModel


class EngineManifest(ContractModel):
    """カスタムエンジンパターンのマニフェスト。"""

    contract_version: str = "1.0.0"

    name: str = Field(..., description="エンジンパターン名（一意）")
    display_name: str = Field("", description="表示用名称")
    description: str = Field("", description="エンジンの説明")

    module: str = Field(..., description="Python モジュールパス")
    class_name: str = Field(..., description="エンジンクラス名")

    # 分類
    pattern_type: str = Field(
        "custom",
        description="パターン種別: simple, pipeline, gate, rag, pev, custom",
    )

    # 機能フラグ
    supports_streaming: bool = Field(True, description="ストリーミング対応")
    supports_hitl: bool = Field(False, description="HITL（人間参加）対応")
    supports_review: bool = Field(False, description="レビューループ対応")

    enabled: bool = Field(True, description="エンジンが有効かどうか")

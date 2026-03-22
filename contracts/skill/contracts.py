"""スキルマニフェスト契約 — 動的スキル検出・登録用。"""

from __future__ import annotations

from pydantic import Field

from contracts.base import ContractModel


class SkillManifest(ContractModel):
    """スキルプラグインのマニフェスト。

    マニフェストファイル経由でスキルを検出・登録し、
    コード変更なしで動的ロードを可能にする。
    """

    contract_version: str = "1.0.0"

    name: str = Field(..., description="スキル識別子（一意）")
    display_name: str = Field("", description="表示用名称")
    description: str = Field("", description="スキルの説明")
    version: str = Field("1.0.0", description="スキルバージョン")

    # 動的ロード用モジュールパス
    module: str = Field(..., description="Python モジュールパス（例: 'shared.skills.builtin.rag'）")
    class_name: str = Field("", description="メインクラス名（空の場合は自動検出）")

    # 分類
    category: str = Field("general", description="スキルカテゴリ: general, domain, infrastructure")
    tags: list[str] = Field(default_factory=list, description="検索用タグ")

    # 依存関係
    requires: list[str] = Field(default_factory=list, description="必須スキル名")
    optional_deps: list[str] = Field(default_factory=list, description="オプションパッケージ依存")

    # 入出力能力
    input_types: list[str] = Field(default_factory=list, description="受け付ける入力タイプ")
    output_types: list[str] = Field(default_factory=list, description="生成する出力タイプ")

    # 機能フラグ
    enabled: bool = Field(True, description="スキルが有効かどうか")

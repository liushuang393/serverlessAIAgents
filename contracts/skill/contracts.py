"""スキルマニフェスト契約 — 動的スキル検出・登録用。"""

from __future__ import annotations

from typing import Literal

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


class CLIHarnessManifest(ContractModel):
    """CLI-Native harness の canonical 契約.

    CLI-Anything 等で生成された agent-native CLI harness を
    Framework 内で一貫した形式で登録・公開する。
    """

    harness_id: str = Field(..., description="Harness 識別子")
    software_name: str = Field(..., description="対象ソフトウェア名")
    source_ref: str = Field(..., description="生成元 ref または import 元識別子")
    package_dir: str = Field(..., description="管理対象 package ディレクトリ")
    cli_command: str = Field(..., description="実行する console script 名")
    skill_md_path: str = Field(..., description="元の SKILL.md パス")
    command_groups: dict[str, list[str]] = Field(
        default_factory=dict,
        description="トップレベル command group とその subcommand 一覧",
    )
    install_state: Literal["pending", "installed", "failed", "build_planned"] = Field(
        default="pending",
        description="install/import/build の状態",
    )
    risk_profile: Literal["low", "medium", "high", "critical"] = Field(
        default="high",
        description="既定のリスクプロファイル",
    )
    description: str = Field(default="", description="Harness の説明")
    shim_skill_path: str = Field(default="", description="生成した shim skill のパス")
    command_options: dict[str, list[str]] = Field(
        default_factory=dict,
        description="`group` / `group subcommand` 単位の許可 option 一覧",
    )

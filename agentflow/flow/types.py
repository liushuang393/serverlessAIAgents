"""Flow型定義.

フロー編成フレームワークのすべての型、列挙型、データクラスを定義。

設計原則:
- 型安全：すべての公開型に完全なアノテーション
- 不変性：データクラスはfrozen=Trueを使用
- プロトコル駆動：具体実装ではなく抽象プロトコルに依存
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Protocol, runtime_checkable


# ============================================================
# 列挙型
# ============================================================


class NodeType(Enum):
    """ノード型."""

    GATE = auto()  # ゲートノード：条件インターセプト
    AGENT = auto()  # Agentノード：Agent実行
    PARALLEL = auto()  # 並列ノード：複数Agentを並列実行
    REVIEW = auto()  # レビューノード：PASS/REVISE/COACH
    CONDITIONAL = auto()  # 条件分岐ノード
    SWITCH = auto()  # switch-case ノード
    ROLLBACK = auto()  # ロールバックノード


class NextAction(Enum):
    """ノード実行後の次のアクション."""

    CONTINUE = auto()  # 次のノードに続行
    STOP = auto()  # フローを停止
    EARLY_RETURN = auto()  # 早期リターン（Gateインターセプト）
    RETRY_FROM = auto()  # 指定ノードからリトライ


class ReviewVerdict(Enum):
    """Review判定結果.

    - PASS: 全チェッククリア、問題なし
    - REVISE: 軽微な問題あり、修正推奨（差戻し）
    - COACH: 重大課題検出、コーチング型改善指導（即終了せずレポートに指摘表示）
    """

    PASS = "PASS"
    REVISE = "REVISE"
    COACH = "COACH"


# ============================================================
# データクラス
# ============================================================


@dataclass(frozen=True)
class NodeResult:
    """ノード実行結果.

    Attributes:
        success: 成功したかどうか
        data: 結果データ
        action: 次のアクション
        retry_from: ロールバック先ノードID
        early_return_data: 早期リターンデータ
    """

    success: bool
    data: dict[str, Any] = field(default_factory=dict)
    action: NextAction = NextAction.CONTINUE
    retry_from: str | None = None
    early_return_data: dict[str, Any] | None = None


@dataclass(frozen=True)
class FlowConfig:
    """Flow設定.

    Attributes:
        enable_progress: 進捗追跡を有効化
        enable_memory: メモリシステムを有効化
        max_revisions: 最大リビジョン回数
        auto_initialize: Agentを自動初期化
    """

    enable_progress: bool = True
    enable_memory: bool = True
    max_revisions: int = 2
    auto_initialize: bool = True


# ============================================================
# プロトコル
# ============================================================


@runtime_checkable
class AgentProtocol(Protocol):
    """Agentプロトコル：Agentが実装すべきインターフェースを定義."""

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Agentを実行."""
        ...


@runtime_checkable
class InitializableAgent(Protocol):
    """初期化可能なAgent."""

    async def initialize(self) -> None:
        """初期化."""
        ...


@runtime_checkable
class CleanableAgent(Protocol):
    """クリーンアップ可能なAgent."""

    async def cleanup(self) -> None:
        """リソースをクリーンアップ."""
        ...


# ============================================================
# 型エイリアス
# ============================================================

# 条件チェック: (result) -> bool
CheckFunc = "Callable[[dict[str, Any]], bool]"

# 失敗処理: (context) -> dict | Event
OnFailFunc = "Callable[[FlowContext], dict[str, Any] | Any]"

# 成功処理: (context) -> dict
OnPassFunc = "Callable[[FlowContext], dict[str, Any]]"

# コーチング処理: (context) -> dict
OnCoachFunc = "Callable[[FlowContext], dict[str, Any]]"

# 入力マッピング: (context) -> dict
InputMapper = "Callable[[FlowContext], dict[str, Any]]"


__all__ = [
    "AgentProtocol",
    "CheckFunc",
    "CleanableAgent",
    "FlowConfig",
    "InitializableAgent",
    "InputMapper",
    "NextAction",
    "NodeResult",
    "NodeType",
    "OnCoachFunc",
    "OnFailFunc",
    "OnPassFunc",
    "ReviewVerdict",
]

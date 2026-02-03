"""RLM Config - Recursive Language Model 設定.

RLM (Recursive Language Model) の動作パラメータを定義する設定クラス。

設計原則:
- 予算ファースト: Token予算とサブコール回数を厳格に管理
- 閾値ベース活性化: 入力が閾値を超えた場合のみRLMモードを発動
- 収束判定: 信頼度が閾値に達したら早期終了

使用例:
    >>> from agentflow.context.rlm import RLMConfig
    >>> config = RLMConfig(
    ...     activation_threshold=20_000,  # 20Kトークン以上でRLM発動
    ...     max_iterations=30,            # 最大30イテレーション
    ... )
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class StopReason(str, Enum):
    """RLM停止理由."""

    ANSWER_FOUND = "answer_found"  # 信頼度が閾値に達した
    BUDGET_EXHAUSTED = "budget_exhausted"  # Token予算を使い切った
    MAX_ITERATIONS = "max_iterations"  # 最大イテレーション数に達した
    CONVERGENCE = "convergence"  # 新情報が抽出されなくなった
    ERROR = "error"  # エラーで停止
    USER_ABORT = "user_abort"  # ユーザーによる中断


class ActionType(str, Enum):
    """RLMアクションタイプ."""

    # 低コスト操作（LLM呼び出し不要）
    PEEK = "peek"  # 行範囲を閲覧
    REGEX_FIND = "regex_find"  # 正規表現検索
    KEYWORD_FIND = "keyword_find"  # キーワード検索
    GET_STRUCTURE = "get_structure"  # 構造/アウトライン取得

    # セマンティック操作（LLM呼び出し必要）
    SEMANTIC_SEARCH = "semantic_search"  # 意味検索
    SUMMARIZE = "summarize"  # 要約生成
    EXTRACT = "extract"  # 情報抽出

    # 終了アクション
    SYNTHESIZE = "synthesize"  # 最終回答合成
    GIVE_UP = "give_up"  # 回答不可


@dataclass
class RLMConfig:
    """RLM設定.

    Attributes:
        activation_threshold: RLM発動のToken閾値（デフォルト15,000）
        max_iterations: 最大ループ回数（デフォルト20）
        max_subcall_budget: サブコール総Token予算（デフォルト50,000）
        max_subcalls: 最大サブコール回数（デフォルト30）
        convergence_threshold: 収束判定の信頼度閾値（デフォルト0.90）
        chunk_size: peek操作のデフォルト行数（デフォルト2000）
        min_confidence_delta: 収束判定の最小変化量（デフォルト0.01）
        stagnation_rounds: 収束判定の停滞ラウンド数（デフォルト3）
        prefer_deterministic: 決定的操作を優先するか（デフォルトTrue）
        workspace_capacity: ワークスペース最大変数数（デフォルト50）
        workspace_token_budget: ワークスペースToken予算（デフォルト10,000）
    """

    # ===========================================================================
    # 活性化設定
    # ===========================================================================
    activation_threshold: int = 15_000
    """RLMモード発動のToken閾値。入力がこれを超えるとRLMが有効化。"""

    # ===========================================================================
    # ループ制御
    # ===========================================================================
    max_iterations: int = 20
    """最大ループ回数。無限ループ防止。"""

    convergence_threshold: float = 0.90
    """収束判定の信頼度閾値。この値以上で回答を確定。"""

    min_confidence_delta: float = 0.01
    """収束判定の最小変化量。これ以下の変化が続くと収束とみなす。"""

    stagnation_rounds: int = 3
    """収束判定の停滞ラウンド数。この回数連続で変化が小さいと収束。"""

    # ===========================================================================
    # 予算制御
    # ===========================================================================
    max_subcall_budget: int = 50_000
    """サブコール（LLM呼び出し）の総Token予算。"""

    max_subcalls: int = 30
    """最大サブコール回数。"""

    # ===========================================================================
    # 操作設定
    # ===========================================================================
    chunk_size: int = 2000
    """peek操作のデフォルト行数。"""

    prefer_deterministic: bool = True
    """決定的操作（regex, keyword）をセマンティック操作より優先するか。"""

    # ===========================================================================
    # ワークスペース設定
    # ===========================================================================
    workspace_capacity: int = 50
    """ワークスペースの最大変数数。"""

    workspace_token_budget: int = 10_000
    """ワークスペースのToken予算。超過時は古い変数を自動削除。"""

    # ===========================================================================
    # 追加設定
    # ===========================================================================
    emit_events: bool = True
    """AG-UIイベントを発行するか。"""

    debug_mode: bool = False
    """デバッグモード（詳細ログ出力）。"""

    def validate(self) -> list[str]:
        """設定を検証.

        Returns:
            エラーメッセージのリスト（空リストなら有効）
        """
        errors: list[str] = []

        if self.activation_threshold < 1000:
            errors.append("activation_threshold should be >= 1000")

        if self.max_iterations < 1:
            errors.append("max_iterations must be >= 1")

        if self.max_iterations > 100:
            errors.append("max_iterations should be <= 100 to prevent runaway")

        if not 0 < self.convergence_threshold <= 1.0:
            errors.append("convergence_threshold must be in (0, 1]")

        if self.max_subcall_budget < 1000:
            errors.append("max_subcall_budget should be >= 1000")

        if self.max_subcalls < 1:
            errors.append("max_subcalls must be >= 1")

        if self.chunk_size < 100:
            errors.append("chunk_size should be >= 100")

        if self.workspace_capacity < 5:
            errors.append("workspace_capacity should be >= 5")

        return errors

    def is_valid(self) -> bool:
        """設定が有効かチェック.

        Returns:
            有効ならTrue
        """
        return len(self.validate()) == 0


@dataclass
class SubCallBudget:
    """サブコール予算追跡.

    Attributes:
        max_tokens: 最大Token数
        max_calls: 最大呼び出し回数
        tokens_used: 使用済みToken数
        calls_made: 実行済み呼び出し回数
    """

    max_tokens: int = 50_000
    max_calls: int = 30
    tokens_used: int = 0
    calls_made: int = 0

    @property
    def remaining_tokens(self) -> int:
        """残りToken数を取得."""
        return max(0, self.max_tokens - self.tokens_used)

    @property
    def remaining_calls(self) -> int:
        """残り呼び出し回数を取得."""
        return max(0, self.max_calls - self.calls_made)

    @property
    def is_exhausted(self) -> bool:
        """予算が尽きたかチェック."""
        return self.remaining_tokens == 0 or self.remaining_calls == 0

    def can_afford(self, estimated_tokens: int) -> bool:
        """指定Token数の操作が可能かチェック.

        Args:
            estimated_tokens: 推定Token数

        Returns:
            実行可能ならTrue
        """
        return self.remaining_tokens >= estimated_tokens and self.remaining_calls > 0

    def consume(self, tokens: int) -> None:
        """予算を消費.

        Args:
            tokens: 消費するToken数
        """
        self.tokens_used += tokens
        self.calls_made += 1

    def to_dict(self) -> dict[str, int]:
        """辞書形式に変換.

        Returns:
            予算情報の辞書
        """
        return {
            "max_tokens": self.max_tokens,
            "max_calls": self.max_calls,
            "tokens_used": self.tokens_used,
            "calls_made": self.calls_made,
            "remaining_tokens": self.remaining_tokens,
            "remaining_calls": self.remaining_calls,
        }

    @classmethod
    def from_config(cls, config: RLMConfig) -> SubCallBudget:
        """RLMConfigから予算を作成.

        Args:
            config: RLM設定

        Returns:
            SubCallBudget インスタンス
        """
        return cls(
            max_tokens=config.max_subcall_budget,
            max_calls=config.max_subcalls,
        )

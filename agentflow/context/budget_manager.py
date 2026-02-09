"""Token Budget Manager - 上下文予算管理.

上下文を「注意力予算」として管理し、各コンポーネントに
適切なToken数を配分する。

設計原則:
- システムプロンプト: ≤500 token（コア指示に集中）
- ツール説明: ≤300 token（Top5-7のみ）
- RAGコンテキスト: ≤2000 token（Top1-2片段）
- 会話履歴: 残り予算で動的調整

使用例:
    >>> manager = TokenBudgetManager()
    >>> prompt = manager.allocate_system_prompt(base_prompt, skills)
    >>> assert manager.count_tokens(prompt) <= 500
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING, Any, Protocol


if TYPE_CHECKING:
    from agentflow.providers.tool_provider import RegisteredTool

_logger = logging.getLogger(__name__)


class BudgetCategory(str, Enum):
    """予算カテゴリ."""

    SYSTEM_PROMPT = "system_prompt"  # システムプロンプト
    TOOLS = "tools"                   # ツール説明
    RAG_CONTEXT = "rag_context"       # RAG検索結果
    HISTORY = "history"               # 会話履歴
    KEY_NOTES = "key_notes"           # 重要Notes


@dataclass
class BudgetConfig:
    """予算設定.

    Attributes:
        system_prompt_budget: システムプロンプト予算（デフォルト500）
        tools_budget: ツール説明予算（デフォルト300）
        rag_context_budget: RAGコンテキスト予算（デフォルト2000）
        history_budget: 会話履歴予算（デフォルト4000）
        key_notes_budget: 重要Notes予算（デフォルト500）
        total_budget: 総予算（デフォルト8000）
        reserve_ratio: 予備率（デフォルト0.1）
    """

    system_prompt_budget: int = 500
    tools_budget: int = 300
    rag_context_budget: int = 2000
    history_budget: int = 4000
    key_notes_budget: int = 500
    total_budget: int = 8000
    reserve_ratio: float = 0.1


@dataclass
class BudgetAllocation:
    """予算配分結果.

    Attributes:
        content: 配分後のコンテンツ
        token_count: 実際のToken数
        budget_used: 使用した予算
        truncated: 切り詰められたか
        original_token_count: 元のToken数
    """

    content: str
    token_count: int
    budget_used: int
    truncated: bool = False
    original_token_count: int = 0


class TokenCounter(Protocol):
    """Token計数インターフェース（DI用）."""

    def count(self, text: str) -> int:
        """テキストのToken数を計算."""
        ...


class SimpleTokenCounter:
    """簡易Token計数（tiktoken不要時のフォールバック）.

    日本語・中国語は1文字≒1.5token、英語は4文字≒1tokenで概算。
    """

    def count(self, text: str) -> int:
        """テキストのToken数を概算.

        Args:
            text: 計算対象テキスト

        Returns:
            概算Token数
        """
        if not text:
            return 0

        # 日本語・中国語文字をカウント
        cjk_chars = len(re.findall(r"[\u4e00-\u9fff\u3040-\u309f\u30a0-\u30ff]", text))
        # ASCII文字をカウント
        ascii_chars = len(re.findall(r"[a-zA-Z0-9\s]", text))
        # その他
        other_chars = len(text) - cjk_chars - ascii_chars

        # 概算: CJK=1.5token/char, ASCII=0.25token/char, other=1token/char
        return int(cjk_chars * 1.5 + ascii_chars * 0.25 + other_chars)


class TiktokenCounter:
    """tiktoken ベースのToken計数."""

    def __init__(self, model: str = "gpt-4") -> None:
        """初期化.

        Args:
            model: エンコーディングモデル
        """
        self._model = model
        self._encoding = None

    def _get_encoding(self) -> Any:
        """エンコーディングを遅延取得."""
        if self._encoding is None:
            try:
                import tiktoken

                self._encoding = tiktoken.encoding_for_model(self._model)
            except ImportError:
                _logger.warning("tiktoken未インストール、簡易計数にフォールバック")
                return None
        return self._encoding

    def count(self, text: str) -> int:
        """テキストのToken数を計算.

        Args:
            text: 計算対象テキスト

        Returns:
            Token数
        """
        encoding = self._get_encoding()
        if encoding is None:
            return SimpleTokenCounter().count(text)
        return len(encoding.encode(text))


class TokenBudgetManager:
    """Token予算管理器.

    上下文の各コンポーネントに対してToken予算を管理し、
    予算内に収まるように自動的に切り詰め・優先順位付けを行う。

    Example:
        >>> manager = TokenBudgetManager()
        >>>
        >>> # システムプロンプトを予算内に収める
        >>> allocation = manager.allocate_system_prompt(
        ...     base_prompt="あなたは親切なアシスタントです...",
        ...     skills=[skill1, skill2],
        ... )
        >>> print(f"使用Token: {allocation.token_count}/{allocation.budget_used}")
    """

    def __init__(
        self,
        config: BudgetConfig | None = None,
        counter: TokenCounter | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 予算設定
            counter: Token計数器（DIによる注入可能）
        """
        self._config = config or BudgetConfig()
        self._counter = counter or TiktokenCounter()
        self._usage: dict[BudgetCategory, int] = dict.fromkeys(BudgetCategory, 0)
        self._logger = logging.getLogger(__name__)

    def count_tokens(self, text: str) -> int:
        """Token数を計算.

        Args:
            text: 対象テキスト

        Returns:
            Token数
        """
        return self._counter.count(text)

    def get_remaining_budget(self, category: BudgetCategory) -> int:
        """カテゴリの残り予算を取得.

        Args:
            category: 予算カテゴリ

        Returns:
            残りToken数
        """
        budgets = {
            BudgetCategory.SYSTEM_PROMPT: self._config.system_prompt_budget,
            BudgetCategory.TOOLS: self._config.tools_budget,
            BudgetCategory.RAG_CONTEXT: self._config.rag_context_budget,
            BudgetCategory.HISTORY: self._config.history_budget,
            BudgetCategory.KEY_NOTES: self._config.key_notes_budget,
        }
        return budgets.get(category, 0) - self._usage.get(category, 0)

    def allocate_system_prompt(
        self,
        base_prompt: str,
        skills: list[Any] | None = None,
        priority_sections: list[str] | None = None,
    ) -> BudgetAllocation:
        """システムプロンプトを予算内に配分.

        分層注入戦略:
        1. コアプロンプト（必須）
        2. 優先セクション（高優先度）
        3. スキル説明（優先度順に追加）

        Args:
            base_prompt: 基本システムプロンプト
            skills: スキルリスト（to_prompt()メソッドを持つ）
            priority_sections: 優先的に含めるセクション

        Returns:
            予算配分結果
        """
        budget = self._config.system_prompt_budget
        original_tokens = self.count_tokens(base_prompt)

        # コアプロンプトが予算を超える場合
        if original_tokens > budget:
            truncated = self._truncate_text(base_prompt, budget)
            return BudgetAllocation(
                content=truncated,
                token_count=self.count_tokens(truncated),
                budget_used=budget,
                truncated=True,
                original_token_count=original_tokens,
            )

        # スキルを追加
        result_parts = [base_prompt]
        remaining = budget - original_tokens

        if skills:
            skills_header = "\n\n---\n# 利用可能なスキル\n"
            header_tokens = self.count_tokens(skills_header)

            if remaining > header_tokens:
                result_parts.append(skills_header)
                remaining -= header_tokens

                for skill in skills:
                    if hasattr(skill, "to_prompt"):
                        skill_text = skill.to_prompt()
                        skill_tokens = self.count_tokens(skill_text)

                        if skill_tokens <= remaining:
                            result_parts.append(f"\n{skill_text}")
                            remaining -= skill_tokens
                        else:
                            # 予算超過、これ以上追加しない
                            break

        final_content = "".join(result_parts)
        final_tokens = self.count_tokens(final_content)
        self._usage[BudgetCategory.SYSTEM_PROMPT] = final_tokens

        return BudgetAllocation(
            content=final_content,
            token_count=final_tokens,
            budget_used=budget,
            truncated=final_tokens < original_tokens,
            original_token_count=original_tokens,
        )

    def allocate_tools(
        self,
        tools: list[RegisteredTool],
        max_tools: int = 7,
    ) -> BudgetAllocation:
        """ツール説明を予算内に配分.

        Args:
            tools: ツールリスト（スコア順）
            max_tools: 最大ツール数

        Returns:
            予算配分結果
        """
        budget = self._config.tools_budget
        selected_tools: list[str] = []
        current_tokens = 0

        for tool in tools[:max_tools]:
            # ツール説明を構築
            tool_desc = f"- {tool.name}: {tool.description}"
            tool_tokens = self.count_tokens(tool_desc)

            if current_tokens + tool_tokens <= budget:
                selected_tools.append(tool_desc)
                current_tokens += tool_tokens
            else:
                break

        content = "\n".join(selected_tools)
        self._usage[BudgetCategory.TOOLS] = current_tokens

        return BudgetAllocation(
            content=content,
            token_count=current_tokens,
            budget_used=budget,
            truncated=len(selected_tools) < len(tools),
            original_token_count=sum(
                self.count_tokens(f"- {t.name}: {t.description}") for t in tools
            ),
        )

    def allocate_rag_context(
        self,
        chunks: list[str],
        max_chunks: int = 2,
    ) -> BudgetAllocation:
        """RAGコンテキストを予算内に配分.

        Args:
            chunks: 検索結果チャンク（スコア順）
            max_chunks: 最大チャンク数（デフォルト2）

        Returns:
            予算配分結果
        """
        budget = self._config.rag_context_budget
        selected_chunks: list[str] = []
        current_tokens = 0

        for i, chunk in enumerate(chunks[:max_chunks]):
            chunk_with_ref = f"[{i + 1}] {chunk}"
            chunk_tokens = self.count_tokens(chunk_with_ref)

            if current_tokens + chunk_tokens <= budget:
                selected_chunks.append(chunk_with_ref)
                current_tokens += chunk_tokens
            else:
                # 残り予算で部分的に追加
                remaining = budget - current_tokens
                if remaining > 50:  # 最低50tokenは必要
                    truncated_chunk = self._truncate_text(chunk_with_ref, remaining)
                    selected_chunks.append(truncated_chunk)
                    current_tokens += self.count_tokens(truncated_chunk)
                break

        content = "\n\n".join(selected_chunks)
        self._usage[BudgetCategory.RAG_CONTEXT] = current_tokens

        return BudgetAllocation(
            content=content,
            token_count=current_tokens,
            budget_used=budget,
            truncated=len(selected_chunks) < len(chunks),
            original_token_count=sum(self.count_tokens(c) for c in chunks),
        )

    def allocate_history(
        self,
        messages: list[dict[str, str]],
    ) -> BudgetAllocation:
        """会話履歴を予算内に配分.

        最新のメッセージを優先して保持。

        Args:
            messages: メッセージリスト（古い順）

        Returns:
            予算配分結果
        """
        budget = self._config.history_budget
        selected_messages: list[dict[str, str]] = []
        current_tokens = 0

        # 逆順（最新から）で処理
        for msg in reversed(messages):
            msg_text = f"{msg.get('role', 'user')}: {msg.get('content', '')}"
            msg_tokens = self.count_tokens(msg_text)

            if current_tokens + msg_tokens <= budget:
                selected_messages.insert(0, msg)  # 先頭に追加（順序維持）
                current_tokens += msg_tokens
            else:
                break

        # コンテンツを構築
        content = "\n".join(
            f"{m.get('role', 'user')}: {m.get('content', '')}" for m in selected_messages
        )
        self._usage[BudgetCategory.HISTORY] = current_tokens

        return BudgetAllocation(
            content=content,
            token_count=current_tokens,
            budget_used=budget,
            truncated=len(selected_messages) < len(messages),
            original_token_count=sum(
                self.count_tokens(f"{m.get('role', '')}: {m.get('content', '')}")
                for m in messages
            ),
        )

    def reset_usage(self) -> None:
        """使用量をリセット."""
        self._usage = dict.fromkeys(BudgetCategory, 0)

    def get_usage_summary(self) -> dict[str, Any]:
        """使用量サマリを取得.

        Returns:
            カテゴリ別使用量と残り予算
        """
        return {
            "usage": {cat.value: tokens for cat, tokens in self._usage.items()},
            "remaining": {
                cat.value: self.get_remaining_budget(cat) for cat in BudgetCategory
            },
            "total_used": sum(self._usage.values()),
            "total_budget": self._config.total_budget,
        }

    def _truncate_text(self, text: str, max_tokens: int) -> str:
        """テキストを指定Token数に切り詰め.

        Args:
            text: 対象テキスト
            max_tokens: 最大Token数

        Returns:
            切り詰められたテキスト
        """
        current_tokens = self.count_tokens(text)
        if current_tokens <= max_tokens:
            return text

        # バイナリサーチで適切な長さを見つける
        low, high = 0, len(text)
        while low < high:
            mid = (low + high + 1) // 2
            if self.count_tokens(text[:mid]) <= max_tokens:
                low = mid
            else:
                high = mid - 1

        return text[:low] + "..."

# -*- coding: utf-8 -*-
"""DeepAgent コンテキスト圧縮システム.

業界最佳実践に基づいた多層コンテキスト圧縮を実装:

1. 階層的メモリ管理（MemGPT/Letta風）
   - Working Memory: 直近の重要情報
   - Session Memory: セッション内の要約
   - Archival Memory: 長期保存情報

2. 選択的保持（LLMLingua風）
   - 重要度スコアリング
   - 依存関係追跡
   - 結果・決定の優先保持

3. 圧縮操作（Mem0風）
   - ADD: 新規追加
   - UPDATE: 既存更新
   - MERGE: 類似項目統合
   - ARCHIVE: 長期保存へ移動
   - DISCARD: 削除

参考文献:
- Anthropic Context Engineering (2025.09)
- Mem0 Memory Architecture (arXiv:2504.19413)
- MemGPT/Letta OS-style Memory (2024)
- LLMLingua Selective Context (ACL 2024)
"""

from __future__ import annotations

import logging
from datetime import datetime
from typing import Any

from agentflow.patterns.deep_agent.da_models import (
    AgentMessage,
    CompactionResult,
    CompactionStrategy,
    MemoryTier,
    MessageType,
)

_logger = logging.getLogger(__name__)


class ContextCompressor:
    """専門的コンテキスト圧縮システム.

    Example:
        >>> compressor = ContextCompressor(llm_client=my_llm)
        >>> result = await compressor.compact(messages, max_tokens=4000)
    """

    # 保持優先キーワード（削除しない）
    PRESERVE_PREFIXES = ("result_", "final_", "decision_", "error_", "critical_")
    # 圧縮候補キーワード（優先的に圧縮）
    COMPRESS_PREFIXES = ("temp_", "debug_", "log_", "raw_")

    def __init__(
        self,
        llm_client: Any = None,
        max_working_tokens: int = 8000,
        max_session_tokens: int = 16000,
        max_archival_tokens: int = 32000,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（要約生成用）
            max_working_tokens: Working Memory最大トークン
            max_session_tokens: Session Memory最大トークン
            max_archival_tokens: Archival Memory最大トークン
        """
        self._llm = llm_client
        self._working = MemoryTier("working", max_working_tokens, priority=3)
        self._session = MemoryTier("session", max_session_tokens, priority=2)
        self._archival = MemoryTier("archival", max_archival_tokens, priority=1)
        self._key_importance: dict[str, float] = {}
        self._key_access_count: dict[str, int] = {}
        self._key_last_access: dict[str, datetime] = {}

    def score_importance(self, key: str, value: Any) -> float:
        """情報の重要度をスコアリング（0.0-1.0）."""
        score = 0.5
        if any(key.startswith(p) for p in self.PRESERVE_PREFIXES):
            score += 0.4
        if any(key.startswith(p) for p in self.COMPRESS_PREFIXES):
            score -= 0.3
        access_count = self._key_access_count.get(key, 0)
        if access_count > 5:
            score += 0.1
        elif access_count > 10:
            score += 0.2
        last_access = self._key_last_access.get(key)
        if last_access:
            age_minutes = (datetime.now() - last_access).total_seconds() / 60
            if age_minutes > 30:
                score -= 0.1
            if age_minutes > 60:
                score -= 0.1
        if len(str(value)) > 5000:
            score -= 0.1
        return max(0.0, min(1.0, score))

    def track_access(self, key: str) -> None:
        """キーアクセスを追跡."""
        self._key_access_count[key] = self._key_access_count.get(key, 0) + 1
        self._key_last_access[key] = datetime.now()

    async def compact_messages(
        self,
        messages: list[AgentMessage],
        max_tokens: int = 4000,
        strategy: CompactionStrategy = CompactionStrategy.HYBRID,
    ) -> tuple[list[AgentMessage], CompactionResult]:
        """メッセージ履歴を圧縮."""
        if not messages:
            return [], CompactionResult(0, 0, 1.0, [])

        original_tokens = sum(len(str(m.content)) // 4 for m in messages)
        if original_tokens <= max_tokens:
            return messages, CompactionResult(
                original_tokens, original_tokens, 1.0, [m.id for m in messages]
            )

        if strategy == CompactionStrategy.SELECTIVE:
            return await self._selective_compact(messages, max_tokens)
        elif strategy == CompactionStrategy.SUMMARIZE:
            return await self._summarize_compact(messages, max_tokens)
        elif strategy == CompactionStrategy.HIERARCHICAL:
            return await self._hierarchical_compact(messages, max_tokens)
        else:
            return await self._hybrid_compact(messages, max_tokens)

    async def _selective_compact(
        self, messages: list[AgentMessage], max_tokens: int
    ) -> tuple[list[AgentMessage], CompactionResult]:
        """選択的圧縮（LLMLingua風）."""
        scored = [(self._score_message_importance(m), m) for m in messages]
        scored.sort(key=lambda x: -x[0])
        kept, current_tokens, preserved_ids = [], 0, []
        for _, msg in scored:
            msg_tokens = len(str(msg.content)) // 4
            if current_tokens + msg_tokens <= max_tokens:
                kept.append(msg)
                preserved_ids.append(msg.id)
                current_tokens += msg_tokens
        kept.sort(key=lambda m: m.timestamp)
        original_tokens = sum(len(str(m.content)) // 4 for m in messages)
        return kept, CompactionResult(
            original_tokens, current_tokens,
            current_tokens / original_tokens if original_tokens > 0 else 1.0,
            preserved_ids,
        )

    def _score_message_importance(self, msg: AgentMessage) -> float:
        """メッセージの重要度をスコアリング."""
        type_scores = {
            MessageType.RESULT: 0.9, MessageType.ERROR: 0.95,
            MessageType.REQUEST: 0.6, MessageType.NOTIFY: 0.4, MessageType.SYSTEM: 0.7,
        }
        score = type_scores.get(msg.msg_type, 0.5)
        age_minutes = (datetime.now() - msg.timestamp).total_seconds() / 60
        if age_minutes < 5:
            score += 0.2
        elif age_minutes < 15:
            score += 0.1
        content_str = str(msg.content).lower()
        if any(kw in content_str for kw in ["error", "failed", "exception"]):
            score += 0.2
        if any(kw in content_str for kw in ["result", "success", "completed"]):
            score += 0.15
        return min(1.0, score)

    async def _summarize_compact(
        self, messages: list[AgentMessage], max_tokens: int
    ) -> tuple[list[AgentMessage], CompactionResult]:
        """要約圧縮（LLMで要約生成）."""
        original_tokens = sum(len(str(m.content)) // 4 for m in messages)
        if not self._llm:
            return await self._selective_compact(messages, max_tokens)

        # 最新メッセージは保持、古いものを要約
        recent_count = min(5, len(messages))
        recent = messages[-recent_count:]
        to_summarize = messages[:-recent_count] if len(messages) > recent_count else []

        if not to_summarize:
            return recent, CompactionResult(
                original_tokens, sum(len(str(m.content)) // 4 for m in recent),
                1.0, [m.id for m in recent],
            )

        # 要約生成（LLM呼び出し）
        summary_text = await self._generate_summary(to_summarize)
        summary_msg = AgentMessage(
            from_agent="system",
            to_agent="*",
            msg_type=MessageType.SYSTEM,
            content={"summary": summary_text, "summarized_count": len(to_summarize)},
        )

        result_messages = [summary_msg] + recent
        compressed_tokens = sum(len(str(m.content)) // 4 for m in result_messages)
        return result_messages, CompactionResult(
            original_tokens, compressed_tokens,
            compressed_tokens / original_tokens if original_tokens > 0 else 1.0,
            [m.id for m in recent], summary=summary_text,
        )

    async def _generate_summary(self, messages: list[AgentMessage]) -> str:
        """メッセージ群の要約を生成."""
        if not self._llm:
            return f"[{len(messages)}件のメッセージを要約]"

        content_text = "\n".join(
            f"[{m.from_agent}→{m.to_agent}] {m.msg_type.value}: {m.content}"
            for m in messages
        )
        prompt = f"""以下のAgent間通信履歴を簡潔に要約してください。
重要な決定、結果、エラーを優先的に含めてください。

---
{content_text}
---

要約（日本語、200文字以内）:"""

        try:
            response = await self._llm.generate(prompt)
            return response.strip()
        except Exception as e:
            _logger.warning("要約生成失敗: %s", e)
            return f"[{len(messages)}件のメッセージ - 要約生成失敗]"

    async def _hierarchical_compact(
        self, messages: list[AgentMessage], max_tokens: int
    ) -> tuple[list[AgentMessage], CompactionResult]:
        """階層的圧縮（MemGPT風）.

        Note:
            asyncは将来のLLM統合のために保持。
        """
        original_tokens = sum(len(str(m.content)) // 4 for m in messages)

        # メッセージを重要度でソート
        scored = [(self._score_message_importance(m), m) for m in messages]

        # 高重要度 → Working Memory
        high_importance = [m for s, m in scored if s >= 0.7]
        # 中重要度 → Session Memory
        mid_importance = [m for s, m in scored if 0.4 <= s < 0.7]
        # 低重要度 → Archival（将来の拡張用、現在は破棄）
        _ = [m for s, m in scored if s < 0.4]  # noqa: F841

        # Working Memoryに収まる分を保持
        working_budget = int(max_tokens * 0.6)
        session_budget = int(max_tokens * 0.3)

        kept_working, working_tokens = [], 0
        for msg in high_importance:
            msg_tokens = len(str(msg.content)) // 4
            if working_tokens + msg_tokens <= working_budget:
                kept_working.append(msg)
                working_tokens += msg_tokens

        kept_session, session_tokens = [], 0
        for msg in mid_importance:
            msg_tokens = len(str(msg.content)) // 4
            if session_tokens + msg_tokens <= session_budget:
                kept_session.append(msg)
                session_tokens += msg_tokens

        result = kept_working + kept_session
        result.sort(key=lambda m: m.timestamp)
        compressed_tokens = working_tokens + session_tokens

        return result, CompactionResult(
            original_tokens, compressed_tokens,
            compressed_tokens / original_tokens if original_tokens > 0 else 1.0,
            [m.id for m in result],
        )

    async def _hybrid_compact(
        self, messages: list[AgentMessage], max_tokens: int
    ) -> tuple[list[AgentMessage], CompactionResult]:
        """ハイブリッド圧縮（推奨）."""
        original_tokens = sum(len(str(m.content)) // 4 for m in messages)

        # Step 1: 階層分類
        scored = [(self._score_message_importance(m), m) for m in messages]
        critical = [m for s, m in scored if s >= 0.8]
        important = [m for s, m in scored if 0.5 <= s < 0.8]
        normal = [m for s, m in scored if s < 0.5]

        # Step 2: Critical は必ず保持
        kept = list(critical)
        current_tokens = sum(len(str(m.content)) // 4 for m in kept)

        # Step 3: Important を追加
        for msg in important:
            msg_tokens = len(str(msg.content)) // 4
            if current_tokens + msg_tokens <= max_tokens * 0.8:
                kept.append(msg)
                current_tokens += msg_tokens

        # Step 4: Normal を要約して追加（余裕があれば）
        remaining_budget = max_tokens - current_tokens
        if normal and remaining_budget > 500 and self._llm:
            summary_text = await self._generate_summary(normal)
            summary_msg = AgentMessage(
                from_agent="system", to_agent="*",
                msg_type=MessageType.SYSTEM,
                content={"summary": summary_text, "summarized_count": len(normal)},
            )
            kept.append(summary_msg)
            current_tokens += len(summary_text) // 4

        kept.sort(key=lambda m: m.timestamp)
        return kept, CompactionResult(
            original_tokens, current_tokens,
            current_tokens / original_tokens if original_tokens > 0 else 1.0,
            [m.id for m in kept if m.id.startswith("msg-")],
        )


# =============================================================================
# エクスポート
# =============================================================================

__all__ = ["ContextCompressor"]


"""AgentFlow Memory System - Short-Term Memory (Light2).

短期記憶: トピックバッファ + 閾値到達時の要約生成。

参考論文: LightMem Light2
- トピック別バッファ管理
- Token閾値監視
- 一括要約生成（API呼び出し削減）
- 主題純度の維持
"""

from __future__ import annotations

import logging
import uuid
from datetime import datetime
from typing import Any

from agentflow.memory.types import MemoryEntry, MemoryType, TopicBuffer


class ShortTermMemory:
    """短期記憶（Light2）.

    職責:
    - トピック別バッファ管理
    - Token閾値監視
    - 閾値到達時の要約生成
    - API呼び出し削減（17-177倍）

    Example:
        >>> stm = ShortTermMemory(token_threshold=1000)
        >>> await stm.add_entry(entry)
        >>> if stm.should_summarize("AI"):
        ...     summary = await stm.summarize_topic("AI")
    """

    def __init__(
        self,
        token_threshold: int = 1000,
        llm_client: Any | None = None,
    ) -> None:
        """初期化.

        Args:
            token_threshold: Token閾値（デフォルト: 1000）
            llm_client: LLMクライアント（要約生成用）
        """
        self._token_threshold = token_threshold
        self._llm_client = llm_client
        self._buffers: dict[str, TopicBuffer] = {}
        self._logger = logging.getLogger(__name__)

    async def add_entry(
        self,
        entry: MemoryEntry,
        token_count: int | None = None,
    ) -> None:
        """記憶エントリを追加.

        Args:
            entry: 記憶エントリ
            token_count: Token数（Noneの場合は推定）
        """
        topic = entry.topic

        # バッファが存在しない場合は作成
        if topic not in self._buffers:
            self._buffers[topic] = TopicBuffer(topic=topic)

        # Token数を推定（簡易版: 単語数 * 1.3）
        if token_count is None:
            token_count = int(len(entry.content.split()) * 1.3)

        # バッファに追加
        self._buffers[topic].add_entry(entry, token_count)

        self._logger.debug(
            f"Added entry to topic '{topic}': "
            f"{self._buffers[topic].total_tokens} tokens"
        )

    def should_summarize(self, topic: str) -> bool:
        """トピックの要約が必要かチェック.

        Args:
            topic: トピック名

        Returns:
            要約が必要な場合True
        """
        if topic not in self._buffers:
            return False

        return self._buffers[topic].should_summarize(self._token_threshold)

    async def summarize_topic(
        self,
        topic: str,
        custom_prompt: str | None = None,
    ) -> MemoryEntry | None:
        """トピックを要約.

        Args:
            topic: トピック名
            custom_prompt: カスタムプロンプト

        Returns:
            要約済み記憶エントリ、またはNone
        """
        if topic not in self._buffers:
            self._logger.warning(f"Topic '{topic}' not found")
            return None

        buffer = self._buffers[topic]

        # 全エントリの内容を結合
        combined_content = "\n".join([entry.content for entry in buffer.entries])

        # 要約生成
        summary = await self._generate_summary(combined_content, topic, custom_prompt)

        # 要約エントリ作成
        summary_entry = MemoryEntry(
            id=str(uuid.uuid4()),
            content=summary,
            topic=topic,
            timestamp=datetime.now(),
            memory_type=MemoryType.SHORT_TERM,
            importance_score=self._calculate_average_importance(buffer),
            metadata={
                "original_entries": len(buffer.entries),
                "original_tokens": buffer.total_tokens,
                "summarized_at": datetime.now().isoformat(),
            },
        )

        # バッファをクリア
        self._buffers[topic] = TopicBuffer(topic=topic)

        self._logger.info(
            f"Summarized topic '{topic}': "
            f"{len(buffer.entries)} entries -> 1 summary"
        )

        return summary_entry

    async def _generate_summary(
        self,
        content: str,
        topic: str,
        custom_prompt: str | None = None,
    ) -> str:
        """要約を生成.

        Args:
            content: 要約対象の内容
            topic: トピック名
            custom_prompt: カスタムプロンプト

        Returns:
            要約テキスト
        """
        # LLMクライアントが設定されている場合は使用
        if self._llm_client:
            pass
            # 実際のLLM呼び出し（実装は省略）
            # summary = await self._llm_client.generate(prompt)
            # return summary

        # フォールバック: 簡易要約（最初の200文字）
        return content[:200] + "..." if len(content) > 200 else content

    def _calculate_average_importance(self, buffer: TopicBuffer) -> float:
        """バッファ内エントリの平均重要度を計算.

        Args:
            buffer: トピックバッファ

        Returns:
            平均重要度スコア
        """
        if not buffer.entries:
            return 0.5

        total_importance = sum(entry.importance_score for entry in buffer.entries)
        return total_importance / len(buffer.entries)

    def get_buffer_status(self) -> dict[str, dict[str, Any]]:
        """全バッファの状態を取得.

        Returns:
            トピック別バッファ状態
        """
        return {
            topic: {
                "entries": len(buffer.entries),
                "total_tokens": buffer.total_tokens,
                "should_summarize": buffer.should_summarize(self._token_threshold),
                "created_at": buffer.created_at.isoformat(),
                "last_updated": buffer.last_updated.isoformat(),
            }
            for topic, buffer in self._buffers.items()
        }


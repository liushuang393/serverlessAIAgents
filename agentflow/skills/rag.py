"""RAG Skill - 検索増強生成（Retrieval-Augmented Generation）能力.

このモジュールは、知識ベースと LLM を統合した RAG 機能を提供します：
- 知識ベースへのドキュメント追加
- 意味検索によるコンテキスト取得
- LLM による回答生成

設計原則：
- 簡単：最小設定で動作
- 柔軟：Memory システムと LLM を抽象化
- 拡張：カスタム埋め込み・検索対応
- 松耦合：LLM プロバイダーを意識しない
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from agentflow.memory.memory_manager import MemoryManager
from agentflow.memory.types import CompressionConfig
from agentflow.providers import get_llm


if TYPE_CHECKING:
    from agentflow.providers.llm_provider import LLMProvider


@dataclass
class RAGConfig:
    """RAG 設定.

    Attributes:
        top_k: 検索結果の上位 K 件
        min_similarity: 最小類似度閾値
        system_prompt: システムプロンプト
        context_template: コンテキストテンプレート
    """

    top_k: int = 5
    min_similarity: float = 0.3
    system_prompt: str = "あなたは知識ベースに基づいて質問に答えるアシスタントです。"
    context_template: str = "参考情報:\n{context}\n\n質問: {query}"


@dataclass
class RAGResult:
    """RAG 結果.

    Attributes:
        answer: LLM 生成回答
        sources: 使用したソース情報
        context_used: 使用したコンテキスト
    """

    answer: str
    sources: list[dict[str, Any]] = field(default_factory=list)
    context_used: str = ""


class RAGSkill:
    """RAG Skill - 検索増強生成（松耦合設計）.

    知識ベースから関連情報を検索し、LLM で回答を生成します。
    LLM プロバイダー/モデルは環境変数から自動検出されます。

    Example:
        >>> # 初期化（プロバイダー指定不要）
        >>> rag = RAGSkill()
        >>> await rag.start()
        >>>
        >>> # ドキュメント追加
        >>> await rag.add_document("AgentFlow は軽量 AI Agent フレームワークです。")
        >>>
        >>> # 質問応答
        >>> result = await rag.query("AgentFlow とは何ですか？")
        >>> print(result.answer)
    """

    def __init__(
        self,
        rag_config: RAGConfig | None = None,
        memory_manager: MemoryManager | None = None,
        *,
        temperature: float | None = None,
    ) -> None:
        """初期化.

        Note:
            LLM プロバイダーは環境変数から自動検出されます（松耦合設計）。

        Args:
            rag_config: RAG 設定
            memory_manager: 外部メモリマネージャー（None の場合は内部作成）
            temperature: LLM 温度パラメータ（省略時はデフォルト）
        """
        self._rag_config = rag_config or RAGConfig()
        self._logger = logging.getLogger(__name__)

        # LLM プロバイダー（環境変数から自動検出・松耦合）
        self._llm: LLMProvider = get_llm(temperature=temperature)

        # Memory Manager（ベクトル検索有効）
        if memory_manager:
            self._memory = memory_manager
            self._external_memory = True
        else:
            self._memory = MemoryManager(
                compression_config=CompressionConfig(),
                enable_vector_search=True,
                embedding_dim=384,
            )
            self._external_memory = False

    async def start(self) -> None:
        """RAG システムを開始."""
        if not self._external_memory:
            await self._memory.start()
        self._logger.info("RAG Skill started")

    async def stop(self) -> None:
        """RAG システムを停止."""
        if not self._external_memory:
            await self._memory.stop()
        self._logger.info("RAG Skill stopped")

    async def add_document(
        self,
        content: str,
        topic: str = "default",
        metadata: dict[str, Any] | None = None,
    ) -> str:
        """ドキュメントを知識ベースに追加.

        Args:
            content: ドキュメント内容
            topic: トピック分類
            metadata: 追加メタデータ

        Returns:
            ドキュメント ID
        """
        entry = await self._memory.remember(content, topic, metadata)
        self._logger.debug(f"Added document: {entry.id}")
        return entry.id

    async def query(
        self,
        question: str,
        topic: str | None = None,
    ) -> RAGResult:
        """質問に対して RAG で回答.

        Args:
            question: 質問文
            topic: 検索対象トピック（None で全検索）

        Returns:
            RAG 結果
        """
        # 1. 関連コンテキストを検索
        memories = await self._memory.recall(
            topic=topic,
            limit=self._rag_config.top_k,
            query=question,
            min_similarity=self._rag_config.min_similarity,
        )

        # 2. コンテキストを構築
        context_parts = [m.content for m in memories]
        context = "\n---\n".join(context_parts) if context_parts else "関連情報なし"

        # 3. プロンプト作成
        user_prompt = self._rag_config.context_template.format(
            context=context,
            query=question,
        )

        # 4. LLM で回答生成（松耦合：プロバイダー不明）
        messages = [
            {"role": "system", "content": self._rag_config.system_prompt},
            {"role": "user", "content": user_prompt},
        ]
        response = await self._llm.chat(messages)

        # 5. ソース情報を構築
        sources = [
            {
                "id": m.id,
                "content": m.content[:100] + "..." if len(m.content) > 100 else m.content,
                "topic": m.topic,
            }
            for m in memories
        ]

        return RAGResult(
            answer=response["content"],
            sources=sources,
            context_used=context,
        )

    def get_status(self) -> dict[str, Any]:
        """RAG システム状態を取得.

        Returns:
            システム状態
        """
        memory_status = self._memory.get_status()
        return {
            "rag_config": {
                "top_k": self._rag_config.top_k,
                "min_similarity": self._rag_config.min_similarity,
            },
            "memory": memory_status,
        }

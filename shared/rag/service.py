"""Layer 2 の Shared RAG サービス.

Embedding Gateway / Rerank Gateway / LLM Gateway を組み合わせた
Retrieval-Augmented Generation の共有サービス。
全 App / 全 Agent が統一的に RAG パイプラインを利用できる。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from contracts.base import ContractModel
from pydantic import Field

from shared.gateway.embedding.service import SharedEmbeddingGateway
from shared.gateway.llm.service import SharedLLMGateway
from shared.gateway.rerank.service import SharedRerankGateway


if TYPE_CHECKING:
    from shared.registry import ComponentToggle


class RAGResult(ContractModel):
    """RAG パイプラインの実行結果."""

    answer: str = Field(default="", description="生成された回答")
    sources: list[dict[str, Any]] = Field(
        default_factory=list, description="参照されたソース一覧"
    )
    metadata: dict[str, Any] = Field(
        default_factory=dict, description="パイプライン実行メタデータ"
    )


class SharedRAGService:
    """Retrieval-Augmented Generation の共有サービス.

    Embedding → 検索 → Rerank → LLM 生成 のパイプラインを提供する。
    """

    def __init__(
        self,
        *,
        llm_gateway: SharedLLMGateway | None = None,
        embedding_gateway: SharedEmbeddingGateway | None = None,
        rerank_gateway: SharedRerankGateway | None = None,
        toggle: ComponentToggle | None = None,
    ) -> None:
        """RAG サービスを初期化する.

        Args:
            llm_gateway: LLM Gateway（省略時は内部で生成）
            embedding_gateway: Embedding Gateway（省略時は内部で生成）
            rerank_gateway: Rerank Gateway（省略時は内部で生成）
            toggle: コンポーネント切替設定
        """
        self._toggle = toggle
        # 遅延初期化: 実際の Gateway 注入は起動時に行う
        self._llm = llm_gateway
        self._embedding = embedding_gateway
        self._rerank = rerank_gateway

    def _ensure_gateways(self) -> None:
        """未注入の Gateway を遅延生成する."""
        if self._llm is None:
            self._llm = SharedLLMGateway(toggle=self._toggle)
        if self._embedding is None:
            self._embedding = SharedEmbeddingGateway(toggle=self._toggle)
        if self._rerank is None:
            self._rerank = SharedRerankGateway(toggle=self._toggle)

    async def query(
        self,
        question: str,
        *,
        documents: list[str] | None = None,
        top_k: int = 5,
        system_prompt: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> RAGResult:
        """RAG パイプラインを実行する.

        Args:
            question: ユーザー質問
            documents: 検索対象文書（省略時は空リスト）
            top_k: 上位 k 件の文書を使用
            system_prompt: LLM へのシステムプロンプト
            metadata: 追加メタデータ

        Returns:
            RAG 実行結果
        """
        self._ensure_gateways()
        if self._embedding is None or self._rerank is None or self._llm is None:
            msg = "RAG Gateway の初期化に失敗しました"
            raise RuntimeError(msg)

        docs = documents or []
        if not docs:
            return RAGResult(answer="", sources=[], metadata=metadata or {})

        # Step 1: Rerank で関連文書を絞る
        ranked = await self._rerank.rerank(question, docs, top_k=top_k)
        context_docs = [r.text for r in ranked]

        # Step 2: コンテキストを組み立てて LLM に生成させる
        context_block = "\n---\n".join(context_docs)
        messages: list[dict[str, Any]] = []
        if system_prompt:
            messages.append({"role": "system", "content": system_prompt})
        messages.append(
            {
                "role": "user",
                "content": (
                    f"以下の参考情報に基づいて質問に回答してください。\n\n"
                    f"参考情報:\n{context_block}\n\n質問: {question}"
                ),
            }
        )

        result = await self._llm.generate(role="rag", messages=messages)
        answer = str(result.get("content", result.get("text", "")))

        sources = [
            {"index": r.index, "score": r.score, "text": r.text[:200]}
            for r in ranked
        ]

        return RAGResult(
            answer=answer,
            sources=sources,
            metadata={"top_k": top_k, **(metadata or {})},
        )

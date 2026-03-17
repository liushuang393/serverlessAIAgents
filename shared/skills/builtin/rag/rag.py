"""RAG Skill - 検索増強生成（Retrieval-Augmented Generation）能力.

このモジュールは、UnifiedRAGService を利用して RAG 機能を提供します。
"""

from __future__ import annotations
import logging
from typing import Any, Dict, List, Optional
from dataclasses import dataclass, field


from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from shared.services.unified_rag import UnifiedRAGService



def _get_skill_base() -> type:
    """遅延インポート: kernel.skills.base.Skill（L2→L3 違反回避）."""
    from kernel.skills.base import Skill as _Skill
    return _Skill


@dataclass
class RAGConfig:
    """RAG 設定."""
    top_k: int = 5
    min_similarity: float = 0.3
    system_prompt: str = "あなたは知識ベースに基づいて質問に答えるアシスタントです。"
    context_template: str = "参考情報:\n{context}\n\n質問: {query}"

@dataclass
class RAGResult:
    """RAG 結果."""
    answer: str
    sources: List[Dict[str, Any]] = field(default_factory=list)
    context_used: str = ""

class RAGSkill(_get_skill_base()):
    """RAG Skill - 統合RAGサービスを利用した実装."""

    def __init__(
        self,
        collection_name: str = "default",
        rag_config: Optional[RAGConfig] = None,
    ) -> None:
        """初期化."""
        super().__init__()
        self._rag_config = rag_config or RAGConfig()
        self.rag_service = UnifiedRAGService(collection_name=collection_name)
        self._logger = logging.getLogger(__name__)

    async def start(self) -> None:
        """開始."""
        await self.rag_service._ensure_connected()
        self._logger.info("RAG Skill started")

    async def stop(self) -> None:
        """停止."""
        await self.rag_service.close()
        self._logger.info("RAG Skill stopped")

    async def add_document(
        self,
        content: str,
        topic: str = "default",
        metadata: Optional[Dict[str, Any]] = None,
    ) -> str:
        """ドキュメント追加."""
        ids = await self.rag_service.add_documents(
            documents=[content],
            metadatas=[{"topic": topic, **(metadata or {})}]
        )
        return ids[0]

    async def query(
        self,
        question: str,
        topic: Optional[str] = None,
    ) -> RAGResult:
        """クエリ実行."""
        filter = {"topic": topic} if topic else None
        
        # 1. 検索
        docs = await self.rag_service.retrieve(
            query=question,
            top_k=self._rag_config.top_k,
            filter=filter
        )
        
        # 2. コンテキスト構築
        context = "\n---\n".join([d["document"] for d in docs]) if docs else "関連情報なし"
        
        # 3. 生成
        answer = await self.rag_service.query(
            query=question,
            top_k=self._rag_config.top_k,
            filter=filter,
            system_prompt=self._rag_config.system_prompt
        )
        
        return RAGResult(
            answer=answer,
            sources=docs,
            context_used=context
        )

    def get_status(self) -> Dict[str, Any]:
        """ステータス取得."""
        return {
            "collection": self.rag_service.collection_name,
            "config": {
                "top_k": self._rag_config.top_k,
                "min_similarity": self._rag_config.min_similarity,
            }
        }

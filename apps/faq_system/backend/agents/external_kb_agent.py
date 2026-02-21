"""対客KB Agent - 公開情報専用.

対客向け知識ベース専用のAgent。
公開可能な情報のみを提供。

設計原則:
- 公開可能資料のみアクセス
- 物理的に隔離された知識ベース
- 社内情報の漏洩防止

使用例:
    >>> from apps.faq_system.backend.agents import ExternalKBAgent
    >>>
    >>> agent = ExternalKBAgent()
    >>> result = await agent.run({
    ...     "question": "製品Aの保証期間は？",
    ... })
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from agentflow.core import ResilientAgent
from agentflow.knowledge.isolated_kb import IsolatedKBManager, KBType


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)


@dataclass
class ExternalKBConfig:
    """対客KB Agent 設定."""

    collection: str = "external_kb"
    top_k: int = 5
    min_similarity: float = 0.4

    # 引用設定
    require_citation: bool = True

    # 信頼度
    min_confidence: float = 0.5

    # LLM設定
    temperature: float = 0.3


class ExternalKBResponse(BaseModel):
    """対客KB Agent レスポンス."""

    question: str = ""
    answer: str = ""
    confidence: float = 0.0

    citations: list[dict[str, Any]] = Field(default_factory=list)
    suggestions: list[dict[str, str]] = Field(default_factory=list)

    execution_time_ms: float = 0
    error: str = ""


class ExternalKBAgent(ResilientAgent):
    """対客KB Agent（公開情報専用）.

    対客向け知識ベース専用のAgent。

    特徴:
    - 公開可能資料のみアクセス
    - 認証不要
    - 社内情報は絶対に含まない
    """

    name = "ExternalKBAgent"

    SYSTEM_PROMPT = """あなたは製品・サービスに関するカスタマーサポートアシスタントです。

職責:
1. 製品やサービスに関する質問に回答する
2. 公開されている情報のみを使用する
3. わからないことは正直に伝える

回答ルール:
- お客様に親切で丁寧な対応
- 公開情報に基づいた正確な回答
- 社内情報や機密情報は絶対に含めない"""

    def __init__(
        self,
        config: ExternalKBConfig | None = None,
        kb_manager: IsolatedKBManager | None = None,
    ) -> None:
        """初期化."""
        super().__init__()
        self._config = config or ExternalKBConfig()
        self._kb_manager = kb_manager
        self._logger = logging.getLogger(self.name)
        self._initialized = False

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent 実行."""
        start_time = datetime.now()
        question = input_data.get("question", "")

        if not question:
            return ExternalKBResponse(error="質問が指定されていません").model_dump()

        await self._ensure_initialized()

        try:
            # 検索実行（対客KBのみ）
            search_results = await self._search(question)

            # 回答生成
            response = await self._generate_answer(question, search_results)
            response.question = question

            # 提案生成
            response.suggestions = self._generate_suggestions(question)

            response.execution_time_ms = (datetime.now() - start_time).total_seconds() * 1000

            return response.model_dump()

        except Exception as e:
            self._logger.exception("ExternalKBAgent エラー: %s", e)
            return ExternalKBResponse(
                question=question,
                error=str(e),
            ).model_dump()

    async def run_stream(self, input_data: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行."""
        yield {"type": "progress", "progress": 0, "message": "処理中..."}

        result = await self.run(input_data)

        yield {"type": "progress", "progress": 100, "message": "完了"}
        yield {"type": "result", "data": result}

    async def _search(self, question: str) -> list[dict[str, Any]]:
        """検索実行（対客KBのみ）."""
        if self._kb_manager:
            results = await self._kb_manager.search(
                kb_type=KBType.EXTERNAL,
                query=question,
                top_k=self._config.top_k,
            )
            return [
                {
                    "document_id": r.document_id,
                    "content": r.content,
                    "score": r.score,
                    "citation": r.citation,
                }
                for r in results
            ]
        return []

    async def _generate_answer(
        self,
        question: str,
        search_results: list[dict[str, Any]],
    ) -> ExternalKBResponse:
        """回答生成."""
        if not search_results:
            return ExternalKBResponse(
                answer="申し訳ございません。ご質問に関する情報が見つかりませんでした。"
                "カスタマーサポートまでお問い合わせください。",
                confidence=0.0,
            )

        top_result = search_results[0]
        confidence = top_result.get("score", 0.5)

        if confidence < self._config.min_confidence:
            return ExternalKBResponse(
                answer="ご質問の内容について、適切な情報を見つけることができませんでした。"
                "お手数ですが、カスタマーサポートまでお問い合わせください。",
                confidence=confidence,
            )

        citations = [
            {
                "index": i + 1,
                "title": r.get("citation", {}).get("title", ""),
                "source": r.get("citation", {}).get("source", ""),
                "snippet": r.get("content", "")[:150],
            }
            for i, r in enumerate(search_results[:3])
        ]

        answer = f"お問い合わせありがとうございます。\n\n{top_result['content'][:500]}\n\n詳細は [1] をご参照ください。"

        return ExternalKBResponse(
            answer=answer,
            confidence=confidence,
            citations=citations,
        )

    def _generate_suggestions(self, question: str) -> list[dict[str, str]]:
        """提案生成."""
        return [
            {"text": "他に質問はありますか？", "type": "followup"},
            {"text": "詳細資料をご希望ですか？", "type": "action"},
        ]

    async def _ensure_initialized(self) -> None:
        """初期化確認."""
        if self._initialized:
            return

        if not self._kb_manager:
            self._kb_manager = IsolatedKBManager()
            await self._kb_manager.start()

        self._initialized = True


__all__ = [
    "ExternalKBAgent",
    "ExternalKBConfig",
    "ExternalKBResponse",
]

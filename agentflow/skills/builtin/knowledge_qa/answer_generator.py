# -*- coding: utf-8 -*-
"""回答生成スキル - Answer Generator.

検索結果に基づいて回答を生成するスキル。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from agentflow.core.agent_block import AgentBlock


logger = logging.getLogger(__name__)


@dataclass
class AnswerConfig:
    """回答設定."""

    max_tokens: int = 500
    temperature: float = 0.3
    include_citations: bool = True
    language: str = "ja"


@dataclass
class Citation:
    """引用."""

    doc_id: str
    title: str
    snippet: str
    relevance_score: float


@dataclass
class GeneratedAnswer:
    """生成された回答."""

    answer: str
    confidence: float
    citations: list[Citation]
    need_human_review: bool
    generated_at: datetime = field(default_factory=datetime.now)


class AnswerGenerator(AgentBlock):
    """回答生成スキル."""

    def __init__(
        self,
        config: AnswerConfig | None = None,
        llm_client: Any | None = None,
    ) -> None:
        super().__init__()
        self._config = config or AnswerConfig()
        self._llm_client = llm_client

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """スキル実行."""
        question = input_data.get("question", "")
        context = input_data.get("context", [])

        result = await self.generate(question=question, context=context)

        return {
            "answer": result.answer,
            "confidence": result.confidence,
            "citations": [
                {
                    "doc_id": c.doc_id,
                    "title": c.title,
                    "snippet": c.snippet,
                }
                for c in result.citations
            ],
            "need_human_review": result.need_human_review,
            "generated_at": result.generated_at.isoformat(),
        }

    async def generate(
        self,
        question: str,
        context: list[dict[str, Any]] | None = None,
    ) -> GeneratedAnswer:
        """回答を生成."""
        context = context or []
        logger.info("回答生成開始: question=%s", question[:50])

        # LLMを使用して回答生成
        if self._llm_client:
            try:
                context_text = "\n".join(
                    c.get("content", "") for c in context
                )
                prompt = f"""以下のコンテキストに基づいて質問に回答してください。

コンテキスト:
{context_text}

質問: {question}

回答:"""
                response = await self._llm_client.chat([
                    {"role": "user", "content": prompt}
                ])
                answer = response.get("content", "")
                confidence = 0.9
            except Exception as e:
                logger.warning("LLM回答生成エラー: %s", e)
                answer = "申し訳ありませんが、回答を生成できませんでした。"
                confidence = 0.0
        else:
            # フォールバック
            answer = f"質問「{question}」に対する回答: コンテキストに基づく情報です。"
            confidence = 0.8

        # 引用生成
        citations = [
            Citation(
                doc_id=ctx.get("doc_id", "unknown"),
                title=ctx.get("title", "ドキュメント"),
                snippet=ctx.get("content", "")[:100],
                relevance_score=ctx.get("score", 0.8),
            )
            for ctx in context[:3]
        ]

        return GeneratedAnswer(
            answer=answer,
            confidence=confidence,
            citations=citations,
            need_human_review=confidence < 0.7,
        )

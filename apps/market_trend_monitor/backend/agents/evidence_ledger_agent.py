"""証拠台帳エージェント.

全ての結論に対する証拠の追跡可能性を確保します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
- 型安全：Pydantic による I/O 検証
- 健壮性：ResilientAgent によるリトライ・タイムアウト制御
"""

import logging
from typing import Any

from apps.market_trend_monitor.backend.models import (
    Article,
    ArticleSchema,
    EvidenceSchema,
    SourceType,
)
from apps.market_trend_monitor.backend.services import EvidenceService
from pydantic import BaseModel, Field

from agentflow import ResilientAgent


# ============================================================
# Agent I/O スキーマ
# ============================================================


class EvidenceLedgerInput(BaseModel):
    """EvidenceLedgerAgent 入力スキーマ."""

    articles: list[ArticleSchema] = Field(default_factory=list)


class EvidenceLedgerOutput(BaseModel):
    """EvidenceLedgerAgent 出力スキーマ."""

    evidence_ids: list[str] = Field(default_factory=list)
    total_registered: int = 0
    sources_verified: int = 0
    evidences: list[EvidenceSchema] = Field(default_factory=list)


# ============================================================
# Agent 実装
# ============================================================


class EvidenceLedgerAgent(
    ResilientAgent[EvidenceLedgerInput, EvidenceLedgerOutput]
):
    """証拠台帳エージェント（ResilientAgent 継承・型安全）.

    役割:
    - 収集した記事を証拠として登録
    - 証拠の信頼度評価
    - 重複検出と排除

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます（松耦合）。
        ResilientAgent によりリトライ・タイムアウトが自動制御されます。
    """

    # ResilientAgent 設定
    name = "EvidenceLedgerAgent"
    temperature = 0.2  # 証拠評価は低温度

    def __init__(
        self,
        llm_client: Any = None,
        evidence_service: EvidenceService | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント（None の場合は自動取得）
            evidence_service: 証拠サービス（None の場合は新規作成）
        """
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)
        self._evidence_service = evidence_service or EvidenceService()

    def _parse_input(
        self, input_data: dict[str, Any]
    ) -> EvidenceLedgerInput:
        """入力データを Pydantic モデルに変換."""
        return EvidenceLedgerInput(**input_data)

    async def process(
        self, input_data: EvidenceLedgerInput
    ) -> EvidenceLedgerOutput:
        """証拠登録を実行.

        Args:
            input_data: 型付き入力データ

        Returns:
            型付き登録結果
        """
        articles = input_data.articles
        self._logger.info(
            f"Processing {len(articles)} articles for evidence registration"
        )

        evidence_ids: list[str] = []
        evidences: list[EvidenceSchema] = []
        sources: set[str] = set()

        for article_schema in articles:
            # ArticleSchema から Article に変換
            article = self._schema_to_article(article_schema)

            # 証拠として登録
            evidence = await self._evidence_service.register_evidence_from_article(article)

            evidence_ids.append(evidence.id)
            sources.add(article_schema.source)

            # EvidenceSchema に変換
            evidences.append(
                EvidenceSchema(
                    id=evidence.id,
                    source_id=evidence.source_id,
                    source_type=evidence.source_type.value,
                    url=evidence.url,
                    title=evidence.title,
                    content_hash=evidence.content_hash,
                    extracted_data=evidence.extracted_data,
                    collected_at=evidence.collected_at.isoformat(),
                    reliability_score=evidence.reliability_score,
                    metadata=evidence.metadata,
                )
            )

        self._logger.info(
            f"Registered {len(evidence_ids)} evidences from "
            f"{len(sources)} sources"
        )

        return EvidenceLedgerOutput(
            evidence_ids=evidence_ids,
            total_registered=len(evidence_ids),
            sources_verified=len(sources),
            evidences=evidences,
        )

    def _schema_to_article(self, schema: ArticleSchema) -> Article:
        """ArticleSchema を Article に変換."""
        from datetime import datetime

        return Article(
            id=schema.id,
            title=schema.title,
            url=schema.url,
            source=SourceType(schema.source),
            published_at=datetime.fromisoformat(schema.published_at),
            content=schema.content,
            keywords=schema.keywords,
            collected_at=datetime.fromisoformat(schema.collected_at),
            metadata=schema.metadata,
        )

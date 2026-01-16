# -*- coding: utf-8 -*-
"""Evidence Ledger サービス.

証拠台帳の管理を行うビジネスロジック層。
- 証拠の登録・検索・更新
- 主張の管理とレベル自動昇格
- 証拠チェーンの構築
"""

import hashlib
import logging
import uuid
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.models import (
    Article,
    Claim,
    ClaimLevel,
    Evidence,
    SourceType,
)


class EvidenceService:
    """証拠管理サービス.

    全ての結論に対する証拠の追跡可能性を確保します。
    """

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        # インメモリストレージ（将来的にDB化）
        self._evidences: dict[str, Evidence] = {}
        self._claims: dict[str, Claim] = {}
        # 情報源別信頼度設定
        self._source_reliability: dict[SourceType, float] = {
            SourceType.NEWS: 0.6,
            SourceType.GITHUB: 0.8,
            SourceType.ARXIV: 0.9,
            SourceType.RSS: 0.5,
        }

    def register_evidence_from_article(self, article: Article) -> Evidence:
        """記事から証拠を登録.

        Args:
            article: 収集した記事

        Returns:
            登録された証拠
        """
        content_hash = self._compute_content_hash(article.url, article.content)

        # 重複チェック
        existing = self._find_by_hash(content_hash)
        if existing:
            self._logger.debug(f"Duplicate evidence found: {existing.id}")
            return existing

        evidence = Evidence(
            id=str(uuid.uuid4()),
            source_id=article.id,
            source_type=article.source,
            url=article.url,
            title=article.title,
            content_hash=content_hash,
            extracted_data={
                "keywords": article.keywords,
                "metadata": article.metadata,
            },
            collected_at=article.collected_at,
            reliability_score=self._calculate_reliability(article.source),
        )

        self._evidences[evidence.id] = evidence
        self._logger.info(f"Evidence registered: {evidence.id}")
        return evidence

    def register_evidences_batch(
        self, articles: list[Article]
    ) -> list[Evidence]:
        """複数記事から証拠を一括登録.

        Args:
            articles: 記事リスト

        Returns:
            登録された証拠リスト
        """
        return [self.register_evidence_from_article(a) for a in articles]

    def create_claim(
        self,
        statement: str,
        evidence_ids: list[str],
    ) -> Claim:
        """主張を作成.

        Args:
            statement: 主張内容
            evidence_ids: 根拠となる証拠ID

        Returns:
            作成された主張
        """
        # 証拠の存在確認
        valid_evidence_ids = [
            eid for eid in evidence_ids if eid in self._evidences
        ]

        # 信頼度計算
        confidence = self._calculate_claim_confidence(valid_evidence_ids)

        claim = Claim(
            id=str(uuid.uuid4()),
            statement=statement,
            evidence_ids=valid_evidence_ids,
            confidence=confidence,
        )
        claim.update_level()

        self._claims[claim.id] = claim
        self._logger.info(
            f"Claim created: {claim.id}, level={claim.level.value}"
        )
        return claim

    def add_evidence_to_claim(
        self, claim_id: str, evidence_id: str
    ) -> Claim | None:
        """主張に証拠を追加.

        Args:
            claim_id: 主張ID
            evidence_id: 追加する証拠ID

        Returns:
            更新された主張（存在しない場合はNone）
        """
        claim = self._claims.get(claim_id)
        if not claim:
            return None

        if evidence_id not in self._evidences:
            return claim

        if evidence_id not in claim.evidence_ids:
            claim.evidence_ids.append(evidence_id)
            claim.confidence = self._calculate_claim_confidence(
                claim.evidence_ids
            )
            claim.update_level()

        return claim

    def get_evidence_chain(self, claim_id: str) -> list[Evidence]:
        """主張の証拠チェーンを取得.

        Args:
            claim_id: 主張ID

        Returns:
            証拠リスト（信頼度順）
        """
        claim = self._claims.get(claim_id)
        if not claim:
            return []

        evidences = [
            self._evidences[eid]
            for eid in claim.evidence_ids
            if eid in self._evidences
        ]
        return sorted(
            evidences, key=lambda e: e.reliability_score, reverse=True
        )

    def get_evidence(self, evidence_id: str) -> Evidence | None:
        """証拠を取得.

        Args:
            evidence_id: 証拠ID

        Returns:
            証拠（存在しない場合はNone）
        """
        return self._evidences.get(evidence_id)

    def get_claim(self, claim_id: str) -> Claim | None:
        """主張を取得.

        Args:
            claim_id: 主張ID

        Returns:
            主張（存在しない場合はNone）
        """
        return self._claims.get(claim_id)

    def list_evidences(
        self,
        source_type: SourceType | None = None,
        min_reliability: float = 0.0,
    ) -> list[Evidence]:
        """証拠一覧を取得.

        Args:
            source_type: フィルタ用ソースタイプ
            min_reliability: 最小信頼度

        Returns:
            証拠リスト
        """
        evidences = list(self._evidences.values())

        if source_type:
            evidences = [e for e in evidences if e.source_type == source_type]

        evidences = [
            e for e in evidences if e.reliability_score >= min_reliability
        ]

        return sorted(
            evidences, key=lambda e: e.collected_at, reverse=True
        )

    def list_claims(
        self,
        level: ClaimLevel | None = None,
        min_confidence: float = 0.0,
    ) -> list[Claim]:
        """主張一覧を取得.

        Args:
            level: フィルタ用主張レベル
            min_confidence: 最小信頼度

        Returns:
            主張リスト
        """
        claims = list(self._claims.values())

        if level:
            claims = [c for c in claims if c.level == level]

        claims = [c for c in claims if c.confidence >= min_confidence]

        return sorted(claims, key=lambda c: c.updated_at, reverse=True)

    def _compute_content_hash(self, url: str, content: str) -> str:
        """コンテンツハッシュを計算."""
        data = f"{url}:{content}".encode("utf-8")
        return hashlib.sha256(data).hexdigest()[:16]

    def _find_by_hash(self, content_hash: str) -> Evidence | None:
        """ハッシュで証拠を検索."""
        for evidence in self._evidences.values():
            if evidence.content_hash == content_hash:
                return evidence
        return None

    def _calculate_reliability(self, source_type: SourceType) -> float:
        """情報源タイプに基づく信頼度を計算."""
        return self._source_reliability.get(source_type, 0.5)

    def _calculate_claim_confidence(
        self, evidence_ids: list[str]
    ) -> float:
        """証拠に基づく主張信頼度を計算.

        計算方法:
        - 各証拠の信頼度の加重平均
        - 証拠数によるボーナス（最大+0.15）
        """
        if not evidence_ids:
            return 0.0

        evidences = [
            self._evidences[eid]
            for eid in evidence_ids
            if eid in self._evidences
        ]

        if not evidences:
            return 0.0

        # 平均信頼度
        avg_reliability = sum(
            e.reliability_score for e in evidences
        ) / len(evidences)

        # 証拠数ボーナス（5件で最大+0.15）
        evidence_bonus = min(len(evidences) / 5, 1.0) * 0.15

        return min(avg_reliability + evidence_bonus, 1.0)

"""Evidence Ledger ユニットテスト.

Evidence/Claim モデルおよび EvidenceService のテスト。
"""

from __future__ import annotations

import hashlib
from datetime import datetime, timedelta

import pytest
from apps.market_trend_monitor.backend.db.base import Base
from apps.market_trend_monitor.backend.models import (
    Article,
    Claim,
    ClaimLevel,
    Evidence,
    SourceType,
)
from apps.market_trend_monitor.backend.services.evidence_service import EvidenceService
from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker, create_async_engine


# ============================================================
# Model Tests
# ============================================================


class TestEvidenceModel:
    """Evidence データモデルのテスト."""

    def test_evidence_creation(self) -> None:
        """証拠の生成テスト."""
        evidence = Evidence(
            id="ev-1",
            source_id="article-1",
            source_type=SourceType.NEWS,
            url="https://example.com/article",
            title="テスト記事",
            content_hash="abc123",
            extracted_data={"keywords": ["AI"]},
            reliability_score=0.7,
        )
        assert evidence.id == "ev-1"
        assert evidence.source_type == SourceType.NEWS
        assert evidence.reliability_score == 0.7

    def test_evidence_to_dict(self) -> None:
        """to_dict 変換テスト."""
        evidence = Evidence(
            id="ev-1",
            source_id="article-1",
            source_type=SourceType.ARXIV,
            url="https://arxiv.org/abs/1234",
            title="論文タイトル",
            content_hash="def456",
        )
        d = evidence.to_dict()
        assert d["id"] == "ev-1"
        assert d["source_type"] == "arxiv"
        assert d["url"] == "https://arxiv.org/abs/1234"
        assert "collected_at" in d

    def test_evidence_default_reliability(self) -> None:
        """デフォルト信頼度テスト."""
        evidence = Evidence(
            id="ev-1",
            source_id="s-1",
            source_type=SourceType.RSS,
            url="https://example.com",
            title="test",
            content_hash="hash1",
        )
        assert evidence.reliability_score == 0.5


class TestClaimModel:
    """Claim データモデルのテスト."""

    def test_claim_creation(self) -> None:
        """主張の生成テスト."""
        claim = Claim(id="c-1", statement="AI市場は成長する")
        assert claim.id == "c-1"
        assert claim.level == ClaimLevel.LEAD
        assert claim.confidence == 0.0
        assert claim.evidence_ids == []

    def test_claim_to_dict(self) -> None:
        """to_dict 変換テスト."""
        claim = Claim(
            id="c-1",
            statement="テスト主張",
            level=ClaimLevel.HYPOTHESIS,
            confidence=0.6,
            evidence_ids=["ev-1", "ev-2"],
        )
        d = claim.to_dict()
        assert d["level"] == "hypothesis"
        assert d["confidence"] == 0.6
        assert len(d["evidence_ids"]) == 2

    def test_claim_update_level_lead(self) -> None:
        """LEAD レベルのテスト（証拠1件、信頼度低）."""
        claim = Claim(
            id="c-1",
            statement="test",
            evidence_ids=["ev-1"],
            confidence=0.3,
        )
        claim.update_level()
        assert claim.level == ClaimLevel.LEAD

    def test_claim_update_level_hypothesis(self) -> None:
        """HYPOTHESIS レベルのテスト（証拠2件以上、信頼度0.5-0.7）."""
        claim = Claim(
            id="c-1",
            statement="test",
            evidence_ids=["ev-1", "ev-2"],
            confidence=0.55,
        )
        claim.update_level()
        assert claim.level == ClaimLevel.HYPOTHESIS

    def test_claim_update_level_finding(self) -> None:
        """FINDING レベルのテスト（証拠3件以上、信頼度0.7-0.85）."""
        claim = Claim(
            id="c-1",
            statement="test",
            evidence_ids=["ev-1", "ev-2", "ev-3"],
            confidence=0.75,
        )
        claim.update_level()
        assert claim.level == ClaimLevel.FINDING

    def test_claim_update_level_conclusion(self) -> None:
        """CONCLUSION レベルのテスト（証拠5件以上、信頼度>0.85）."""
        claim = Claim(
            id="c-1",
            statement="test",
            evidence_ids=["ev-1", "ev-2", "ev-3", "ev-4", "ev-5"],
            confidence=0.9,
        )
        claim.update_level()
        assert claim.level == ClaimLevel.CONCLUSION

    def test_claim_update_level_boundary_not_promoted(self) -> None:
        """境界値テスト: 証拠数は十分でも信頼度が不足する場合."""
        claim = Claim(
            id="c-1",
            statement="test",
            evidence_ids=["ev-1", "ev-2", "ev-3"],
            confidence=0.65,
        )
        claim.update_level()
        assert claim.level == ClaimLevel.HYPOTHESIS


class TestClaimLevel:
    """ClaimLevel Enum のテスト."""

    def test_claim_level_values(self) -> None:
        """全レベルの値テスト."""
        assert ClaimLevel.LEAD.value == "lead"
        assert ClaimLevel.HYPOTHESIS.value == "hypothesis"
        assert ClaimLevel.FINDING.value == "finding"
        assert ClaimLevel.CONCLUSION.value == "conclusion"


# ============================================================
# Service Tests
# ============================================================


@pytest.fixture
async def test_session_factory() -> async_sessionmaker[AsyncSession]:
    """テスト用インメモリDBセッションファクトリ."""
    engine = create_async_engine("sqlite+aiosqlite:///:memory:", echo=False)
    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)

    factory: async_sessionmaker[AsyncSession] = async_sessionmaker(engine, expire_on_commit=False)
    return factory


@pytest.fixture
def evidence_service(
    test_session_factory: async_sessionmaker[AsyncSession],
) -> EvidenceService:
    """テスト用 EvidenceService."""
    return EvidenceService(session_factory=test_session_factory)


def _make_article(
    article_id: str = "art-1",
    source: SourceType = SourceType.NEWS,
    title: str = "テスト記事",
    url: str = "https://example.com/article",
    content: str = "テスト内容",
    keywords: list[str] | None = None,
    collected_at: datetime | None = None,
) -> Article:
    """テスト用 Article を生成."""
    return Article(
        id=article_id,
        title=title,
        url=url,
        source=source,
        published_at=datetime.now(),
        content=content,
        keywords=keywords or ["AI", "test"],
        collected_at=collected_at or datetime.now(),
    )


class TestEvidenceService:
    """EvidenceService のテスト."""

    async def test_register_evidence_from_article(self, evidence_service: EvidenceService) -> None:
        """記事からの証拠登録テスト."""
        article = _make_article()
        evidence = await evidence_service.register_evidence_from_article(article)

        assert evidence.id is not None
        assert evidence.source_id == "art-1"
        assert evidence.source_type == SourceType.NEWS
        assert evidence.title == "テスト記事"

    async def test_duplicate_detection(self, evidence_service: EvidenceService) -> None:
        """重複検出テスト."""
        article = _make_article()
        ev1 = await evidence_service.register_evidence_from_article(article)
        ev2 = await evidence_service.register_evidence_from_article(article)

        assert ev1.id == ev2.id

    async def test_reliability_by_source_type(self, evidence_service: EvidenceService) -> None:
        """情報源別信頼度テスト."""
        news_article = _make_article(article_id="n-1", source=SourceType.NEWS)
        github_article = _make_article(
            article_id="g-1",
            source=SourceType.GITHUB,
            url="https://github.com/test",
            content="github content",
        )
        arxiv_article = _make_article(
            article_id="a-1",
            source=SourceType.ARXIV,
            url="https://arxiv.org/test",
            content="arxiv content",
        )
        rss_article = _make_article(
            article_id="r-1",
            source=SourceType.RSS,
            url="https://rss.example.com",
            content="rss content",
        )

        ev_news = await evidence_service.register_evidence_from_article(news_article)
        ev_github = await evidence_service.register_evidence_from_article(github_article)
        ev_arxiv = await evidence_service.register_evidence_from_article(arxiv_article)
        ev_rss = await evidence_service.register_evidence_from_article(rss_article)

        assert ev_news.reliability_score == pytest.approx(0.6)
        assert ev_github.reliability_score == pytest.approx(0.8)
        assert ev_arxiv.reliability_score == pytest.approx(0.9)
        assert ev_rss.reliability_score == pytest.approx(0.5)

    async def test_create_claim(self, evidence_service: EvidenceService) -> None:
        """主張作成テスト."""
        article = _make_article()
        evidence = await evidence_service.register_evidence_from_article(article)

        claim = await evidence_service.create_claim(
            statement="AI市場は成長する",
            evidence_ids=[evidence.id],
        )

        assert claim.id is not None
        assert claim.statement == "AI市場は成長する"
        assert len(claim.evidence_ids) == 1
        assert claim.confidence > 0.0

    async def test_create_claim_with_no_evidence(self, evidence_service: EvidenceService) -> None:
        """証拠なしの主張作成テスト."""
        claim = await evidence_service.create_claim(
            statement="根拠なし主張",
            evidence_ids=[],
        )
        assert claim.confidence == 0.0
        assert claim.level == ClaimLevel.LEAD

    async def test_add_evidence_to_claim(self, evidence_service: EvidenceService) -> None:
        """主張に証拠追加テスト."""
        art1 = _make_article(article_id="a1", url="https://example.com/1", content="c1")
        art2 = _make_article(article_id="a2", url="https://example.com/2", content="c2")
        ev1 = await evidence_service.register_evidence_from_article(art1)
        ev2 = await evidence_service.register_evidence_from_article(art2)

        claim = await evidence_service.create_claim(
            statement="テスト主張",
            evidence_ids=[ev1.id],
        )
        updated = await evidence_service.add_evidence_to_claim(claim.id, ev2.id)

        assert updated is not None
        assert len(updated.evidence_ids) == 2

    async def test_get_evidence_chain(self, evidence_service: EvidenceService) -> None:
        """証拠チェーン取得テスト."""
        art1 = _make_article(
            article_id="a1",
            source=SourceType.ARXIV,
            url="https://arxiv.org/1",
            content="arxiv1",
        )
        art2 = _make_article(
            article_id="a2",
            source=SourceType.NEWS,
            url="https://news.com/1",
            content="news1",
        )
        ev1 = await evidence_service.register_evidence_from_article(art1)
        ev2 = await evidence_service.register_evidence_from_article(art2)

        claim = await evidence_service.create_claim(
            statement="テスト",
            evidence_ids=[ev1.id, ev2.id],
        )
        chain = await evidence_service.get_evidence_chain(claim.id)

        assert len(chain) == 2
        # 信頼度の高い順にソートされている
        assert chain[0].reliability_score >= chain[1].reliability_score

    async def test_list_evidences_filter(self, evidence_service: EvidenceService) -> None:
        """証拠一覧フィルタテスト."""
        art_news = _make_article(article_id="n1", source=SourceType.NEWS)
        art_arxiv = _make_article(
            article_id="a1",
            source=SourceType.ARXIV,
            url="https://arxiv.org/2",
            content="arxiv2",
        )
        await evidence_service.register_evidence_from_article(art_news)
        await evidence_service.register_evidence_from_article(art_arxiv)

        all_evidences = await evidence_service.list_evidences()
        assert len(all_evidences) == 2

        news_only = await evidence_service.list_evidences(source_type=SourceType.NEWS)
        assert len(news_only) == 1
        assert news_only[0].source_type == SourceType.NEWS

        high_reliability = await evidence_service.list_evidences(min_reliability=0.8)
        assert len(high_reliability) == 1
        assert high_reliability[0].source_type == SourceType.ARXIV

    async def test_list_claims_filter(self, evidence_service: EvidenceService) -> None:
        """主張一覧フィルタテスト."""
        await evidence_service.create_claim(statement="claim1", evidence_ids=[])
        await evidence_service.create_claim(statement="claim2", evidence_ids=[])

        claims = await evidence_service.list_claims()
        assert len(claims) == 2

    async def test_get_evidence_not_found(self, evidence_service: EvidenceService) -> None:
        """存在しない証拠の取得テスト."""
        result = await evidence_service.get_evidence("nonexistent-id")
        assert result is None

    async def test_get_claim_not_found(self, evidence_service: EvidenceService) -> None:
        """存在しない主張の取得テスト."""
        result = await evidence_service.get_claim("nonexistent-id")
        assert result is None

    async def test_content_hash_computation(self, evidence_service: EvidenceService) -> None:
        """コンテンツハッシュ計算テスト."""
        h = evidence_service._compute_content_hash("https://example.com", "content")
        expected = hashlib.sha256(b"https://example.com:content").hexdigest()[:16]
        assert h == expected

    async def test_calculate_claim_level(self, evidence_service: EvidenceService) -> None:
        """主張レベル計算テスト."""
        assert evidence_service._calculate_claim_level(1, 0.3) == ClaimLevel.LEAD
        assert evidence_service._calculate_claim_level(2, 0.5) == ClaimLevel.HYPOTHESIS
        assert evidence_service._calculate_claim_level(3, 0.7) == ClaimLevel.FINDING
        assert evidence_service._calculate_claim_level(5, 0.9) == ClaimLevel.CONCLUSION

    async def test_register_batch(self, evidence_service: EvidenceService) -> None:
        """一括登録テスト."""
        articles = [
            _make_article(article_id=f"batch-{i}", url=f"https://example.com/{i}", content=f"c{i}") for i in range(3)
        ]
        results = await evidence_service.register_evidences_batch(articles)
        assert len(results) == 3

    async def test_get_grounding_guard_ready(self, evidence_service: EvidenceService) -> None:
        """Grounding Guard が ready を返すテスト."""
        long_content = "市場分析データ " * 20
        articles = [
            _make_article(
                article_id="n-ready",
                source=SourceType.NEWS,
                url="https://example.com/news-ready",
                content=long_content,
            ),
            _make_article(
                article_id="g-ready",
                source=SourceType.GITHUB,
                url="https://github.com/example/ready",
                content=long_content,
            ),
            _make_article(
                article_id="a-ready",
                source=SourceType.ARXIV,
                url="https://arxiv.org/abs/2501.00001",
                content=long_content,
            ),
        ]
        evidences = await evidence_service.register_evidences_batch(articles)
        claim = await evidence_service.create_claim(
            statement="AI市場は継続成長する",
            evidence_ids=[e.id for e in evidences],
        )
        guard = await evidence_service.get_grounding_guard()

        assert guard["status"] == "ready"
        assert guard["blockers"] == []
        assert guard["summary"]["citation_ready_ratio"] >= 0.7
        claim_diag = next(item for item in guard["claim_diagnostics"] if item["claim_id"] == claim.id)
        assert claim_diag["status"] == "supported"

    async def test_get_grounding_guard_needs_more_evidence(self, evidence_service: EvidenceService) -> None:
        """Grounding Guard が不足状態を検出するテスト."""
        stale = datetime.now() - timedelta(days=60)
        article = _make_article(
            article_id="rss-weak",
            source=SourceType.RSS,
            url="https://example.com/rss-weak",
            content="短い本文",
            collected_at=stale,
        )
        evidence = await evidence_service.register_evidence_from_article(article)
        await evidence_service.create_claim(
            statement="根拠が薄い主張",
            evidence_ids=[evidence.id],
        )

        guard = await evidence_service.get_grounding_guard()
        blocker_codes = {item["code"] for item in guard["blockers"]}

        assert guard["status"] == "needs_more_evidence"
        assert "LOW_EVIDENCE_COUNT" in blocker_codes
        assert "LOW_CITATION_READY_RATIO" in blocker_codes

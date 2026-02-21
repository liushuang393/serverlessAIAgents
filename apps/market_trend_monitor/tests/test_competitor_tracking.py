"""競合追跡ユニットテスト.

CompetitorTrackingAgent のテスト。
Mock LLM を使用（外部依存なし）。
"""

from __future__ import annotations

import json
from datetime import datetime
from unittest.mock import AsyncMock

from apps.market_trend_monitor.backend.agents.competitor_tracking_agent import (
    CompetitorProfile,
    CompetitorTrackingAgent,
    MarketPosition,
)
from apps.market_trend_monitor.backend.models import Article, SourceType


# ============================================================
# Helpers
# ============================================================


def _make_article(
    article_id: str = "a-1",
    title: str = "IBM launches new COBOL migration tool",
    content: str = "IBM announced a new AI-powered tool for legacy modernization",
    metadata: dict | None = None,
) -> Article:
    """テスト用 Article を生成."""
    return Article(
        id=article_id,
        title=title,
        url=f"https://example.com/{article_id}",
        source=SourceType.NEWS,
        published_at=datetime(2026, 1, 15),
        content=content,
        keywords=["COBOL", "migration"],
        metadata=metadata or {},
    )


def _make_mock_llm(response: str | None = None):
    """テスト用 Mock LLM を生成."""
    mock = AsyncMock()
    default_response = json.dumps(
        {
            "focus_areas": ["AI migration", "legacy modernization"],
            "market_position": "leader",
            "threat_level": 0.8,
            "opportunity_level": 0.6,
        }
    )
    mock.chat = AsyncMock(return_value=response or default_response)
    return mock


# ============================================================
# Model Tests
# ============================================================


class TestCompetitorProfile:
    """CompetitorProfile データモデルのテスト."""

    def test_profile_creation(self) -> None:
        """プロファイル生成テスト."""
        profile = CompetitorProfile(
            name="IBM",
            focus_areas=["AI", "modernization"],
            threat_level=0.8,
        )
        assert profile.name == "IBM"
        assert profile.threat_level == 0.8

    def test_profile_to_dict(self) -> None:
        """to_dict 変換テスト."""
        profile = CompetitorProfile(name="TCS")
        d = profile.to_dict()
        assert d["name"] == "TCS"
        assert "threat_level" in d
        assert "opportunity_level" in d

    def test_profile_defaults(self) -> None:
        """デフォルト値テスト."""
        profile = CompetitorProfile(name="Test")
        assert profile.focus_areas == []
        assert profile.market_position == "follower"
        assert profile.threat_level == 0.5

    def test_market_position_enum(self) -> None:
        """MarketPosition Enum テスト."""
        assert MarketPosition.LEADER.value == "leader"
        assert MarketPosition.CHALLENGER.value == "challenger"
        assert MarketPosition.FOLLOWER.value == "follower"
        assert MarketPosition.NICHE.value == "niche"


# ============================================================
# Service Tests
# ============================================================


class TestCompetitorTrackingAgent:
    """CompetitorTrackingAgent のテスト."""

    async def test_track_competitors_basic(self) -> None:
        """基本的な競合追跡テスト."""
        mock_llm = _make_mock_llm()
        agent = CompetitorTrackingAgent(
            llm=mock_llm,
            competitors=["IBM", "Accenture"],
        )

        articles = [
            _make_article("a-1", "IBM launches migration tool", "IBM AI modernization"),
            _make_article("a-2", "Accenture wins contract", "Accenture COBOL migration"),
        ]

        profiles = await agent.track_competitors(articles)
        assert len(profiles) == 2
        names = [p.name for p in profiles]
        assert "IBM" in names
        assert "Accenture" in names

    async def test_track_competitors_no_articles(self) -> None:
        """記事なしの競合追跡テスト."""
        agent = CompetitorTrackingAgent(
            llm=_make_mock_llm(),
            competitors=["IBM"],
        )
        articles = [
            _make_article("a-1", "Spring Boot update", "Java framework"),
        ]
        profiles = await agent.track_competitors(articles)
        assert len(profiles) == 0

    async def test_track_competitors_sorted_by_threat(self) -> None:
        """脅威度降順ソートテスト."""
        mock_llm = AsyncMock()
        call_count = [0]

        async def mock_chat(messages):
            call_count[0] += 1
            if call_count[0] == 1:
                return json.dumps({"threat_level": 0.3, "market_position": "follower"})
            return json.dumps({"threat_level": 0.9, "market_position": "leader"})

        mock_llm.chat = mock_chat
        agent = CompetitorTrackingAgent(
            llm=mock_llm,
            competitors=["IBM", "Accenture"],
        )

        articles = [
            _make_article("a-1", "IBM update", "IBM content"),
            _make_article("a-2", "Accenture news", "Accenture content"),
        ]
        profiles = await agent.track_competitors(articles)
        if len(profiles) >= 2:
            assert profiles[0].threat_level >= profiles[1].threat_level

    async def test_track_competitors_llm_failure(self) -> None:
        """LLM失敗時のフォールバックテスト."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(side_effect=Exception("LLM error"))
        agent = CompetitorTrackingAgent(
            llm=mock_llm,
            competitors=["IBM"],
        )

        articles = [_make_article("a-1", "IBM tool", "IBM content")]
        profiles = await agent.track_competitors(articles)
        assert len(profiles) == 1
        assert profiles[0].name == "IBM"
        assert profiles[0].metadata.get("analysis_failed") is True

    async def test_compare_positioning(self) -> None:
        """ポジショニング比較テスト."""
        agent = CompetitorTrackingAgent(
            llm=_make_mock_llm(),
            competitors=["IBM"],
        )

        # プロファイルを設定
        agent._profiles["IBM"] = CompetitorProfile(
            name="IBM",
            focus_areas=["AI migration"],
            market_position="leader",
            threat_level=0.8,
            opportunity_level=0.6,
        )

        result = await agent.compare_positioning(["AI expertise", "Japan market"])
        assert "our_strengths" in result
        assert "competitors" in result
        assert "IBM" in result["competitors"]
        assert len(result["threats"]) > 0

    async def test_compare_positioning_empty(self) -> None:
        """プロファイルなしのポジショニング比較テスト."""
        agent = CompetitorTrackingAgent(llm=_make_mock_llm())
        result = await agent.compare_positioning(["strength1"])
        assert result["competitors"] == {}
        assert result["threats"] == []

    def test_get_profile(self) -> None:
        """プロファイル取得テスト."""
        agent = CompetitorTrackingAgent(llm=_make_mock_llm())
        agent._profiles["IBM"] = CompetitorProfile(name="IBM")
        assert agent.get_profile("IBM") is not None
        assert agent.get_profile("Unknown") is None

    def test_list_profiles(self) -> None:
        """プロファイル一覧テスト."""
        agent = CompetitorTrackingAgent(llm=_make_mock_llm())
        agent._profiles["IBM"] = CompetitorProfile(name="IBM", threat_level=0.9)
        agent._profiles["TCS"] = CompetitorProfile(name="TCS", threat_level=0.5)

        profiles = agent.list_profiles()
        assert len(profiles) == 2
        assert profiles[0].threat_level >= profiles[1].threat_level

    def test_set_competitors_normalize(self) -> None:
        """追跡対象競合の正規化更新テスト."""
        agent = CompetitorTrackingAgent(llm=_make_mock_llm())
        updated = agent.set_competitors([" IBM ", "ibm", "Accenture", ""])
        assert updated == ["IBM", "Accenture"]
        assert agent.get_competitors() == ["IBM", "Accenture"]

    async def test_track_competitors_include_unmatched(self) -> None:
        """未検出競合をプレースホルダー表示するテスト."""
        agent = CompetitorTrackingAgent(
            llm=_make_mock_llm(),
            competitors=["IBM", "Accenture"],
        )
        articles = [_make_article("a-1", "IBM launch", "IBM updates")]
        profiles = await agent.track_competitors(articles, include_unmatched=True)
        assert len(profiles) == 2
        acc = next(p for p in profiles if p.name == "Accenture")
        assert int(acc.metadata.get("article_count", 0)) == 0
        assert acc.metadata.get("detection") == "not_found"

    def test_parse_analysis_valid(self) -> None:
        """正常なJSONパーステスト."""
        raw = '{"focus_areas": ["AI"], "threat_level": 0.8}'
        result = CompetitorTrackingAgent._parse_analysis(raw)
        assert result["threat_level"] == 0.8

    def test_parse_analysis_with_noise(self) -> None:
        """ノイズ付きJSONパーステスト."""
        raw = 'Analysis: {"market_position": "leader"} end'
        result = CompetitorTrackingAgent._parse_analysis(raw)
        assert result["market_position"] == "leader"

    def test_parse_analysis_invalid(self) -> None:
        """不正JSONパーステスト."""
        result = CompetitorTrackingAgent._parse_analysis("not json")
        assert result == {}

    async def test_track_case_insensitive(self) -> None:
        """大小文字非依存の競合マッチテスト."""
        agent = CompetitorTrackingAgent(
            llm=_make_mock_llm(),
            competitors=["IBM"],
        )
        articles = [
            _make_article("a-1", "ibm releases tool", "ibm content"),
        ]
        profiles = await agent.track_competitors(articles)
        assert len(profiles) == 1

    async def test_track_alias_full_name(self) -> None:
        """正式名称から正規名へ統一されるテスト."""
        agent = CompetitorTrackingAgent(
            llm=_make_mock_llm(),
            competitors=["IBM"],
        )
        articles = [
            _make_article(
                "a-1",
                "International Business Machines launches modernization suite",
                "International Business Machines expands AI migration tools",
            ),
        ]
        profiles = await agent.track_competitors(articles)
        assert len(profiles) == 1
        assert profiles[0].name == "IBM"
        aliases = list(profiles[0].metadata.get("matched_aliases", []))
        assert "International Business Machines" in aliases

    async def test_track_alias_from_metadata_entities(self) -> None:
        """メタデータエンティティから競合が検出できるテスト."""
        agent = CompetitorTrackingAgent(
            llm=_make_mock_llm(),
            competitors=["IBM"],
        )
        articles = [
            _make_article(
                "a-1",
                "Legacy migration trend report",
                "Industry report highlights platform shifts.",
                metadata={"entities": [{"name": "International Business Machines"}]},
            ),
        ]
        profiles = await agent.track_competitors(articles)
        assert len(profiles) == 1
        assert profiles[0].name == "IBM"

    async def test_set_competitor_aliases_custom(self) -> None:
        """カスタム別名を追加して検出できるテスト."""
        agent = CompetitorTrackingAgent(
            llm=_make_mock_llm(),
            competitors=["IBM"],
        )
        aliases = agent.set_competitor_aliases({"IBM": ["Big Blue"]})
        assert "Big Blue" in aliases["IBM"]

        articles = [
            _make_article(
                "a-1",
                "Big Blue invests in modernization",
                "Big Blue enters legacy transformation market",
            ),
        ]
        profiles = await agent.track_competitors(articles)
        assert len(profiles) == 1
        assert profiles[0].name == "IBM"

    async def test_existing_profile_preserved(self) -> None:
        """記事なし時の既存プロファイル保持テスト."""
        agent = CompetitorTrackingAgent(
            llm=_make_mock_llm(),
            competitors=["IBM"],
        )
        existing = CompetitorProfile(name="IBM", threat_level=0.9)
        agent._profiles["IBM"] = existing

        # IBMに言及しない記事
        articles = [_make_article("a-1", "unrelated", "content")]
        profiles = await agent.track_competitors(articles)
        assert len(profiles) == 1
        assert profiles[0].threat_level == 0.9

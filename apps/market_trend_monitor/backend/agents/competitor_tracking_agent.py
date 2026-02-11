"""競合追跡エージェント.

競合企業の動向を追跡し、市場ポジショニングを分析します。
"""

from __future__ import annotations

import json
import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from agentflow import ResilientAgent, get_llm

from apps.market_trend_monitor.backend.models import Article


class MarketPosition(str, Enum):
    """市場ポジション."""

    LEADER = "leader"
    CHALLENGER = "challenger"
    FOLLOWER = "follower"
    NICHE = "niche"


@dataclass
class CompetitorProfile:
    """競合プロファイルデータモデル."""

    name: str
    focus_areas: list[str] = field(default_factory=list)
    recent_activities: list[str] = field(default_factory=list)
    market_position: str = MarketPosition.FOLLOWER.value
    threat_level: float = 0.5
    opportunity_level: float = 0.5
    last_updated: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "name": self.name,
            "focus_areas": self.focus_areas,
            "recent_activities": self.recent_activities,
            "market_position": self.market_position,
            "threat_level": self.threat_level,
            "opportunity_level": self.opportunity_level,
            "last_updated": self.last_updated.isoformat(),
            "metadata": self.metadata,
        }


# 追跡対象競合企業
DEFAULT_COMPETITORS: list[str] = [
    "IBM", "Accenture", "TCS", "Infosys", "NTT DATA",
    "Fujitsu", "Wipro", "Capgemini", "DXC Technology", "Micro Focus",
]


class CompetitorTrackingAgent:
    """競合追跡エージェント.

    - 競合企業の動向追跡
    - 競合戦略の分析とマッピング
    - 市場ポジショニングの可視化データ生成
    """

    def __init__(
        self,
        *,
        llm: Any | None = None,
        competitors: list[str] | None = None,
    ) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._llm = llm
        self._competitors = competitors or DEFAULT_COMPETITORS
        self._profiles: dict[str, CompetitorProfile] = {}

    def _get_llm(self) -> Any:
        """LLMインスタンスを取得."""
        if self._llm is None:
            self._llm = get_llm(temperature=0.3)
        return self._llm

    async def track_competitors(
        self,
        articles: list[Article],
    ) -> list[CompetitorProfile]:
        """記事から競合動向を追跡.

        Args:
            articles: 分析対象記事リスト

        Returns:
            競合プロファイルリスト
        """
        competitor_articles: dict[str, list[Article]] = {
            c: [] for c in self._competitors
        }

        for article in articles:
            text = f"{article.title} {article.content}".lower()
            for competitor in self._competitors:
                if competitor.lower() in text:
                    competitor_articles[competitor].append(article)

        profiles: list[CompetitorProfile] = []
        for competitor, comp_articles in competitor_articles.items():
            if not comp_articles:
                # 記事なしの場合は既存プロファイルを維持
                if competitor in self._profiles:
                    profiles.append(self._profiles[competitor])
                continue

            profile = await self._analyze_competitor(competitor, comp_articles)
            self._profiles[competitor] = profile
            profiles.append(profile)

        return sorted(profiles, key=lambda p: p.threat_level, reverse=True)

    async def _analyze_competitor(
        self,
        competitor: str,
        articles: list[Article],
    ) -> CompetitorProfile:
        """競合を分析してプロファイルを生成."""
        activities = [a.title for a in articles[:5]]

        try:
            llm = self._get_llm()
            titles_text = "\n".join(activities)
            prompt = (
                f"Analyze the competitive position of {competitor} "
                f"in the COBOL to Java migration market.\n\n"
                f"Recent activities:\n{titles_text}\n\n"
                "Return JSON: {"
                '"focus_areas": ["..."], '
                '"market_position": "leader|challenger|follower|niche", '
                '"threat_level": 0.0-1.0, '
                '"opportunity_level": 0.0-1.0'
                "}\n\nJSON:"
            )
            response = await llm.chat([{"role": "user", "content": prompt}])
            raw = response if isinstance(response, str) else str(response)
            analysis = self._parse_analysis(raw)

            return CompetitorProfile(
                name=competitor,
                focus_areas=analysis.get("focus_areas", []),
                recent_activities=activities,
                market_position=analysis.get("market_position", "follower"),
                threat_level=float(analysis.get("threat_level", 0.5)),
                opportunity_level=float(analysis.get("opportunity_level", 0.5)),
                metadata={"article_count": len(articles)},
            )

        except Exception as e:
            self._logger.warning("競合分析失敗 %s: %s", competitor, e)
            return CompetitorProfile(
                name=competitor,
                recent_activities=activities,
                metadata={"article_count": len(articles), "analysis_failed": True},
            )

    async def compare_positioning(
        self,
        our_strengths: list[str],
    ) -> dict[str, Any]:
        """市場ポジショニング比較.

        Args:
            our_strengths: 自社の強み

        Returns:
            ポジショニング比較結果
        """
        positioning = {
            "our_strengths": our_strengths,
            "competitors": {},
            "opportunities": [],
            "threats": [],
        }

        for name, profile in self._profiles.items():
            positioning["competitors"][name] = {
                "position": profile.market_position,
                "threat": profile.threat_level,
                "opportunity": profile.opportunity_level,
                "focus_areas": profile.focus_areas,
            }
            if profile.threat_level >= 0.7:
                positioning["threats"].append(
                    f"{name}: high threat (focus: {', '.join(profile.focus_areas[:3])})"
                )
            if profile.opportunity_level >= 0.6:
                positioning["opportunities"].append(
                    f"{name} gap: potential opportunity (level: {profile.opportunity_level:.1f})"
                )

        return positioning

    def get_profile(self, competitor: str) -> CompetitorProfile | None:
        """競合プロファイルを取得."""
        return self._profiles.get(competitor)

    def list_profiles(self) -> list[CompetitorProfile]:
        """全プロファイルを取得."""
        return sorted(
            self._profiles.values(),
            key=lambda p: p.threat_level,
            reverse=True,
        )

    @staticmethod
    def _parse_analysis(raw: str) -> dict[str, Any]:
        """LLMレスポンスをパース."""
        try:
            start = raw.find("{")
            end = raw.rfind("}")
            if start != -1 and end != -1 and end > start:
                return json.loads(raw[start:end + 1])
        except (json.JSONDecodeError, ValueError):
            pass
        return {}

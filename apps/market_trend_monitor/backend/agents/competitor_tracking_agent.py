"""競合追跡エージェント.

競合企業の動向を追跡し、市場ポジショニングを分析します。
"""

from __future__ import annotations

import json
import logging
import re
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from agentflow import get_llm


if TYPE_CHECKING:
    from apps.market_trend_monitor.backend.models import Article

from agentflow import get_llm


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
    "IBM",
    "Accenture",
    "TCS",
    "Infosys",
    "NTT DATA",
    "Fujitsu",
    "Wipro",
    "Capgemini",
    "DXC Technology",
    "Micro Focus",
    "Deloitte",
    "OptiSol Business Solutions",
    "Daiwa Institute of Research",
]

# 競合企業別の別名辞書（Entity Normalization 用）
DEFAULT_COMPETITOR_ALIASES: dict[str, list[str]] = {
    "IBM": [
        "International Business Machines",
        "IBM Corporation",
        "IBM Corp",
    ],
    "Accenture": [
        "Accenture plc",
    ],
    "TCS": [
        "Tata Consultancy Services",
    ],
    "Infosys": [
        "Infosys Limited",
    ],
    "NTT DATA": [
        "NTT Data",
        "NTT DATA Group",
    ],
    "Fujitsu": [
        "Fujitsu Limited",
    ],
    "Wipro": [
        "Wipro Limited",
    ],
    "Capgemini": [
        "Cap Gemini",
    ],
    "DXC Technology": [
        "DXC",
        "DXC Tech",
    ],
    "Micro Focus": [
        "OpenText Micro Focus",
        "MicroFocus",
    ],
    "TIS": [
        "TIS Inc.",
        "TIS INTEC Group",
        "TIS株式会社",
        "ティーアイエス",
    ],
    "Deloitte": [
        "Deloitte Touche Tohmatsu",
        "デロイト",
        "デロイト トーマツ",
        "DTT",
    ],
    "OptiSol Business Solutions": [
        "OptiSol",
        "OptiSol Business",
    ],
    "Daiwa Institute of Research": [
        "DIR",
        "株式会社大和総研",
        "大和総研",
    ],
}


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
        competitor_aliases: dict[str, list[str]] | None = None,
    ) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._llm = llm
        self._competitors = competitors or DEFAULT_COMPETITORS
        self._competitor_aliases = self._build_default_aliases(self._competitors)
        if competitor_aliases:
            self._merge_aliases(competitor_aliases)
        self._alias_lookup: dict[str, str] = {}
        self._alias_patterns: list[tuple[str, str, re.Pattern[str]]] = []
        self._rebuild_alias_index()
        self._profiles: dict[str, CompetitorProfile] = {}

    def _get_llm(self) -> Any:
        """LLMインスタンスを取得."""
        if self._llm is None:
            self._llm = get_llm(temperature=0.3)
        return self._llm

    async def track_competitors(
        self,
        articles: list[Article],
        *,
        include_unmatched: bool = False,
    ) -> list[CompetitorProfile]:
        """記事から競合動向を追跡.

        Args:
            articles: 分析対象記事リスト

        Returns:
            競合プロファイルリスト
        """
        competitor_articles: dict[str, list[Article]] = {c: [] for c in self._competitors}
        alias_hit_counts: dict[str, dict[str, int]] = {c: {} for c in self._competitors}

        for article in articles:
            mentions = self._extract_competitor_mentions(article)
            for competitor, aliases in mentions.items():
                competitor_articles[competitor].append(article)
                for alias in aliases:
                    current = alias_hit_counts[competitor].get(alias, 0)
                    alias_hit_counts[competitor][alias] = current + 1

        profiles: list[CompetitorProfile] = []
        for competitor, comp_articles in competitor_articles.items():
            if not comp_articles:
                # 記事なしの場合は既存プロファイルを維持
                if competitor in self._profiles:
                    profiles.append(self._profiles[competitor])
                elif include_unmatched:
                    profiles.append(
                        CompetitorProfile(
                            name=competitor,
                            market_position=MarketPosition.NICHE.value,
                            threat_level=0.0,
                            opportunity_level=0.0,
                            metadata={
                                "article_count": 0,
                                "detection": "not_found",
                            },
                        )
                    )
                continue

            profile = await self._analyze_competitor(competitor, comp_articles)
            matched_aliases = sorted(alias_hit_counts[competitor].keys())
            profile.metadata = {
                **profile.metadata,
                "matched_aliases": matched_aliases,
                "alias_hit_count": sum(alias_hit_counts[competitor].values()),
            }
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
                positioning["threats"].append(f"{name}: high threat (focus: {', '.join(profile.focus_areas[:3])})")
            if profile.opportunity_level >= 0.6:
                positioning["opportunities"].append(
                    f"{name} gap: potential opportunity (level: {profile.opportunity_level:.1f})"
                )

        return positioning

    def get_profile(self, competitor: str) -> CompetitorProfile | None:
        """競合プロファイルを取得."""
        return self._profiles.get(competitor)

    def get_competitors(self) -> list[str]:
        """追跡対象競合一覧を取得."""
        return list(self._competitors)

    def set_competitors(self, competitors: list[str]) -> list[str]:
        """追跡対象競合一覧を更新."""
        normalized = self._normalize_competitors(competitors)
        if not normalized:
            normalized = self._normalize_competitors(DEFAULT_COMPETITORS)
        previous_aliases = self._competitor_aliases
        self._competitors = normalized
        self._competitor_aliases = self._build_default_aliases(self._competitors)
        self._merge_aliases(previous_aliases)
        allowed = set(self._competitors)
        self._profiles = {name: profile for name, profile in self._profiles.items() if name in allowed}
        self._rebuild_alias_index()
        return self.get_competitors()

    def get_competitor_aliases(self) -> dict[str, list[str]]:
        """競合別の別名辞書を取得."""
        return {key: list(values) for key, values in self._competitor_aliases.items()}

    def set_competitor_aliases(
        self, competitor_aliases: dict[str, list[str]]
    ) -> dict[str, list[str]]:
        """競合別の別名辞書を更新."""
        self._merge_aliases(competitor_aliases)
        self._rebuild_alias_index()
        return self.get_competitor_aliases()

    def list_profiles(self) -> list[CompetitorProfile]:
        """全プロファイルを取得."""
        return sorted(
            self._profiles.values(),
            key=lambda p: p.threat_level,
            reverse=True,
        )

    @staticmethod
    def _normalize_competitors(competitors: list[str]) -> list[str]:
        """競合一覧を正規化."""
        normalized: list[str] = []
        seen: set[str] = set()
        for item in competitors:
            name = item.strip()
            if not name:
                continue
            key = name.casefold()
            if key in seen:
                continue
            seen.add(key)
            normalized.append(name)
        return normalized

    @staticmethod
    def _normalize_entity_name(name: str) -> str:
        """エンティティ名を正規化."""
        lowered = name.strip().casefold()
        lowered = re.sub(r"[^0-9a-z\s]+", " ", lowered)
        return re.sub(r"\s+", " ", lowered).strip()

    def _build_default_aliases(self, competitors: list[str]) -> dict[str, list[str]]:
        """デフォルト別名辞書を構築."""
        alias_map: dict[str, list[str]] = {}
        for competitor in competitors:
            defaults = list(DEFAULT_COMPETITOR_ALIASES.get(competitor, []))
            alias_map[competitor] = self._dedupe_aliases([competitor, *defaults])
        return alias_map

    def _merge_aliases(self, competitor_aliases: dict[str, list[str]]) -> None:
        """別名辞書をマージ."""
        canonical_map = {name.casefold(): name for name in self._competitors}
        for raw_name, raw_aliases in competitor_aliases.items():
            canonical = canonical_map.get(raw_name.strip().casefold())
            if not canonical:
                continue
            merged = [canonical, *self._competitor_aliases.get(canonical, [])]
            if isinstance(raw_aliases, list):
                merged.extend(str(alias) for alias in raw_aliases)
            self._competitor_aliases[canonical] = self._dedupe_aliases(merged)

    @staticmethod
    def _dedupe_aliases(values: list[str]) -> list[str]:
        """別名リストを重複除去."""
        deduped: list[str] = []
        seen: set[str] = set()
        for item in values:
            alias = item.strip()
            if not alias:
                continue
            key = alias.casefold()
            if key in seen:
                continue
            seen.add(key)
            deduped.append(alias)
        return deduped

    @staticmethod
    def _build_alias_pattern(alias: str) -> re.Pattern[str]:
        """別名用の単語境界パターンを生成."""
        tokens = [token for token in alias.strip().split() if token]
        if not tokens:
            return re.compile(r"$^")
        body = r"\s+".join(re.escape(token) for token in tokens)
        return re.compile(rf"(?<![A-Za-z0-9]){body}(?![A-Za-z0-9])", re.IGNORECASE)

    def _rebuild_alias_index(self) -> None:
        """別名検索インデックスを再構築."""
        alias_lookup: dict[str, str] = {}
        alias_patterns: list[tuple[str, str, re.Pattern[str]]] = []

        for competitor in self._competitors:
            aliases = self._competitor_aliases.get(competitor, [competitor])
            for alias in aliases:
                normalized = self._normalize_entity_name(alias)
                if normalized:
                    alias_lookup[normalized] = competitor
                alias_patterns.append((competitor, alias, self._build_alias_pattern(alias)))

        # 長い別名を先に評価して、短い略語の誤爆を抑制
        alias_patterns.sort(key=lambda item: len(item[1]), reverse=True)
        self._alias_lookup = alias_lookup
        self._alias_patterns = alias_patterns

    def _extract_entity_candidates(self, article: Article) -> list[str]:
        """記事メタデータからエンティティ候補を抽出."""
        candidates: list[str] = []
        metadata = article.metadata
        entities = metadata.get("entities", [])
        if isinstance(entities, list):
            for item in entities:
                if isinstance(item, str):
                    candidates.append(item)
                elif isinstance(item, dict):
                    for key in ("name", "text", "value"):
                        value = item.get(key)
                        if isinstance(value, str) and value.strip():
                            candidates.append(value)
                            break

        orgs = metadata.get("organizations", [])
        if isinstance(orgs, list):
            for item in orgs:
                if isinstance(item, str):
                    candidates.append(item)
                elif isinstance(item, dict):
                    value = item.get("name")
                    if isinstance(value, str) and value.strip():
                        candidates.append(value)
        return candidates

    def _resolve_competitor(self, raw_name: str) -> str | None:
        """名称を競合正規名へ解決."""
        normalized = self._normalize_entity_name(raw_name)
        if not normalized:
            return None
        return self._alias_lookup.get(normalized)

    def _extract_competitor_mentions(self, article: Article) -> dict[str, set[str]]:
        """記事から競合言及を抽出し、正規名へ統一."""
        mentions: dict[str, set[str]] = defaultdict(set)
        text = f"{article.title}\n{article.content}"

        for competitor, alias, pattern in self._alias_patterns:
            if pattern.search(text):
                mentions[competitor].add(alias)

        for candidate in self._extract_entity_candidates(article):
            competitor = self._resolve_competitor(candidate)
            if competitor:
                mentions[competitor].add(candidate)

        return mentions

    @staticmethod
    def _parse_analysis(raw: str) -> dict[str, Any]:
        """LLMレスポンスをパース."""
        try:
            start = raw.find("{")
            end = raw.rfind("}")
            if start != -1 and end != -1 and end > start:
                return json.loads(raw[start : end + 1])
        except (json.JSONDecodeError, ValueError):
            pass
        return {}

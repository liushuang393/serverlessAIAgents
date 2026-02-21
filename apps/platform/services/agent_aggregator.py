"""Agent Aggregator Service — 全 App 横断の Agent 集約・検索."""

from __future__ import annotations

import logging
from collections import Counter
from typing import TYPE_CHECKING, Any

from apps.platform.services.agent_taxonomy import AgentTaxonomyService
from apps.platform.services.app_discovery import AppDiscoveryService
from apps.platform.services.capability_registry import CapabilityRegistry


if TYPE_CHECKING:
    from apps.platform.services.app_discovery import AppDiscoveryService


_logger = logging.getLogger(__name__)


class AggregatedAgent:
    """App 横断で集約された Agent 情報.

    Attributes:
        name: Agent 名
        app_name: 所属 App 名
        app_display_name: 所属 App 表示名
        app_icon: 所属 App アイコン
        module: Python モジュールパス
        capabilities: 能力タグ一覧
        business_base: Agent 業務基盤分類
        agent_pattern: Agent パターン分類
        app_business_base: App 業務基盤分類
        app_engine_pattern: App エンジンパターン
    """

    __slots__ = (
        "agent_pattern",
        "app_business_base",
        "app_display_name",
        "app_engine_pattern",
        "app_icon",
        "app_name",
        "business_base",
        "capabilities",
        "capabilities_legacy",
        "module",
        "name",
    )

    def __init__(
        self,
        name: str,
        app_name: str,
        app_display_name: str,
        app_icon: str,
        module: str | None,
        capabilities: list[dict[str, Any]],
        capabilities_legacy: list[str],
        business_base: str,
        agent_pattern: str,
        app_business_base: str,
        app_engine_pattern: str | None,
    ) -> None:
        """初期化."""
        self.name = name
        self.app_name = app_name
        self.app_display_name = app_display_name
        self.app_icon = app_icon
        self.module = module
        self.capabilities = capabilities
        self.capabilities_legacy = capabilities_legacy
        self.business_base = business_base
        self.agent_pattern = agent_pattern
        self.app_business_base = app_business_base
        self.app_engine_pattern = app_engine_pattern

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "app_name": self.app_name,
            "app_display_name": self.app_display_name,
            "app_icon": self.app_icon,
            "module": self.module,
            "capabilities": self.capabilities,
            "capabilities_legacy": self.capabilities_legacy,
            "business_base": self.business_base,
            "agent_pattern": self.agent_pattern,
            "app_business_base": self.app_business_base,
            "app_engine_pattern": self.app_engine_pattern,
        }


class AgentAggregatorService:
    """全 App 横断の Agent 集約サービス.

    Attributes:
        _discovery: App 検出サービス
    """

    def __init__(self, discovery: AppDiscoveryService) -> None:
        """初期化.

        Args:
            discovery: App 検出サービス
        """
        self._discovery = discovery
        self._capability_registry = CapabilityRegistry()
        self._taxonomy = AgentTaxonomyService(self._capability_registry)

    def list_all(self) -> list[AggregatedAgent]:
        """全 App の Agent を一覧取得.

        Returns:
            AggregatedAgent のリスト（App 名 → Agent 名順）
        """
        agents: list[AggregatedAgent] = []
        for app_config in self._discovery.list_apps():
            app_business_base = self._resolve_app_business_base(app_config)
            app_engine_pattern = self._taxonomy.normalize_engine_pattern(
                app_config.blueprint.engine_pattern,
            )
            for agent_info in app_config.agents:
                canonical = self._capability_registry.canonicalize_many(agent_info.capabilities)
                agent_business_base = self._taxonomy.infer_agent_business_base(
                    raw_business_base=agent_info.business_base,
                    capabilities=agent_info.capabilities,
                    fallback_app_base=app_business_base,
                )
                agent_pattern = self._taxonomy.infer_agent_pattern(
                    raw_pattern=agent_info.pattern,
                    name=agent_info.name,
                    module=agent_info.module,
                    engine_pattern=app_engine_pattern,
                )
                agents.append(
                    AggregatedAgent(
                        name=agent_info.name,
                        app_name=app_config.name,
                        app_display_name=app_config.display_name,
                        app_icon=app_config.icon,
                        module=agent_info.module,
                        capabilities=[item.model_dump() for item in canonical],
                        capabilities_legacy=list(agent_info.capabilities),
                        business_base=agent_business_base,
                        agent_pattern=agent_pattern,
                        app_business_base=app_business_base,
                        app_engine_pattern=app_engine_pattern,
                    )
                )
        return agents

    def search_by_capability(self, capability: str) -> list[AggregatedAgent]:
        """能力タグで Agent を検索.

        Args:
            capability: 検索する能力タグ

        Returns:
            マッチした AggregatedAgent のリスト
        """
        cap_lower = capability.lower()
        matches: list[AggregatedAgent] = []
        for agent in self.list_all():
            if any(cap_lower in item.lower() for item in agent.capabilities_legacy):
                matches.append(agent)
                continue

            canonical_values = [
                f"{cap.get('id', '')} {cap.get('label', '')} {' '.join(cap.get('aliases', []))}".lower()
                for cap in agent.capabilities
            ]
            if any(cap_lower in item for item in canonical_values):
                matches.append(agent)
        return matches

    def group_by_app(self) -> list[dict[str, Any]]:
        """App 別にグルーピング.

        Returns:
            [{app_name, display_name, icon, agents}]
        """
        grouped: dict[str, dict[str, Any]] = {}
        for agent in self.list_all():
            if agent.app_name not in grouped:
                grouped[agent.app_name] = {
                    "app_name": agent.app_name,
                    "display_name": agent.app_display_name,
                    "icon": agent.app_icon,
                    "agents": [],
                }
            grouped[agent.app_name]["agents"].append(agent.to_dict())

        return [grouped[key] for key in sorted(grouped.keys())]

    def all_capabilities(self) -> list[dict[str, Any]]:
        """全能力タグとその出現回数を取得.

        Returns:
            [{"id": str, "domain": str, "label": str, "count": int, "apps": list[str], "aliases": list[str]}]
        """
        return self._capability_registry.aggregate(
            [
                (
                    agent.app_name,
                    self._capability_registry.canonicalize_many(agent.capabilities_legacy),
                )
                for agent in self.list_all()
            ],
        )

    def stats(self) -> dict[str, Any]:
        """Agent 統計情報.

        Returns:
            統計辞書
        """
        agents = self.list_all()
        apps_with_agents = {a.app_name for a in agents}
        return {
            "total_agents": len(agents),
            "total_apps_with_agents": len(apps_with_agents),
            "total_capabilities": len({c["id"] for a in agents for c in a.capabilities}),
            "by_business_base": self._aggregate_counter([a.business_base for a in agents]),
            "by_agent_pattern": self._aggregate_counter([a.agent_pattern for a in agents]),
        }

    def grouped_business_bases(self) -> list[dict[str, Any]]:
        """business base 別に Agent を集約."""
        grouped: dict[str, list[dict[str, Any]]] = {}
        for agent in self.list_all():
            grouped.setdefault(agent.business_base, []).append(agent.to_dict())
        return [
            {"business_base": key, "count": len(grouped[key]), "agents": grouped[key]} for key in sorted(grouped.keys())
        ]

    def grouped_patterns(self) -> list[dict[str, Any]]:
        """Agent pattern 別に Agent を集約."""
        grouped: dict[str, list[dict[str, Any]]] = {}
        for agent in self.list_all():
            grouped.setdefault(agent.agent_pattern, []).append(agent.to_dict())
        return [{"pattern": key, "count": len(grouped[key]), "agents": grouped[key]} for key in sorted(grouped.keys())]

    def _resolve_app_business_base(self, app_config: Any) -> str:
        """App の business base を解決."""
        normalized = self._taxonomy.normalize_business_base(app_config.business_base)
        if normalized is not None:
            return normalized
        return self._taxonomy.infer_app_business_base(
            app_name=app_config.name,
            tags=app_config.tags,
            contracts_rag_enabled=app_config.contracts.rag.enabled,
            agent_capabilities=[agent.capabilities for agent in app_config.agents],
        )

    @staticmethod
    def _aggregate_counter(items: list[str]) -> list[dict[str, Any]]:
        """文字列リストを count 降順で集計."""
        counter = Counter(items)
        return [
            {"name": key, "count": counter[key]}
            for key in sorted(counter.keys(), key=lambda name: (-counter[name], name))
        ]

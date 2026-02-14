# -*- coding: utf-8 -*-
"""Agent Aggregator Service — 全 App 横断の Agent 集約・検索.

AppDiscoveryService が保持する app_config.json の agents 情報を
横断的に集約し、能力ベースの検索やグルーピングを提供する。

使用例:
    >>> aggregator = AgentAggregatorService(discovery)
    >>> all_agents = aggregator.list_all()
    >>> rag_agents = aggregator.search_by_capability("rag")
"""

from __future__ import annotations

import logging
from typing import Any

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
    """

    __slots__ = (
        "name", "app_name", "app_display_name", "app_icon",
        "module", "capabilities",
    )

    def __init__(
        self,
        name: str,
        app_name: str,
        app_display_name: str,
        app_icon: str,
        module: str | None,
        capabilities: list[str],
    ) -> None:
        """初期化."""
        self.name = name
        self.app_name = app_name
        self.app_display_name = app_display_name
        self.app_icon = app_icon
        self.module = module
        self.capabilities = capabilities

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "app_name": self.app_name,
            "app_display_name": self.app_display_name,
            "app_icon": self.app_icon,
            "module": self.module,
            "capabilities": self.capabilities,
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

    def list_all(self) -> list[AggregatedAgent]:
        """全 App の Agent を一覧取得.

        Returns:
            AggregatedAgent のリスト（App 名 → Agent 名順）
        """
        agents: list[AggregatedAgent] = []
        for app_config in self._discovery.list_apps():
            for agent_info in app_config.agents:
                agents.append(AggregatedAgent(
                    name=agent_info.name,
                    app_name=app_config.name,
                    app_display_name=app_config.display_name,
                    app_icon=app_config.icon,
                    module=agent_info.module,
                    capabilities=list(agent_info.capabilities),
                ))
        return agents

    def search_by_capability(self, capability: str) -> list[AggregatedAgent]:
        """能力タグで Agent を検索.

        Args:
            capability: 検索する能力タグ

        Returns:
            マッチした AggregatedAgent のリスト
        """
        cap_lower = capability.lower()
        return [
            a for a in self.list_all()
            if any(cap_lower in c.lower() for c in a.capabilities)
        ]

    def group_by_app(self) -> dict[str, list[dict[str, Any]]]:
        """App 別にグルーピング.

        Returns:
            App 名 → Agent 辞書リスト
        """
        groups: dict[str, list[dict[str, Any]]] = {}
        for agent in self.list_all():
            groups.setdefault(agent.app_name, []).append(agent.to_dict())
        return groups

    def all_capabilities(self) -> list[dict[str, Any]]:
        """全能力タグとその出現回数を取得.

        Returns:
            [{"tag": str, "count": int, "apps": list[str]}]
        """
        cap_map: dict[str, dict[str, Any]] = {}
        for agent in self.list_all():
            for cap in agent.capabilities:
                if cap not in cap_map:
                    cap_map[cap] = {"tag": cap, "count": 0, "apps": set()}
                cap_map[cap]["count"] += 1
                cap_map[cap]["apps"].add(agent.app_name)

        result = []
        for item in sorted(cap_map.values(), key=lambda x: x["count"], reverse=True):
            result.append({
                "tag": item["tag"],
                "count": item["count"],
                "apps": sorted(item["apps"]),
            })
        return result

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
            "total_capabilities": len({c for a in agents for c in a.capabilities}),
        }


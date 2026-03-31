"""Unified agent registry.

能力ベースの検索 API を後方互換で維持しつつ、runtime/bus 用の canonical
descriptor / instance / feedback 情報を一元管理する。
"""

from __future__ import annotations

import threading
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from kernel.agents.contracts import (
    AgentDescriptor,
    AgentFeedback,
    AgentFeedbackSummary,
)


if TYPE_CHECKING:
    from collections.abc import Callable

    from kernel.core.capability_spec import AgentCapabilitySpec, CapabilityRequirement
    from kernel.protocols.a2a_card import AgentCard


@dataclass(slots=True)
class AgentEntry:
    """Agent レジストリエントリ."""

    agent_id: str
    capability: AgentCapabilitySpec | None = None
    factory: Callable[[], Any] | None = None
    descriptor: AgentDescriptor | None = None
    instance: Any | None = None
    a2a_card: AgentCard | None = None
    feedback: AgentFeedbackSummary = field(default_factory=AgentFeedbackSummary)


class AgentRegistry:
    """統一 agent レジストリ.

    役割:
    - 旧 capability/factory 登録の維持
    - runtime descriptor / instance の正本管理
    - feedback 集計
    - protocol adapter 向けの metadata 供給
    """

    def __init__(self) -> None:
        """空のレジストリを初期化."""
        self._agents: dict[str, AgentEntry] = {}
        self._lock = threading.RLock()

    def register(
        self,
        agent_id: str,
        capability: AgentCapabilitySpec,
        factory: Callable[[], Any],
    ) -> None:
        """旧 capability/factory ベース登録."""
        with self._lock:
            entry = self._agents.get(agent_id)
            if entry is None:
                entry = AgentEntry(agent_id=agent_id)
                self._agents[agent_id] = entry
            entry.capability = capability
            entry.factory = factory

    def register_runtime(
        self,
        *,
        agent_id: str,
        descriptor: AgentDescriptor,
        instance: Any,
        replace: bool = False,
        factory: Callable[[], Any] | None = None,
        capability: AgentCapabilitySpec | None = None,
        a2a_card: AgentCard | None = None,
    ) -> AgentEntry:
        """runtime descriptor / instance を登録する."""
        with self._lock:
            existing = self._agents.get(agent_id)
            if existing is not None and existing.instance is not None and not replace:
                return existing

            entry = existing or AgentEntry(agent_id=agent_id)
            entry.descriptor = descriptor
            entry.instance = instance
            if factory is not None:
                entry.factory = factory
            if capability is not None:
                entry.capability = capability
            if a2a_card is not None:
                entry.a2a_card = a2a_card
            self._agents[agent_id] = entry
            return entry

    def unregister(self, agent_id: str) -> bool:
        """Agent をレジストリから削除."""
        with self._lock:
            if agent_id in self._agents:
                del self._agents[agent_id]
                return True
            return False

    def get_factory(self, agent_id: str) -> Callable[[], Any] | None:
        """Agent のファクトリ関数を取得."""
        entry = self._agents.get(agent_id)
        return entry.factory if entry else None

    def get_capability(self, agent_id: str) -> AgentCapabilitySpec | None:
        """Agent の能力宣言を取得."""
        entry = self._agents.get(agent_id)
        return entry.capability if entry else None

    def get_descriptor(self, agent_id: str) -> AgentDescriptor | None:
        """Agent descriptor を取得."""
        entry = self._agents.get(agent_id)
        return entry.descriptor if entry else None

    def get_instance(self, agent_id: str) -> Any | None:
        """Agent instance を取得."""
        entry = self._agents.get(agent_id)
        return entry.instance if entry else None

    def get_feedback_summary(self, agent_id: str) -> AgentFeedbackSummary | None:
        """feedback 集計を取得."""
        entry = self._agents.get(agent_id)
        return entry.feedback if entry else None

    def record_feedback(self, agent_id: str, feedback: AgentFeedback) -> None:
        """agent feedback を集計."""
        with self._lock:
            entry = self._agents.get(agent_id)
            if entry is None:
                entry = AgentEntry(agent_id=agent_id)
                self._agents[agent_id] = entry
            entry.feedback.record(feedback)

    def set_a2a_card(self, agent_id: str, card: AgentCard) -> None:
        """A2A card override を設定."""
        with self._lock:
            entry = self._agents.get(agent_id)
            if entry is None:
                entry = AgentEntry(agent_id=agent_id)
                self._agents[agent_id] = entry
            entry.a2a_card = card

    def get_a2a_card(self, agent_id: str) -> AgentCard | None:
        """descriptor から A2A card を取得."""
        entry = self._agents.get(agent_id)
        if entry is None:
            return None
        if entry.a2a_card is not None:
            return entry.a2a_card
        if entry.descriptor is None:
            return None
        from kernel.adapters.protocol_adapter import ProtocolAdapter

        return ProtocolAdapter.generate_a2a_card_from_descriptor(entry.descriptor)

    def list_a2a_cards(self) -> list[AgentCard]:
        """登録済み runtime の A2A card を列挙."""
        cards: list[AgentCard] = []
        for agent_id in self.list_runtime_agent_ids():
            card = self.get_a2a_card(agent_id)
            if card is not None:
                cards.append(card)
        return cards

    def find_by_tags(self, tags: list[str]) -> list[str]:
        """指定タグをすべて持つ Agent を検索."""
        results = []
        for agent_id, entry in self._agents.items():
            capability = entry.capability
            if capability is None:
                continue
            if all(tag in capability.tags for tag in tags):
                results.append(agent_id)
        return results

    def find_matching(
        self,
        requirement: CapabilityRequirement,
        limit: int = 5,
    ) -> list[tuple[str, float]]:
        """タスク要件にマッチする Agent を検索."""
        scored = []
        for agent_id, entry in self._agents.items():
            capability = entry.capability
            if capability is None:
                continue
            score = capability.matches(requirement)
            if score > 0:
                scored.append((agent_id, score))

        scored.sort(key=lambda x: x[1], reverse=True)
        return scored[:limit]

    def list_all(self) -> list[AgentEntry]:
        """登録された全 Agent エントリを取得."""
        return list(self._agents.values())

    def list_descriptors(self) -> list[AgentDescriptor]:
        """runtime descriptor 一覧を取得."""
        return [entry.descriptor for entry in self._agents.values() if entry.descriptor is not None]

    def list_runtime_agent_ids(self) -> list[str]:
        """runtime instance を持つ agent id 一覧."""
        return sorted(agent_id for agent_id, entry in self._agents.items() if entry.instance is not None)

    def get_all_capabilities(self) -> dict[str, AgentCapabilitySpec]:
        """全 Agent の能力を取得."""
        result: dict[str, AgentCapabilitySpec] = {}
        for agent_id, entry in self._agents.items():
            if entry.capability is not None:
                result[agent_id] = entry.capability
        return result

    def clear(self) -> None:
        """レジストリを完全にクリア."""
        with self._lock:
            self._agents.clear()

    def __contains__(self, agent_id: str) -> bool:
        """Agent が登録されているかチェック."""
        return agent_id in self._agents

    def __len__(self) -> int:
        """登録 Agent 数を返す."""
        return len(self._agents)


_global_agent_registry: AgentRegistry | None = None
_registry_lock = threading.Lock()


def get_global_agent_registry() -> AgentRegistry:
    """グローバル AgentRegistry を取得または作成."""
    global _global_agent_registry
    if _global_agent_registry is None:
        with _registry_lock:
            if _global_agent_registry is None:
                _global_agent_registry = AgentRegistry()
    return _global_agent_registry


def reset_global_agent_registry() -> None:
    """グローバル AgentRegistry をリセット."""
    global _global_agent_registry
    with _registry_lock:
        if _global_agent_registry is None:
            _global_agent_registry = AgentRegistry()
        else:
            _global_agent_registry.clear()


__all__ = [
    "AgentDescriptor",
    "AgentEntry",
    "AgentFeedback",
    "AgentFeedbackSummary",
    "AgentRegistry",
    "get_global_agent_registry",
    "reset_global_agent_registry",
]

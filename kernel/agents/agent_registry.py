"""Unified agent registry.

能力ベースの検索 API を後方互換で維持しつつ、runtime/bus 用の canonical
descriptor / instance / feedback 情報を一元管理する。
"""

from __future__ import annotations

import asyncio
import logging
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
    - テナントスコープ対応（tenant_id=None はグローバル/共有）
    """

    def __init__(self) -> None:
        """空のレジストリを初期化."""
        # テナント対応: (tenant_id, agent_id) -> AgentEntry
        self._scoped_agents: dict[tuple[str | None, str], AgentEntry] = {}
        # 後方互換: agent_id -> AgentEntry（グローバル参照用ビュー）
        self._agents: dict[str, AgentEntry] = {}
        self._lock = threading.RLock()
        # グレースフルシャットダウン用: agent_id -> アクティブリクエスト数
        self._active_count: dict[str, int] = {}
        self._drain_events: dict[str, asyncio.Event] = {}
        self._logger = logging.getLogger(__name__)

    def _scoped_key(self, tenant_id: str | None, agent_id: str) -> tuple[str | None, str]:
        """テナントスコープキーを生成.

        Args:
            tenant_id: テナントID（None=グローバル）
            agent_id: エージェントID

        Returns:
            複合キー
        """
        return (tenant_id, agent_id)

    def _get_entry(self, agent_id: str, tenant_id: str | None = None) -> AgentEntry | None:
        """テナントスコープを考慮してエントリを取得（テナント優先→グローバルフォールバック）.

        Args:
            agent_id: エージェントID
            tenant_id: テナントID

        Returns:
            エントリまたは None
        """
        if tenant_id is not None:
            # テナント固有を先にチェック
            entry = self._scoped_agents.get(self._scoped_key(tenant_id, agent_id))
            if entry is not None:
                return entry
        # グローバルにフォールバック
        return self._scoped_agents.get(self._scoped_key(None, agent_id))

    def register(
        self,
        agent_id: str,
        capability: AgentCapabilitySpec,
        factory: Callable[[], Any],
        tenant_id: str | None = None,
    ) -> None:
        """旧 capability/factory ベース登録.

        Args:
            agent_id: エージェントID
            capability: 能力宣言
            factory: ファクトリ関数
            tenant_id: テナントID（None=グローバル）
        """
        with self._lock:
            key = self._scoped_key(tenant_id, agent_id)
            entry = self._scoped_agents.get(key)
            if entry is None:
                entry = AgentEntry(agent_id=agent_id)
                self._scoped_agents[key] = entry
            entry.capability = capability
            entry.factory = factory
            # 後方互換: グローバル登録時は _agents にも反映
            if tenant_id is None:
                self._agents[agent_id] = entry

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
        tenant_id: str | None = None,
    ) -> AgentEntry:
        """runtime descriptor / instance を登録する."""
        with self._lock:
            key = self._scoped_key(tenant_id, agent_id)
            existing = self._scoped_agents.get(key)
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
            self._scoped_agents[key] = entry
            # 後方互換
            if tenant_id is None:
                self._agents[agent_id] = entry
            return entry

    def increment_active(self, agent_id: str) -> int:
        """アクティブリクエスト数をインクリメント.

        Args:
            agent_id: エージェントID

        Returns:
            インクリメント後のアクティブ数
        """
        with self._lock:
            count = self._active_count.get(agent_id, 0) + 1
            self._active_count[agent_id] = count
            return count

    def decrement_active(self, agent_id: str) -> int:
        """アクティブリクエスト数をデクリメント.

        Args:
            agent_id: エージェントID

        Returns:
            デクリメント後のアクティブ数
        """
        with self._lock:
            count = max(0, self._active_count.get(agent_id, 0) - 1)
            self._active_count[agent_id] = count
            # ドレイン待機中のイベントに通知
            if count == 0:
                event = self._drain_events.get(agent_id)
                if event is not None:
                    event.set()
            return count

    def get_active_count(self, agent_id: str) -> int:
        """アクティブリクエスト数を取得.

        Args:
            agent_id: エージェントID

        Returns:
            アクティブ数
        """
        return self._active_count.get(agent_id, 0)

    async def unregister(
        self,
        agent_id: str,
        tenant_id: str | None = None,
        drain_timeout_seconds: float = 5.0,
    ) -> bool:
        """Agent をレジストリから削除（グレースフルシャットダウン対応）.

        アクティブリクエストがある場合、drain_timeout_seconds まで完了を待つ。
        タイムアウト後は強制削除する。

        Args:
            agent_id: エージェントID
            tenant_id: テナントID（None=グローバル）
            drain_timeout_seconds: ドレイン待機秒数（デフォルト5秒）

        Returns:
            削除できたか
        """
        with self._lock:
            key = self._scoped_key(tenant_id, agent_id)
            if key not in self._scoped_agents:
                return False

        # アクティブリクエストがある場合はドレイン待機
        active = self.get_active_count(agent_id)
        if active > 0:
            self._logger.info(
                "Agent %s にアクティブリクエスト %d 件あり、ドレイン待機開始（最大 %.1f 秒）",
                agent_id,
                active,
                drain_timeout_seconds,
            )
            drain_event = asyncio.Event()
            with self._lock:
                self._drain_events[agent_id] = drain_event
                # ロック取得後に再チェック（デクリメントが既に発生した可能性）
                if self.get_active_count(agent_id) == 0:
                    drain_event.set()

            try:
                await asyncio.wait_for(
                    drain_event.wait(),
                    timeout=drain_timeout_seconds,
                )
                self._logger.info("Agent %s のドレイン完了", agent_id)
            except TimeoutError:
                self._logger.warning(
                    "Agent %s のドレインがタイムアウト（残り %d 件）、強制削除",
                    agent_id,
                    self.get_active_count(agent_id),
                )
            finally:
                with self._lock:
                    self._drain_events.pop(agent_id, None)

        # 削除実行
        with self._lock:
            key = self._scoped_key(tenant_id, agent_id)
            if key in self._scoped_agents:
                del self._scoped_agents[key]
                if tenant_id is None:
                    self._agents.pop(agent_id, None)
                self._active_count.pop(agent_id, None)
                return True
            return False

    def get_factory(
        self,
        agent_id: str,
        tenant_id: str | None = None,
    ) -> Callable[[], Any] | None:
        """Agent のファクトリ関数を取得."""
        entry = self._get_entry(agent_id, tenant_id)
        return entry.factory if entry else None

    def get_capability(
        self,
        agent_id: str,
        tenant_id: str | None = None,
    ) -> AgentCapabilitySpec | None:
        """Agent の能力宣言を取得."""
        entry = self._get_entry(agent_id, tenant_id)
        return entry.capability if entry else None

    def get_descriptor(
        self,
        agent_id: str,
        tenant_id: str | None = None,
    ) -> AgentDescriptor | None:
        """Agent descriptor を取得."""
        entry = self._get_entry(agent_id, tenant_id)
        return entry.descriptor if entry else None

    def get_instance(
        self,
        agent_id: str,
        tenant_id: str | None = None,
    ) -> Any | None:
        """Agent instance を取得（テナント優先→グローバルフォールバック）."""
        entry = self._get_entry(agent_id, tenant_id)
        return entry.instance if entry else None

    def get_feedback_summary(self, agent_id: str) -> AgentFeedbackSummary | None:
        """feedback 集計を取得."""
        entry = self._get_entry(agent_id)
        return entry.feedback if entry else None

    def record_feedback(self, agent_id: str, feedback: AgentFeedback) -> None:
        """agent feedback を集計."""
        with self._lock:
            key = self._scoped_key(None, agent_id)
            entry = self._scoped_agents.get(key)
            if entry is None:
                entry = AgentEntry(agent_id=agent_id)
                self._scoped_agents[key] = entry
                self._agents[agent_id] = entry
            entry.feedback.record(feedback)

    def set_a2a_card(self, agent_id: str, card: AgentCard) -> None:
        """A2A card override を設定."""
        with self._lock:
            key = self._scoped_key(None, agent_id)
            entry = self._scoped_agents.get(key)
            if entry is None:
                entry = AgentEntry(agent_id=agent_id)
                self._scoped_agents[key] = entry
                self._agents[agent_id] = entry
            entry.a2a_card = card

    def get_a2a_card(self, agent_id: str) -> AgentCard | None:
        """descriptor から A2A card を取得."""
        entry = self._get_entry(agent_id)
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

    def _iter_entries(
        self,
        tenant_id: str | None = None,
    ) -> list[tuple[str, AgentEntry]]:
        """テナントスコープを考慮してエントリを列挙（テナント固有 + グローバル、重複排除）.

        Args:
            tenant_id: テナントID（None=グローバルのみ）

        Returns:
            (agent_id, entry) のリスト
        """
        seen: dict[str, AgentEntry] = {}
        # テナント固有を先に追加
        if tenant_id is not None:
            for (tid, aid), entry in self._scoped_agents.items():
                if tid == tenant_id:
                    seen[aid] = entry
        # グローバルをフォールバック追加
        for (tid, aid), entry in self._scoped_agents.items():
            if tid is None and aid not in seen:
                seen[aid] = entry
        return list(seen.items())

    def find_by_tags(self, tags: list[str], tenant_id: str | None = None) -> list[str]:
        """指定タグをすべて持つ Agent を検索."""
        results: list[str] = []
        for agent_id, entry in self._iter_entries(tenant_id):
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
        tenant_id: str | None = None,
    ) -> list[tuple[str, float]]:
        """タスク要件にマッチする Agent を検索."""
        scored: list[tuple[str, float]] = []
        for agent_id, entry in self._iter_entries(tenant_id):
            capability = entry.capability
            if capability is None:
                continue
            score = capability.matches(requirement)
            if score > 0:
                scored.append((agent_id, score))

        scored.sort(key=lambda x: x[1], reverse=True)
        return scored[:limit]

    def list_all(self, tenant_id: str | None = None) -> list[AgentEntry]:
        """登録された全 Agent エントリを取得."""
        return [entry for _, entry in self._iter_entries(tenant_id)]

    def list_descriptors(self, tenant_id: str | None = None) -> list[AgentDescriptor]:
        """runtime descriptor 一覧を取得."""
        return [entry.descriptor for _, entry in self._iter_entries(tenant_id) if entry.descriptor is not None]

    def list_runtime_agent_ids(self, tenant_id: str | None = None) -> list[str]:
        """runtime instance を持つ agent id 一覧."""
        return sorted(agent_id for agent_id, entry in self._iter_entries(tenant_id) if entry.instance is not None)

    def get_all_capabilities(self, tenant_id: str | None = None) -> dict[str, AgentCapabilitySpec]:
        """全 Agent の能力を取得."""
        result: dict[str, AgentCapabilitySpec] = {}
        for agent_id, entry in self._iter_entries(tenant_id):
            if entry.capability is not None:
                result[agent_id] = entry.capability
        return result

    def clear(self) -> None:
        """レジストリを完全にクリア."""
        with self._lock:
            self._scoped_agents.clear()
            self._agents.clear()
            self._active_count.clear()
            self._drain_events.clear()

    def __contains__(self, agent_id: str) -> bool:
        """Agent が登録されているかチェック（グローバルスコープ）."""
        return self._get_entry(agent_id) is not None

    def __len__(self) -> int:
        """登録 Agent 数を返す（全スコープの一意 agent_id 数）."""
        return len({aid for (_, aid) in self._scoped_agents})


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

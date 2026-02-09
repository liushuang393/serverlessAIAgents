"""InMemory Agent Registry 実装.

開発・テスト環境向けのメモリ内 Agent レジストリ。
"""

import asyncio
import random
from datetime import UTC, datetime, timedelta

from agentflow.discovery.base import (
    AgentDiscoveryBase,
    AgentEntry,
    AgentStatus,
    LoadBalanceStrategy,
)


class InMemoryAgentRegistry(AgentDiscoveryBase):
    """メモリ内 Agent レジストリ.

    開発・テスト環境向け。本番環境では Redis/etcd ベースの実装を推奨。

    Attributes:
        _agents: Agent ID → AgentEntry のマッピング
        _capability_index: 機能 → Agent ID リストのインデックス
        _rr_counters: ラウンドロビン用カウンター
        _lock: 非同期ロック
    """

    def __init__(self, heartbeat_timeout: int = 30) -> None:
        """初期化.

        Args:
            heartbeat_timeout: ハートビートタイムアウト秒数
        """
        self._agents: dict[str, AgentEntry] = {}
        self._capability_index: dict[str, set[str]] = {}
        self._rr_counters: dict[str, int] = {}
        self._lock = asyncio.Lock()
        self._heartbeat_timeout = timedelta(seconds=heartbeat_timeout)

    async def register(self, entry: AgentEntry) -> None:
        """Agent を登録."""
        async with self._lock:
            # 既存エントリがあれば更新
            self._agents[entry.agent_id] = entry

            # 機能インデックスを更新
            for cap in entry.capabilities:
                if cap not in self._capability_index:
                    self._capability_index[cap] = set()
                self._capability_index[cap].add(entry.agent_id)

    async def unregister(self, agent_id: str) -> None:
        """Agent を登録解除."""
        async with self._lock:
            entry = self._agents.pop(agent_id, None)
            if entry:
                # 機能インデックスから削除
                for cap in entry.capabilities:
                    if cap in self._capability_index:
                        self._capability_index[cap].discard(agent_id)

    async def discover(
        self,
        capability: str | None = None,
        status: AgentStatus | None = None,
    ) -> list[AgentEntry]:
        """Agent を検索."""
        async with self._lock:
            # 機能でフィルタ
            if capability:
                agent_ids = self._capability_index.get(capability, set())
                candidates = [self._agents[aid] for aid in agent_ids if aid in self._agents]
            else:
                candidates = list(self._agents.values())

            # ステータスでフィルタ
            if status:
                candidates = [e for e in candidates if e.status == status]

            return candidates

    async def get(self, agent_id: str) -> AgentEntry | None:
        """Agent を取得."""
        async with self._lock:
            return self._agents.get(agent_id)

    async def heartbeat(self, agent_id: str) -> bool:
        """ハートビートを送信."""
        async with self._lock:
            entry = self._agents.get(agent_id)
            if not entry:
                return False

            # ハートビート時刻を更新
            entry.last_heartbeat = datetime.now(UTC)
            entry.status = AgentStatus.HEALTHY
            return True

    async def select(
        self,
        capability: str,
        strategy: LoadBalanceStrategy = LoadBalanceStrategy.ROUND_ROBIN,
    ) -> AgentEntry | None:
        """負荷分散戦略に従って Agent を選択."""
        candidates = await self.discover(capability=capability, status=AgentStatus.HEALTHY)

        if not candidates:
            return None

        if strategy == LoadBalanceStrategy.RANDOM:
            return random.choice(candidates)

        if strategy == LoadBalanceStrategy.WEIGHTED:
            # 重み付きランダム選択
            total_weight = sum(e.weight for e in candidates)
            r = random.randint(1, total_weight)
            cumulative = 0
            for entry in candidates:
                cumulative += entry.weight
                if r <= cumulative:
                    return entry
            return candidates[-1]

        # ROUND_ROBIN（デフォルト）
        async with self._lock:
            counter = self._rr_counters.get(capability, 0)
            selected = candidates[counter % len(candidates)]
            self._rr_counters[capability] = counter + 1
            return selected

    async def cleanup_stale(self) -> int:
        """タイムアウトした Agent を UNHEALTHY に変更.

        Returns:
            更新された Agent 数
        """
        async with self._lock:
            now = datetime.now(UTC)
            count = 0
            for entry in self._agents.values():
                if now - entry.last_heartbeat > self._heartbeat_timeout:
                    if entry.status == AgentStatus.HEALTHY:
                        entry.status = AgentStatus.UNHEALTHY
                        count += 1
            return count


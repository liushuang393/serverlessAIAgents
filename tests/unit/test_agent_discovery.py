"""Agent Discovery 単体テスト.

AgentDiscovery、AgentEntry、LoadBalanceStrategy のテスト。
"""

import pytest

from agentflow.discovery import (
    AgentDiscovery,
    AgentEntry,
    AgentStatus,
    LoadBalanceStrategy,
)


@pytest.fixture
def discovery() -> AgentDiscovery:
    """AgentDiscovery インスタンスを作成."""
    return AgentDiscovery(heartbeat_timeout=5)


@pytest.fixture
def sample_entry() -> AgentEntry:
    """サンプル AgentEntry を作成."""
    return AgentEntry(
        agent_id="agent-001",
        name="TestAgent",
        endpoint="http://localhost:8001",
        capabilities=["research", "summarize"],
        status=AgentStatus.HEALTHY,
    )


class TestAgentEntry:
    """AgentEntry モデルのテスト."""

    def test_create_entry(self) -> None:
        """エントリ作成のテスト."""
        entry = AgentEntry(
            agent_id="test-agent",
            name="TestAgent",
            endpoint="http://localhost:8000",
        )
        assert entry.agent_id == "test-agent"
        assert entry.name == "TestAgent"
        assert entry.status == AgentStatus.UNKNOWN
        assert entry.capabilities == []
        assert entry.weight == 1

    def test_entry_with_capabilities(self) -> None:
        """機能付きエントリのテスト."""
        entry = AgentEntry(
            agent_id="cap-agent",
            name="CapableAgent",
            endpoint="http://localhost:8001",
            capabilities=["analyze", "generate"],
        )
        assert "analyze" in entry.capabilities
        assert "generate" in entry.capabilities


class TestAgentDiscovery:
    """AgentDiscovery のテスト."""

    @pytest.mark.asyncio
    async def test_register_and_get(self, discovery: AgentDiscovery, sample_entry: AgentEntry) -> None:
        """登録と取得のテスト."""
        await discovery.register(sample_entry)
        result = await discovery.get("agent-001")
        assert result is not None
        assert result.name == "TestAgent"

    @pytest.mark.asyncio
    async def test_unregister(self, discovery: AgentDiscovery, sample_entry: AgentEntry) -> None:
        """登録解除のテスト."""
        await discovery.register(sample_entry)
        await discovery.unregister("agent-001")
        result = await discovery.get("agent-001")
        assert result is None

    @pytest.mark.asyncio
    async def test_discover_by_capability(self, discovery: AgentDiscovery) -> None:
        """機能による検索のテスト."""
        entry1 = AgentEntry(
            agent_id="agent-1",
            name="Agent1",
            endpoint="http://localhost:8001",
            capabilities=["research"],
            status=AgentStatus.HEALTHY,
        )
        entry2 = AgentEntry(
            agent_id="agent-2",
            name="Agent2",
            endpoint="http://localhost:8002",
            capabilities=["summarize"],
            status=AgentStatus.HEALTHY,
        )
        await discovery.register(entry1)
        await discovery.register(entry2)

        results = await discovery.discover(capability="research")
        assert len(results) == 1
        assert results[0].agent_id == "agent-1"

    @pytest.mark.asyncio
    async def test_discover_by_status(self, discovery: AgentDiscovery) -> None:
        """ステータスによる検索のテスト."""
        entry1 = AgentEntry(
            agent_id="healthy-agent",
            name="HealthyAgent",
            endpoint="http://localhost:8001",
            status=AgentStatus.HEALTHY,
        )
        entry2 = AgentEntry(
            agent_id="unhealthy-agent",
            name="UnhealthyAgent",
            endpoint="http://localhost:8002",
            status=AgentStatus.UNHEALTHY,
        )
        await discovery.register(entry1)
        await discovery.register(entry2)

        results = await discovery.discover(status=AgentStatus.HEALTHY)
        assert len(results) == 1
        assert results[0].agent_id == "healthy-agent"

    @pytest.mark.asyncio
    async def test_heartbeat(self, discovery: AgentDiscovery, sample_entry: AgentEntry) -> None:
        """ハートビートのテスト."""
        await discovery.register(sample_entry)
        result = await discovery.heartbeat("agent-001")
        assert result is True

        # 存在しない Agent へのハートビート
        result = await discovery.heartbeat("non-existent")
        assert result is False


class TestLoadBalancing:
    """負荷分散のテスト."""

    @pytest.mark.asyncio
    async def test_round_robin_selection(self, discovery: AgentDiscovery) -> None:
        """ラウンドロビン選択のテスト."""
        for i in range(3):
            entry = AgentEntry(
                agent_id=f"agent-{i}",
                name=f"Agent{i}",
                endpoint=f"http://localhost:800{i}",
                capabilities=["common"],
                status=AgentStatus.HEALTHY,
            )
            await discovery.register(entry)

        # ラウンドロビンで順番に選択される
        selected_ids = []
        for _ in range(6):
            result = await discovery.select("common", LoadBalanceStrategy.ROUND_ROBIN)
            assert result is not None
            selected_ids.append(result.agent_id)

        # 全ての Agent が少なくとも1回選択される
        assert "agent-0" in selected_ids
        assert "agent-1" in selected_ids
        assert "agent-2" in selected_ids

    @pytest.mark.asyncio
    async def test_random_selection(self, discovery: AgentDiscovery) -> None:
        """ランダム選択のテスト."""
        for i in range(3):
            entry = AgentEntry(
                agent_id=f"agent-{i}",
                name=f"Agent{i}",
                endpoint=f"http://localhost:800{i}",
                capabilities=["common"],
                status=AgentStatus.HEALTHY,
            )
            await discovery.register(entry)

        result = await discovery.select("common", LoadBalanceStrategy.RANDOM)
        assert result is not None
        assert result.agent_id.startswith("agent-")

    @pytest.mark.asyncio
    async def test_select_no_candidates(self, discovery: AgentDiscovery) -> None:
        """候補なしの選択テスト."""
        result = await discovery.select("non_existent")
        assert result is None

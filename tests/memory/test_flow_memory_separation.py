"""FlowContextのクリアが長期記憶に影響しないことを確認する分離テスト."""

import datetime

import pytest

from infrastructure.memory.types import MemoryEntry, MemoryType
from kernel.flow.context import FlowContext
from shared.memory.memory_manager import MemoryManager


@pytest.mark.asyncio
class TestFlowMemorySeparation:
    """Scratchpad（FlowContext）とLong-termの分離テスト."""

    async def test_flow_context_clear_does_not_affect_long_term(self) -> None:
        """FlowContext.clear()を呼んでも長期記憶は消えない."""
        # FlowContext（Scratchpad）
        ctx = FlowContext("test-separation")
        ctx.set("key1", "value1")
        assert ctx.get("key1") == "value1"

        # MemoryManager（Long-term）
        manager = MemoryManager(token_threshold=1000)
        await manager.start()

        entry = MemoryEntry(
            id="sep-test-001",
            content="長期記憶テストデータ",
            topic="sep_test",
            timestamp=datetime.datetime.now(),
            memory_type=MemoryType.LONG_TERM,
        )
        await manager._long_term.store(entry)

        # FlowContextをクリア（run()ごとに実行される）
        ctx.clear()

        # FlowContextはクリアされている
        assert ctx.get("key1") is None

        # 長期記憶は影響を受けない
        memories = await manager.recall(topic="sep_test")
        assert any(m.id == "sep-test-001" for m in memories), "FlowContext.clear()が長期記憶に影響してはいけない"

        await manager.stop()

    async def test_flow_has_long_term_memory_property(self) -> None:
        """Flowがlong_term_memoryプロパティとattach_long_term_memory()を持つことを確認."""
        from kernel.agents.agent_block import AgentBlock
        from kernel.flow.builder import FlowBuilder

        class DummyAgent(AgentBlock):
            async def run(self, input_data: dict) -> dict:  # type: ignore[override]
                return {"result": "ok"}

        flow = FlowBuilder("test-ltm-flow").then(DummyAgent).build()

        # デフォルトはNone
        assert flow.long_term_memory is None

        # MemoryManagerを接続
        manager = MemoryManager()
        flow.attach_long_term_memory(manager)
        assert flow.long_term_memory is manager

    async def test_memory_accessor_is_scratchpad(self) -> None:
        """MemoryAccessor（flow.memory）がScratchpadであることを確認."""
        from kernel.agents.agent_block import AgentBlock
        from kernel.flow.builder import FlowBuilder

        class DummyAgent(AgentBlock):
            async def run(self, input_data: dict) -> dict:  # type: ignore[override]
                return {"result": "ok"}

        flow = FlowBuilder("test-scratchpad").then(DummyAgent).build()

        # Scratchpadにデータを書き込む
        flow.memory.remember("temp_key", "temp_value")
        assert flow.memory.recall("temp_key") == "temp_value"

        # run()するとScratchpadはクリアされる（FlowContext.clear()が呼ばれる）
        await flow.run({"input": "test"})

        # Scratchpadはクリアされている
        assert flow.memory.recall("temp_key") is None

    async def test_long_term_memory_not_cleared_by_run(self) -> None:
        """flow.run()がScratchpadのみクリアし長期記憶は保持することを確認."""
        from kernel.agents.agent_block import AgentBlock
        from kernel.flow.builder import FlowBuilder

        class DummyAgent(AgentBlock):
            async def run(self, input_data: dict) -> dict:  # type: ignore[override]
                return {"result": "ok"}

        flow = FlowBuilder("test-ltm-persistence").then(DummyAgent).build()

        manager = MemoryManager(token_threshold=1000)
        await manager.start()
        flow.attach_long_term_memory(manager)

        # 長期記憶にデータを書き込む
        entry = MemoryEntry(
            id="persist-001",
            content="run後も残るべき長期記憶",
            topic="persist_test",
            timestamp=datetime.datetime.now(),
            memory_type=MemoryType.LONG_TERM,
        )
        await manager._long_term.store(entry)

        # flow.run()を実行（Scratchpadはクリアされる）
        await flow.run({"input": "test"})

        # 長期記憶はまだ存在する
        memories = await manager.recall(topic="persist_test")
        assert any(m.id == "persist-001" for m in memories), "flow.run()は長期記憶を削除してはいけない"

        await manager.stop()

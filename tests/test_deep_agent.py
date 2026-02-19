"""DeepAgent パターンのテスト.

モジュール構成:
- da_models: データモデル
- da_stores: ストレージ
- da_compressor: コンテキスト圧縮
- da_pool: Agent池
- da_progress: 進捗管理
- da_evolver: 自己進化
- da_coordinator: メイン協調器
"""

import pytest

from agentflow.patterns.deep_agent import (
    AgentMessage,
    AgentPool,
    AgentType,
    CognitiveAnalysis,
    CompactionStrategy,
    ContextCompressor,
    DeepAgentCoordinator,
    MemoryEvolutionStore,
    MemoryRuntimeStore,
    MessageType,
    ProgressManager,
    QualityReview,
    SelfEvolver,
    TaskStatus,
    TodoItem,
)


class TestTodoItem:
    """TodoItemのテスト."""

    def test_create_todo(self) -> None:
        """TodoItem作成テスト."""
        todo = TodoItem(task="テストタスク", agent_type=AgentType.RESEARCH.value)
        assert todo.task == "テストタスク"
        assert todo.agent_type == "research"
        assert todo.status == TaskStatus.PENDING
        assert todo.id.startswith("todo-")

    def test_is_ready_no_dependencies(self) -> None:
        """依存なしの場合は即実行可能."""
        todo = TodoItem(task="タスク")
        assert todo.is_ready(set()) is True

    def test_is_ready_with_dependencies(self) -> None:
        """依存ありの場合は依存完了後に実行可能."""
        todo = TodoItem(task="タスク", dependencies=["dep-1", "dep-2"])
        assert todo.is_ready(set()) is False
        assert todo.is_ready({"dep-1"}) is False
        assert todo.is_ready({"dep-1", "dep-2"}) is True

    def test_mark_completed(self) -> None:
        """完了マークテスト."""
        todo = TodoItem(task="タスク")
        todo.mark_completed()
        assert todo.status == TaskStatus.COMPLETED
        assert todo.completed_at is not None

    def test_mark_failed(self) -> None:
        """失敗マークテスト."""
        todo = TodoItem(task="タスク")
        todo.mark_failed("エラー発生")
        assert todo.status == TaskStatus.FAILED
        assert todo.error == "エラー発生"
        assert todo.retry_count == 1


class TestAgentPool:
    """AgentPoolのテスト."""

    def test_get_agent(self) -> None:
        """Agent取得テスト."""
        pool = AgentPool()
        agent = pool.get_agent(AgentType.RESEARCH)
        assert agent.name == "research-agent"
        assert agent.agent_type == AgentType.RESEARCH

    def test_get_agent_by_string(self) -> None:
        """文字列でAgent取得."""
        pool = AgentPool()
        agent = pool.get_agent("analysis")
        assert agent.agent_type == AgentType.ANALYSIS

    def test_create_new_agent(self) -> None:
        """新規Agent生成テスト."""
        pool = AgentPool()
        agent1 = pool.create_agent(AgentType.EXECUTION)
        agent2 = pool.create_agent(AgentType.EXECUTION)
        assert agent1 is not agent2

    @pytest.mark.asyncio
    async def test_agent_execute(self) -> None:
        """Agent実行テスト."""
        pool = AgentPool()
        agent = pool.get_agent(AgentType.RESEARCH)
        todo = TodoItem(task="調査タスク")
        result = await agent.execute(todo, {})
        assert result["status"] == "completed"
        assert result["agent"] == "research-agent"


class TestProgressManager:
    """ProgressManagerのテスト."""

    def test_register_todos(self) -> None:
        """Todo登録テスト."""
        pm = ProgressManager()
        todos = [TodoItem(task=f"タスク{i}") for i in range(3)]
        pm.register_todos(todos)
        assert len(pm.get_all_todos()) == 3

    def test_mark_started(self) -> None:
        """開始マークテスト."""
        pm = ProgressManager()
        todo = TodoItem(task="タスク")
        pm.register_todos([todo])
        pm.mark_started(todo.id)
        assert pm.get_todo(todo.id).status == TaskStatus.IN_PROGRESS

    def test_get_stats(self) -> None:
        """統計取得テスト."""
        pm = ProgressManager()
        todos = [TodoItem(task=f"タスク{i}") for i in range(5)]
        pm.register_todos(todos)
        pm.mark_completed(todos[0].id)
        pm.mark_completed(todos[1].id)
        pm.mark_failed(todos[2].id, "エラー")
        stats = pm.get_stats()
        assert stats["total"] == 5
        assert stats["completed"] == 2
        assert stats["failed"] == 1
        assert stats["pending"] == 2

    def test_get_ready_todos(self) -> None:
        """実行可能Todo取得テスト."""
        pm = ProgressManager()
        todo1 = TodoItem(task="タスク1")
        todo2 = TodoItem(task="タスク2", dependencies=[todo1.id])
        pm.register_todos([todo1, todo2])
        ready = pm.get_ready_todos()
        assert len(ready) == 1
        assert ready[0].id == todo1.id


class TestMemoryStores:
    """メモリストアのテスト."""

    @pytest.mark.asyncio
    async def test_runtime_store_context(self) -> None:
        """RuntimeStore コンテキスト保存テスト."""
        store = MemoryRuntimeStore()
        await store.save_context("key1", {"data": "value"})
        result = await store.load_context("key1")
        assert result == {"data": "value"}

    @pytest.mark.asyncio
    async def test_runtime_store_artifact(self) -> None:
        """RuntimeStore artifact保存テスト."""
        store = MemoryRuntimeStore()
        await store.write_artifact("/test/file.txt", "テスト内容")
        content = await store.read_artifact("/test/file.txt")
        assert content == b"\xe3\x83\x86\xe3\x82\xb9\xe3\x83\x88\xe5\x86\x85\xe5\xae\xb9"
        exists = await store.artifact_exists("/test/file.txt")
        assert exists is True

    @pytest.mark.asyncio
    async def test_evolution_store(self) -> None:
        """EvolutionStore パターン保存テスト."""
        store = MemoryEvolutionStore()
        await store.save_pattern("pattern-1", {"intent": "テスト"})
        result = await store.load_pattern("pattern-1")
        assert result["intent"] == "テスト"
        stats = await store.get_stats()
        assert stats["total_patterns"] == 1


class TestContextCompressor:
    """ContextCompressorのテスト."""

    @pytest.mark.asyncio
    async def test_compact_empty_messages(self) -> None:
        """空メッセージの圧縮テスト."""
        compressor = ContextCompressor()
        result, stats = await compressor.compact_messages([])
        assert result == []
        assert stats.original_tokens == 0

    @pytest.mark.asyncio
    async def test_compact_within_limit(self) -> None:
        """制限内メッセージは圧縮しない."""
        compressor = ContextCompressor()
        messages = [AgentMessage(from_agent="a", to_agent="b", content="短いメッセージ")]
        result, stats = await compressor.compact_messages(messages, max_tokens=1000)
        assert len(result) == 1
        assert abs(stats.compression_ratio - 1.0) < 0.01

    @pytest.mark.asyncio
    async def test_selective_compact(self) -> None:
        """選択的圧縮テスト."""
        compressor = ContextCompressor()
        messages = [
            AgentMessage(from_agent="a", msg_type=MessageType.RESULT, content="重要"),
            AgentMessage(from_agent="b", msg_type=MessageType.NOTIFY, content="通知"),
        ]
        result, _ = await compressor.compact_messages(
            messages, max_tokens=50, strategy=CompactionStrategy.SELECTIVE
        )
        # 重要度の高いRESULTが優先される
        assert any(m.msg_type == MessageType.RESULT for m in result)


class TestSelfEvolver:
    """SelfEvolverのテスト."""

    @pytest.mark.asyncio
    async def test_learn_success_pattern(self) -> None:
        """成功パターン学習テスト."""
        evolver = SelfEvolver()
        todos = [TodoItem(task="タスク1", agent_type="research")]
        analysis = CognitiveAnalysis(intent="テスト", domains=["test"], complexity="low")
        review = QualityReview(is_acceptable=True, score=85.0)
        pattern_key = await evolver.learn_success_pattern(todos, {}, analysis, review)
        assert pattern_key is not None
        assert pattern_key.startswith("pattern-")

    @pytest.mark.asyncio
    async def test_find_similar_pattern(self) -> None:
        """類似パターン検索テスト."""
        evolver = SelfEvolver()
        # まずパターンを学習
        todos = [TodoItem(task="タスク", agent_type="research")]
        analysis = CognitiveAnalysis(intent="市場調査", domains=["market"], complexity="medium")
        review = QualityReview(is_acceptable=True, score=90.0)
        await evolver.learn_success_pattern(todos, {}, analysis, review)
        # 類似パターンを検索
        similar_analysis = CognitiveAnalysis(
            intent="市場分析", domains=["market"], complexity="medium"
        )
        pattern = await evolver.find_similar_pattern(similar_analysis)
        assert pattern is not None


class TestDeepAgentCoordinator:
    """DeepAgentCoordinatorのテスト."""

    @pytest.mark.asyncio
    async def test_execute_simple_task(self) -> None:
        """シンプルタスク実行テスト."""
        coordinator = DeepAgentCoordinator()
        result = await coordinator.execute("テストタスク")
        assert result["status"] in ("completed", "partial")
        assert "report" in result
        assert "stats" in result

    @pytest.mark.asyncio
    async def test_execute_with_context(self) -> None:
        """コンテキスト付き実行テスト."""
        coordinator = DeepAgentCoordinator()
        result = await coordinator.execute("データ分析", context={"data_source": "test.csv"})
        assert result["status"] in ("completed", "partial")

    @pytest.mark.asyncio
    async def test_compress_context(self) -> None:
        """コンテキスト圧縮テスト."""
        coordinator = DeepAgentCoordinator()
        await coordinator.execute("タスク1")
        # 圧縮実行（エラーなく完了すればOK）
        await coordinator.compress_context(max_tokens=2000)

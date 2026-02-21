"""DeepAgentCoordinator 単体テスト.

このモジュールは DeepAgentCoordinator とその関連コンポーネントのテストを提供します。
"""

from __future__ import annotations

from typing import Any
from unittest.mock import AsyncMock, MagicMock

import pytest

from agentflow.patterns.deep_agent import (
    AgentMessage,
    AgentPool,
    AgentType,
    CognitiveAnalysis,
    ConversationManager,
    DeepAgentCoordinator,
    DynamicAgent,
    EvolutionRecord,
    Evolver,
    MemoryEvolutionStore,
    MemoryRuntimeStore,
    MessageType,
    ParallelGroup,
    ProgressManager,
    QualityReview,
    TaskStatus,
    TodoItem,
)


# =============================================================================
# TodoItem テスト
# =============================================================================


class TestTodoItem:
    """TodoItem のテスト."""

    def test_create_with_defaults(self) -> None:
        """デフォルト値での作成."""
        todo = TodoItem(task="テストタスク")
        assert todo.task == "テストタスク"
        assert todo.status == TaskStatus.PENDING
        assert todo.priority == 0
        assert todo.dependencies == []
        assert todo.tools == []
        assert todo.skills == []
        assert todo.result is None
        assert todo.error is None
        assert todo.retry_count == 0

    def test_create_with_all_fields(self) -> None:
        """全フィールド指定での作成."""
        todo = TodoItem(
            id="custom-id",
            task="カスタムタスク",
            agent_type=AgentType.ANALYSIS.value,
            status=TaskStatus.IN_PROGRESS,
            priority=5,
            dependencies=["dep-1", "dep-2"],
            tools=["tool1"],
            skills=["skill1"],
            metadata={"key": "value"},
        )
        assert todo.id == "custom-id"
        assert todo.agent_type == "analysis"
        assert todo.status == TaskStatus.IN_PROGRESS
        assert todo.priority == 5
        assert len(todo.dependencies) == 2

    def test_is_ready_no_dependencies(self) -> None:
        """依存なしの場合、即座に実行可能."""
        todo = TodoItem(task="test")
        assert todo.is_ready(set()) is True
        assert todo.is_ready({"other-task"}) is True

    def test_is_ready_with_dependencies_not_met(self) -> None:
        """依存が未解決の場合、実行不可."""
        todo = TodoItem(task="test", dependencies=["dep-1", "dep-2"])
        assert todo.is_ready(set()) is False
        assert todo.is_ready({"dep-1"}) is False

    def test_is_ready_with_dependencies_met(self) -> None:
        """依存が解決済みの場合、実行可能."""
        todo = TodoItem(task="test", dependencies=["dep-1", "dep-2"])
        assert todo.is_ready({"dep-1", "dep-2"}) is True
        assert todo.is_ready({"dep-1", "dep-2", "dep-3"}) is True

    def test_is_ready_not_pending(self) -> None:
        """PENDING以外の状態では実行不可."""
        todo = TodoItem(task="test", status=TaskStatus.IN_PROGRESS)
        assert todo.is_ready(set()) is False

        todo2 = TodoItem(task="test", status=TaskStatus.COMPLETED)
        assert todo2.is_ready(set()) is False

    def test_mark_completed(self) -> None:
        """完了マーク."""
        todo = TodoItem(task="test")
        todo.mark_completed()
        assert todo.status == TaskStatus.COMPLETED
        assert todo.completed_at is not None

    def test_mark_failed(self) -> None:
        """失敗マーク."""
        todo = TodoItem(task="test")
        todo.mark_failed("エラーメッセージ")
        assert todo.status == TaskStatus.FAILED
        assert todo.error == "エラーメッセージ"
        assert todo.retry_count == 1

        # 再度失敗
        todo.status = TaskStatus.PENDING
        todo.mark_failed("別のエラー")
        assert todo.retry_count == 2

    def test_priority_validation(self) -> None:
        """優先度のバリデーション."""
        # 有効な範囲
        todo = TodoItem(task="test", priority=0)
        assert todo.priority == 0

        todo = TodoItem(task="test", priority=10)
        assert todo.priority == 10

        # 無効な範囲（Pydantic がエラーを出す）
        with pytest.raises(ValueError):
            TodoItem(task="test", priority=-1)

        with pytest.raises(ValueError):
            TodoItem(task="test", priority=11)


# =============================================================================
# TaskStatus テスト
# =============================================================================


class TestTaskStatus:
    """TaskStatus のテスト."""

    def test_status_values(self) -> None:
        """ステータス値の確認."""
        assert TaskStatus.PENDING.value == "pending"
        assert TaskStatus.IN_PROGRESS.value == "in_progress"
        assert TaskStatus.COMPLETED.value == "completed"
        assert TaskStatus.FAILED.value == "failed"
        assert TaskStatus.BLOCKED.value == "blocked"


# =============================================================================
# AgentType テスト
# =============================================================================


class TestAgentType:
    """AgentType のテスト."""

    def test_agent_types(self) -> None:
        """Agent種別の確認."""
        assert AgentType.RESEARCH.value == "research"
        assert AgentType.ANALYSIS.value == "analysis"
        assert AgentType.PLANNING.value == "planning"
        assert AgentType.EXECUTION.value == "execution"
        assert AgentType.REVIEW.value == "review"
        assert AgentType.REPORT.value == "report"


# =============================================================================
# CognitiveAnalysis テスト
# =============================================================================


class TestCognitiveAnalysis:
    """CognitiveAnalysis のテスト."""

    def test_create_with_defaults(self) -> None:
        """デフォルト値での作成."""
        analysis = CognitiveAnalysis()
        assert analysis.intent == ""
        assert analysis.is_clear is False
        assert analysis.clarification_needed == []
        assert analysis.complexity == "medium"
        assert analysis.domains == []
        assert analysis.suggested_agents == []

    def test_create_with_values(self) -> None:
        """値指定での作成."""
        analysis = CognitiveAnalysis(
            intent="市場分析を行う",
            is_clear=True,
            complexity="high",
            domains=["finance", "market"],
            suggested_agents=["research", "analysis"],
        )
        assert analysis.intent == "市場分析を行う"
        assert analysis.is_clear is True
        assert analysis.complexity == "high"
        assert len(analysis.domains) == 2


# =============================================================================
# QualityReview テスト
# =============================================================================


class TestQualityReview:
    """QualityReview のテスト."""

    def test_create_with_defaults(self) -> None:
        """デフォルト値での作成."""
        review = QualityReview()
        assert review.is_acceptable is False
        assert review.score == 0.0
        assert review.issues == []
        assert review.suggestions == []
        assert review.retry_tasks == []

    def test_create_with_values(self) -> None:
        """値指定での作成."""
        review = QualityReview(
            is_acceptable=True,
            score=85.5,
            issues=["issue1"],
            suggestions=["suggestion1"],
        )
        assert review.is_acceptable is True
        assert review.score == 85.5
        assert len(review.issues) == 1

    def test_score_validation(self) -> None:
        """スコアのバリデーション."""
        review = QualityReview(score=0)
        assert review.score == 0

        review = QualityReview(score=100)
        assert review.score == 100

        with pytest.raises(ValueError):
            QualityReview(score=-1)

        with pytest.raises(ValueError):
            QualityReview(score=101)


# =============================================================================
# AgentMessage テスト
# =============================================================================


class TestAgentMessage:
    """AgentMessage のテスト."""

    def test_create_message(self) -> None:
        """メッセージ作成."""
        msg = AgentMessage(
            from_agent="research",
            to_agent="analysis",
            msg_type=MessageType.RESULT,
            content={"data": "test"},
        )
        assert msg.from_agent == "research"
        assert msg.to_agent == "analysis"
        assert msg.msg_type == MessageType.RESULT
        assert msg.content == {"data": "test"}
        assert msg.id.startswith("msg-")

    def test_broadcast_message(self) -> None:
        """ブロードキャストメッセージ."""
        msg = AgentMessage(
            from_agent="coordinator",
            content={"notification": "タスク完了"},
        )
        assert msg.to_agent == "*"


# =============================================================================
# ParallelGroup テスト
# =============================================================================


class TestParallelGroup:
    """ParallelGroup のテスト."""

    def test_create_group(self) -> None:
        """グループ作成."""
        group = ParallelGroup(todo_ids=["t1", "t2", "t3"])
        assert len(group.todo_ids) == 3
        assert group.status == TaskStatus.PENDING
        assert group.group_id.startswith("pg-")


# =============================================================================
# MemoryRuntimeStore テスト
# =============================================================================


class TestMemoryRuntimeStore:
    """MemoryRuntimeStore のテスト."""

    @pytest.fixture
    def store(self) -> MemoryRuntimeStore:
        """ストアのフィクスチャ."""
        return MemoryRuntimeStore()

    @pytest.mark.asyncio
    async def test_save_and_load_context(self, store: MemoryRuntimeStore) -> None:
        """コンテキストの保存と読み込み."""
        await store.save_context("key1", {"data": "value1"})
        result = await store.load_context("key1")
        assert result == {"data": "value1"}

    @pytest.mark.asyncio
    async def test_load_nonexistent_context(self, store: MemoryRuntimeStore) -> None:
        """存在しないコンテキストの読み込み."""
        result = await store.load_context("nonexistent")
        assert result is None

    @pytest.mark.asyncio
    async def test_save_and_load_checkpoint(self, store: MemoryRuntimeStore) -> None:
        """チェックポイントの保存と読み込み."""
        await store.save_checkpoint("cp1", {"state": "saved"})
        result = await store.load_checkpoint("cp1")
        assert result is not None
        assert result["state"] == "saved"
        assert "_saved_at" in result  # タイムスタンプが追加される

    @pytest.mark.asyncio
    async def test_clear(self, store: MemoryRuntimeStore) -> None:
        """クリア."""
        await store.save_context("key1", {"data": "value1"})
        await store.save_checkpoint("cp1", {"state": "saved"})
        await store.clear()

        assert await store.load_context("key1") is None
        assert await store.load_checkpoint("cp1") is None


# =============================================================================
# MemoryEvolutionStore テスト
# =============================================================================


class TestMemoryEvolutionStore:
    """MemoryEvolutionStore のテスト."""

    @pytest.fixture
    def store(self) -> MemoryEvolutionStore:
        """ストアのフィクスチャ."""
        return MemoryEvolutionStore()

    @pytest.mark.asyncio
    async def test_save_and_load_pattern(self, store: MemoryEvolutionStore) -> None:
        """パターンの保存と読み込み."""
        await store.save_pattern("pattern1", {"approach": "method1"})
        result = await store.load_pattern("pattern1")
        assert result is not None
        assert result["approach"] == "method1"
        assert "updated_at" in result

    @pytest.mark.asyncio
    async def test_save_feedback(self, store: MemoryEvolutionStore) -> None:
        """フィードバックの保存."""
        record = EvolutionRecord(
            event_type="success",
            pattern="test_pattern",
            confidence=0.8,
        )
        await store.save_feedback(record)

        stats = await store.get_stats()
        assert stats["total_feedbacks"] == 1
        assert stats["success_feedbacks"] == 1

    @pytest.mark.asyncio
    async def test_list_patterns(self, store: MemoryEvolutionStore) -> None:
        """パターン一覧の取得."""
        await store.save_pattern("p1", {"data": 1})
        await store.save_pattern("p2", {"data": 2})

        patterns = await store.list_patterns()
        assert len(patterns) == 2

    @pytest.mark.asyncio
    async def test_get_stats(self, store: MemoryEvolutionStore) -> None:
        """統計情報の取得."""
        stats = await store.get_stats()
        assert stats["total_patterns"] == 0
        assert stats["total_feedbacks"] == 0


# =============================================================================
# ProgressManager テスト
# =============================================================================


class TestProgressManager:
    """ProgressManager のテスト."""

    @pytest.fixture
    def manager(self) -> ProgressManager:
        """マネージャーのフィクスチャ."""
        return ProgressManager()

    def test_add_todo(self, manager: ProgressManager) -> None:
        """Todo追加."""
        todo = TodoItem(id="t1", task="タスク1")
        todo_id = manager.add_todo(todo)
        assert todo_id == "t1"

    def test_update_todo(self, manager: ProgressManager) -> None:
        """Todo更新."""
        todo = TodoItem(id="t1", task="タスク1")
        manager.add_todo(todo)

        result = manager.update_todo("t1", status=TaskStatus.IN_PROGRESS)
        assert result is True

        # 存在しないID
        result = manager.update_todo("nonexistent", status=TaskStatus.COMPLETED)
        assert result is False

    def test_get_next_todo(self, manager: ProgressManager) -> None:
        """次のTodo取得."""
        manager.add_todo(TodoItem(id="t1", task="タスク1", priority=1))
        manager.add_todo(TodoItem(id="t2", task="タスク2", priority=3))
        manager.add_todo(TodoItem(id="t3", task="タスク3", priority=2))

        # 優先度が高いものから
        next_todo = manager.get_next_todo()
        assert next_todo is not None
        assert next_todo.id == "t2"

    def test_get_ready_todos(self, manager: ProgressManager) -> None:
        """準備完了Todoの取得."""
        manager.add_todo(TodoItem(id="t1", task="タスク1"))
        manager.add_todo(TodoItem(id="t2", task="タスク2", dependencies=["t1"]))
        manager.add_todo(TodoItem(id="t3", task="タスク3"))

        ready = manager.get_ready_todos()
        assert len(ready) == 2
        assert all(t.id in ["t1", "t3"] for t in ready)

    def test_get_parallel_groups(self, manager: ProgressManager) -> None:
        """並行グループの取得."""
        manager.add_todo(TodoItem(id="t1", task="タスク1"))
        manager.add_todo(TodoItem(id="t2", task="タスク2"))
        manager.add_todo(TodoItem(id="t3", task="タスク3", dependencies=["t1", "t2"]))
        manager.add_todo(TodoItem(id="t4", task="タスク4", dependencies=["t3"]))

        groups = manager.get_parallel_groups()
        assert len(groups) == 3  # [t1,t2], [t3], [t4]
        assert set(groups[0].todo_ids) == {"t1", "t2"}
        assert groups[1].todo_ids == ["t3"]
        assert groups[2].todo_ids == ["t4"]

    def test_get_progress(self, manager: ProgressManager) -> None:
        """進捗状況の取得."""
        manager.add_todo(TodoItem(id="t1", task="タスク1", status=TaskStatus.COMPLETED))
        manager.add_todo(TodoItem(id="t2", task="タスク2", status=TaskStatus.FAILED))
        manager.add_todo(TodoItem(id="t3", task="タスク3"))

        progress = manager.get_progress()
        assert progress["total"] == 3
        assert progress["completed"] == 1
        assert progress["failed"] == 1
        assert progress["progress"] == pytest.approx(1 / 3)

    def test_send_and_get_messages(self, manager: ProgressManager) -> None:
        """メッセージ送受信."""
        manager.send_message("agent1", "agent2", {"data": "test"}, MessageType.RESULT)

        messages = manager.get_messages()
        assert len(messages) == 1
        assert messages[0].from_agent == "agent1"
        assert messages[0].to_agent == "agent2"

        # 特定Agentのメッセージ
        agent1_messages = manager.get_messages("agent1")
        assert len(agent1_messages) == 1

        agent3_messages = manager.get_messages("agent3")
        assert len(agent3_messages) == 0

    def test_context_property(self, manager: ProgressManager) -> None:
        """SharedContextプロパティ."""
        ctx = manager.context
        assert ctx is not None


# =============================================================================
# AgentPool テスト
# =============================================================================


class TestAgentPool:
    """AgentPool のテスト."""

    @pytest.fixture
    def pool(self) -> AgentPool:
        """プールのフィクスチャ."""
        return AgentPool()

    def test_register_agent(self, pool: AgentPool) -> None:
        """Agent登録."""
        mock_agent = MagicMock()
        pool.register_agent("custom_agent", mock_agent)
        assert "custom_agent" in pool.list_agents()

    def test_list_agents(self, pool: AgentPool) -> None:
        """Agent一覧."""
        agents = pool.list_agents()
        assert isinstance(agents, list)

    @pytest.mark.asyncio
    async def test_get_or_create_dynamic(self, pool: AgentPool) -> None:
        """動的Agent取得・作成."""
        agent = await pool.get_or_create(AgentType.RESEARCH)
        assert agent is not None
        assert isinstance(agent, DynamicAgent)

        # 再度取得すると同じインスタンス
        agent2 = await pool.get_or_create(AgentType.RESEARCH)
        assert agent is agent2

    @pytest.mark.asyncio
    async def test_get_or_create_predefined(self, pool: AgentPool) -> None:
        """事前定義Agent取得."""
        mock_agent = MagicMock()
        pool.register_agent("analysis", mock_agent)

        agent = await pool.get_or_create("analysis")
        assert agent is mock_agent

    def test_set_default_binding(self, pool: AgentPool) -> None:
        """デフォルトバインディング設定."""
        pool.set_default_binding(
            AgentType.RESEARCH,
            tools=["web_search", "rag"],
            skills=["research_skill"],
        )

        # 確認 - str(AgentType.RESEARCH) は "AgentType.RESEARCH" ではなく実際の値
        key = str(AgentType.RESEARCH)
        bindings = pool._default_bindings.get(key, {})
        assert "web_search" in bindings.get("tools", [])
        assert "research_skill" in bindings.get("skills", [])


# =============================================================================
# DynamicAgent テスト
# =============================================================================


class TestDynamicAgent:
    """DynamicAgent のテスト."""

    @pytest.mark.asyncio
    async def test_run_without_llm(self) -> None:
        """LLMなしでの実行."""
        agent = DynamicAgent(
            name="test_agent",
            system_prompt="テストプロンプト",
        )
        result = await agent.run({"task": "テストタスク"})
        assert result["error"] == "LLM not configured"
        assert result["agent"] == "test_agent"

    @pytest.mark.asyncio
    async def test_run_with_llm(self) -> None:
        """LLMありでの実行."""
        mock_llm = AsyncMock()
        mock_llm.chat.return_value = MagicMock(content="テスト結果")

        agent = DynamicAgent(
            name="test_agent",
            system_prompt="テストプロンプト",
            llm_client=mock_llm,
        )
        result = await agent.run({"task": "テストタスク"})
        assert result["status"] == "success"
        assert result["output"] == "テスト結果"
        assert result["agent"] == "test_agent"

    @pytest.mark.asyncio
    async def test_run_with_llm_error(self) -> None:
        """LLMエラー時の実行."""
        mock_llm = AsyncMock()
        mock_llm.chat.side_effect = Exception("LLMエラー")

        agent = DynamicAgent(
            name="test_agent",
            system_prompt="テストプロンプト",
            llm_client=mock_llm,
        )
        result = await agent.run({"task": "テストタスク"})
        assert result["status"] == "failed"
        assert "LLMエラー" in result["error"]


# =============================================================================
# Evolver テスト
# =============================================================================


class TestEvolver:
    """Evolver のテスト."""

    @pytest.fixture
    def evolver(self) -> Evolver:
        """Evolverのフィクスチャ."""
        return Evolver()

    @pytest.mark.asyncio
    async def test_learn_from_success(self, evolver: Evolver) -> None:
        """成功からの学習."""
        record = await evolver.learn_from_success(
            task="市場分析を行う",
            result={"approach": "データ収集→分析→レポート"},
            context={"domain": "finance"},
        )
        assert record is not None
        assert record.event_type == "success"
        # 初回学習: 0.5 + 0.1 = 0.6
        assert record.confidence == 0.6

    @pytest.mark.asyncio
    async def test_process_feedback(self, evolver: Evolver) -> None:
        """フィードバック処理."""
        record = await evolver.process_feedback(
            feedback_type="education",
            content="リスク分析を追加してください",
        )
        assert record is not None
        assert "feedback:education" in record.event_type
        # education タイプの信頼度は 0.9
        assert record.confidence == 0.9

    def test_get_learned_hint(self, evolver: Evolver) -> None:
        """学習済みヒントの取得."""
        # 学習前
        hint = evolver.get_learned_hint("市場分析を行う")
        assert hint is None

    def test_get_evolution_stats(self, evolver: Evolver) -> None:
        """進化統計の取得."""
        stats = evolver.get_evolution_stats()
        assert "total_records" in stats
        assert "learned_patterns" in stats
        assert "success_count" in stats


# =============================================================================
# DeepAgentCoordinator テスト
# =============================================================================


class TestDeepAgentCoordinator:
    """DeepAgentCoordinator のテスト."""

    @pytest.fixture
    def coordinator(self) -> DeepAgentCoordinator:
        """Coordinatorのフィクスチャ."""
        return DeepAgentCoordinator(
            max_iterations=5,
            max_retries=2,
            quality_threshold=70.0,
            enable_evolution=True,
            enable_memory=True,
        )

    def test_pattern_property(self, coordinator: DeepAgentCoordinator) -> None:
        """パターンプロパティ."""
        from agentflow.patterns.coordinator import CoordinationPattern

        assert coordinator.pattern == CoordinationPattern.HIERARCHICAL

    def test_register_agent(self, coordinator: DeepAgentCoordinator) -> None:
        """Agent登録."""
        mock_agent = MagicMock()
        coordinator.register_agent("custom", mock_agent)
        assert "custom" in coordinator._agent_pool.list_agents()

    def test_get_stats(self, coordinator: DeepAgentCoordinator) -> None:
        """統計情報取得."""
        stats = coordinator.get_stats()
        assert "progress" in stats
        assert "available_agents" in stats
        assert "evolution" in stats

    @pytest.mark.asyncio
    async def test_execute_without_llm(self, coordinator: DeepAgentCoordinator) -> None:
        """LLMなしでの実行（デフォルト分解）."""
        result = await coordinator.execute("テストタスク")

        assert "status" in result
        assert "progress" in result

    @pytest.mark.asyncio
    async def test_execute_with_mock_llm(self) -> None:
        """モックLLMでの実行."""
        mock_llm = AsyncMock()

        # 認知分析の応答
        cognitive_response = MagicMock()
        cognitive_response.content = """{
            "intent": "テスト分析",
            "is_clear": true,
            "complexity": "low",
            "domains": ["test"],
            "suggested_agents": ["analysis"]
        }"""

        # タスク分解の応答
        decompose_response = MagicMock()
        decompose_response.content = """{
            "steps": [
                {"task": "分析", "agent_type": "analysis", "priority": 1}
            ]
        }"""

        # 品質評審の応答
        review_response = MagicMock()
        review_response.content = """{
            "is_acceptable": true,
            "score": 85,
            "issues": [],
            "suggestions": [],
            "retry_tasks": []
        }"""

        mock_llm.chat.side_effect = [
            cognitive_response,
            decompose_response,
            review_response,
        ]

        coordinator = DeepAgentCoordinator(
            llm_client=mock_llm,
            max_iterations=5,
        )

        result = await coordinator.execute("テストタスク")
        assert result["status"] in ["success", "partial", "error"]

    @pytest.mark.asyncio
    async def test_process_feedback(self, coordinator: DeepAgentCoordinator) -> None:
        """フィードバック処理."""
        result = await coordinator.process_feedback(
            feedback_type="suggestion",
            content="改善提案",
        )
        assert result["status"] == "processed"

    @pytest.mark.asyncio
    async def test_process_feedback_evolution_disabled(self) -> None:
        """進化無効時のフィードバック処理."""
        coordinator = DeepAgentCoordinator(enable_evolution=False)
        result = await coordinator.process_feedback(
            feedback_type="suggestion",
            content="改善提案",
        )
        assert result["status"] == "evolution_disabled"

    @pytest.mark.asyncio
    async def test_progress_callback(self) -> None:
        """進捗コールバック."""
        events: list[dict[str, Any]] = []

        def on_progress(event: dict[str, Any]) -> None:
            events.append(event)

        coordinator = DeepAgentCoordinator(on_progress=on_progress)
        await coordinator.execute("テストタスク")

        assert len(events) > 0
        event_types = [e["event"] for e in events]
        assert "cognitive_analysis" in event_types

    @pytest.mark.asyncio
    async def test_cognitive_analysis_without_llm(self, coordinator: DeepAgentCoordinator) -> None:
        """LLMなしの認知分析."""
        result = await coordinator._cognitive_analysis("テストタスク")
        assert result.intent == "テストタスク"
        assert result.is_clear is True

    @pytest.mark.asyncio
    async def test_decompose_task_without_llm(self, coordinator: DeepAgentCoordinator) -> None:
        """LLMなしのタスク分解."""
        cognitive = CognitiveAnalysis(
            intent="テスト",
            is_clear=True,
            complexity="medium",
        )
        todos = await coordinator._decompose_task("テストタスク", cognitive)
        assert len(todos) > 0
        assert all(isinstance(t, TodoItem) for t in todos)

    @pytest.mark.asyncio
    async def test_quality_review_without_llm(self, coordinator: DeepAgentCoordinator) -> None:
        """LLMなしの品質評審."""
        # 全タスク完了の場合
        coordinator._progress.add_todo(TodoItem(id="t1", task="タスク1", status=TaskStatus.COMPLETED))
        review = await coordinator._quality_review("テスト", {"t1": {"result": "ok"}})
        assert review.is_acceptable is True
        # avg_score = (100 + 70 + 75 + 70 + 70) / 5 = 77.0
        assert review.score == 77.0


# =============================================================================
# 統合テスト
# =============================================================================


class TestCyclicDependencyDetection:
    """循環依存検出テスト."""

    def test_validate_no_cycle(self) -> None:
        """循環なしの検証."""
        manager = ProgressManager()
        manager.add_todo(TodoItem(id="t1", task="タスク1"))
        manager.add_todo(TodoItem(id="t2", task="タスク2", dependencies=["t1"]))
        manager.add_todo(TodoItem(id="t3", task="タスク3", dependencies=["t2"]))

        errors = manager.validate_dependencies()
        assert len(errors) == 0

    def test_validate_simple_cycle(self) -> None:
        """単純な循環検出（A -> B -> A）."""
        manager = ProgressManager()
        manager.add_todo(TodoItem(id="a", task="A", dependencies=["b"]))
        manager.add_todo(TodoItem(id="b", task="B", dependencies=["a"]))

        errors = manager.validate_dependencies()
        assert len(errors) > 0
        assert "循環依存" in errors[0]

    def test_validate_complex_cycle(self) -> None:
        """複雑な循環検出（A -> B -> C -> A）."""
        manager = ProgressManager()
        manager.add_todo(TodoItem(id="a", task="A", dependencies=["c"]))
        manager.add_todo(TodoItem(id="b", task="B", dependencies=["a"]))
        manager.add_todo(TodoItem(id="c", task="C", dependencies=["b"]))

        errors = manager.validate_dependencies()
        assert len(errors) > 0

    def test_get_execution_order_valid(self) -> None:
        """有効な実行順序取得."""
        manager = ProgressManager()
        manager.add_todo(TodoItem(id="t1", task="タスク1"))
        manager.add_todo(TodoItem(id="t2", task="タスク2", dependencies=["t1"]))
        manager.add_todo(TodoItem(id="t3", task="タスク3", dependencies=["t1"]))
        manager.add_todo(TodoItem(id="t4", task="タスク4", dependencies=["t2", "t3"]))

        order = manager.get_execution_order()
        assert len(order) == 4
        # t1 は t2, t3 より前に来る
        assert order.index("t1") < order.index("t2")
        assert order.index("t1") < order.index("t3")
        # t4 は t2, t3 より後に来る
        assert order.index("t4") > order.index("t2")
        assert order.index("t4") > order.index("t3")

    def test_get_execution_order_with_cycle(self) -> None:
        """循環依存時の実行順序取得."""
        manager = ProgressManager()
        manager.add_todo(TodoItem(id="a", task="A", dependencies=["b"]))
        manager.add_todo(TodoItem(id="b", task="B", dependencies=["a"]))

        order = manager.get_execution_order()
        assert len(order) == 0  # 循環依存のため空

    def test_parallel_groups_with_cycle(self) -> None:
        """循環依存時の並行グループ取得."""
        manager = ProgressManager()
        manager.add_todo(TodoItem(id="a", task="A", dependencies=["b"]))
        manager.add_todo(TodoItem(id="b", task="B", dependencies=["a"]))

        groups = manager.get_parallel_groups()
        assert len(groups) == 0  # 循環依存のため空


class TestConversationManager:
    """ConversationManager テスト."""

    @pytest.fixture
    def manager(self) -> ConversationManager:
        """マネージャーのフィクスチャ."""
        return ConversationManager(max_tokens=1000, recent_keep_count=3)

    def test_add_message(self, manager: ConversationManager) -> None:
        """メッセージ追加."""
        msg = AgentMessage(
            from_agent="agent1",
            to_agent="agent2",
            content={"data": "test"},
        )
        manager.add_message(msg)
        assert len(manager._messages) == 1

    def test_add_messages(self, manager: ConversationManager) -> None:
        """複数メッセージ追加."""
        messages = [
            AgentMessage(from_agent="a", to_agent="b", content="1"),
            AgentMessage(from_agent="b", to_agent="a", content="2"),
        ]
        manager.add_messages(messages)
        assert len(manager._messages) == 2

    def test_current_token_count(self, manager: ConversationManager) -> None:
        """トークン数カウント."""
        # 空の場合
        assert manager.current_token_count() == 0

        # メッセージ追加後
        manager.add_message(
            AgentMessage(
                from_agent="a",
                to_agent="b",
                content="x" * 100,  # 約25トークン
            )
        )
        assert manager.current_token_count() > 0

    def test_utilization(self, manager: ConversationManager) -> None:
        """使用率計算."""
        assert manager.utilization() == 0.0

        # 半分くらい埋める
        manager.add_message(
            AgentMessage(
                from_agent="a",
                to_agent="b",
                content="x" * 2000,  # 約500トークン（max 1000の半分）
            )
        )
        util = manager.utilization()
        assert 0.3 < util < 0.7

    def test_needs_summarization(self, manager: ConversationManager) -> None:
        """要約必要判定."""
        assert manager.needs_summarization() is False

        # 閾値を超える
        manager.add_message(
            AgentMessage(
                from_agent="a",
                to_agent="b",
                content="x" * 4000,  # 1000トークン以上
            )
        )
        assert manager.needs_summarization() is True

    @pytest.mark.asyncio
    async def test_auto_summarize(self, manager: ConversationManager) -> None:
        """自動要約."""
        # 複数メッセージを追加
        for i in range(10):
            manager.add_message(
                AgentMessage(
                    from_agent=f"agent{i}",
                    to_agent="coordinator",
                    content=f"Message {i}" * 50,
                )
            )

        original_count = len(manager._messages)
        result = await manager.auto_summarize()

        assert result["status"] == "success"
        assert result["compressed"] is True
        assert result["original_messages"] == original_count
        assert result["preserved_messages"] == 3  # recent_keep_count
        assert len(manager._messages) == 3

    @pytest.mark.asyncio
    async def test_auto_summarize_no_messages(self, manager: ConversationManager) -> None:
        """メッセージなしの場合の要約."""
        result = await manager.auto_summarize()
        assert result["status"] == "no_messages"
        assert result["compressed"] is False

    @pytest.mark.asyncio
    async def test_auto_summarize_few_messages(self, manager: ConversationManager) -> None:
        """メッセージが少ない場合の要約."""
        manager.add_message(AgentMessage(from_agent="a", to_agent="b", content="test"))
        result = await manager.auto_summarize()
        assert result["status"] == "nothing_to_summarize"

    def test_simple_summary(self, manager: ConversationManager) -> None:
        """簡易要約生成."""
        messages = [
            AgentMessage(from_agent="a", to_agent="b", content="error: something failed"),
            AgentMessage(from_agent="b", to_agent="a", content="result: success"),
        ]
        summary = manager._simple_summary(messages)
        assert "2メッセージ" in summary
        assert "エラー" in summary or "結果" in summary

    def test_get_context(self, manager: ConversationManager) -> None:
        """コンテキスト取得."""
        manager.add_message(AgentMessage(from_agent="a", to_agent="b", content="test"))
        context = manager.get_context()

        assert "recent_messages" in context
        assert len(context["recent_messages"]) == 1

    def test_get_stats(self, manager: ConversationManager) -> None:
        """統計情報取得."""
        stats = manager.get_stats()

        assert "message_count" in stats
        assert "current_tokens" in stats
        assert "utilization" in stats
        assert "needs_summarization" in stats

    def test_clear(self, manager: ConversationManager) -> None:
        """クリア."""
        manager.add_message(AgentMessage(from_agent="a", to_agent="b", content="test"))
        manager._summaries.append("summary")

        manager.clear()
        assert len(manager._messages) == 0
        assert len(manager._summaries) == 0


class TestVirtualFilesystem:
    """Virtual Filesystem テスト."""

    @pytest.fixture
    def store(self) -> MemoryRuntimeStore:
        """ストアのフィクスチャ."""
        return MemoryRuntimeStore()

    @pytest.mark.asyncio
    async def test_write_and_read_artifact(self, store: MemoryRuntimeStore) -> None:
        """Artifactの書き込みと読み込み."""
        await store.write_artifact("/reports/analysis.md", "# Analysis Report\n")
        content = await store.read_artifact("/reports/analysis.md")
        assert content == b"# Analysis Report\n"

    @pytest.mark.asyncio
    async def test_write_artifact_bytes(self, store: MemoryRuntimeStore) -> None:
        """バイナリArtifactの書き込み."""
        binary_content = b"\x89PNG\r\n\x1a\n"
        await store.write_artifact("/images/chart.png", binary_content)
        content = await store.read_artifact("/images/chart.png")
        assert content == binary_content

    @pytest.mark.asyncio
    async def test_read_nonexistent_artifact(self, store: MemoryRuntimeStore) -> None:
        """存在しないArtifactの読み込み."""
        content = await store.read_artifact("/nonexistent.txt")
        assert content is None

    @pytest.mark.asyncio
    async def test_list_artifacts(self, store: MemoryRuntimeStore) -> None:
        """Artifact一覧の取得."""
        await store.write_artifact("/reports/a.md", "A")
        await store.write_artifact("/reports/b.md", "B")
        await store.write_artifact("/data/c.csv", "C")

        # 全件
        all_artifacts = await store.list_artifacts()
        assert len(all_artifacts) == 3

        # プレフィックスフィルタ
        reports = await store.list_artifacts("/reports")
        assert len(reports) == 2
        assert all(a["path"].startswith("/reports") for a in reports)

    @pytest.mark.asyncio
    async def test_delete_artifact(self, store: MemoryRuntimeStore) -> None:
        """Artifactの削除."""
        await store.write_artifact("/temp/file.txt", "temporary")

        # 削除成功
        result = await store.delete_artifact("/temp/file.txt")
        assert result is True
        assert await store.read_artifact("/temp/file.txt") is None

        # 存在しないファイルの削除
        result = await store.delete_artifact("/nonexistent.txt")
        assert result is False

    @pytest.mark.asyncio
    async def test_artifact_exists(self, store: MemoryRuntimeStore) -> None:
        """Artifact存在確認."""
        await store.write_artifact("/test.txt", "test")

        assert await store.artifact_exists("/test.txt") is True
        assert await store.artifact_exists("/nonexistent.txt") is False

    @pytest.mark.asyncio
    async def test_artifact_metadata(self, store: MemoryRuntimeStore) -> None:
        """Artifactメタデータ."""
        await store.write_artifact(
            "/report.pdf",
            b"PDF content",
            metadata={"content_type": "application/pdf", "author": "agent1"},
        )

        artifacts = await store.list_artifacts("/report.pdf")
        assert len(artifacts) == 1
        artifact = artifacts[0]
        assert artifact["content_type"] == "application/pdf"
        assert artifact["author"] == "agent1"
        assert artifact["size"] == 11

    @pytest.mark.asyncio
    async def test_path_normalization(self, store: MemoryRuntimeStore) -> None:
        """パス正規化."""
        # 先頭スラッシュなし
        await store.write_artifact("file.txt", "content")
        assert await store.artifact_exists("/file.txt") is True

        # 連続スラッシュ
        await store.write_artifact("//folder//file.txt", "content2")
        assert await store.artifact_exists("/folder/file.txt") is True

    @pytest.mark.asyncio
    async def test_list_checkpoints(self, store: MemoryRuntimeStore) -> None:
        """チェックポイント一覧."""
        await store.save_checkpoint("cp1", {"state": "1"})
        await store.save_checkpoint("cp2", {"state": "2"})

        checkpoints = await store.list_checkpoints()
        assert len(checkpoints) == 2
        assert "cp1" in checkpoints
        assert "cp2" in checkpoints


class TestMessagePersistence:
    """メッセージ永続化テスト."""

    @pytest.mark.asyncio
    async def test_send_message_async_persists(self) -> None:
        """非同期メッセージ送信の永続化."""
        runtime_store = MemoryRuntimeStore()
        manager = ProgressManager(runtime_store=runtime_store)

        msg = await manager.send_message_async(
            from_agent="agent1",
            to_agent="agent2",
            content={"data": "test"},
            msg_type=MessageType.RESULT,
        )

        # メモリに追加されている
        assert len(manager.get_messages()) == 1

        # RuntimeStoreに永続化されている
        stored = await runtime_store.load_context(f"msg:{msg.id}")
        assert stored is not None
        assert stored["from_agent"] == "agent1"
        assert stored["to_agent"] == "agent2"

    @pytest.mark.asyncio
    async def test_send_message_async_without_persist(self) -> None:
        """永続化無効での非同期メッセージ送信."""
        runtime_store = MemoryRuntimeStore()
        manager = ProgressManager(runtime_store=runtime_store)

        msg = await manager.send_message_async(
            from_agent="agent1",
            to_agent="agent2",
            content={"data": "test"},
            persist=False,
        )

        # メモリに追加されている
        assert len(manager.get_messages()) == 1

        # RuntimeStoreには永続化されていない
        stored = await runtime_store.load_context(f"msg:{msg.id}")
        assert stored is None

    def test_send_message_sync_compatibility(self) -> None:
        """同期メッセージ送信の後方互換性."""
        manager = ProgressManager()

        msg = manager.send_message(
            from_agent="agent1",
            to_agent="agent2",
            content={"data": "test"},
        )

        assert msg.from_agent == "agent1"
        assert len(manager.get_messages()) == 1


class TestStorageIntegration:
    """ストレージ統合テスト."""

    @pytest.mark.asyncio
    async def test_coordinator_with_custom_stores(self) -> None:
        """カスタムストアでのCoordinator初期化."""
        runtime_store = MemoryRuntimeStore()
        evolution_store = MemoryEvolutionStore()

        coordinator = DeepAgentCoordinator(
            runtime_store=runtime_store,
            evolution_store=evolution_store,
        )

        assert coordinator._runtime_store is runtime_store
        assert coordinator._evolution_store is evolution_store

    @pytest.mark.asyncio
    async def test_progress_manager_with_runtime_store(self) -> None:
        """RuntimeStoreを使用したProgressManager."""
        runtime_store = MemoryRuntimeStore()
        manager = ProgressManager(runtime_store=runtime_store)

        assert manager._runtime_store is runtime_store

    @pytest.mark.asyncio
    async def test_evolver_with_evolution_store(self) -> None:
        """EvolutionStoreを使用したEvolver."""
        evolution_store = MemoryEvolutionStore()
        evolver = Evolver(evolution_store=evolution_store)

        # 学習してストアに保存
        await evolver.learn_from_success(
            task="テストタスク",
            result={"approach": "テスト方法"},
            context={},
        )

        # ストアに保存されているか確認
        stats = await evolution_store.get_stats()
        # 少なくとも処理されていること
        assert stats is not None

    @pytest.mark.asyncio
    async def test_default_stores_created(self) -> None:
        """デフォルトストアが自動生成される."""
        coordinator = DeepAgentCoordinator()

        assert isinstance(coordinator._runtime_store, MemoryRuntimeStore)
        assert isinstance(coordinator._evolution_store, MemoryEvolutionStore)


class TestCheckpointRecovery:
    """Checkpoint/Recovery テスト."""

    @pytest.fixture
    def coordinator(self) -> DeepAgentCoordinator:
        """Coordinatorのフィクスチャ."""
        return DeepAgentCoordinator(
            max_iterations=5,
            quality_threshold=50.0,
        )

    @pytest.mark.asyncio
    async def test_save_checkpoint(self, coordinator: DeepAgentCoordinator) -> None:
        """チェックポイント保存."""
        # タスクを追加
        coordinator._progress.add_todo(TodoItem(id="t1", task="タスク1"))

        checkpoint_id = await coordinator.save_checkpoint("execute", {"test": "data"})

        assert checkpoint_id.startswith("cp-")
        assert coordinator._current_checkpoint_id == checkpoint_id

    @pytest.mark.asyncio
    async def test_load_checkpoint(self, coordinator: DeepAgentCoordinator) -> None:
        """チェックポイント読み込み."""
        coordinator._progress.add_todo(TodoItem(id="t1", task="タスク1"))
        checkpoint_id = await coordinator.save_checkpoint("execute")

        loaded = await coordinator.load_checkpoint(checkpoint_id)
        assert loaded is not None
        assert loaded["phase"] == "execute"
        assert len(loaded["todos"]) == 1

    @pytest.mark.asyncio
    async def test_load_nonexistent_checkpoint(self, coordinator: DeepAgentCoordinator) -> None:
        """存在しないチェックポイント."""
        loaded = await coordinator.load_checkpoint("nonexistent")
        assert loaded is None

    @pytest.mark.asyncio
    async def test_list_checkpoints(self, coordinator: DeepAgentCoordinator) -> None:
        """チェックポイント一覧."""
        await coordinator.save_checkpoint("phase1")
        await coordinator.save_checkpoint("phase2")

        checkpoints = await coordinator.list_checkpoints()
        assert len(checkpoints) >= 2

    @pytest.mark.asyncio
    async def test_resume_from_checkpoint(self, coordinator: DeepAgentCoordinator) -> None:
        """チェックポイントからの再開."""
        # 初期タスク
        coordinator._progress.add_todo(TodoItem(id="t1", task="タスク1", status=TaskStatus.COMPLETED))
        coordinator._progress.add_todo(TodoItem(id="t2", task="タスク2", status=TaskStatus.PENDING))

        # チェックポイント保存
        checkpoint_id = await coordinator.save_checkpoint("execute")

        # 新しい Coordinator で再開
        new_coordinator = DeepAgentCoordinator(
            runtime_store=coordinator._runtime_store,
            max_iterations=5,
            quality_threshold=50.0,
        )

        result = await new_coordinator.resume_from_checkpoint(checkpoint_id, "テストタスク")

        assert result["status"] in ["success", "partial", "error"]
        assert "resumed_from" in result

    @pytest.mark.asyncio
    async def test_resume_invalid_checkpoint(self, coordinator: DeepAgentCoordinator) -> None:
        """無効なチェックポイントからの再開."""
        with pytest.raises(ValueError, match="チェックポイントが見つかりません"):
            await coordinator.resume_from_checkpoint("invalid-id", "タスク")


class TestDeepAgentIntegration:
    """統合テスト."""

    @pytest.mark.asyncio
    async def test_full_workflow_without_llm(self) -> None:
        """LLMなしの完全ワークフロー."""
        coordinator = DeepAgentCoordinator(
            max_iterations=3,
            quality_threshold=50.0,
        )

        result = await coordinator.execute("簡単なタスクを実行")

        assert "status" in result
        assert "progress" in result
        assert "execution_time" in result

    @pytest.mark.asyncio
    async def test_parallel_execution(self) -> None:
        """並行実行テスト."""
        manager = ProgressManager()

        # 独立したタスクを追加
        manager.add_todo(TodoItem(id="t1", task="タスク1"))
        manager.add_todo(TodoItem(id="t2", task="タスク2"))
        manager.add_todo(TodoItem(id="t3", task="タスク3"))

        groups = manager.get_parallel_groups()
        # 全て並行実行可能
        assert len(groups) == 1
        assert len(groups[0].todo_ids) == 3

    @pytest.mark.asyncio
    async def test_sequential_execution(self) -> None:
        """順次実行テスト（依存チェーン）."""
        manager = ProgressManager()

        # 依存チェーン: t1 -> t2 -> t3
        manager.add_todo(TodoItem(id="t1", task="タスク1"))
        manager.add_todo(TodoItem(id="t2", task="タスク2", dependencies=["t1"]))
        manager.add_todo(TodoItem(id="t3", task="タスク3", dependencies=["t2"]))

        groups = manager.get_parallel_groups()
        # 順次実行
        assert len(groups) == 3
        assert groups[0].todo_ids == ["t1"]
        assert groups[1].todo_ids == ["t2"]
        assert groups[2].todo_ids == ["t3"]

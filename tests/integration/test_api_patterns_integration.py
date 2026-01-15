# -*- coding: utf-8 -*-
"""統合テスト - API層 + Patterns の統合テスト.

5つのパターンを使用して、新しい API 層の使いやすさと健壮性をテストします：
1. AgentPipeline - 順次実行
2. ReflectionWorkflow - 自己改善
3. DeepAgentCoordinator - 智能協調
4. AgentComposer - Agent組合
5. TaskDecomposer - タスク分解

テスト観点:
- API層の使いやすさ
- レスポンス形式の一貫性
- エラーハンドリング
- WebSocket / SSE 統合
"""

import asyncio
import json
import logging
from collections.abc import AsyncIterator
from dataclasses import dataclass
from typing import Any

import pytest

# =============================================================================
# API 層（新規作成）
# =============================================================================
from agentflow.api import (
    APIResponse,
    ErrorCode,
    PagedResponse,
    RichResponseBuilder,
    SSEEmitter,
    StreamEvent,
    StreamEventType,
    WebSocketHub,
    WSMessage,
    WSMessageType,
)

# =============================================================================
# Patterns
# =============================================================================
from agentflow.patterns import (
    # 1. AgentPipeline
    AgentPipeline,
    AgentConfig,
    PipelineConfig,
    # 2. ReflectionWorkflow
    ReflectionWorkflow,
    ReflectionResult,
    # 3. AgentComposer
    AgentComposer,
    CompositionPattern,
    CompositionConfig,
    AgentRole,
    # 4. TaskDecomposer
    TaskDecomposer,
    DecompositionConfig,
    TaskGranularity,
    TaskPriority,
    DecomposedTask,
    DecompositionPlan,
    # 5. SharedContext
    SharedContext,
)

# =============================================================================
# Core
# =============================================================================
from agentflow.core.agent_block import AgentBlock


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# テスト用 Mock Agent
# =============================================================================


class MockAgent(AgentBlock):
    """テスト用モック Agent."""

    def __init__(self, name: str = "MockAgent", delay: float = 0.1) -> None:
        super().__init__()
        self.name = name
        self.delay = delay
        self.call_count = 0

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """実行."""
        self.call_count += 1
        await asyncio.sleep(self.delay)
        return {
            "agent": self.name,
            "input": input_data,
            "call_count": self.call_count,
            "status": "success",
        }

    async def run_stream(
        self, input_data: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行."""
        yield {"type": "progress", "progress": 0, "message": f"{self.name} 開始"}
        await asyncio.sleep(self.delay / 2)
        yield {"type": "progress", "progress": 50, "message": f"{self.name} 処理中"}
        await asyncio.sleep(self.delay / 2)
        result = await self.run(input_data)
        yield {"type": "result", "data": result}


class AnalyzerAgent(MockAgent):
    """分析 Agent."""

    def __init__(self) -> None:
        super().__init__(name="AnalyzerAgent", delay=0.05)

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        result = await super().run(input_data)
        result["analysis"] = {
            "keywords": ["test", "integration", "api"],
            "sentiment": "positive",
            "confidence": 0.85,
        }
        return result


class TransformerAgent(MockAgent):
    """変換 Agent."""

    def __init__(self) -> None:
        super().__init__(name="TransformerAgent", delay=0.05)

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        result = await super().run(input_data)
        result["transformation"] = {
            "original": input_data.get("text", ""),
            "transformed": input_data.get("text", "").upper(),
        }
        return result


class ReporterAgent(MockAgent):
    """レポート生成 Agent."""

    def __init__(self) -> None:
        super().__init__(name="ReporterAgent", delay=0.05)

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        result = await super().run(input_data)
        # RichResponseBuilder を使用
        builder = RichResponseBuilder()
        report = (
            builder
            .add_markdown("# 処理レポート")
            .add_markdown(f"入力データ: {input_data}")
            .add_table([{"項目": k, "値": str(v)[:50]} for k, v in input_data.items()])
            .add_success("処理が完了しました")
            .build()
        )
        result["rich_report"] = report
        return result


# =============================================================================
# テストクラス
# =============================================================================


class TestAPIResponseIntegration:
    """APIResponse 統合テスト."""

    def test_success_response(self):
        """成功レスポンスのテスト."""
        response = APIResponse.ok(data={"result": "test"})

        assert response.success is True
        assert response.data == {"result": "test"}
        assert response.error is None
        assert response.timestamp is not None

    def test_error_response(self):
        """エラーレスポンスのテスト."""
        response = APIResponse.fail(
            code=ErrorCode.VALIDATION_ERROR,
            message="Invalid input",
            details={"field": "name"},
        )

        assert response.success is False
        assert response.error is not None
        assert response.error.code == ErrorCode.VALIDATION_ERROR
        assert response.error.message == "Invalid input"

    def test_paged_response(self):
        """ページネーションレスポンスのテスト."""
        items = [{"id": i} for i in range(25)]
        paged = PagedResponse.create(
            items=items[:10],
            total=25,
            page=1,
            page_size=10,
        )

        assert paged.total == 25
        assert paged.page == 1
        assert paged.has_next is True
        assert paged.has_prev is False

    def test_stream_event(self):
        """StreamEvent のテスト."""
        progress = StreamEvent.progress(50, "処理中...")
        assert progress.type == StreamEventType.PROGRESS
        assert progress.data["value"] == 50

        result = StreamEvent.result({"status": "ok"})
        assert result.type == StreamEventType.RESULT

        error = StreamEvent.error("エラー発生")
        assert error.type == StreamEventType.ERROR


class TestSSEEmitterIntegration:
    """SSEEmitter 統合テスト."""

    def test_progress_event(self):
        """進捗イベントのテスト."""
        emitter = SSEEmitter()
        event = emitter.progress(75, "Almost done")

        assert "event: progress" in event
        assert '"value": 75' in event
        assert "Almost done" in event

    def test_data_event(self):
        """データイベントのテスト."""
        emitter = SSEEmitter()
        event = emitter.data({"result": "success"})

        assert "event: data" in event
        assert "success" in event

    def test_complete_event(self):
        """完了イベントのテスト."""
        emitter = SSEEmitter()
        event = emitter.complete({"total": 100})

        assert "event: complete" in event

    def test_error_event(self):
        """エラーイベントのテスト."""
        emitter = SSEEmitter()
        event = emitter.error("Something went wrong", code="TEST_ERROR")

        assert "event: error" in event
        assert "TEST_ERROR" in event


class TestRichResponseBuilderIntegration:
    """RichResponseBuilder 統合テスト."""

    def test_basic_builder(self):
        """基本的なビルダーテスト."""
        builder = RichResponseBuilder()
        response = (
            builder
            .add_markdown("# Title")
            .add_paragraph("This is a test.")
            .build()
        )

        assert "components" in response
        assert len(response["components"]) == 2

    def test_table_builder(self):
        """テーブルビルダーテスト."""
        data = [
            {"name": "Alice", "score": 95},
            {"name": "Bob", "score": 87},
        ]
        builder = RichResponseBuilder()
        response = builder.add_table(data, title="Scores").build()

        components = response["components"]
        assert len(components) == 1
        assert components[0]["type"] == "data_table"

    def test_chart_builder(self):
        """チャートビルダーテスト."""
        builder = RichResponseBuilder()
        response = (
            builder
            .add_bar_chart(["A", "B", "C"], [10, 20, 30], title="Test Chart")
            .add_pie_chart([{"name": "A", "value": 60}, {"name": "B", "value": 40}])
            .build()
        )

        components = response["components"]
        assert len(components) == 2
        assert all(c["type"] == "chart" for c in components)

    def test_alert_builder(self):
        """アラートビルダーテスト."""
        builder = RichResponseBuilder()
        response = (
            builder
            .add_info("Information message")
            .add_success("Success message")
            .add_warning("Warning message")
            .add_error("Error message")
            .build()
        )

        components = response["components"]
        assert len(components) == 4
        assert all(c["type"] == "alert" for c in components)

    def test_section_builder(self):
        """セクションビルダーテスト."""
        builder = RichResponseBuilder()
        response = (
            builder
            .section("分析結果")
            .add_markdown("分析内容...")
            .add_table([{"key": "value"}])
            .end_section()
            .section("推奨事項")
            .add_markdown("推奨内容...")
            .end_section()
            .build()
        )

        # セクションヘッダーを含む
        assert len(response["components"]) >= 4


class TestWebSocketHubIntegration:
    """WebSocketHub 統合テスト."""

    def test_hub_initialization(self):
        """Hub 初期化テスト."""
        hub = WebSocketHub()
        assert hub.client_count == 0

    def test_ws_message_serialization(self):
        """WSMessage シリアライズテスト."""
        msg = WSMessage(
            type=WSMessageType.DATA,
            data={"test": "value"},
            room="test-room",
        )
        json_str = msg.to_json()
        parsed = WSMessage.from_json(json_str)

        assert parsed.type == WSMessageType.DATA
        assert parsed.data == {"test": "value"}
        assert parsed.room == "test-room"


# =============================================================================
# Pattern + API 統合テスト
# =============================================================================


class TestAgentPipelineWithAPI:
    """AgentPipeline + API 層統合テスト."""

    @pytest.mark.asyncio
    async def test_pipeline_with_rich_response(self):
        """Pipeline 実行結果を RichResponse で構築."""
        # Pipeline 作成（AgentConfig でラップ）
        pipeline = AgentPipeline(
            agents=[
                AgentConfig(agent=AnalyzerAgent(), id="analyzer", name="Analyzer"),
                AgentConfig(agent=TransformerAgent(), id="transformer", name="Transformer"),
                AgentConfig(agent=ReporterAgent(), id="reporter", name="Reporter"),
            ],
        )

        # 実行（run メソッドが追加されたので直接呼べる）
        input_data = {"text": "test input", "task": "analyze"}
        result = await pipeline.run(input_data)

        # API レスポンスでラップ
        response = APIResponse.ok(data=result)
        assert response.success is True

        # RichResponse でレポート構築
        builder = RichResponseBuilder()
        report = (
            builder
            .add_markdown("# Pipeline 実行結果")
            .add_table([
                {"Agent": "Analyzer", "Status": "完了"},
                {"Agent": "Transformer", "Status": "完了"},
                {"Agent": "Reporter", "Status": "完了"},
            ])
            .add_success("Pipeline が正常に完了しました")
            .build()
        )

        assert len(report["components"]) == 3


class TestAgentComposerWithAPI:
    """AgentComposer + API 層統合テスト."""

    @pytest.mark.asyncio
    async def test_composer_sequential(self):
        """Sequential（PIPELINE）パターンのテスト."""
        composer = AgentComposer(
            config=CompositionConfig(
                pattern=CompositionPattern.SEQUENTIAL,  # PIPELINE のエイリアス
            ),
        )

        # Agent 追加（新しい役割を使用）
        composer.add_agent(AnalyzerAgent(), role=AgentRole.ANALYZER)
        composer.add_agent(TransformerAgent(), role=AgentRole.EXECUTOR)

        # 実行
        result = await composer.execute({"input": "test"})

        # API レスポンス
        response = APIResponse.ok(data=result)
        assert response.success is True
        assert result is not None

    @pytest.mark.asyncio
    async def test_composer_parallel(self):
        """Parallel（BROADCAST）パターンのテスト."""
        composer = AgentComposer(
            config=CompositionConfig(
                pattern=CompositionPattern.PARALLEL,  # BROADCAST のエイリアス
            ),
        )

        # 複数 Agent 追加
        for i in range(3):
            composer.add_agent(
                MockAgent(name=f"Worker-{i}", delay=0.05),
                role=AgentRole.EXECUTOR,
            )

        # 実行
        result = await composer.execute({"task": "parallel_test"})

        # SSE イベント生成
        emitter = SSEEmitter()
        events = [
            emitter.progress(100, "Parallel execution complete"),
            emitter.data(result.results if hasattr(result, "results") else {"status": "ok"}),
            emitter.complete(),
        ]

        assert all("data:" in e for e in events)


class TestTaskDecomposerWithAPI:
    """TaskDecomposer + API 層統合テスト."""

    @pytest.mark.asyncio
    async def test_decomposition_with_rich_response(self):
        """タスク分解結果を RichResponse で表示."""
        # モックの分解結果を作成（LLM なしでテスト可能に）
        plan = DecompositionPlan(
            id="plan-test-001",
            root_task_id="task-root",
            tasks={
                "task-1": DecomposedTask(
                    id="task-1",
                    name="要件定義",
                    description="アプリケーションの要件を定義する",
                    priority=TaskPriority.HIGH,
                    dependencies=[],
                ),
                "task-2": DecomposedTask(
                    id="task-2",
                    name="設計",
                    description="アーキテクチャを設計する",
                    priority=TaskPriority.HIGH,
                    dependencies=["task-1"],
                ),
                "task-3": DecomposedTask(
                    id="task-3",
                    name="実装",
                    description="機能を実装する",
                    priority=TaskPriority.MEDIUM,
                    dependencies=["task-2"],
                ),
            },
            execution_order=["task-1", "task-2", "task-3"],
        )

        task = "Webアプリケーションを構築する"

        # RichResponse でビジュアル化
        builder = RichResponseBuilder()
        response = (
            builder
            .add_heading(f"タスク分解: {task}", level=1)
            .add_markdown(f"**サブタスク数**: {len(plan.tasks)}")
        )

        # サブタスクテーブル
        task_data = [
            {
                "ID": t.id,
                "タスク": t.description[:30],
                "優先度": t.priority.value if hasattr(t.priority, "value") else str(t.priority),
            }
            for t in plan.tasks.values()
        ]
        response.add_table(task_data, title="サブタスク一覧")

        # 依存関係図（簡易）
        response.add_markdown("## 依存関係")
        dep_items = []
        for t in plan.tasks.values():
            if t.dependencies:
                dep_items.append(f"- {t.id} → {', '.join(t.dependencies)}")
        if dep_items:
            response.add_markdown("\n".join(dep_items))

        result = response.build()
        assert len(result["components"]) >= 3


class TestSharedContextWithAPI:
    """SharedContext + API 層統合テスト."""

    @pytest.mark.asyncio
    async def test_shared_context_state(self):
        """共有コンテキストの状態管理."""
        context = SharedContext()

        # 状態設定
        context.set("user_id", "user-123")
        context.set("session", {"started": "2024-01-15"})
        context.set("results", [])

        # Agent 実行
        agent = MockAgent()
        result = await agent.run({"context_id": context.get("user_id")})

        # 結果を追加
        results = context.get("results", [])
        results.append(result)
        context.set("results", results)

        # API レスポンス
        response = APIResponse.ok(
            data={
                "context": {
                    "user_id": context.get("user_id"),
                    "results_count": len(context.get("results", [])),
                },
                "latest_result": result,
            },
        )

        assert response.success is True
        assert response.data["context"]["results_count"] == 1


# =============================================================================
# 複合シナリオテスト
# =============================================================================


class TestComplexScenario:
    """複合シナリオテスト - 全パターン統合."""

    @pytest.mark.asyncio
    async def test_full_workflow_scenario(self):
        """完全なワークフローシナリオ.

        1. TaskDecomposer でタスク分解（モック）
        2. AgentComposer で Agent 割当
        3. AgentPipeline で実行
        4. RichResponse でレポート生成
        5. APIResponse でラップ
        """
        # 1. タスク分解（モック）
        task = "データ分析レポートを作成する"
        plan = DecompositionPlan(
            id="plan-workflow-001",
            root_task_id="task-root",
            tasks={
                "task-1": DecomposedTask(
                    id="task-1",
                    name="データ収集",
                    description="必要なデータを収集する",
                    priority=TaskPriority.HIGH,
                ),
                "task-2": DecomposedTask(
                    id="task-2",
                    name="データ分析",
                    description="データを分析する",
                    priority=TaskPriority.HIGH,
                    dependencies=["task-1"],
                ),
            },
            execution_order=["task-1", "task-2"],
        )

        # 2. Agent 構成
        composer = AgentComposer()
        composer.add_agent(AnalyzerAgent(), role=AgentRole.ANALYZER)
        composer.add_agent(TransformerAgent(), role=AgentRole.EXECUTOR)
        composer.add_agent(ReporterAgent(), role=AgentRole.REPORTER)

        # 3. 実行（簡易版）
        execution_results = []
        for task_id, subtask in list(plan.tasks.items())[:2]:
            result = await composer.execute({"task": subtask.description})
            execution_results.append({
                "task_id": task_id,
                "result": result,
            })

        # 4. レポート生成
        builder = RichResponseBuilder()
        report = (
            builder
            .add_heading("ワークフロー実行レポート", level=1)
            .add_markdown(f"**元タスク**: {task}")
            .add_markdown(f"**分解サブタスク数**: {len(plan.tasks)}")
            .add_markdown(f"**実行済み**: {len(execution_results)}")
            .section("実行結果")
            .add_table([
                {
                    "タスクID": r["task_id"],
                    "ステータス": "完了",
                }
                for r in execution_results
            ])
            .end_section()
            .add_success("ワークフローが正常に完了しました")
            .build()
        )

        # 5. API レスポンス
        response = APIResponse.ok(
            data={
                "report": report,
                "summary": {
                    "total_tasks": len(plan.tasks),
                    "completed_tasks": len(execution_results),
                },
            },
            meta={"workflow_id": "wf-001"},
        )

        assert response.success is True
        assert response.data["summary"]["completed_tasks"] == 2
        assert len(response.data["report"]["components"]) >= 5

    @pytest.mark.asyncio
    async def test_sse_streaming_scenario(self):
        """SSE ストリーミングシナリオ."""
        emitter = SSEEmitter()
        pipeline = AgentPipeline(
            agents=[
                AgentConfig(agent=MockAgent(delay=0.01), id=f"agent-{i}", name=f"Agent-{i}")
                for i in range(3)
            ],
        )

        # ストリームイベント収集
        events = []

        # 開始
        events.append(emitter.start("Pipeline 実行開始"))

        # 進捗シミュレーション
        for i, config in enumerate(pipeline.agents):
            events.append(emitter.agent_start(config.name))
            events.append(emitter.progress(
                (i + 1) * 33,
                f"Agent {i + 1}/3 実行中...",
            ))

        # 結果
        result = await pipeline.run({"test": True})
        events.append(emitter.data(result))

        # 完了
        events.append(emitter.complete({"total_agents": 3}))

        # 検証
        assert len(events) >= 8
        assert all(isinstance(e, str) for e in events)
        assert all("data:" in e or ": heartbeat" in e for e in events)


# =============================================================================
# エラーハンドリングテスト
# =============================================================================


class TestErrorHandling:
    """エラーハンドリングテスト."""

    def test_api_error_codes(self):
        """エラーコードの網羅性テスト."""
        error_cases = [
            (ErrorCode.VALIDATION_ERROR, "入力エラー"),
            (ErrorCode.NOT_FOUND, "リソースが見つかりません"),
            (ErrorCode.AGENT_ERROR, "Agent 実行エラー"),
            (ErrorCode.LLM_ERROR, "LLM エラー"),
            (ErrorCode.RATE_LIMITED, "レート制限"),
        ]

        for code, message in error_cases:
            response = APIResponse.fail(code=code, message=message)
            assert response.success is False
            assert response.error.code == code
            assert response.error.message == message

    @pytest.mark.asyncio
    async def test_pipeline_error_recovery(self):
        """Pipeline エラーリカバリーテスト."""

        class FailingAgent(MockAgent):
            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                if input_data.get("fail"):
                    raise ValueError("Intentional failure")
                return await super().run(input_data)

        pipeline = AgentPipeline(
            agents=[AgentConfig(agent=FailingAgent(), id="failing", name="FailingAgent")]
        )

        try:
            await pipeline.run({"fail": True})
            response = APIResponse.ok()
        except Exception as e:
            response = APIResponse.fail(
                code=ErrorCode.AGENT_ERROR,
                message=str(e),
            )

        assert response.success is False
        assert "Intentional failure" in response.error.message


# =============================================================================
# 使いやすさテスト
# =============================================================================


class TestUsability:
    """使いやすさテスト - API 設計の検証."""

    def test_fluent_api(self):
        """流暢 API のテスト."""
        # メソッドチェーンが自然に動作すること
        builder = RichResponseBuilder()
        result = (
            builder
            .add_markdown("# Title")
            .add_paragraph("Content")
            .add_table([{"a": 1}])
            .add_chart("bar", {"data": []})
            .add_success("Done")
            .set_metadata("version", "1.0")
            .build()
        )

        assert "components" in result
        assert "metadata" in result
        assert result["metadata"]["version"] == "1.0"

    def test_one_liner_response(self):
        """ワンライナーレスポンスのテスト."""
        # 最小コードで API レスポンスが作れること
        ok = APIResponse.ok(data={"status": "success"})
        fail = APIResponse.fail(message="error")

        assert ok.success is True
        assert fail.success is False

    def test_type_safety(self):
        """型安全性のテスト."""
        # 型が正しく推論されること
        response: APIResponse[dict] = APIResponse.ok(data={"key": "value"})
        paged: PagedResponse[dict] = PagedResponse.create(
            items=[{"id": 1}],
            total=1,
        )

        assert isinstance(response.data, dict)
        assert isinstance(paged.items, list)


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])

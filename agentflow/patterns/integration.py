"""AgentFlowEngine Integration - Plan-and-Execute パターンを AgentFlowEngine に統合.

業界最佳実践に基づいた統合:
- 既存の WorkflowConfig との互換性
- ライフサイクルフックとの統合
- プロトコルサポート

参考:
- LangChain: Agent integration patterns
- Anthropic: Building Effective Agents
"""

from __future__ import annotations

import logging
from typing import Any

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.types import ExecutionContext, ExecutionResult, WorkflowConfig
from agentflow.patterns.agent import PlanAndExecuteAgent
from agentflow.patterns.executor import PlanExecutor
from agentflow.patterns.planner import DynamicPlanner
from agentflow.patterns.replanner import Replanner
from agentflow.patterns.validator import StepValidator


class PlanAndExecuteWorkflow:
    """Plan-and-Execute ワークフローラッパー.

    AgentFlowEngine の WorkflowConfig 形式で Plan-and-Execute エージェントを実行します。

    Example:
        >>> engine = AgentFlowEngine()
        >>> workflow = PlanAndExecuteWorkflow.create_workflow(
        ...     workflow_id="plan-execute-demo",
        ...     llm=my_llm,
        ...     tool_caller=my_tool_caller,
        ...     available_tools=["search", "calculate"]
        ... )
        >>> engine.register_workflow(workflow)
        >>> result = await engine.execute("plan-execute-demo", {"task": "Find capital of France"})
    """

    @staticmethod
    def create_workflow(
        workflow_id: str,
        llm: Any,
        tool_caller: Any,
        available_tools: list[str],
        name: str = "Plan-and-Execute Workflow",
        description: str = "Dynamic planning and execution workflow",
        max_iterations: int = 5,
        logger: logging.Logger | None = None,
    ) -> WorkflowConfig:
        """Plan-and-Execute ワークフローを作成.

        Args:
            workflow_id: ワークフロー ID
            llm: LLM インスタンス
            tool_caller: ツール呼び出しインターフェース
            available_tools: 利用可能なツールリスト
            name: ワークフロー名
            description: ワークフロー説明
            max_iterations: 最大イテレーション数
            logger: ロガーインスタンス

        Returns:
            WorkflowConfig インスタンス
        """
        # Plan-and-Execute エージェントを作成
        planner = DynamicPlanner(
            llm=llm,
            available_tools=available_tools,
            logger=logger,
        )
        executor = PlanExecutor(
            tool_caller=tool_caller,
            logger=logger,
        )
        validator = StepValidator(
            llm=llm,
            logger=logger,
        )
        replanner = Replanner(
            planner=planner,
            logger=logger,
        )
        agent = PlanAndExecuteAgent(
            planner=planner,
            executor=executor,
            validator=validator,
            replanner=replanner,
            max_iterations=max_iterations,
            logger=logger,
        )

        # WorkflowConfig を作成
        # 注: 実際のノードとエッジは AgentFlowEngine の実行時に動的に生成されます
        return WorkflowConfig(
            workflow_id=workflow_id,
            name=name,
            description=description,
            nodes=[
                {
                    "id": "plan_and_execute",
                    "type": "agent",
                    "agent": agent,
                }
            ],
            edges=[],
            config={
                "pattern": "plan-and-execute",
                "max_iterations": max_iterations,
            },
        )

    @staticmethod
    async def execute_with_engine(
        engine: AgentFlowEngine,
        workflow_id: str,
        task: str,
        context: dict[str, Any] | None = None,
        user_id: str = "system",
    ) -> ExecutionResult:
        """AgentFlowEngine を使用して Plan-and-Execute ワークフローを実行.

        Args:
            engine: AgentFlowEngine インスタンス
            workflow_id: ワークフロー ID
            task: タスク説明
            context: 追加コンテキスト
            user_id: ユーザー ID

        Returns:
            ExecutionResult

        Example:
            >>> result = await PlanAndExecuteWorkflow.execute_with_engine(
            ...     engine=engine,
            ...     workflow_id="plan-execute-demo",
            ...     task="Find the capital of France and its population"
            ... )
        """
        # ワークフローを取得
        workflow = engine._workflows.get(workflow_id)
        if not workflow:
            msg = f"Workflow not found: {workflow_id}"
            raise ValueError(msg)

        # エージェントを取得
        agent_node = workflow.nodes[0]
        agent: PlanAndExecuteAgent = agent_node["agent"]

        # エージェントを実行
        result = await agent.execute(task, context, user_id)

        # ExecutionResult に変換
        execution_context = ExecutionContext(
            workflow_id=workflow_id,
            execution_id=result.get("plan", {}).get("created_at", "unknown"),
            inputs={"task": task, "context": context or {}},
        )

        if result.get("success", False):
            return ExecutionResult(
                status="success",
                output=result,
                error=None,
                duration=0.0,  # TODO: 実際の実行時間を計測
                context=execution_context,
            )
        return ExecutionResult(
            status="error",
            output=result,
            error=result.get("error", "Unknown error"),
            duration=0.0,
            context=execution_context,
        )


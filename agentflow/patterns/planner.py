# -*- coding: utf-8 -*-
"""PlannerAgent - 計画型 Agent パターン.

このモジュールは Plan-and-Execute パターンを実装します。

主要コンポーネント:
- Plan / Step: 計画とステップのデータモデル
- PlannerAgent: LLM を使用して計画を生成
- PlanExecutor: 計画を実行し、結果を検証

認知アーキテクチャ:
    ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐
    │  Goal   │ →  │  Plan   │ →  │ Execute │ →  │ Verify  │
    │ (目標)  │    │ (計画)  │    │ (実行)  │    │ (検証)  │
    └─────────┘    └─────────┘    └─────────┘    └─────────┘
         ↑                              │              │
         └──────────────────────────────┴──────────────┘
                        Re-plan (必要に応じて)

使用例:
    >>> from agentflow.patterns.planner import PlannerAgent, PlanExecutor
    >>>
    >>> # 計画を生成
    >>> planner = PlannerAgent()
    >>> plan = await planner.create_plan(
    ...     goal="新規事業を立ち上げる",
    ...     context={"budget": 1000000, "timeline": "6ヶ月"}
    ... )
    >>>
    >>> # 計画を実行
    >>> executor = PlanExecutor(agents={"research": ResearchAgent(), ...})
    >>> result = await executor.execute(plan)
"""

from __future__ import annotations

import logging
from datetime import datetime
from enum import Enum
from typing import Any, Callable

from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)


# =============================================================================
# データモデル
# =============================================================================


class StepStatus(str, Enum):
    """ステップの状態."""

    PENDING = "pending"  # 未実行
    RUNNING = "running"  # 実行中
    COMPLETED = "completed"  # 完了
    FAILED = "failed"  # 失敗
    SKIPPED = "skipped"  # スキップ


class PlanStatus(str, Enum):
    """計画の状態."""

    DRAFT = "draft"  # 下書き
    APPROVED = "approved"  # 承認済み
    EXECUTING = "executing"  # 実行中
    COMPLETED = "completed"  # 完了
    FAILED = "failed"  # 失敗
    REPLANNING = "replanning"  # 再計画中


class Step(BaseModel):
    """計画の1ステップ.

    Attributes:
        id: ステップID
        name: ステップ名
        description: 詳細説明
        agent: 実行する Agent 名
        inputs: 入力データ
        expected_output: 期待される出力の説明
        dependencies: 依存するステップID
        status: 現在の状態
        result: 実行結果
        error: エラーメッセージ
    """

    id: str = Field(..., description="ステップID")
    name: str = Field(..., description="ステップ名")
    description: str = Field("", description="詳細説明")
    agent: str | None = Field(None, description="実行する Agent 名")
    inputs: dict[str, Any] = Field(default_factory=dict, description="入力データ")
    expected_output: str = Field("", description="期待される出力の説明")
    dependencies: list[str] = Field(default_factory=list, description="依存ステップID")
    status: StepStatus = Field(StepStatus.PENDING, description="状態")
    result: dict[str, Any] | None = Field(None, description="実行結果")
    error: str | None = Field(None, description="エラーメッセージ")
    started_at: datetime | None = Field(None, description="開始時刻")
    completed_at: datetime | None = Field(None, description="完了時刻")


class Plan(BaseModel):
    """実行計画.

    Attributes:
        id: 計画ID
        goal: 達成目標
        context: コンテキスト情報
        steps: ステップリスト
        status: 計画の状態
        created_at: 作成日時
        metadata: メタデータ
    """

    id: str = Field(default_factory=lambda: f"plan-{datetime.now().strftime('%Y%m%d%H%M%S')}")
    goal: str = Field(..., description="達成目標")
    context: dict[str, Any] = Field(default_factory=dict, description="コンテキスト")
    steps: list[Step] = Field(default_factory=list, description="ステップリスト")
    status: PlanStatus = Field(PlanStatus.DRAFT, description="状態")
    created_at: datetime = Field(default_factory=datetime.now)
    updated_at: datetime = Field(default_factory=datetime.now)
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")

    def get_next_step(self) -> Step | None:
        """次に実行すべきステップを取得."""
        for step in self.steps:
            if step.status == StepStatus.PENDING:
                # 依存関係をチェック
                deps_satisfied = all(
                    self._get_step(dep_id).status == StepStatus.COMPLETED
                    for dep_id in step.dependencies
                    if self._get_step(dep_id)
                )
                if deps_satisfied:
                    return step
        return None

    def _get_step(self, step_id: str) -> Step | None:
        """ステップIDからステップを取得."""
        for step in self.steps:
            if step.id == step_id:
                return step
        return None

    def is_complete(self) -> bool:
        """計画が完了したかを確認."""
        return all(
            step.status in (StepStatus.COMPLETED, StepStatus.SKIPPED) for step in self.steps
        )

    def has_failed(self) -> bool:
        """計画が失敗したかを確認."""
        return any(step.status == StepStatus.FAILED for step in self.steps)

    def get_progress(self) -> float:
        """進捗率を取得 (0.0 - 1.0)."""
        if not self.steps:
            return 0.0
        completed = sum(
            1 for s in self.steps if s.status in (StepStatus.COMPLETED, StepStatus.SKIPPED)
        )
        return completed / len(self.steps)


class PlanResult(BaseModel):
    """計画実行結果.

    Attributes:
        plan: 実行された計画
        success: 成功したかどうか
        outputs: 各ステップの出力
        error: エラーメッセージ
    """

    plan: Plan
    success: bool = False
    outputs: dict[str, Any] = Field(default_factory=dict)
    error: str | None = None
    execution_time_seconds: float = 0.0


# =============================================================================
# PlannerAgent - 計画生成 Agent
# =============================================================================


class PlannerAgent:
    """計画生成 Agent.

    目標を分析し、実行可能なステップに分解する。

    使用例:
        >>> planner = PlannerAgent()
        >>> plan = await planner.create_plan(
        ...     goal="Webアプリをデプロイする",
        ...     context={"stack": "FastAPI + React"},
        ...     available_agents=["CodeAgent", "TestAgent", "DeployAgent"],
        ... )
        >>> print(plan.steps)
    """

    def __init__(
        self,
        llm: Any | None = None,
        system_prompt: str | None = None,
        max_steps: int = 10,
    ) -> None:
        """初期化.

        Args:
            llm: LLM プロバイダー（None の場合は自動取得）
            system_prompt: カスタムシステムプロンプト
            max_steps: 最大ステップ数
        """
        self._llm = llm
        self._max_steps = max_steps
        self._system_prompt = system_prompt or self._default_system_prompt()

    def _default_system_prompt(self) -> str:
        """デフォルトのシステムプロンプト."""
        return """あなたは計画立案の専門家です。
与えられた目標を、具体的で実行可能なステップに分解してください。

ルール:
1. 各ステップは独立して実行可能であること
2. 依存関係を明確にすること
3. 各ステップに期待される成果を記述すること
4. 現実的なタイムラインを考慮すること

出力形式 (JSON):
{
  "steps": [
    {
      "id": "step-1",
      "name": "ステップ名",
      "description": "詳細説明",
      "agent": "実行するAgent名（あれば）",
      "expected_output": "期待される成果",
      "dependencies": []
    }
  ]
}"""

    async def create_plan(
        self,
        goal: str,
        context: dict[str, Any] | None = None,
        available_agents: list[str] | None = None,
        constraints: dict[str, Any] | None = None,
    ) -> Plan:
        """目標から計画を生成.

        Args:
            goal: 達成目標
            context: コンテキスト情報
            available_agents: 利用可能な Agent 名リスト
            constraints: 制約条件（予算、期限など）

        Returns:
            生成された Plan
        """
        context = context or {}
        constraints = constraints or {}

        # LLM を使用して計画を生成
        if self._llm:
            plan = await self._generate_with_llm(goal, context, available_agents, constraints)
        else:
            # LLM がない場合はシンプルな計画を生成
            plan = self._generate_simple_plan(goal, context)

        logger.info(f"Plan created: {plan.id} with {len(plan.steps)} steps")
        return plan

    async def _generate_with_llm(
        self,
        goal: str,
        context: dict[str, Any],
        available_agents: list[str] | None,
        constraints: dict[str, Any],
    ) -> Plan:
        """LLM を使用して計画を生成."""
        from agentflow.providers import get_llm

        llm = self._llm or get_llm()

        user_prompt = f"""目標: {goal}

コンテキスト: {context}

利用可能なAgent: {available_agents or "指定なし"}

制約条件: {constraints}

上記の目標を達成するための計画を作成してください。"""

        messages = [
            {"role": "system", "content": self._system_prompt},
            {"role": "user", "content": user_prompt},
        ]

        response = await llm.chat(messages, response_format={"type": "json_object"})

        # レスポンスをパース
        import json

        try:
            data = json.loads(response.content)
            steps = [
                Step(
                    id=s.get("id", f"step-{i+1}"),
                    name=s.get("name", f"Step {i+1}"),
                    description=s.get("description", ""),
                    agent=s.get("agent"),
                    expected_output=s.get("expected_output", ""),
                    dependencies=s.get("dependencies", []),
                )
                for i, s in enumerate(data.get("steps", [])[:self._max_steps])
            ]
        except (json.JSONDecodeError, KeyError) as e:
            logger.warning(f"Failed to parse LLM response: {e}")
            steps = []

        return Plan(goal=goal, context=context, steps=steps, metadata={"constraints": constraints})

    def _generate_simple_plan(self, goal: str, context: dict[str, Any]) -> Plan:
        """シンプルな計画を生成（LLM なし）."""
        steps = [
            Step(id="step-1", name="分析", description=f"目標「{goal}」を分析する"),
            Step(id="step-2", name="設計", description="実行計画を設計する", dependencies=["step-1"]),
            Step(id="step-3", name="実行", description="計画を実行する", dependencies=["step-2"]),
            Step(id="step-4", name="検証", description="結果を検証する", dependencies=["step-3"]),
        ]
        return Plan(goal=goal, context=context, steps=steps)

    async def replan(
        self,
        original_plan: Plan,
        failed_step: Step,
        error: str,
    ) -> Plan:
        """失敗したステップを考慮して再計画.

        Args:
            original_plan: 元の計画
            failed_step: 失敗したステップ
            error: エラー内容

        Returns:
            修正された Plan
        """
        logger.info(f"Replanning due to failure at step: {failed_step.id}")

        # 失敗したステップ以降をリセット
        new_steps = []
        for step in original_plan.steps:
            if step.id == failed_step.id:
                # 失敗したステップを修正
                new_step = step.model_copy(
                    update={
                        "status": StepStatus.PENDING,
                        "description": f"{step.description} (再試行: {error})",
                        "error": None,
                    }
                )
                new_steps.append(new_step)
            elif step.status in (StepStatus.COMPLETED, StepStatus.SKIPPED):
                new_steps.append(step)
            else:
                # 未実行のステップはそのまま
                new_steps.append(step.model_copy(update={"status": StepStatus.PENDING}))

        return original_plan.model_copy(
            update={"steps": new_steps, "status": PlanStatus.REPLANNING, "updated_at": datetime.now()}
        )


# =============================================================================
# PlanExecutor - 計画実行器
# =============================================================================


class PlanExecutor:
    """計画実行器.

    計画のステップを順次実行し、結果を検証する。

    使用例:
        >>> executor = PlanExecutor(
        ...     agents={"CodeAgent": code_agent, "TestAgent": test_agent},
        ...     on_step_complete=lambda step: print(f"Completed: {step.name}"),
        ... )
        >>> result = await executor.execute(plan)
        >>> if result.success:
        ...     print("Plan completed successfully!")
    """

    def __init__(
        self,
        agents: dict[str, Any] | None = None,
        default_agent: Any | None = None,
        on_step_start: Callable[[Step], None] | None = None,
        on_step_complete: Callable[[Step], None] | None = None,
        on_step_failed: Callable[[Step, str], None] | None = None,
        max_retries: int = 1,
        enable_replan: bool = False,
        planner: PlannerAgent | None = None,
    ) -> None:
        """初期化.

        Args:
            agents: Agent 名から Agent インスタンスへのマップ
            default_agent: デフォルト Agent
            on_step_start: ステップ開始時コールバック
            on_step_complete: ステップ完了時コールバック
            on_step_failed: ステップ失敗時コールバック
            max_retries: 最大リトライ回数
            enable_replan: 失敗時に再計画を有効化
            planner: 再計画用の PlannerAgent
        """
        self._agents = agents or {}
        self._default_agent = default_agent
        self._on_step_start = on_step_start
        self._on_step_complete = on_step_complete
        self._on_step_failed = on_step_failed
        self._max_retries = max_retries
        self._enable_replan = enable_replan
        self._planner = planner

    async def execute(self, plan: Plan) -> PlanResult:
        """計画を実行.

        Args:
            plan: 実行する計画

        Returns:
            実行結果
        """
        import time

        start_time = time.time()
        plan.status = PlanStatus.EXECUTING
        outputs: dict[str, Any] = {}

        logger.info(f"Executing plan: {plan.id} ({len(plan.steps)} steps)")

        while not plan.is_complete() and not plan.has_failed():
            step = plan.get_next_step()
            if step is None:
                # 実行可能なステップがない（循環依存の可能性）
                logger.error("No executable step found - possible circular dependency")
                break

            try:
                result = await self._execute_step(step, plan, outputs)
                outputs[step.id] = result
            except Exception as e:
                error_msg = str(e)
                step.status = StepStatus.FAILED
                step.error = error_msg

                if self._on_step_failed:
                    self._on_step_failed(step, error_msg)

                if self._enable_replan and self._planner:
                    # 再計画を試行
                    plan = await self._planner.replan(plan, step, error_msg)
                    continue

                logger.error(f"Step {step.id} failed: {error_msg}")
                break

        execution_time = time.time() - start_time
        success = plan.is_complete() and not plan.has_failed()
        plan.status = PlanStatus.COMPLETED if success else PlanStatus.FAILED

        logger.info(f"Plan {plan.id} {'completed' if success else 'failed'} in {execution_time:.2f}s")

        return PlanResult(
            plan=plan,
            success=success,
            outputs=outputs,
            error=plan.steps[-1].error if plan.has_failed() else None,
            execution_time_seconds=execution_time,
        )

    async def _execute_step(
        self,
        step: Step,
        plan: Plan,
        previous_outputs: dict[str, Any],
    ) -> dict[str, Any]:
        """1ステップを実行.

        Args:
            step: 実行するステップ
            plan: 計画全体
            previous_outputs: 前のステップの出力

        Returns:
            ステップの出力
        """
        step.status = StepStatus.RUNNING
        step.started_at = datetime.now()

        if self._on_step_start:
            self._on_step_start(step)

        logger.info(f"Executing step: {step.id} - {step.name}")

        # 入力を準備（依存ステップの出力を含める）
        inputs = {**step.inputs}
        for dep_id in step.dependencies:
            if dep_id in previous_outputs:
                inputs[f"from_{dep_id}"] = previous_outputs[dep_id]

        # Agent を取得して実行
        agent = self._agents.get(step.agent) if step.agent else self._default_agent

        if agent:
            # Agent が run メソッドを持っているか確認
            if hasattr(agent, "run"):
                result = await agent.run(inputs)
            elif callable(agent):
                result = await agent(inputs) if callable(agent) else {"status": "no_agent"}
            else:
                result = {"status": "executed", "step": step.name}
        else:
            # Agent がない場合はパススルー
            logger.warning(f"No agent for step {step.id}, using passthrough")
            result = {"status": "passthrough", "inputs": inputs}

        step.status = StepStatus.COMPLETED
        step.completed_at = datetime.now()
        step.result = result

        if self._on_step_complete:
            self._on_step_complete(step)

        return result

    async def execute_step_by_id(self, plan: Plan, step_id: str) -> dict[str, Any]:
        """特定のステップのみを実行.

        Args:
            plan: 計画
            step_id: 実行するステップID

        Returns:
            ステップの出力
        """
        step = plan._get_step(step_id)
        if step is None:
            raise ValueError(f"Step not found: {step_id}")

        return await self._execute_step(step, plan, {})


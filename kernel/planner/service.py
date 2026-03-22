"""計画Agent - タスク分解と実行計画作成（Layer 3 Kernel）.

Manus分析に基づく計画駆動型実行の実装。
タスクを分解し、実行可能なステップに変換する。

設計原則:
- LLMによる知能的分解
- 依存関係の明示
- 動的再計画のサポート
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class StepType(str, Enum):
    """ステップ種別."""

    TOOL_CALL = "tool_call"
    LLM_GENERATION = "llm_generation"
    HUMAN_INPUT = "human_input"
    CONDITIONAL = "conditional"
    PARALLEL = "parallel"
    SEQUENTIAL = "sequential"
    SUB_PLAN = "sub_plan"


class StepStatus(str, Enum):
    """ステップ状態."""

    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"
    WAITING = "waiting"


class PlanStep(BaseModel):
    """計画ステップ.

    Attributes:
        id: ステップID
        name: ステップ名
        description: 説明
        step_type: ステップ種別
        order: 実行順序
        dependencies: 依存ステップID
        tool_uri: 使用するツールURI（tool_call時）
        params: パラメータ
        expected_output: 期待される出力形式
        timeout_seconds: タイムアウト
        retry_count: リトライ回数
        status: 状態
    """

    id: str = Field(default_factory=lambda: f"step-{uuid.uuid4().hex[:8]}")
    name: str = Field(default="", description="ステップ名")
    description: str = Field(default="", description="説明")
    step_type: StepType = Field(default=StepType.TOOL_CALL)
    order: int = Field(default=0, ge=0)
    dependencies: list[str] = Field(default_factory=list)
    tool_uri: str | None = Field(default=None)
    params: dict[str, Any] = Field(default_factory=dict)
    expected_output: dict[str, Any] = Field(default_factory=dict)
    timeout_seconds: float = Field(default=60.0, ge=1.0)
    retry_count: int = Field(default=2, ge=0)
    status: StepStatus = Field(default=StepStatus.PENDING)
    result: dict[str, Any] | None = Field(default=None)
    error: str | None = Field(default=None)
    sub_steps: list[PlanStep] = Field(default_factory=list)


class ExecutionPlan(BaseModel):
    """実行計画.

    Attributes:
        id: 計画ID
        name: 計画名
        description: 説明
        goal: 目標
        steps: ステップリスト
        total_steps: 総ステップ数
        estimated_duration: 推定所要時間
        created_at: 作成時刻
        context: コンテキスト情報
    """

    id: str = Field(default_factory=lambda: f"plan-{uuid.uuid4().hex[:8]}")
    name: str = Field(default="", description="計画名")
    description: str = Field(default="", description="説明")
    goal: str = Field(default="", description="目標")
    steps: list[PlanStep] = Field(default_factory=list)
    total_steps: int = Field(default=0)
    estimated_duration: float = Field(default=0.0)
    created_at: datetime = Field(default_factory=datetime.now)
    context: dict[str, Any] = Field(default_factory=dict)

    def get_ready_steps(self) -> list[PlanStep]:
        """実行準備完了のステップを取得（依存関係が全て解決済み）."""
        completed_ids = {step.id for step in self.steps if step.status == StepStatus.COMPLETED}
        return [
            step
            for step in self.steps
            if step.status == StepStatus.PENDING and all(dep in completed_ids for dep in step.dependencies)
        ]

    def get_step(self, step_id: str) -> PlanStep | None:
        """ステップを取得."""
        for step in self.steps:
            if step.id == step_id:
                return step
        return None

    def get_progress(self) -> float:
        """進捗率を取得（0.0-1.0）."""
        if not self.steps:
            return 0.0
        completed = sum(1 for step in self.steps if step.status == StepStatus.COMPLETED)
        return completed / len(self.steps)


@dataclass
class PlannerConfig:
    """計画Agent設定.

    Attributes:
        max_steps: 最大ステップ数
        default_timeout: デフォルトタイムアウト
        enable_parallel: 並列実行を有効化
        max_parallel: 最大並列数
    """

    max_steps: int = 20
    default_timeout: float = 60.0
    enable_parallel: bool = True
    max_parallel: int = 5


class PlannerAgent:
    """計画Agent - タスクを分析し、実行可能な計画に分解する.

    主な機能:
    - LLMによるタスク分解
    - 依存関係の自動推論
    - 並列実行可能なステップの識別
    - 動的再計画
    """

    def __init__(
        self,
        llm_client: Any = None,
        config: PlannerConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            config: 設定
        """
        self._llm = llm_client
        self._config = config or PlannerConfig()
        self._logger = logging.getLogger(__name__)

    async def create_plan(
        self,
        goal: str,
        context: dict[str, Any] | None = None,
        available_tools: list[str] | None = None,
    ) -> ExecutionPlan:
        """実行計画を作成.

        Args:
            goal: 目標
            context: コンテキスト情報
            available_tools: 利用可能なツール

        Returns:
            ExecutionPlan
        """
        context = context or {}
        available_tools = available_tools or []

        self._logger.info("計画作成開始: %s", goal)

        if self._llm:
            plan = await self._create_plan_with_llm(goal, context, available_tools)
        else:
            plan = self._create_simple_plan(goal, context)

        plan.total_steps = len(plan.steps)
        plan.estimated_duration = sum(s.timeout_seconds for s in plan.steps)

        self._logger.info(
            "計画作成完了: %dステップ, 推定%.0f秒",
            plan.total_steps,
            plan.estimated_duration,
        )

        return plan

    async def _create_plan_with_llm(
        self,
        goal: str,
        context: dict[str, Any],
        available_tools: list[str],
    ) -> ExecutionPlan:
        """LLMで計画を作成."""
        tools_desc = "\n".join(f"- {tool}" for tool in available_tools) if available_tools else "なし"

        prompt = (
            "以下の目標を達成するための実行計画を作成してください。\n\n"
            f"目標: {goal}\n\nコンテキスト: {context}\n\n"
            f"利用可能なツール:\n{tools_desc}\n\n"
            f"要件:\n- 各ステップは具体的で実行可能であること\n"
            f"- ステップ間の依存関係を明示すること\n"
            f"- 並列実行可能なステップを識別すること\n"
            f"- 最大{self._config.max_steps}ステップ以内\n\n"
            "JSON形式で回答:\n"
            '{\n  "name": "計画名",\n  "description": "計画の説明",\n'
            '  "steps": [\n    {\n      "name": "ステップ名",\n'
            '      "description": "説明",\n'
            '      "step_type": "tool_call|llm_generation|human_input",\n'
            '      "tool_uri": "使用するツールURI",\n'
            '      "dependencies": ["依存するステップ名"],\n'
            '      "params": {},\n      "timeout_seconds": 60\n    }\n  ]\n}'
        )

        try:
            response = await self._llm.generate(prompt)
            content = response.get("content", str(response))

            import json

            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            elif "```" in content:
                content = content.split("```")[1].split("```")[0]

            data = json.loads(content)

            steps: list[PlanStep] = []
            name_to_id: dict[str, str] = {}

            for i, step_data in enumerate(data.get("steps", [])):
                step = PlanStep(
                    name=step_data.get("name", f"ステップ{i + 1}"),
                    description=step_data.get("description", ""),
                    step_type=StepType(step_data.get("step_type", "tool_call")),
                    order=i,
                    tool_uri=step_data.get("tool_uri"),
                    params=step_data.get("params", {}),
                    timeout_seconds=step_data.get("timeout_seconds", self._config.default_timeout),
                )
                name_to_id[step.name] = step.id
                steps.append(step)

            for i, step_data in enumerate(data.get("steps", [])):
                dep_names = step_data.get("dependencies", [])
                steps[i].dependencies = [name_to_id[name] for name in dep_names if name in name_to_id]

            return ExecutionPlan(
                name=data.get("name", "自動生成計画"),
                description=data.get("description", ""),
                goal=goal,
                steps=steps,
                context=context,
            )

        except Exception as e:
            self._logger.warning("LLM計画作成失敗: %s", e)
            return self._create_simple_plan(goal, context)

    def _create_simple_plan(
        self,
        goal: str,
        context: dict[str, Any],
    ) -> ExecutionPlan:
        """シンプルな計画を作成（LLMなし）."""
        steps = [
            PlanStep(
                name="分析",
                description=f"目標を分析: {goal}",
                step_type=StepType.LLM_GENERATION,
                order=0,
            ),
            PlanStep(
                name="実行",
                description="分析結果に基づいて実行",
                step_type=StepType.TOOL_CALL,
                order=1,
                dependencies=[],
            ),
            PlanStep(
                name="検証",
                description="結果を検証",
                step_type=StepType.LLM_GENERATION,
                order=2,
            ),
        ]

        steps[1].dependencies = [steps[0].id]
        steps[2].dependencies = [steps[1].id]

        return ExecutionPlan(
            name="シンプル計画",
            description=f"目標: {goal}",
            goal=goal,
            steps=steps,
            context=context,
        )

    async def replan(
        self,
        plan: ExecutionPlan,
        failed_step: PlanStep,
        error: str,
    ) -> ExecutionPlan:
        """失敗したステップを再計画.

        Args:
            plan: 元の計画
            failed_step: 失敗したステップ
            error: エラー内容

        Returns:
            更新された計画
        """
        self._logger.info("再計画開始: %s (%s)", failed_step.name, error)

        if not self._llm:
            retry_step = PlanStep(
                name=f"{failed_step.name}_リトライ",
                description=f"リトライ: {failed_step.description}",
                step_type=failed_step.step_type,
                order=failed_step.order,
                tool_uri=failed_step.tool_uri,
                params=failed_step.params,
                dependencies=failed_step.dependencies,
            )
            plan.steps.append(retry_step)
            plan.total_steps = len(plan.steps)
            return plan

        prompt = (
            "以下のステップが失敗しました。代替案を提案してください。\n\n"
            f"失敗ステップ: {failed_step.name}\n"
            f"説明: {failed_step.description}\nエラー: {error}\n\n"
            f"元の目標: {plan.goal}\n\n"
            "JSON形式で代替ステップを提案:\n"
            '{\n  "alternative_steps": [\n    {\n'
            '      "name": "ステップ名",\n      "description": "説明",\n'
            '      "step_type": "tool_call|llm_generation",\n'
            '      "tool_uri": "ツールURI",\n      "params": {}\n    }\n  ]\n}'
        )

        try:
            response = await self._llm.generate(prompt)
            content = response.get("content", str(response))

            import json

            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            elif "```" in content:
                content = content.split("```")[1].split("```")[0]

            data = json.loads(content)

            for i, alt_data in enumerate(data.get("alternative_steps", [])):
                alt_step = PlanStep(
                    name=alt_data.get("name", f"代替{i + 1}"),
                    description=alt_data.get("description", ""),
                    step_type=StepType(alt_data.get("step_type", "tool_call")),
                    order=len(plan.steps),
                    tool_uri=alt_data.get("tool_uri"),
                    params=alt_data.get("params", {}),
                    dependencies=failed_step.dependencies,
                )
                plan.steps.append(alt_step)

            plan.total_steps = len(plan.steps)
            self._logger.info(
                "再計画完了: %d個の代替ステップ追加",
                len(data.get("alternative_steps", [])),
            )

        except Exception as e:
            self._logger.warning("LLM再計画失敗: %s", e)

        return plan


__all__ = [
    "ExecutionPlan",
    "PlanStep",
    "PlannerAgent",
    "PlannerConfig",
    "StepStatus",
    "StepType",
]

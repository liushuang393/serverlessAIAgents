"""階層的計画 - 目標をサブ目標→タスクに分解.

長期計画と動的再計画をサポートする階層的プランナー。

設計原則:
- 目標を階層的に分解（Goal → SubGoal → Task）
- 依存関係と並列実行可能性を考慮
- 失敗時の部分再計画をサポート

使用例:
    >>> planner = HierarchicalPlanner(llm_client=my_llm)
    >>> plan = await planner.create_plan(
    ...     goal="競合分析レポートを作成",
    ...     context={"competitors": ["A社", "B社"]},
    ... )
    >>> for level in plan.levels:
    ...     print(f"Level {level.depth}: {[g.name for g in level.goals]}")
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class GoalStatus(str, Enum):
    """目標状態."""

    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    BLOCKED = "blocked"
    REPLANNING = "replanning"


class SubGoal(BaseModel):
    """サブ目標.

    Attributes:
        id: 目標ID
        name: 目標名
        description: 説明
        parent_id: 親目標ID
        dependencies: 依存する目標ID
        estimated_effort: 推定工数（0-100）
        priority: 優先度（1=最高）
        status: 状態
        result: 実行結果
        error: エラー情報
    """

    id: str = Field(default_factory=lambda: f"goal-{uuid.uuid4().hex[:8]}")
    name: str = Field(..., description="目標名")
    description: str = Field(default="", description="説明")
    parent_id: str | None = Field(default=None, description="親目標ID")
    dependencies: list[str] = Field(default_factory=list, description="依存目標ID")
    estimated_effort: int = Field(default=50, ge=0, le=100, description="推定工数")
    priority: int = Field(default=1, ge=1, description="優先度")
    status: GoalStatus = Field(default=GoalStatus.PENDING)
    assigned_agent: str | None = Field(default=None, description="担当Agent")
    result: dict[str, Any] | None = Field(default=None)
    error: str | None = Field(default=None)
    created_at: datetime = Field(default_factory=datetime.now)
    completed_at: datetime | None = Field(default=None)


class PlanLevel(BaseModel):
    """計画の階層レベル.

    Attributes:
        depth: 深さ（0=ルート）
        goals: このレベルの目標リスト
    """

    depth: int = Field(default=0, ge=0)
    goals: list[SubGoal] = Field(default_factory=list)


class HierarchicalPlan(BaseModel):
    """階層的計画.

    Attributes:
        id: 計画ID
        root_goal: ルート目標（最終目標）
        levels: 階層レベルリスト
        total_effort: 総推定工数
        created_at: 作成日時
        status: 計画全体の状態
    """

    id: str = Field(default_factory=lambda: f"plan-{uuid.uuid4().hex[:8]}")
    root_goal: str = Field(..., description="ルート目標")
    levels: list[PlanLevel] = Field(default_factory=list)
    total_effort: int = Field(default=0)
    created_at: datetime = Field(default_factory=datetime.now)
    status: GoalStatus = Field(default=GoalStatus.PENDING)
    context: dict[str, Any] = Field(default_factory=dict)
    replan_count: int = Field(default=0, description="再計画回数")

    def get_ready_goals(self) -> list[SubGoal]:
        """実行準備完了の目標を取得."""
        ready = []
        completed_ids = self._get_completed_goal_ids()

        for level in self.levels:
            for goal in level.goals:
                if goal.status == GoalStatus.PENDING:
                    # 依存が全て完了しているか
                    if all(dep in completed_ids for dep in goal.dependencies):
                        ready.append(goal)
        return ready

    def _get_completed_goal_ids(self) -> set[str]:
        """完了した目標IDを取得."""
        completed = set()
        for level in self.levels:
            for goal in level.goals:
                if goal.status == GoalStatus.COMPLETED:
                    completed.add(goal.id)
        return completed

    def get_goal(self, goal_id: str) -> SubGoal | None:
        """目標を取得."""
        for level in self.levels:
            for goal in level.goals:
                if goal.id == goal_id:
                    return goal
        return None

    def update_goal_status(
        self,
        goal_id: str,
        status: GoalStatus,
        result: dict[str, Any] | None = None,
        error: str | None = None,
    ) -> bool:
        """目標の状態を更新."""
        goal = self.get_goal(goal_id)
        if not goal:
            return False

        goal.status = status
        if result:
            goal.result = result
        if error:
            goal.error = error
        if status == GoalStatus.COMPLETED:
            goal.completed_at = datetime.now()

        return True

    def get_progress(self) -> float:
        """進捗率を取得（0.0-1.0）."""
        total = sum(len(level.goals) for level in self.levels)
        if total == 0:
            return 0.0
        completed = sum(
            1 for level in self.levels
            for goal in level.goals
            if goal.status == GoalStatus.COMPLETED
        )
        return completed / total


@dataclass
class HierarchicalPlanner:
    """階層的プランナー.

    目標を階層的に分解し、実行可能な計画を作成する。

    主な機能:
    - LLMによる目標分解
    - 依存関係の自動推論
    - 失敗時の部分再計画
    """

    llm_client: Any = None
    max_depth: int = 3
    max_goals_per_level: int = 10
    _logger: logging.Logger = field(
        default_factory=lambda: logging.getLogger("agentflow.pev.planner")
    )

    async def create_plan(
        self,
        goal: str,
        context: dict[str, Any] | None = None,
        available_agents: list[str] | None = None,
    ) -> HierarchicalPlan:
        """階層的計画を作成.

        Args:
            goal: 達成すべき目標
            context: コンテキスト情報
            available_agents: 利用可能なAgent

        Returns:
            HierarchicalPlan
        """
        context = context or {}
        available_agents = available_agents or []

        self._logger.info(f"計画作成開始: {goal}")

        plan = HierarchicalPlan(root_goal=goal, context=context)

        if self.llm_client:
            await self._decompose_with_llm(plan, goal, context, available_agents)
        else:
            self._create_simple_plan(plan, goal)

        # 総工数を計算
        plan.total_effort = sum(
            goal.estimated_effort
            for level in plan.levels
            for goal in level.goals
        )

        self._logger.info(
            f"計画作成完了: {sum(len(l.goals) for l in plan.levels)}目標, "
            f"推定工数{plan.total_effort}"
        )

        return plan

    async def _decompose_with_llm(
        self,
        plan: HierarchicalPlan,
        goal: str,
        context: dict[str, Any],
        available_agents: list[str],
    ) -> None:
        """LLMで目標を分解."""
        agents_desc = ", ".join(available_agents) if available_agents else "汎用Agent"

        prompt = f"""以下の目標を階層的に分解してください。

目標: {goal}

コンテキスト: {context}

利用可能なAgent: {agents_desc}

要件:
- 最大{self.max_depth}階層まで分解
- 各階層で最大{self.max_goals_per_level}個のサブ目標
- 依存関係を明示
- 並列実行可能なものを識別

JSON形式で回答:
{{
    "levels": [
        {{
            "depth": 0,
            "goals": [
                {{
                    "name": "サブ目標名",
                    "description": "説明",
                    "dependencies": ["依存するサブ目標名"],
                    "estimated_effort": 50,
                    "priority": 1,
                    "assigned_agent": "担当Agent名"
                }}
            ]
        }}
    ]
}}"""

        try:
            response = await self.llm_client.generate(prompt)
            content = response.get("content", str(response))

            import json
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            elif "```" in content:
                content = content.split("```")[1].split("```")[0]

            data = json.loads(content)
            self._parse_llm_response(plan, data)

        except Exception as e:
            self._logger.warning(f"LLM分解失敗: {e}")
            self._create_simple_plan(plan, goal)

    def _parse_llm_response(
        self,
        plan: HierarchicalPlan,
        data: dict[str, Any],
    ) -> None:
        """LLMレスポンスをパース."""
        name_to_id: dict[str, str] = {}

        for level_data in data.get("levels", []):
            level = PlanLevel(depth=level_data.get("depth", 0))

            for goal_data in level_data.get("goals", []):
                subgoal = SubGoal(
                    name=goal_data.get("name", ""),
                    description=goal_data.get("description", ""),
                    estimated_effort=goal_data.get("estimated_effort", 50),
                    priority=goal_data.get("priority", 1),
                    assigned_agent=goal_data.get("assigned_agent"),
                )
                name_to_id[subgoal.name] = subgoal.id
                level.goals.append(subgoal)

            plan.levels.append(level)

        # 依存関係を名前からIDに変換
        for level_data in data.get("levels", []):
            for i, goal_data in enumerate(level_data.get("goals", [])):
                dep_names = goal_data.get("dependencies", [])
                level_idx = level_data.get("depth", 0)
                if level_idx < len(plan.levels) and i < len(plan.levels[level_idx].goals):
                    plan.levels[level_idx].goals[i].dependencies = [
                        name_to_id[name] for name in dep_names if name in name_to_id
                    ]

    def _create_simple_plan(self, plan: HierarchicalPlan, goal: str) -> None:
        """シンプルな計画を作成（LLMなし）."""
        level = PlanLevel(depth=0)
        level.goals = [
            SubGoal(name="分析", description=f"目標を分析: {goal}", estimated_effort=30),
            SubGoal(name="実行", description="分析結果に基づいて実行", estimated_effort=50),
            SubGoal(name="検証", description="結果を検証", estimated_effort=20),
        ]
        # 依存関係を設定
        level.goals[1].dependencies = [level.goals[0].id]
        level.goals[2].dependencies = [level.goals[1].id]

        plan.levels.append(level)

    async def replan(
        self,
        plan: HierarchicalPlan,
        failed_goal: SubGoal,
        error: str,
        context: dict[str, Any] | None = None,
    ) -> HierarchicalPlan:
        """失敗した目標から再計画.

        Args:
            plan: 元の計画
            failed_goal: 失敗した目標
            error: エラー内容
            context: 追加コンテキスト

        Returns:
            更新された計画
        """
        self._logger.info(f"再計画開始: {failed_goal.name} ({error})")
        plan.replan_count += 1

        if not self.llm_client:
            # LLMなしの場合はリトライ目標を追加
            retry_goal = SubGoal(
                name=f"{failed_goal.name}_リトライ",
                description=f"リトライ: {failed_goal.description}",
                dependencies=failed_goal.dependencies,
                estimated_effort=failed_goal.estimated_effort,
            )

            # 最後のレベルに追加
            if plan.levels:
                plan.levels[-1].goals.append(retry_goal)

            return plan

        # LLMで代替計画を生成
        prompt = f"""以下の目標が失敗しました。代替案を提案してください。

失敗した目標: {failed_goal.name}
説明: {failed_goal.description}
エラー: {error}

元の計画の進捗: {plan.get_progress() * 100:.0f}%

JSON形式で代替目標を提案:
{{
    "alternative_goals": [
        {{
            "name": "代替目標名",
            "description": "説明",
            "estimated_effort": 50
        }}
    ]
}}"""

        try:
            response = await self.llm_client.generate(prompt)
            content = response.get("content", str(response))

            import json
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            elif "```" in content:
                content = content.split("```")[1].split("```")[0]

            data = json.loads(content)

            for alt_data in data.get("alternative_goals", []):
                alt_goal = SubGoal(
                    name=alt_data.get("name", "代替"),
                    description=alt_data.get("description", ""),
                    estimated_effort=alt_data.get("estimated_effort", 50),
                    dependencies=failed_goal.dependencies,
                )
                if plan.levels:
                    plan.levels[-1].goals.append(alt_goal)

        except Exception as e:
            self._logger.warning(f"LLM再計画失敗: {e}")

        return plan


__all__ = [
    "GoalStatus",
    "HierarchicalPlan",
    "HierarchicalPlanner",
    "PlanLevel",
    "SubGoal",
]


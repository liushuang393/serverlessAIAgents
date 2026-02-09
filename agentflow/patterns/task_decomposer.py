"""TaskDecomposer - 高度なタスク分解システム.

Manus分析に基づく「递归分解+依赖图+动态调度」の実装：

1. 递归分解（Recursive Decomposition）
   - 複雑なタスクを再帰的に分解
   - 各サブタスクが十分に小さくなるまで分解
   - 分解の深さ制限と粒度制御

2. 依赖图（Dependency Graph）
   - タスク間の依存関係を明示的に管理
   - 循環依存の検出と解決
   - 並行実行可能なタスクの識別

3. 动态调度（Dynamic Scheduling）
   - 実行時の状況に応じた動的なタスク調整
   - 失敗時の再計画
   - リソース制約の考慮

使用例:
    >>> from agentflow.patterns.task_decomposer import TaskDecomposer
    >>>
    >>> decomposer = TaskDecomposer(llm_client=my_llm)
    >>> plan = await decomposer.decompose("新規事業の投資判断を行いたい")
    >>>
    >>> # 実行
    >>> async for step in decomposer.execute_plan(plan):
    ...     print(f"完了: {step.task}")
"""

from __future__ import annotations

import asyncio
import logging
import uuid
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from collections.abc import AsyncIterator, Callable


logger = logging.getLogger(__name__)


class TaskGranularity(str, Enum):
    """タスク粒度."""

    COARSE = "coarse"      # 粗い（さらに分解可能）
    MEDIUM = "medium"      # 中程度
    FINE = "fine"          # 細かい（これ以上分解不要）
    ATOMIC = "atomic"      # 原子的（分解不可）


class TaskPriority(str, Enum):
    """タスク優先度."""

    CRITICAL = "critical"  # 最優先
    HIGH = "high"          # 高
    MEDIUM = "medium"      # 中
    LOW = "low"            # 低
    BACKGROUND = "background"  # バックグラウンド


class DecomposedTask(BaseModel):
    """分解されたタスク.

    Attributes:
        id: タスクID
        name: タスク名
        description: 詳細説明
        granularity: 粒度
        priority: 優先度
        dependencies: 依存タスクID
        subtasks: サブタスクID
        parent_id: 親タスクID
        estimated_duration: 推定所要時間（秒）
        required_tools: 必要なツール
        required_skills: 必要なスキル
        metadata: メタデータ
    """

    id: str = Field(default_factory=lambda: f"task-{uuid.uuid4().hex[:8]}")
    name: str = Field(default="", description="タスク名")
    description: str = Field(default="", description="詳細説明")
    granularity: TaskGranularity = Field(default=TaskGranularity.MEDIUM)
    priority: TaskPriority = Field(default=TaskPriority.MEDIUM)
    dependencies: list[str] = Field(default_factory=list)
    subtasks: list[str] = Field(default_factory=list)
    parent_id: str | None = Field(default=None)
    estimated_duration: float = Field(default=60.0, ge=0)
    required_tools: list[str] = Field(default_factory=list)
    required_skills: list[str] = Field(default_factory=list)
    metadata: dict[str, Any] = Field(default_factory=dict)
    created_at: datetime = Field(default_factory=datetime.now)

    # 実行状態
    status: str = Field(default="pending")
    result: dict[str, Any] | None = Field(default=None)
    error: str | None = Field(default=None)
    started_at: datetime | None = Field(default=None)
    completed_at: datetime | None = Field(default=None)


class DecompositionPlan(BaseModel):
    """分解計画.

    Attributes:
        id: 計画ID
        root_task_id: ルートタスクID
        tasks: 全タスク（ID -> Task）
        execution_order: 実行順序
        parallel_groups: 並行実行グループ
        total_estimated_duration: 総推定時間
    """

    id: str = Field(default_factory=lambda: f"plan-{uuid.uuid4().hex[:8]}")
    root_task_id: str = Field(default="")
    tasks: dict[str, DecomposedTask] = Field(default_factory=dict)
    execution_order: list[str] = Field(default_factory=list)
    parallel_groups: list[list[str]] = Field(default_factory=list)
    total_estimated_duration: float = Field(default=0.0)
    created_at: datetime = Field(default_factory=datetime.now)

    def get_task(self, task_id: str) -> DecomposedTask | None:
        """タスクを取得."""
        return self.tasks.get(task_id)

    def get_ready_tasks(self) -> list[DecomposedTask]:
        """実行準備完了のタスクを取得."""
        completed_ids = {
            tid for tid, task in self.tasks.items()
            if task.status == "completed"
        }
        return [
            task for task in self.tasks.values()
            if task.status == "pending"
            and all(dep in completed_ids for dep in task.dependencies)
        ]


@dataclass
class DecompositionConfig:
    """分解設定.

    Attributes:
        max_depth: 最大分解深度
        min_granularity: 最小粒度
        max_subtasks: 最大サブタスク数
        parallel_threshold: 並行実行閾値
    """

    max_depth: int = 5
    min_granularity: TaskGranularity = TaskGranularity.FINE
    max_subtasks: int = 10
    parallel_threshold: int = 3  # これ以上のタスクは並行実行を検討


class DependencyGraph:
    """依存関係グラフ.

    タスク間の依存関係を管理し、実行順序を決定する。

    機能:
    - 依存関係の追加・削除
    - 循環依存の検出
    - トポロジカルソート
    - 並行実行グループの計算
    """

    def __init__(self) -> None:
        """初期化."""
        self._graph: dict[str, set[str]] = {}  # task_id -> 依存先IDs
        self._reverse: dict[str, set[str]] = {}  # task_id -> 依存元IDs
        self._logger = logging.getLogger(__name__)

    def add_task(self, task_id: str, dependencies: list[str] | None = None) -> None:
        """タスクを追加.

        Args:
            task_id: タスクID
            dependencies: 依存タスクID
        """
        if task_id not in self._graph:
            self._graph[task_id] = set()
            self._reverse[task_id] = set()

        for dep in dependencies or []:
            self.add_dependency(task_id, dep)

    def add_dependency(self, task_id: str, depends_on: str) -> bool:
        """依存関係を追加.

        Args:
            task_id: タスクID
            depends_on: 依存先タスクID

        Returns:
            追加成功したか（循環依存の場合はFalse）
        """
        # 循環依存チェック
        if self._would_create_cycle(task_id, depends_on):
            self._logger.warning(
                f"循環依存を検出: {task_id} -> {depends_on}"
            )
            return False

        if task_id not in self._graph:
            self._graph[task_id] = set()
            self._reverse[task_id] = set()
        if depends_on not in self._graph:
            self._graph[depends_on] = set()
            self._reverse[depends_on] = set()

        self._graph[task_id].add(depends_on)
        self._reverse[depends_on].add(task_id)
        return True

    def _would_create_cycle(self, task_id: str, depends_on: str) -> bool:
        """循環依存が発生するかチェック.

        depends_on から task_id に到達可能なら循環。
        """
        if task_id == depends_on:
            return True

        visited: set[str] = set()
        stack = [depends_on]

        while stack:
            current = stack.pop()
            if current == task_id:
                return True
            if current in visited:
                continue
            visited.add(current)
            stack.extend(self._graph.get(current, set()))

        return False

    def get_execution_order(self) -> list[str]:
        """トポロジカルソートで実行順序を取得.

        Returns:
            タスクIDの実行順序
        """
        from graphlib import CycleError, TopologicalSorter

        try:
            sorter = TopologicalSorter(self._graph)
            return list(sorter.static_order())
        except CycleError:
            self._logger.exception("循環依存のため実行順序を決定できません")
            return []

    def get_parallel_groups(self) -> list[list[str]]:
        """並行実行可能なグループを取得.

        Returns:
            並行実行可能なタスクIDのグループリスト
        """
        groups: list[list[str]] = []
        remaining = set(self._graph.keys())
        completed: set[str] = set()

        while remaining:
            # 依存関係が全て解決済みのタスク
            ready = [
                tid for tid in remaining
                if self._graph[tid].issubset(completed)
            ]
            if not ready:
                break

            groups.append(ready)
            completed.update(ready)
            remaining -= set(ready)

        return groups

    def get_dependencies(self, task_id: str) -> set[str]:
        """タスクの依存先を取得."""
        return self._graph.get(task_id, set())

    def get_dependents(self, task_id: str) -> set[str]:
        """タスクに依存しているタスクを取得."""
        return self._reverse.get(task_id, set())


class TaskDecomposer:
    """高度なタスク分解システム.

    Manus分析に基づく「递归分解+依赖图+动态调度」を実装。

    主な機能:
    - 複雑なタスクの再帰的分解
    - 依存関係グラフの構築
    - 動的な実行スケジューリング
    - 失敗時の再計画

    Example:
        >>> decomposer = TaskDecomposer(llm_client=my_llm)
        >>> plan = await decomposer.decompose("新規事業の投資判断")
        >>>
        >>> async for step in decomposer.execute_plan(plan):
        ...     print(f"完了: {step.name}")
    """

    def __init__(
        self,
        llm_client: Any = None,
        config: DecompositionConfig | None = None,
        task_executor: Callable[[DecomposedTask], Any] | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（分解に使用）
            config: 分解設定
            task_executor: タスク実行関数
        """
        self._llm = llm_client
        self._config = config or DecompositionConfig()
        self._executor = task_executor
        self._logger = logging.getLogger(__name__)

    async def decompose(
        self,
        task_description: str,
        context: dict[str, Any] | None = None,
    ) -> DecompositionPlan:
        """タスクを分解して計画を作成.

        Args:
            task_description: タスクの説明
            context: コンテキスト情報

        Returns:
            DecompositionPlan
        """
        context = context or {}

        # ルートタスクを作成
        root_task = DecomposedTask(
            name="Root",
            description=task_description,
            granularity=TaskGranularity.COARSE,
        )

        # 依存関係グラフを初期化
        dep_graph = DependencyGraph()

        # 再帰的に分解
        all_tasks: dict[str, DecomposedTask] = {root_task.id: root_task}
        await self._recursive_decompose(
            root_task,
            all_tasks,
            dep_graph,
            context,
            depth=0,
        )

        # 実行順序を計算
        execution_order = dep_graph.get_execution_order()
        parallel_groups = dep_graph.get_parallel_groups()

        # 総推定時間を計算
        total_duration = sum(
            task.estimated_duration
            for task in all_tasks.values()
            if task.granularity in (TaskGranularity.FINE, TaskGranularity.ATOMIC)
        )

        return DecompositionPlan(
            root_task_id=root_task.id,
            tasks=all_tasks,
            execution_order=execution_order,
            parallel_groups=parallel_groups,
            total_estimated_duration=total_duration,
        )

    async def _recursive_decompose(
        self,
        task: DecomposedTask,
        all_tasks: dict[str, DecomposedTask],
        dep_graph: DependencyGraph,
        context: dict[str, Any],
        depth: int,
    ) -> None:
        """再帰的にタスクを分解.

        Args:
            task: 分解対象タスク
            all_tasks: 全タスク辞書
            dep_graph: 依存関係グラフ
            context: コンテキスト
            depth: 現在の深度
        """
        # 深度制限チェック
        if depth >= self._config.max_depth:
            task.granularity = TaskGranularity.FINE
            dep_graph.add_task(task.id)
            return

        # 粒度チェック
        if task.granularity in (TaskGranularity.FINE, TaskGranularity.ATOMIC):
            dep_graph.add_task(task.id)
            return

        # LLMで分解
        subtasks = await self._decompose_with_llm(task, context)

        if not subtasks:
            # 分解できない場合は細粒度として扱う
            task.granularity = TaskGranularity.FINE
            dep_graph.add_task(task.id)
            return

        # サブタスクを追加
        for subtask in subtasks:
            subtask.parent_id = task.id
            all_tasks[subtask.id] = subtask
            task.subtasks.append(subtask.id)

            # 依存関係を設定
            dep_graph.add_task(subtask.id, subtask.dependencies)

            # 再帰的に分解
            await self._recursive_decompose(
                subtask,
                all_tasks,
                dep_graph,
                context,
                depth + 1,
            )

    async def _decompose_with_llm(
        self,
        task: DecomposedTask,
        context: dict[str, Any],
    ) -> list[DecomposedTask]:
        """LLMを使ってタスクを分解.

        Args:
            task: 分解対象タスク
            context: コンテキスト

        Returns:
            サブタスクリスト
        """
        if not self._llm:
            # LLMなしの場合は簡易分解
            return self._simple_decompose(task)

        prompt = f"""以下のタスクを実行可能なサブタスクに分解してください。

タスク: {task.description}

要件:
- 各サブタスクは具体的で実行可能であること
- サブタスク間の依存関係を明示すること
- 最大{self._config.max_subtasks}個のサブタスクに分解

JSON形式で回答:
{{
  "subtasks": [
    {{
      "name": "サブタスク名",
      "description": "詳細説明",
      "dependencies": ["依存するサブタスク名"],
      "estimated_duration": 60,
      "required_tools": ["ツール名"],
      "required_skills": ["スキル名"]
    }}
  ]
}}"""

        try:
            response = await self._llm.generate(prompt)
            # JSONパース（実際の実装ではより堅牢なパースが必要）
            import json
            content = response.get("content", str(response))
            # JSON部分を抽出
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            elif "```" in content:
                content = content.split("```")[1].split("```")[0]

            data = json.loads(content)
            subtasks = []
            name_to_id: dict[str, str] = {}

            for item in data.get("subtasks", []):
                subtask = DecomposedTask(
                    name=item.get("name", ""),
                    description=item.get("description", ""),
                    estimated_duration=item.get("estimated_duration", 60),
                    required_tools=item.get("required_tools", []),
                    required_skills=item.get("required_skills", []),
                    granularity=TaskGranularity.MEDIUM,
                )
                name_to_id[subtask.name] = subtask.id
                subtasks.append(subtask)

            # 依存関係を名前からIDに変換
            for i, item in enumerate(data.get("subtasks", [])):
                dep_names = item.get("dependencies", [])
                subtasks[i].dependencies = [
                    name_to_id[name]
                    for name in dep_names
                    if name in name_to_id
                ]

            return subtasks

        except Exception as e:
            self._logger.warning(f"LLM分解失敗: {e}")
            return self._simple_decompose(task)

    def _simple_decompose(self, task: DecomposedTask) -> list[DecomposedTask]:
        """簡易分解（LLMなし）.

        Args:
            task: 分解対象タスク

        Returns:
            サブタスクリスト
        """
        # 基本的な分解パターン
        patterns = [
            ("調査", "関連情報を収集・整理する"),
            ("分析", "収集した情報を分析する"),
            ("計画", "分析結果に基づいて計画を立てる"),
            ("実行", "計画を実行する"),
            ("検証", "結果を検証する"),
        ]

        subtasks = []
        prev_id: str | None = None

        for name, desc in patterns:
            subtask = DecomposedTask(
                name=f"{task.name}_{name}",
                description=f"{task.description}の{desc}",
                granularity=TaskGranularity.FINE,
                dependencies=[prev_id] if prev_id else [],
            )
            subtasks.append(subtask)
            prev_id = subtask.id

        return subtasks

    async def execute_plan(
        self,
        plan: DecompositionPlan,
        max_parallel: int = 3,
    ) -> AsyncIterator[DecomposedTask]:
        """計画を実行.

        Args:
            plan: 実行する計画
            max_parallel: 最大並行実行数

        Yields:
            完了したタスク
        """
        for group in plan.parallel_groups:
            # グループ内のタスクを並行実行
            tasks_to_run = [
                plan.tasks[tid]
                for tid in group
                if plan.tasks[tid].status == "pending"
            ]

            if not tasks_to_run:
                continue

            # 並行実行数を制限
            for i in range(0, len(tasks_to_run), max_parallel):
                batch = tasks_to_run[i:i + max_parallel]
                results = await asyncio.gather(
                    *[self._execute_task(task) for task in batch],
                    return_exceptions=True,
                )

                for task, result in zip(batch, results, strict=False):
                    if isinstance(result, Exception):
                        task.status = "failed"
                        task.error = str(result)
                    else:
                        task.status = "completed"
                        task.result = result
                        task.completed_at = datetime.now()

                    yield task

    async def _execute_task(self, task: DecomposedTask) -> dict[str, Any]:
        """タスクを実行.

        Args:
            task: 実行するタスク

        Returns:
            実行結果
        """
        task.status = "running"
        task.started_at = datetime.now()

        if self._executor:
            return await self._executor(task)

        # デフォルト実行（シミュレーション）
        await asyncio.sleep(0.1)  # シミュレーション
        return {"status": "completed", "task_id": task.id}

    async def replan(
        self,
        plan: DecompositionPlan,
        failed_task_id: str,
        context: dict[str, Any] | None = None,
    ) -> DecompositionPlan:
        """失敗したタスクを再計画.

        Args:
            plan: 元の計画
            failed_task_id: 失敗したタスクID
            context: コンテキスト

        Returns:
            更新された計画
        """
        failed_task = plan.tasks.get(failed_task_id)
        if not failed_task:
            return plan

        # 失敗したタスクを再分解
        new_subtasks = await self._decompose_with_llm(
            failed_task,
            context or {},
        )

        if new_subtasks:
            # 新しいサブタスクを追加
            for subtask in new_subtasks:
                subtask.parent_id = failed_task_id
                plan.tasks[subtask.id] = subtask
                failed_task.subtasks.append(subtask.id)

            # 実行順序を再計算
            dep_graph = DependencyGraph()
            for task in plan.tasks.values():
                dep_graph.add_task(task.id, task.dependencies)

            plan.execution_order = dep_graph.get_execution_order()
            plan.parallel_groups = dep_graph.get_parallel_groups()

        return plan


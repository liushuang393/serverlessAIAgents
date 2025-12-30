"""Supervisor 協調パターン - 監督者による動的タスク割り当て.

このモジュールは Supervisor パターンを実装します：
- 監督者が次のワーカーを動的に選択
- ワーカーの実行結果を評価
- 完了条件まで反復

設計原則：
- LangGraph の supervisor パターンを参考
- 簡単：最小限の設定で動作
- 柔軟：カスタム決定ロジック対応

参考：
- LangGraph: Multi-agent Supervisor
- Azure Architecture: AI Agent Orchestration Patterns
"""

import logging
from dataclasses import dataclass
from typing import Any

from agentflow.core.agent_block import AgentBlock
from agentflow.patterns.coordinator import CoordinationPattern, CoordinatorBase
from agentflow.patterns.multi_agent import SharedContext


@dataclass
class SupervisorDecision:
    """監督者の決定.

    Attributes:
        action: 実行アクション ("DELEGATE" or "FINISH")
        worker_name: 選択されたワーカー名（DELEGATE の場合）
        worker_input: ワーカーへの入力
        result: 最終結果（FINISH の場合）
        reason: 決定理由
    """

    action: str
    worker_name: str | None = None
    worker_input: dict[str, Any] | None = None
    result: Any = None
    reason: str = ""


class SupervisorCoordinator(CoordinatorBase):
    """Supervisor 協調器 - 監督者パターン.

    監督者 Agent が動的にワーカーを選択し、タスクを委譲します。
    各イテレーションで監督者が次のステップを決定します。

    Example:
        >>> supervisor = MySupervisorAgent()
        >>> workers = {"research": ResearchAgent(), "write": WriteAgent()}
        >>> coordinator = SupervisorCoordinator(supervisor, workers)
        >>> result = await coordinator.execute("市場調査レポートを作成")
    """

    # 有効なアクション値
    VALID_ACTIONS = {"DELEGATE", "FINISH"}

    def __init__(
        self,
        supervisor: AgentBlock,
        workers: dict[str, AgentBlock],
        max_iterations: int = 10,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            supervisor: 監督者 Agent
            workers: ワーカー Agent 辞書 {"名前": Agent}
            max_iterations: 最大イテレーション数
            **kwargs: 追加パラメータ

        Raises:
            TypeError: supervisor が AgentBlock でない場合
            ValueError: workers が空の場合、max_iterations が正でない場合
        """
        if not isinstance(supervisor, AgentBlock):
            msg = f"supervisor must be AgentBlock, got {type(supervisor).__name__}"
            raise TypeError(msg)
        if not workers:
            msg = "workers cannot be empty"
            raise ValueError(msg)
        if not isinstance(max_iterations, int) or max_iterations < 1:
            msg = f"max_iterations must be positive integer, got {max_iterations}"
            raise ValueError(msg)

        super().__init__(agents=list(workers.values()))
        self._supervisor = supervisor
        self._workers = workers
        self._max_iterations = max_iterations
        self._logger = logging.getLogger(__name__)

    @property
    def pattern(self) -> CoordinationPattern:
        """協調パターン種別."""
        return CoordinationPattern.SUPERVISOR

    async def execute(self, task: str, **kwargs: Any) -> dict[str, Any]:
        """タスクを実行.

        Args:
            task: 実行タスク
            **kwargs: 追加パラメータ

        Returns:
            実行結果
        """
        context = SharedContext()
        context.set("original_task", task)

        for i in range(self._max_iterations):
            self._logger.info(f"Iteration {i + 1}/{self._max_iterations}")

            # 1. 監督者に次のステップを決定させる
            decision = await self._decide_next_step(task, context)

            # 2. FINISH なら終了
            if decision.action == "FINISH":
                self._logger.info(f"Supervisor decided to finish: {decision.reason}")
                return {
                    "result": decision.result,
                    "iterations": i + 1,
                    "context": context.get_all(),
                }

            # 3. ワーカーを実行
            if decision.worker_name and decision.worker_name in self._workers:
                worker = self._workers[decision.worker_name]
                worker_result = await worker.run(decision.worker_input or {})
                context.set(f"step_{i}_{decision.worker_name}", worker_result)
                self._logger.info(f"Worker {decision.worker_name} completed")
            else:
                self._logger.warning(f"Unknown worker: {decision.worker_name}")

        # 最大イテレーション到達
        self._logger.warning("Max iterations reached")
        return {
            "result": context.get_all(),
            "iterations": self._max_iterations,
            "status": "max_iterations_reached",
        }

    async def _decide_next_step(
        self,
        task: str,
        context: SharedContext,
    ) -> SupervisorDecision:
        """次のステップを決定.

        Args:
            task: 元のタスク
            context: 共有コンテキスト

        Returns:
            監督者の決定
        """
        # 監督者 Agent に決定を求める
        decision_input = {
            "task": task,
            "context": context.get_all(),
            "available_workers": list(self._workers.keys()),
            "action": "decide_next",
        }

        result = await self._supervisor.run(decision_input)

        # 結果をパースして検証
        action = result.get("action", "FINISH")
        if action not in self.VALID_ACTIONS:
            self._logger.warning(f"Invalid action '{action}', defaulting to FINISH")
            action = "FINISH"

        return SupervisorDecision(
            action=action,
            worker_name=result.get("worker_name"),
            worker_input=result.get("worker_input"),
            result=result.get("result"),
            reason=result.get("reason", ""),
        )


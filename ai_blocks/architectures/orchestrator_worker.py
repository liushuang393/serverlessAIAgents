"""
Orchestrator-Worker アーキテクチャパターン

このモジュールは、複雑なタスクを小さなサブタスクに分割し、
複数のワーカーエージェントで並列処理して結果を統合するパターンを実装します。
"""

import asyncio
import time
from dataclasses import dataclass
from enum import Enum
from typing import Any, Callable, Dict, List, Optional

from ..core.models import Message, MessageRole
from ..utils.logging import get_logger
from .base import Agent

logger = get_logger(__name__)


class TaskStatus(str, Enum):
    """タスクの状態"""

    PENDING = "pending"  # 待機中
    RUNNING = "running"  # 実行中
    COMPLETED = "completed"  # 完了
    FAILED = "failed"  # 失敗


@dataclass
class SubTask:
    """サブタスクのデータクラス"""

    id: str
    description: str
    input_data: Any
    worker_type: str
    priority: int = 0
    status: TaskStatus = TaskStatus.PENDING
    result: Any = None
    error: Optional[str] = None
    start_time: Optional[float] = None
    end_time: Optional[float] = None

    @property
    def execution_time(self) -> Optional[float]:
        """実行時間を取得する"""
        if self.start_time and self.end_time:
            return self.end_time - self.start_time
        return None


class WorkerAgent(Agent):
    """ワーカーエージェントの基底クラス"""

    def __init__(
        self, name: str, worker_type: str, config: Optional[Dict[str, Any]] = None
    ):
        """
        ワーカーエージェントを初期化する

        Args:
            name: エージェント名
            worker_type: ワーカータイプ
            config: 設定辞書
        """
        super().__init__(name, config if config is not None else {})
        self.worker_type = worker_type
        self.is_busy = False

        logger.info(f"WorkerAgent '{self.name}' (type: {worker_type}) を初期化しました")

    async def execute_task(self, task: SubTask) -> Any:
        """
        サブタスクを実行する

        Args:
            task: 実行するサブタスク

        Returns:
            Any: 実行結果
        """
        if self.is_busy:
            raise RuntimeError(f"ワーカー '{self.name}' は既に実行中です")

        self.is_busy = True
        task.status = TaskStatus.RUNNING
        task.start_time = time.time()

        try:
            logger.info(f"ワーカー '{self.name}' がタスク '{task.id}' を開始しました")

            # 実際の処理は子クラスで実装
            result = await self._execute_task_impl(task)

            task.status = TaskStatus.COMPLETED
            task.result = result
            task.end_time = time.time()

            logger.info(
                f"ワーカー '{self.name}' がタスク '{task.id}' を完了しました "
                f"(実行時間: {task.execution_time:.2f}秒)"
            )

            return result

        except Exception as e:
            task.status = TaskStatus.FAILED
            task.error = str(e)
            task.end_time = time.time()

            logger.error(f"ワーカー '{self.name}' のタスク '{task.id}' が失敗しました: {e}")
            raise

        finally:
            self.is_busy = False

    async def _execute_task_impl(self, task: SubTask) -> Any:
        """
        サブタスクの実際の実行処理（子クラスで実装）

        Args:
            task: 実行するサブタスク

        Returns:
            Any: 実行結果
        """
        raise NotImplementedError("子クラスで実装してください")


class LLMWorker(WorkerAgent):
    """LLMを使用するワーカーエージェント"""

    def __init__(
        self, name: str, llm_provider: Any, config: Optional[Dict[str, Any]] = None
    ):
        """
        LLMワーカーを初期化する

        Args:
            name: エージェント名
            llm_provider: LLMプロバイダー
            config: 設定辞書
        """
        super().__init__(name, "llm", config)
        self.llm = llm_provider

    async def _execute_task_impl(self, task: SubTask) -> str:
        """LLMを使用してタスクを実行する"""
        messages = [
            Message(role=MessageRole.SYSTEM, content="あなたは専門的なタスクを実行するアシスタントです。"),
            Message(
                role=MessageRole.USER,
                content=f"タスク: {task.description}\n入力: {task.input_data}",
            ),
        ]

        response = await self.llm.generate(messages)
        return str(response.content)


class DataProcessingWorker(WorkerAgent):
    """データ処理専用のワーカーエージェント"""

    def __init__(
        self,
        name: str,
        processor_func: Callable,
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        データ処理ワーカーを初期化する

        Args:
            name: エージェント名
            processor_func: データ処理関数
            config: 設定辞書
        """
        super().__init__(name, "data_processing", config)
        self.processor_func = processor_func

    async def _execute_task_impl(self, task: SubTask) -> Any:
        """データ処理関数を使用してタスクを実行する"""
        if asyncio.iscoroutinefunction(self.processor_func):
            return await self.processor_func(task.input_data)
        else:
            return self.processor_func(task.input_data)


class OrchestratorWorker(Agent):
    """オーケストレーター・ワーカーパターンのメインエージェント"""

    def __init__(
        self,
        name: str = "OrchestratorWorker",
        task_decomposer: Optional[Callable] = None,
        result_aggregator: Optional[Callable] = None,
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        オーケストレーター・ワーカーを初期化する

        Args:
            name: エージェント名
            task_decomposer: タスク分解関数
            result_aggregator: 結果統合関数
            config: 設定辞書
        """
        super().__init__(name, config)

        self.workers: Dict[str, List[WorkerAgent]] = {}
        self.task_decomposer = task_decomposer or self._default_task_decomposer
        self.result_aggregator = result_aggregator or self._default_result_aggregator

        # 設定値
        self.max_concurrent_tasks = self.config.get("max_concurrent_tasks", 5)
        self.task_timeout = self.config.get("task_timeout", 60.0)

        logger.info(f"OrchestratorWorker '{self.name}' を初期化しました")

    def register_worker(self, worker: WorkerAgent) -> None:
        """
        ワーカーエージェントを登録する

        Args:
            worker: 登録するワーカーエージェント
        """
        if worker.worker_type not in self.workers:
            self.workers[worker.worker_type] = []

        self.workers[worker.worker_type].append(worker)
        logger.info(f"ワーカー '{worker.name}' (type: {worker.worker_type}) を登録しました")

    def get_available_worker(self, worker_type: str) -> Optional[WorkerAgent]:
        """
        利用可能なワーカーを取得する

        Args:
            worker_type: ワーカータイプ

        Returns:
            Optional[WorkerAgent]: 利用可能なワーカー、なければNone
        """
        if worker_type not in self.workers:
            return None

        for worker in self.workers[worker_type]:
            if not worker.is_busy:
                return worker

        return None

    async def process(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        入力を処理して応答を生成する

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            str: 処理結果
        """
        context = context or {}
        start_time = time.time()

        try:
            # 1. タスクを分解
            subtasks = await self.task_decomposer(input_text, context)
            logger.info(f"タスクを {len(subtasks)} 個のサブタスクに分解しました")

            # 2. サブタスクを並列実行
            results = await self._execute_subtasks(subtasks)

            # 3. 結果を統合
            final_result = await self.result_aggregator(results, input_text, context)

            execution_time = time.time() - start_time
            logger.info(f"オーケストレーション完了 (実行時間: {execution_time:.2f}秒)")

            return final_result

        except Exception as e:
            logger.error(f"オーケストレーション処理エラー: {e}")
            raise

    async def _execute_subtasks(self, subtasks: List[SubTask]) -> List[SubTask]:
        """
        サブタスクを並列実行する

        Args:
            subtasks: 実行するサブタスクのリスト

        Returns:
            List[SubTask]: 実行結果を含むサブタスクのリスト
        """
        # 優先度でソート
        subtasks.sort(key=lambda t: t.priority, reverse=True)

        # セマフォで同時実行数を制限
        semaphore = asyncio.Semaphore(self.max_concurrent_tasks)

        async def execute_with_semaphore(task: SubTask) -> SubTask:
            async with semaphore:
                worker = self.get_available_worker(task.worker_type)
                if not worker:
                    raise RuntimeError(f"ワーカータイプ '{task.worker_type}' が利用できません")

                try:
                    await asyncio.wait_for(
                        worker.execute_task(task), timeout=self.task_timeout
                    )
                except asyncio.TimeoutError:
                    task.status = TaskStatus.FAILED
                    task.error = "タイムアウト"
                    logger.error(f"タスク '{task.id}' がタイムアウトしました")

                return task

        # 全てのサブタスクを並列実行
        tasks = [execute_with_semaphore(subtask) for subtask in subtasks]
        completed_tasks = await asyncio.gather(*tasks, return_exceptions=True)

        # 例外を処理
        results = []
        for i, result in enumerate(completed_tasks):
            if isinstance(result, Exception):
                subtasks[i].status = TaskStatus.FAILED
                subtasks[i].error = str(result)
                logger.error(f"サブタスク '{subtasks[i].id}' で例外が発生: {result}")
            results.append(subtasks[i])

        return results

    async def _default_task_decomposer(
        self, input_text: str, context: Dict[str, Any]
    ) -> List[SubTask]:
        """
        デフォルトのタスク分解関数

        Args:
            input_text: 入力テキスト
            context: コンテキスト

        Returns:
            List[SubTask]: 分解されたサブタスクのリスト
        """
        # シンプルな例：入力を文で分割してそれぞれを処理
        sentences = [s.strip() for s in input_text.split(".") if s.strip()]

        subtasks = []
        for i, sentence in enumerate(sentences):
            subtask = SubTask(
                id=f"task_{i}",
                description=f"文章を処理: {sentence[:50]}...",
                input_data=sentence,
                worker_type="llm",
                priority=len(sentences) - i,  # 最初の文ほど優先度高
            )
            subtasks.append(subtask)

        return subtasks

    async def _default_result_aggregator(
        self, results: List[SubTask], original_input: str, context: Dict[str, Any]
    ) -> str:
        """
        デフォルトの結果統合関数

        Args:
            results: サブタスクの結果リスト
            original_input: 元の入力
            context: コンテキスト

        Returns:
            str: 統合された結果
        """
        successful_results = [
            task.result
            for task in results
            if task.status == TaskStatus.COMPLETED and task.result
        ]

        if not successful_results:
            return "申し訳ありませんが、処理に失敗しました。"

        # 結果を結合
        combined_result = "\n\n".join(str(result) for result in successful_results)

        # 統計情報を追加
        total_tasks = len(results)
        successful_tasks = len(successful_results)
        failed_tasks = total_tasks - successful_tasks

        if failed_tasks > 0:
            combined_result += f"\n\n[処理統計: {successful_tasks}/{total_tasks} タスク成功]"

        return combined_result

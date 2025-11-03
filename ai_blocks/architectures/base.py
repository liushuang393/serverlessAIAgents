"""
アーキテクチャパターンの基本クラス

このモジュールは、全てのアーキテクチャパターンで使用される基本クラスと
共通インターフェースを定義します。
"""

import asyncio
import time
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional

from ..core.models import ChainResult
from ..utils.logging import get_logger

logger = get_logger(__name__)


class Agent(ABC):
    """全てのAgentの基底クラス"""

    def __init__(
        self, name: Optional[str] = None, config: Optional[Dict[str, Any]] = None
    ):
        """
        Agentを初期化する

        Args:
            name: Agent名
            config: 設定辞書
        """
        self.name = name if name is not None else self.__class__.__name__
        self.config = config if config is not None else {}
        self._metrics = {
            "total_requests": 0,
            "successful_requests": 0,
            "failed_requests": 0,
            "total_execution_time": 0.0,
        }

        logger.info(f"Agent '{self.name}' を初期化しました")

    @abstractmethod
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
        pass

    async def process_with_metrics(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        メトリクス収集付きで処理を実行する

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            str: 処理結果
        """
        start_time = time.time()
        self._metrics["total_requests"] += 1

        try:
            result = await self.process(input_text, context)
            self._metrics["successful_requests"] += 1

            execution_time = time.time() - start_time
            self._metrics["total_execution_time"] += execution_time

            logger.debug(f"Agent '{self.name}' 処理完了（実行時間: {execution_time:.2f}秒）")
            return result

        except Exception as e:
            self._metrics["failed_requests"] += 1
            execution_time = time.time() - start_time
            self._metrics["total_execution_time"] += execution_time

            logger.error(f"Agent '{self.name}' 処理中にエラーが発生しました: {e}")
            raise

    def get_metrics(self) -> Dict[str, Any]:
        """
        メトリクスを取得する

        Returns:
            Dict[str, Any]: メトリクス辞書
        """
        metrics = self._metrics.copy()

        # 追加の計算メトリクス
        if metrics["total_requests"] > 0:
            metrics["success_rate"] = (
                metrics["successful_requests"] / metrics["total_requests"]
            )
            metrics["average_execution_time"] = (
                metrics["total_execution_time"] / metrics["total_requests"]
            )
        else:
            metrics["success_rate"] = 0.0
            metrics["average_execution_time"] = 0.0

        return metrics

    def reset_metrics(self) -> None:
        """メトリクスをリセットする"""
        self._metrics = {
            "total_requests": 0,
            "successful_requests": 0,
            "failed_requests": 0,
            "total_execution_time": 0.0,
        }
        logger.debug(f"Agent '{self.name}' のメトリクスをリセットしました")


class ResultAggregator(ABC):
    """結果統合の抽象クラス"""

    @abstractmethod
    async def aggregate(
        self, results: List[str], context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        複数の結果を統合する

        Args:
            results: 統合する結果のリスト
            context: 統合のコンテキスト

        Returns:
            str: 統合された結果
        """
        pass


class SimpleAggregator(ResultAggregator):
    """シンプルな結果統合器"""

    def __init__(self, separator: str = "\n\n"):
        """
        シンプルな結果統合器を初期化する

        Args:
            separator: 結果を結合する際の区切り文字
        """
        self.separator = separator

    async def aggregate(
        self, results: List[str], context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        複数の結果を単純に結合する

        Args:
            results: 統合する結果のリスト
            context: 統合のコンテキスト

        Returns:
            str: 統合された結果
        """
        if not results:
            return ""

        # 空でない結果のみを結合
        valid_results = [result.strip() for result in results if result.strip()]
        return self.separator.join(valid_results)


class WeightedAggregator(ResultAggregator):
    """重み付き結果統合器"""

    def __init__(self, weights: Optional[List[float]] = None):
        """
        重み付き結果統合器を初期化する

        Args:
            weights: 各結果の重み（Noneの場合は均等重み）
        """
        self.weights = weights

    async def aggregate(
        self, results: List[str], context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        重み付きで複数の結果を統合する

        Args:
            results: 統合する結果のリスト
            context: 統合のコンテキスト

        Returns:
            str: 統合された結果
        """
        if not results:
            return ""

        # 重みを設定
        if self.weights and len(self.weights) == len(results):
            weights = self.weights
        else:
            weights = [1.0] * len(results)

        # 重み付き結合
        weighted_results = []
        total_weight = sum(weights)

        for result, weight in zip(results, weights):
            if result.strip():
                # 重みに応じて結果を重複させる（簡単な実装）
                repetitions = max(1, int(weight / total_weight * len(results)))
                weighted_results.extend([result.strip()] * repetitions)

        # 最も頻繁に現れる結果を選択（または結合）
        if weighted_results:
            # 最初の結果を返す（より高度な統合ロジックも可能）
            return weighted_results[0]
        else:
            return ""


class VotingAggregator(ResultAggregator):
    """投票ベースの結果統合器"""

    def __init__(self, min_votes: int = 2):
        """
        投票ベースの結果統合器を初期化する

        Args:
            min_votes: 採用に必要な最小投票数
        """
        self.min_votes = min_votes

    async def aggregate(
        self, results: List[str], context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        投票により複数の結果を統合する

        Args:
            results: 統合する結果のリスト
            context: 統合のコンテキスト

        Returns:
            str: 統合された結果
        """
        if not results:
            return ""

        # 結果の出現回数をカウント
        result_counts: Dict[str, int] = {}
        for result in results:
            if result.strip():
                normalized_result = result.strip().lower()
                result_counts[normalized_result] = (
                    result_counts.get(normalized_result, 0) + 1
                )

        # 最も多い投票を得た結果を選択
        if result_counts:
            best_result = max(result_counts.items(), key=lambda x: x[1])
            if best_result[1] >= self.min_votes:
                # 元の形式で返す
                for result in results:
                    if result.strip().lower() == best_result[0]:
                        return result.strip()

        # 投票が不十分な場合は最初の結果を返す
        valid_results = [result.strip() for result in results if result.strip()]
        return valid_results[0] if valid_results else ""


class ChainExecutor:
    """チェーン実行器"""

    def __init__(
        self, agents: List[Agent], aggregator: Optional[ResultAggregator] = None
    ):
        """
        チェーン実行器を初期化する

        Args:
            agents: 実行するAgentのリスト
            aggregator: 結果統合器（Noneの場合はSimpleAggregatorを使用）
        """
        self.agents = agents
        self.aggregator = aggregator or SimpleAggregator()

        logger.info(f"チェーン実行器を初期化しました（{len(agents)}個のAgent）")

    async def execute_sequential(
        self, initial_input: str, context: Optional[Dict[str, Any]] = None
    ) -> ChainResult:
        """
        Agentを順次実行する

        Args:
            initial_input: 初期入力
            context: 実行コンテキスト

        Returns:
            ChainResult: チェーン実行結果
        """
        start_time = time.time()
        current_input = initial_input
        intermediate_results = []

        try:
            for i, agent in enumerate(self.agents):
                logger.debug(f"Agent {i+1}/{len(self.agents)} '{agent.name}' を実行中...")

                result = await agent.process_with_metrics(current_input, context)
                intermediate_results.append(
                    {
                        "agent": agent.name,
                        "input": current_input,
                        "output": result,
                        "step": i + 1,
                    }
                )

                # 次のAgentへの入力として使用
                current_input = result

            execution_time = time.time() - start_time

            return ChainResult(
                final_output=current_input,
                intermediate_results=intermediate_results,
                execution_time=execution_time,
                success=True,
            )

        except Exception as e:
            execution_time = time.time() - start_time
            logger.error(f"チェーン実行中にエラーが発生しました: {e}")

            return ChainResult(
                final_output="",
                intermediate_results=intermediate_results,
                execution_time=execution_time,
                success=False,
            )

    async def execute_parallel(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> ChainResult:
        """
        Agentを並列実行する

        Args:
            input_text: 入力テキスト
            context: 実行コンテキスト

        Returns:
            ChainResult: チェーン実行結果
        """
        start_time = time.time()

        try:
            # 全Agentを並列実行
            tasks = [
                agent.process_with_metrics(input_text, context) for agent in self.agents
            ]
            results = await asyncio.gather(*tasks, return_exceptions=True)

            # 結果を整理
            intermediate_results = []
            valid_results = []

            for i, (agent, result) in enumerate(zip(self.agents, results)):
                if isinstance(result, Exception):
                    logger.warning(f"Agent '{agent.name}' でエラーが発生しました: {result}")
                    intermediate_results.append(
                        {
                            "agent": agent.name,
                            "input": input_text,
                            "output": "",
                            "error": str(result),
                            "step": i + 1,
                        }
                    )
                else:
                    intermediate_results.append(
                        {
                            "agent": agent.name,
                            "input": input_text,
                            "output": result,
                            "step": i + 1,
                        }
                    )
                    valid_results.append(result)

            # 結果を統合
            final_output = await self.aggregator.aggregate(valid_results, context)
            execution_time = time.time() - start_time

            return ChainResult(
                final_output=final_output,
                intermediate_results=intermediate_results,
                execution_time=execution_time,
                success=len(valid_results) > 0,
            )

        except Exception as e:
            execution_time = time.time() - start_time
            logger.error(f"並列実行中にエラーが発生しました: {e}")

            return ChainResult(
                final_output="",
                intermediate_results=[],
                execution_time=execution_time,
                success=False,
            )

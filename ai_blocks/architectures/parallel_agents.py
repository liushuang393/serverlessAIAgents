"""
Parallel Agents アーキテクチャパターン

このモジュールは、複数のAgentを並列実行し、結果を統合するパターンを実装します。
"""

from typing import Any, Dict, List, Optional

from ..utils.logging import get_logger
from .base import Agent, ChainExecutor, ResultAggregator, SimpleAggregator

logger = get_logger(__name__)


class ParallelAgents:
    """複数のAgentを並列実行し、結果を統合するパターン"""

    def __init__(
        self,
        agents: Optional[List[Agent]] = None,
        aggregator: Optional[ResultAggregator] = None,
        name: str = "ParallelAgents",
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        Parallel Agentsを初期化する

        Args:
            agents: 並列実行するAgentのリスト（Noneの場合は空リスト）
            aggregator: 結果統合器
            name: パターン名
            config: 設定辞書
        """
        self.name = name
        self.agents = agents if agents is not None else []
        self.aggregator = aggregator if aggregator is not None else SimpleAggregator()
        self.config = config if config is not None else {}
        self.executor = ChainExecutor(self.agents, self.aggregator)

        # 設定値
        self.timeout = self.config.get("timeout", 60.0)
        self.min_success_count = self.config.get("min_success_count", 1)

        logger.info(
            f"Parallel Agents '{self.name}' を初期化しました（{len(self.agents)}個のAgent）"
        )

    async def process(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        複数のAgentを並列実行し、結果を統合する

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            str: 統合された結果
        """
        if not self.agents:
            return "利用可能なAgentがありません。"

        if not input_text.strip():
            return "申し訳ありませんが、入力が空です。"

        try:
            logger.info(
                f"Parallel Agents '{self.name}' で{len(self.agents)}個のAgentを並列実行します"
            )

            # 並列実行
            result = await self.executor.execute_parallel(input_text, context)

            # 成功したAgentの数をチェック
            success_count = sum(
                1 for r in result.intermediate_results if not r.get("error")
            )

            if success_count < self.min_success_count:
                logger.warning(
                    f"成功したAgentが最小要求数({self.min_success_count})を下回りました: {success_count}"
                )
                return "十分な数のAgentが成功しませんでした。"

            logger.info(
                f"Parallel Agents '{self.name}' が完了しました"
                f"（成功: {success_count}/{len(self.agents)}）"
            )

            return result.final_output or "結果の統合に失敗しました。"

        except Exception as e:
            logger.error(f"Parallel Agents '{self.name}' の処理中にエラーが発生しました: {e}")
            return f"申し訳ありませんが、処理中にエラーが発生しました: {str(e)}"

    def add_agent(self, agent: Agent) -> None:
        """
        Agentを追加する

        Args:
            agent: 追加するAgent
        """
        self.agents.append(agent)
        self.executor = ChainExecutor(self.agents, self.aggregator)
        logger.info(f"Agent '{agent.name}' を追加しました")

    def remove_agent(self, agent_name: str) -> bool:
        """
        Agentを削除する

        Args:
            agent_name: 削除するAgent名

        Returns:
            bool: 削除が成功したかどうか
        """
        original_count = len(self.agents)
        self.agents = [agent for agent in self.agents if agent.name != agent_name]

        if len(self.agents) < original_count:
            self.executor = ChainExecutor(self.agents, self.aggregator)
            logger.info(f"Agent '{agent_name}' を削除しました")
            return True

        return False

    def get_agent_names(self) -> List[str]:
        """
        Agent名のリストを取得する

        Returns:
            List[str]: Agent名のリスト
        """
        return [agent.name for agent in self.agents]

    async def get_status(self) -> Dict[str, Any]:
        """
        パターンの状態を取得する

        Returns:
            Dict[str, Any]: 状態情報
        """
        agent_statuses = []
        for agent in self.agents:
            if hasattr(agent, "get_status"):
                status = await agent.get_status()
            else:
                status = {
                    "name": agent.name,
                    "metrics": agent.get_metrics()
                    if hasattr(agent, "get_metrics")
                    else {},
                }
            agent_statuses.append(status)

        return {
            "name": self.name,
            "config": self.config,
            "agent_count": len(self.agents),
            "agent_names": self.get_agent_names(),
            "aggregator": self.aggregator.__class__.__name__,
            "agents": agent_statuses,
        }

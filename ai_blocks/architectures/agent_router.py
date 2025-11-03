"""
Agent Router アーキテクチャパターン

このモジュールは、入力内容に基づいて最適なAgentに振り分けるパターンを実装します。
"""

from typing import Any, Dict, Optional

from ..core import RouterInterface
from ..utils.logging import get_logger
from .base import Agent

logger = get_logger(__name__)


class AgentRouter:
    """入力内容に基づいて最適なAgentに振り分けるパターン"""

    def __init__(
        self,
        router: Optional[RouterInterface] = None,
        agents: Optional[Dict[str, Agent]] = None,
        name: str = "AgentRouter",
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        Agent Routerを初期化する

        Args:
            router: ルーターコンポーネント（Noneの場合はダミー）
            agents: Agent名をキーとするAgentの辞書（Noneの場合は空辞書）
            name: ルーター名
            config: 設定辞書
        """
        self.name = name
        self.router = router if router is not None else None
        self.agents = agents if agents is not None else {}
        self.config = config if config is not None else {}

        # 設定値
        self.fallback_agent = self.config.get("fallback_agent", "default")
        self.min_confidence = self.config.get("min_confidence", 0.5)

        logger.info(f"Agent Router '{self.name}' を初期化しました（{len(self.agents)}個のAgent）")

    async def process(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        入力内容に基づいて最適なAgentに振り分けて処理する

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            str: 処理結果
        """
        if not input_text.strip():
            return "申し訳ありませんが、入力が空です。"

        try:
            # ルーティング決定
            if not self.router:
                raise ValueError("ルーターが設定されていません")

            route_result = await self.router.route(
                input_text, context if context is not None else {}
            )

            # 信頼度チェック
            if route_result.confidence < self.min_confidence:
                logger.warning(f"ルーティング信頼度が低いです: {route_result.confidence:.2f}")
                target_agent_name = self.fallback_agent
            else:
                target_agent_name = route_result.target

            # 対応するAgentを取得
            selected_agent = self.agents.get(target_agent_name)
            if not selected_agent:
                logger.error(f"Agent '{target_agent_name}' が見つかりません")
                # フォールバックAgentを試行
                selected_agent = self.agents.get(self.fallback_agent)
                if not selected_agent:
                    return f"エラー: Agent '{target_agent_name}' が見つかりません"

            logger.info(
                f"Agent '{selected_agent.name}' に振り分けました"
                f"（信頼度: {route_result.confidence:.2f}）"
            )

            # Agentで処理実行
            context = context if context is not None else {}
            context.update({"route_result": route_result, "router_name": self.name})

            return await selected_agent.process(input_text, context)

        except Exception as e:
            logger.error(f"Agent Router '{self.name}' の処理中にエラーが発生しました: {e}")
            return f"申し訳ありませんが、処理中にエラーが発生しました: {str(e)}"

    def add_agent(self, name: str, agent: Agent) -> None:
        """
        Agentを追加する

        Args:
            name: Agent名
            agent: Agentインスタンス
        """
        self.agents[name] = agent
        logger.info(f"Agent '{name}' を追加しました")

    def remove_agent(self, name: str) -> bool:
        """
        Agentを削除する

        Args:
            name: 削除するAgent名

        Returns:
            bool: 削除が成功したかどうか
        """
        if name in self.agents:
            del self.agents[name]
            logger.info(f"Agent '{name}' を削除しました")
            return True
        return False

    def get_agent_names(self) -> list:
        """
        登録されているAgent名のリストを取得する

        Returns:
            list: Agent名のリスト
        """
        return list(self.agents.keys())

    async def get_status(self) -> Dict[str, Any]:
        """
        ルーターの状態を取得する

        Returns:
            Dict[str, Any]: 状態情報
        """
        agent_statuses = {}
        for name, agent in self.agents.items():
            if hasattr(agent, "get_status"):
                status = await agent.get_status()
            else:
                status = {
                    "name": agent.name,
                    "metrics": agent.get_metrics()
                    if hasattr(agent, "get_metrics")
                    else {},
                }
            agent_statuses[name] = status

        return {
            "name": self.name,
            "config": self.config,
            "agent_count": len(self.agents),
            "agent_names": self.get_agent_names(),
            "routes": self.router.get_routes()
            if self.router and hasattr(self.router, "get_routes")
            else [],
            "agents": agent_statuses,
        }

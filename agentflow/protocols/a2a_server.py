"""A2A サーバー実装.

このモジュールはローカルエージェントを A2A エンドポイントとして公開するサーバーを提供します。
"""

import asyncio
import logging
from collections.abc import Callable
from typing import Any

from agentflow.protocols.a2a_card import AgentCard


class A2AServer:
    """ローカルエージェントを A2A エンドポイントとして公開するサーバー.

    このクラスは複数のエージェントを登録し、A2A プロトコルでタスクリクエストを処理します。

    Example:
        >>> server = A2AServer()
        >>> server.register_agent(agent_card, handler)
        >>> await server.handle_task("agent_name", "skill_name", {"input": "data"})
    """

    def __init__(
        self,
        *,
        logger: logging.Logger | None = None,
        default_timeout: float = 30.0,
    ) -> None:
        """A2A サーバーを初期化.

        Args:
            logger: ロガーインスタンス (オプション)
            default_timeout: タスクのデフォルトタイムアウト (秒)
        """
        self._logger = logger or logging.getLogger(__name__)
        self._default_timeout = default_timeout
        self._agents: dict[str, AgentCard] = {}
        self._handlers: dict[str, dict[str, Callable[..., Any]]] = {}

    def register_agent(
        self,
        agent_card: AgentCard,
        handlers: dict[str, Callable[..., Any]],
    ) -> None:
        """エージェントをサーバーに登録.

        Args:
            agent_card: エージェントカード
            handlers: スキル名とハンドラー関数のマッピング

        Raises:
            ValueError: エージェントが既に登録されている場合
        """
        if agent_card.name in self._agents:
            msg = f"Agent already registered: {agent_card.name}"
            raise ValueError(msg)

        self._agents[agent_card.name] = agent_card
        self._handlers[agent_card.name] = handlers

        self._logger.info(
            f"Registered agent: {agent_card.name} with {len(handlers)} skills"
        )

    def unregister_agent(self, agent_name: str) -> None:
        """エージェントをサーバーから削除.

        Args:
            agent_name: エージェント名

        Raises:
            ValueError: エージェントが登録されていない場合
        """
        if agent_name not in self._agents:
            msg = f"Agent not found: {agent_name}"
            raise ValueError(msg)

        del self._agents[agent_name]
        del self._handlers[agent_name]

        self._logger.info(f"Unregistered agent: {agent_name}")

    def get_agent_card(self, agent_name: str) -> AgentCard | None:
        """エージェントカードを取得.

        Args:
            agent_name: エージェント名

        Returns:
            AgentCard、または存在しない場合は None
        """
        return self._agents.get(agent_name)

    def list_agents(self) -> list[str]:
        """登録されているエージェント名のリストを取得.

        Returns:
            エージェント名のリスト
        """
        return list(self._agents.keys())

    async def handle_task(
        self,
        agent_name: str,
        skill_name: str,
        inputs: dict[str, Any],
        *,
        timeout: float | None = None,
    ) -> dict[str, Any]:
        """タスクリクエストを処理.

        Args:
            agent_name: エージェント名
            skill_name: スキル名
            inputs: タスク入力
            timeout: タイムアウト (秒)、None の場合はデフォルト値を使用

        Returns:
            タスク実行結果

        Raises:
            ValueError: エージェントまたはスキルが見つからない場合
            asyncio.TimeoutError: タスクがタイムアウトした場合
        """
        # エージェントの存在確認
        if agent_name not in self._agents:
            msg = f"Agent not found: {agent_name}"
            raise ValueError(msg)

        # スキルの存在確認
        if skill_name not in self._handlers[agent_name]:
            msg = f"Skill not found: {skill_name} in agent {agent_name}"
            raise ValueError(msg)

        # ハンドラーを取得
        handler = self._handlers[agent_name][skill_name]

        # タイムアウトを設定
        task_timeout = timeout if timeout is not None else self._default_timeout

        try:
            # タスクを実行
            self._logger.debug(
                f"Executing task: agent={agent_name}, skill={skill_name}"
            )

            # ハンドラーが async かどうかを確認
            if asyncio.iscoroutinefunction(handler):
                result = await asyncio.wait_for(
                    handler(inputs), timeout=task_timeout
                )
            else:
                # 同期関数の場合は executor で実行
                loop = asyncio.get_event_loop()
                result = await asyncio.wait_for(
                    loop.run_in_executor(None, handler, inputs),
                    timeout=task_timeout,
                )

            self._logger.debug(
                f"Task completed: agent={agent_name}, skill={skill_name}"
            )

        except TimeoutError:
            self._logger.exception(
                f"Task timeout: agent={agent_name}, skill={skill_name}, "
                f"timeout={task_timeout}s"
            )
            return {
                "status": "error",
                "error": "Task timeout",
                "agent": agent_name,
                "skill": skill_name,
            }

        except Exception:
            self._logger.exception(
                f"Task failed: agent={agent_name}, skill={skill_name}"
            )
            return {
                "status": "error",
                "error": "Task execution failed",
                "agent": agent_name,
                "skill": skill_name,
            }
        else:
            return {
                "status": "success",
                "result": result,
                "agent": agent_name,
                "skill": skill_name,
            }

    def get_all_agent_cards(self) -> list[dict[str, Any]]:
        """すべてのエージェントカードを A2A 形式で取得.

        Returns:
            A2A 形式のエージェントカードのリスト
        """
        return [card.to_a2a_format() for card in self._agents.values()]

    def get_agent_skills(self, agent_name: str) -> list[str]:
        """エージェントのスキル名リストを取得.

        Args:
            agent_name: エージェント名

        Returns:
            スキル名のリスト、エージェントが存在しない場合は空リスト
        """
        if agent_name not in self._agents:
            return []

        return [skill.name for skill in self._agents[agent_name].skills]


"""DeepAgent Agent池管理.

通用Agent池の設計思想:
- 5-6個の基本Agent種別（Research, Analysis, Planning, Execution, Review, Report）
- 動的Agent生成（必要に応じてインスタンス化）
- Agent間通信の標準化
- リソース効率的な管理

DeepAgentsフレームワークの核心概念:
- Agentは「役割」であり、固定インスタンスではない
- タスクに応じて適切なAgentを選択・生成
- 並行実行時は複数インスタンスを生成可能
"""

from __future__ import annotations

import asyncio
import logging
from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any, cast

from agentflow.patterns.deep_agent.da_models import (
    AgentMessage,
    AgentType,
    MessageType,
    TodoItem,
)


_logger = logging.getLogger(__name__)


class BaseAgent(ABC):
    """基底Agent抽象クラス.

    全てのAgentはこのクラスを継承し、execute()を実装する。
    """

    def __init__(
        self,
        agent_type: AgentType,
        llm_client: Any = None,
        tools: list[Any] | None = None,
        skills: list[Any] | None = None,
    ) -> None:
        """初期化.

        Args:
            agent_type: Agent種別
            llm_client: LLMクライアント
            tools: 利用可能ツール
            skills: 利用可能スキル
        """
        self.agent_type = agent_type
        self.llm = llm_client
        self.tools = tools or []
        self.skills = skills or []
        self._message_queue: asyncio.Queue[AgentMessage] = asyncio.Queue()
        self._created_at = datetime.now()

    @property
    def name(self) -> str:
        """Agent名を返す."""
        return f"{self.agent_type.value}-agent"

    @abstractmethod
    async def execute(self, todo: TodoItem, context: dict[str, Any]) -> dict[str, Any]:
        """タスクを実行.

        Args:
            todo: 実行するタスク
            context: 実行コンテキスト

        Returns:
            実行結果
        """

    async def receive_message(self, message: AgentMessage) -> None:
        """メッセージを受信."""
        await self._message_queue.put(message)

    async def send_message(
        self,
        to_agent: str,
        content: Any,
        msg_type: MessageType = MessageType.NOTIFY,
    ) -> AgentMessage:
        """メッセージを送信."""
        return AgentMessage(
            from_agent=self.name,
            to_agent=to_agent,
            msg_type=msg_type,
            content=content,
        )


class ResearchAgent(BaseAgent):
    """調査Agent - 情報収集・検索を担当."""

    def __init__(self, llm_client: Any = None, **kwargs: Any) -> None:
        """初期化."""
        super().__init__(AgentType.RESEARCH, llm_client, **kwargs)

    async def execute(self, todo: TodoItem, context: dict[str, Any]) -> dict[str, Any]:
        """調査タスクを実行."""
        _logger.info("[ResearchAgent] タスク実行: %s", todo.task)
        # 実際の実装ではLLM + ツール呼び出し
        return {
            "status": "completed",
            "agent": self.name,
            "task_id": todo.id,
            "findings": f"調査結果: {todo.task}",
            "sources": [],
        }


class AnalysisAgent(BaseAgent):
    """分析Agent - データ分析・推論を担当."""

    def __init__(self, llm_client: Any = None, **kwargs: Any) -> None:
        """初期化."""
        super().__init__(AgentType.ANALYSIS, llm_client, **kwargs)

    async def execute(self, todo: TodoItem, context: dict[str, Any]) -> dict[str, Any]:
        """分析タスクを実行."""
        _logger.info("[AnalysisAgent] タスク実行: %s", todo.task)
        return {
            "status": "completed",
            "agent": self.name,
            "task_id": todo.id,
            "analysis": f"分析結果: {todo.task}",
            "insights": [],
        }


class PlanningAgent(BaseAgent):
    """計画Agent - 計画立案・設計を担当."""

    def __init__(self, llm_client: Any = None, **kwargs: Any) -> None:
        """初期化."""
        super().__init__(AgentType.PLANNING, llm_client, **kwargs)

    async def execute(self, todo: TodoItem, context: dict[str, Any]) -> dict[str, Any]:
        """計画タスクを実行."""
        _logger.info("[PlanningAgent] タスク実行: %s", todo.task)
        return {
            "status": "completed",
            "agent": self.name,
            "task_id": todo.id,
            "plan": f"計画: {todo.task}",
            "steps": [],
        }


class ExecutionAgent(BaseAgent):
    """実行Agent - 実際の操作・実行を担当."""

    def __init__(self, llm_client: Any = None, **kwargs: Any) -> None:
        """初期化."""
        super().__init__(AgentType.EXECUTION, llm_client, **kwargs)

    async def execute(self, todo: TodoItem, context: dict[str, Any]) -> dict[str, Any]:
        """実行タスクを実行."""
        _logger.info("[ExecutionAgent] タスク実行: %s", todo.task)
        return {
            "status": "completed",
            "agent": self.name,
            "task_id": todo.id,
            "output": f"実行結果: {todo.task}",
            "artifacts": [],
        }


class ReviewAgent(BaseAgent):
    """審査Agent - 品質検証・レビューを担当."""

    def __init__(self, llm_client: Any = None, **kwargs: Any) -> None:
        """初期化."""
        super().__init__(AgentType.REVIEW, llm_client, **kwargs)

    async def execute(self, todo: TodoItem, context: dict[str, Any]) -> dict[str, Any]:
        """審査タスクを実行."""
        _logger.info("[ReviewAgent] タスク実行: %s", todo.task)
        return {
            "status": "completed",
            "agent": self.name,
            "task_id": todo.id,
            "review": f"審査結果: {todo.task}",
            "issues": [],
            "approved": True,
        }


class ReportAgent(BaseAgent):
    """報告Agent - 結果報告・総括を担当."""

    def __init__(self, llm_client: Any = None, **kwargs: Any) -> None:
        """初期化."""
        super().__init__(AgentType.REPORT, llm_client, **kwargs)

    async def execute(self, todo: TodoItem, context: dict[str, Any]) -> dict[str, Any]:
        """報告タスクを実行."""
        _logger.info("[ReportAgent] タスク実行: %s", todo.task)
        return {
            "status": "completed",
            "agent": self.name,
            "task_id": todo.id,
            "report": f"報告: {todo.task}",
            "summary": "",
        }


# =============================================================================
# Agent池管理
# =============================================================================


class AgentPool:
    """Agent池 - Agentの生成・管理を担当.

    Example:
        >>> pool = AgentPool(llm_client=my_llm)
        >>> agent = pool.get_agent(AgentType.RESEARCH)
        >>> result = await agent.execute(todo, context)
    """

    # Agent種別とクラスのマッピング
    AGENT_CLASSES: dict[AgentType, type[BaseAgent]] = {
        AgentType.RESEARCH: ResearchAgent,
        AgentType.ANALYSIS: AnalysisAgent,
        AgentType.PLANNING: PlanningAgent,
        AgentType.EXECUTION: ExecutionAgent,
        AgentType.REVIEW: ReviewAgent,
        AgentType.REPORT: ReportAgent,
    }

    def __init__(
        self,
        llm_client: Any = None,
        tools: list[Any] | None = None,
        skills: list[Any] | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: 共有LLMクライアント
            tools: 共有ツール
            skills: 共有スキル
        """
        self._llm = llm_client
        self._tools = tools or []
        self._skills = skills or []
        self._agents: dict[AgentType, BaseAgent] = {}
        self._message_bus: list[AgentMessage] = []
        self._registered_agents: dict[str, Any] = {}
        self._default_bindings: dict[str, dict[str, Any]] = {}

    def register_agent(self, name: str, agent: Any) -> None:
        """カスタムAgentを名前で登録.

        Args:
            name: Agent名
            agent: Agentインスタンス
        """
        self._registered_agents[name] = agent

    def list_agents(self) -> list[str]:
        """登録済みAgent名の一覧を返す.

        Returns:
            Agent名のリスト
        """
        return list(self._registered_agents.keys())

    async def get_or_create(self, agent_type: AgentType | str) -> Any:
        """Agentを取得（なければ動的に作成）.

        Args:
            agent_type: Agent種別またはAgent名

        Returns:
            Agentインスタンス
        """
        # 名前で登録済みエージェントを探す
        if isinstance(agent_type, str) and agent_type in self._registered_agents:
            return self._registered_agents[agent_type]

        # AgentTypeの場合、キャッシュ or DynamicAgent作成
        type_key = str(agent_type)
        if type_key in self._registered_agents:
            return self._registered_agents[type_key]

        # 新規DynamicAgent生成
        from agentflow.patterns.deep_agent.da_dynamic import DynamicAgent

        agent = DynamicAgent(
            name=type_key,
            system_prompt=f"You are a {type_key} agent.",
            llm_client=self._llm,
        )
        self._registered_agents[type_key] = agent
        return agent

    def set_default_binding(
        self,
        agent_type: AgentType | str,
        tools: list[str] | None = None,
        skills: list[str] | None = None,
    ) -> None:
        """デフォルトバインディングを設定.

        Args:
            agent_type: Agent種別
            tools: バインドするツール名リスト
            skills: バインドするスキル名リスト
        """
        key = str(agent_type)
        self._default_bindings[key] = {
            "tools": tools or [],
            "skills": skills or [],
        }

    def get_agent(self, agent_type: AgentType | str) -> BaseAgent:
        """Agentを取得（なければ生成）.

        Args:
            agent_type: Agent種別（AgentType or str）

        Returns:
            Agent インスタンス
        """
        if isinstance(agent_type, str):
            agent_type = AgentType(agent_type)

        if agent_type not in self._agents:
            agent_class: type[Any] = self.AGENT_CLASSES.get(agent_type, ExecutionAgent)
            self._agents[agent_type] = agent_class(
                llm_client=self._llm,
                tools=self._tools,
                skills=self._skills,
            )
            _logger.debug("Agent生成: %s", agent_type.value)

        return self._agents[agent_type]

    def create_agent(self, agent_type: AgentType | str) -> BaseAgent:
        """新規Agentインスタンスを生成（並行実行用）.

        Args:
            agent_type: Agent種別

        Returns:
            新規Agent インスタンス
        """
        if isinstance(agent_type, str):
            agent_type = AgentType(agent_type)

        agent_class: type[Any] = self.AGENT_CLASSES.get(agent_type, ExecutionAgent)
        agent = agent_class(
            llm_client=self._llm,
            tools=self._tools,
            skills=self._skills,
        )
        return cast("BaseAgent", agent)

    async def broadcast_message(self, message: AgentMessage) -> None:
        """全Agentにメッセージをブロードキャスト."""
        self._message_bus.append(message)
        for agent in self._agents.values():
            await agent.receive_message(message)

    async def send_message(self, message: AgentMessage) -> None:
        """特定Agentにメッセージを送信."""
        self._message_bus.append(message)
        if message.to_agent == "*":
            await self.broadcast_message(message)
        else:
            try:
                target_type = AgentType(message.to_agent.replace("-agent", ""))
                if target_type in self._agents:
                    await self._agents[target_type].receive_message(message)
            except ValueError:
                _logger.warning("不明なAgent: %s", message.to_agent)

    def get_message_history(self) -> list[AgentMessage]:
        """メッセージ履歴を取得."""
        return list(self._message_bus)

    def clear_agents(self) -> None:
        """全Agentをクリア."""
        self._agents.clear()
        self._message_bus.clear()


# =============================================================================
# エクスポート
# =============================================================================

__all__ = [
    "AgentPool",
    "AnalysisAgent",
    "BaseAgent",
    "ExecutionAgent",
    "PlanningAgent",
    "ReportAgent",
    "ResearchAgent",
    "ReviewAgent",
]

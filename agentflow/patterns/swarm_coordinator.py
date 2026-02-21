"""SwarmCoordinator - マルチAgent協調パターン（Phase 3.1: 2028）.

【機能】
- 動的Agentスウォーム管理
- Agent間メッセージング
- コンセンサスベースの意思決定
- 負荷分散と障害耐性

使用例:
    >>> from agentflow.patterns import SwarmCoordinator
    >>> swarm = SwarmCoordinator()
    >>> swarm.register_agent("analyst", AnalystAgent())
    >>> result = await swarm.execute("市場分析を実施", consensus_threshold=0.7)
"""

from __future__ import annotations

import asyncio
import logging
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any

from agentflow.patterns.coordinator import CoordinationPattern, CoordinatorBase
from agentflow.patterns.shared_context import SharedContext


logger = logging.getLogger(__name__)


class SwarmMessageType(Enum):
    """スウォームメッセージタイプ."""

    REQUEST = "request"  # タスク要求
    RESPONSE = "response"  # タスク応答
    BROADCAST = "broadcast"  # ブロードキャスト
    VOTE = "vote"  # 投票
    CONSENSUS = "consensus"  # コンセンサス結果
    HEARTBEAT = "heartbeat"  # ヘルスチェック


@dataclass
class SwarmMessage:
    """スウォームメッセージ."""

    message_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    message_type: SwarmMessageType = SwarmMessageType.REQUEST
    source_agent: str = ""
    target_agent: str | None = None  # None = ブロードキャスト
    payload: dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=lambda: datetime.now(UTC))
    correlation_id: str | None = None  # リクエスト追跡用


class SwarmAgent(ABC):
    """スウォームAgent抽象基底クラス."""

    @property
    @abstractmethod
    def agent_id(self) -> str:
        """AgentIDを返す."""
        ...

    @property
    def capabilities(self) -> list[str]:
        """Agent能力リストを返す."""
        return []

    @abstractmethod
    async def process_message(self, message: SwarmMessage) -> SwarmMessage | None:
        """メッセージを処理.

        Args:
            message: 受信メッセージ

        Returns:
            応答メッセージ（Noneは応答なし）
        """
        ...

    async def vote(self, proposal: dict[str, Any]) -> float:
        """提案に対する投票（0.0-1.0）.

        Args:
            proposal: 投票対象の提案

        Returns:
            賛成度（0.0=反対、1.0=賛成）
        """
        return 0.5  # デフォルトは中立


@dataclass
class AgentStatus:
    """Agent状態."""

    agent_id: str
    is_active: bool = True
    last_heartbeat: datetime = field(default_factory=lambda: datetime.now(UTC))
    current_load: float = 0.0  # 0.0-1.0
    processed_count: int = 0
    error_count: int = 0


class SwarmCoordinator(CoordinatorBase):
    """スウォームコーディネーター.

    動的なAgent群を協調させ、コンセンサスベースの意思決定を行う。

    Example:
        >>> swarm = SwarmCoordinator()
        >>> swarm.register_agent("agent1", MyAgent())
        >>> result = await swarm.execute("タスクを実行")
    """

    def __init__(
        self,
        shared_context: SharedContext | None = None,
        consensus_threshold: float = 0.6,
        max_parallel: int = 10,
        heartbeat_interval: float = 30.0,
    ) -> None:
        """初期化.

        Args:
            shared_context: 共有コンテキスト
            consensus_threshold: コンセンサス閾値（0.0-1.0）
            max_parallel: 最大並行実行数
            heartbeat_interval: ハートビート間隔（秒）
        """
        self._context = shared_context or SharedContext()
        self._consensus_threshold = consensus_threshold
        self._max_parallel = max_parallel
        self._heartbeat_interval = heartbeat_interval

        self._agents: dict[str, SwarmAgent] = {}
        self._status: dict[str, AgentStatus] = {}
        self._message_queue: asyncio.Queue[SwarmMessage] = asyncio.Queue()
        self._running = False

    @property
    def pattern(self) -> CoordinationPattern:
        """協調パターンを返す."""
        return CoordinationPattern.PEER_TO_PEER

    def register_agent(self, agent_id: str, agent: SwarmAgent) -> None:
        """Agentを登録."""
        self._agents[agent_id] = agent
        self._status[agent_id] = AgentStatus(agent_id=agent_id)
        logger.info(f"Registered swarm agent: {agent_id}")

    def unregister_agent(self, agent_id: str) -> None:
        """Agentを登録解除."""
        if agent_id in self._agents:
            del self._agents[agent_id]
            del self._status[agent_id]
            logger.info(f"Unregistered swarm agent: {agent_id}")

    def get_active_agents(self) -> list[str]:
        """アクティブなAgentリストを取得."""
        return [aid for aid, status in self._status.items() if status.is_active]

    async def broadcast(self, payload: dict[str, Any], source: str = "coordinator") -> list[SwarmMessage]:
        """全Agentにブロードキャスト.

        Args:
            payload: ブロードキャストデータ
            source: 送信元

        Returns:
            応答メッセージリスト
        """
        message = SwarmMessage(
            message_type=SwarmMessageType.BROADCAST,
            source_agent=source,
            target_agent=None,
            payload=payload,
        )

        responses: list[SwarmMessage] = []
        tasks = []

        for agent_id, agent in self._agents.items():
            if self._status[agent_id].is_active:
                tasks.append(agent.process_message(message))

        results = await asyncio.gather(*tasks, return_exceptions=True)

        for result in results:
            if isinstance(result, SwarmMessage):
                responses.append(result)
            elif isinstance(result, Exception):
                logger.warning(f"Agent broadcast error: {result}")

        return responses

    async def request_consensus(
        self,
        proposal: dict[str, Any],
        threshold: float | None = None,
    ) -> tuple[bool, float, dict[str, float]]:
        """コンセンサスを要求.

        Args:
            proposal: 提案内容
            threshold: 承認閾値（Noneの場合はデフォルト）

        Returns:
            (承認されたか, 平均スコア, Agent別スコア)
        """
        threshold = threshold or self._consensus_threshold
        votes: dict[str, float] = {}

        tasks = []
        agent_ids = []

        for agent_id, agent in self._agents.items():
            if self._status[agent_id].is_active:
                tasks.append(agent.vote(proposal))
                agent_ids.append(agent_id)

        results = await asyncio.gather(*tasks, return_exceptions=True)

        for agent_id, result in zip(agent_ids, results, strict=False):
            if isinstance(result, float):
                votes[agent_id] = result
            else:
                logger.warning(f"Vote error from {agent_id}: {result}")
                votes[agent_id] = 0.0

        if not votes:
            return False, 0.0, votes

        avg_score = sum(votes.values()) / len(votes)
        approved = avg_score >= threshold

        logger.info(f"Consensus result: approved={approved}, score={avg_score:.2f}")
        return approved, avg_score, votes

    async def delegate_task(
        self,
        task: dict[str, Any],
        target_capabilities: list[str] | None = None,
    ) -> dict[str, Any]:
        """タスクを適切なAgentに委譲.

        Args:
            task: タスク内容
            target_capabilities: 必要な能力（Noneは任意）

        Returns:
            実行結果
        """
        # 適切なAgentを選択
        candidates = []
        for agent_id, agent in self._agents.items():
            status = self._status[agent_id]
            if not status.is_active:
                continue
            if target_capabilities:
                if not set(target_capabilities).issubset(set(agent.capabilities)):
                    continue
            candidates.append((agent_id, status.current_load))

        if not candidates:
            return {"error": "No suitable agent found", "success": False}

        # 負荷が最も低いAgentを選択
        candidates.sort(key=lambda x: x[1])
        selected_id = candidates[0][0]
        selected_agent = self._agents[selected_id]

        # メッセージ送信
        message = SwarmMessage(
            message_type=SwarmMessageType.REQUEST,
            source_agent="coordinator",
            target_agent=selected_id,
            payload=task,
        )

        # 負荷更新
        self._status[selected_id].current_load = min(1.0, self._status[selected_id].current_load + 0.1)

        try:
            response = await selected_agent.process_message(message)
            self._status[selected_id].processed_count += 1
            self._status[selected_id].current_load = max(0.0, self._status[selected_id].current_load - 0.1)

            if response:
                return response.payload
            return {"success": True, "agent": selected_id}

        except Exception as e:
            self._status[selected_id].error_count += 1
            logger.exception(f"Task delegation failed: {selected_id}")
            return {"error": str(e), "success": False}

    async def execute(self, task: Any, **kwargs: Any) -> Any:
        """タスクを実行.

        Args:
            task: 実行タスク
            **kwargs: 追加パラメータ
                - consensus_required: コンセンサス必要か
                - target_capabilities: 必要能力
                - parallel: 並行実行

        Returns:
            実行結果
        """
        consensus_required = kwargs.get("consensus_required", False)
        target_capabilities = kwargs.get("target_capabilities")
        parallel = kwargs.get("parallel", False)

        task_dict = {"task": task, **kwargs}

        # コンセンサスが必要な場合
        if consensus_required:
            approved, score, votes = await self.request_consensus(task_dict)
            if not approved:
                return {
                    "success": False,
                    "reason": "Consensus not reached",
                    "score": score,
                    "votes": votes,
                }

        # 並行実行
        if parallel:
            responses = await self.broadcast(task_dict)
            return {
                "success": True,
                "responses": [r.payload for r in responses],
                "count": len(responses),
            }

        # 単一Agent委譲
        return await self.delegate_task(task_dict, target_capabilities)

    def get_swarm_status(self) -> dict[str, Any]:
        """スウォーム状態を取得."""
        return {
            "total_agents": len(self._agents),
            "active_agents": len(self.get_active_agents()),
            "agents": {
                aid: {
                    "is_active": s.is_active,
                    "current_load": s.current_load,
                    "processed_count": s.processed_count,
                    "error_count": s.error_count,
                }
                for aid, s in self._status.items()
            },
        }


__all__ = [
    "AgentStatus",
    "SwarmAgent",
    "SwarmCoordinator",
    "SwarmMessage",
    "SwarmMessageType",
]

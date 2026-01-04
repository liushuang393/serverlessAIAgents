# -*- coding: utf-8 -*-
"""ProgressEmitter - 標準化された進捗イベント発射器.

このモジュールは AG-UI プロトコルに準拠した進捗イベントの発射を簡素化します。
PipelineEngine と連携して、自動的に進捗を計算・発射します。

設計原則:
- 自動進捗計算: Agent数から自動的に進捗率を計算
- 標準化イベント: AG-UI プロトコル準拠
- 低結合: AgentBlock に依存せず、メタデータのみ使用

使用例:
    >>> emitter = ProgressEmitter(flow_id="my-flow", total_agents=5)
    >>> async for event in emitter.emit_node_start("dao", "道", "本質分析"):
    ...     yield event
    >>> async for event in emitter.emit_node_complete("dao", "道", "問題タイプ: TRADE_OFF"):
    ...     yield event
"""

import time
from collections.abc import AsyncIterator
from dataclasses import dataclass, field
from typing import Any

from agentflow.protocols.agui_events import (
    AGUIEvent,
    LogEvent,
    NodeCompleteEvent,
    NodeStartEvent,
    ProgressEvent,
)


@dataclass
class AgentMeta:
    """Agent メタデータ（進捗追跡用）.

    Attributes:
        id: Agent ID (例: "dao", "fa")
        name: Agent 名称 (例: "道", "法")
        label: Agent ラベル (例: "本質分析", "戦略選定")
        icon: アイコン (オプション)
    """

    id: str
    name: str
    label: str
    icon: str = ""


@dataclass
class ProgressEmitter:
    """進捗イベント発射器.

    AG-UI プロトコル準拠の進捗イベントを自動生成・発射。
    PipelineEngine と連携して、複数 Agent の進捗を自動追跡。

    Attributes:
        flow_id: フロー実行 ID
        total_agents: Agent 総数
        completed_agents: 完了した Agent 数
        agent_metas: Agent メタデータリスト（順序保持）

    使用例:
        >>> emitter = ProgressEmitter("decision-flow", total_agents=8)
        >>> # 手動発射
        >>> async for event in emitter.emit_node_start("dao", "道", "本質分析"):
        ...     yield event
        >>> # メタデータから発射
        >>> emitter.register_agents([AgentMeta("dao", "道", "本質分析"), ...])
        >>> async for event in emitter.emit_agent_start(0):
        ...     yield event
    """

    flow_id: str
    total_agents: int
    completed_agents: int = 0
    agent_metas: list[AgentMeta] = field(default_factory=list)

    def register_agents(self, metas: list[AgentMeta]) -> None:
        """Agent メタデータを登録.

        Args:
            metas: Agent メタデータリスト（実行順序で）
        """
        self.agent_metas = metas
        self.total_agents = len(metas)

    def get_agent_meta(self, index: int) -> AgentMeta | None:
        """インデックスから Agent メタデータを取得.

        Args:
            index: Agent インデックス (0-based)

        Returns:
            AgentMeta または None
        """
        if 0 <= index < len(self.agent_metas):
            return self.agent_metas[index]
        return None

    def get_agent_meta_by_id(self, agent_id: str) -> AgentMeta | None:
        """ID から Agent メタデータを取得.

        Args:
            agent_id: Agent ID

        Returns:
            AgentMeta または None
        """
        for meta in self.agent_metas:
            if meta.id == agent_id:
                return meta
        return None

    def _calc_overall_progress(self) -> float:
        """全体進捗率を計算.

        Returns:
            進捗率 (0-100)
        """
        if self.total_agents <= 0:
            return 0.0
        return (self.completed_agents / self.total_agents) * 100

    async def emit_node_start(
        self,
        agent_id: str,
        agent_name: str,
        label: str,
        extra_data: dict[str, Any] | None = None,
    ) -> AsyncIterator[AGUIEvent]:
        """ノード開始イベントを発射.

        Args:
            agent_id: Agent ID
            agent_name: Agent名
            label: Agentラベル
            extra_data: 追加データ（オプション）

        Yields:
            NodeStartEvent, LogEvent
        """
        now = time.time()
        data = {"label": label}
        if extra_data:
            data.update(extra_data)

        # NodeStartEvent
        yield NodeStartEvent(
            timestamp=now,
            flow_id=self.flow_id,
            data=data,
            node_id=agent_id,
            node_name=agent_name,
        )

        # ログイベント
        yield LogEvent(
            timestamp=now,
            flow_id=self.flow_id,
            data={},
            level="INFO",
            message=f"{agent_name}({label})の分析を開始...",
            source=agent_id,
        )

    async def emit_node_complete(
        self,
        agent_id: str,
        agent_name: str,
        result_summary: str = "",
        extra_data: dict[str, Any] | None = None,
    ) -> AsyncIterator[AGUIEvent]:
        """ノード完了イベントを発射.

        完了時にcompleted_agentsを自動インクリメント。

        Args:
            agent_id: Agent ID
            agent_name: Agent名
            result_summary: 結果サマリー
            extra_data: 追加データ（オプション）

        Yields:
            NodeCompleteEvent, ProgressEvent
        """
        now = time.time()
        self.completed_agents += 1
        data = {"result_summary": result_summary}
        if extra_data:
            data.update(extra_data)

        # NodeCompleteEvent
        yield NodeCompleteEvent(
            timestamp=now,
            flow_id=self.flow_id,
            data=data,
            node_id=agent_id,
            node_name=agent_name,
        )

        # 全体 ProgressEvent
        yield ProgressEvent(
            timestamp=now,
            flow_id=self.flow_id,
            data={},
            current=self.completed_agents,
            total=self.total_agents,
            percentage=self._calc_overall_progress(),
        )

    async def emit_node_progress(
        self,
        agent_id: str,
        percentage: int,
        message: str = "",
    ) -> AsyncIterator[AGUIEvent]:
        """ノード単位の進捗イベントを発射.

        Args:
            agent_id: Agent ID
            percentage: 進捗率 (0-100)
            message: 進捗メッセージ

        Yields:
            ProgressEvent (node-level)
        """
        now = time.time()
        yield ProgressEvent(
            timestamp=now,
            flow_id=self.flow_id,
            data={
                "node_id": agent_id,
                "message": message,
            },
            current=percentage,
            total=100,
            percentage=float(percentage),
        )

    async def emit_agent_start(self, index: int) -> AsyncIterator[AGUIEvent]:
        """インデックスから Agent 開始イベントを発射.

        Args:
            index: Agent インデックス (0-based)

        Yields:
            NodeStartEvent, LogEvent
        """
        meta = self.get_agent_meta(index)
        if meta:
            async for event in self.emit_node_start(meta.id, meta.name, meta.label):
                yield event

    async def emit_agent_complete(
        self,
        index: int,
        result_summary: str = "",
    ) -> AsyncIterator[AGUIEvent]:
        """インデックスから Agent 完了イベントを発射.

        Args:
            index: Agent インデックス (0-based)
            result_summary: 結果サマリー

        Yields:
            NodeCompleteEvent, ProgressEvent
        """
        meta = self.get_agent_meta(index)
        if meta:
            async for event in self.emit_node_complete(
                meta.id, meta.name, result_summary
            ):
                yield event

    def reset(self) -> None:
        """進捗をリセット."""
        self.completed_agents = 0


__all__ = [
    "AgentMeta",
    "ProgressEmitter",
]


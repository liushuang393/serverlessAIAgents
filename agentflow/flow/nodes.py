# -*- coding: utf-8 -*-
"""Flowノード定義.

フローグラフ内の各種ノード型を定義:
- FlowNode: 抽象基底クラス
- AgentNode: Agent実行ノード
- GateNode: ゲートノード（条件インターセプト）
- ParallelNode: 並列実行ノード
- ReviewNode: レビューノード（PASS/REVISE/REJECT）

設計原則:
- 単一責任：各ノードは1つのロジックのみを処理
- 開放閉鎖：継承による拡張
"""

from __future__ import annotations

import asyncio
import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, Callable

from agentflow.flow.types import (
    AgentProtocol,
    NextAction,
    NodeResult,
    NodeType,
    ReviewVerdict,
)

if TYPE_CHECKING:
    from agentflow.flow.context import FlowContext


@dataclass
class FlowNode(ABC):
    """ノード基底クラス."""

    id: str
    name: str
    node_type: NodeType = field(default=NodeType.AGENT)
    label: str = ""
    icon: str = ""

    def __post_init__(self) -> None:
        self._logger = logging.getLogger(f"agentflow.flow.node.{self.id}")
        if not self.label:
            self.label = self.name

    @abstractmethod
    async def execute(self, ctx: "FlowContext") -> NodeResult:
        """ノードを実行."""
        ...


@dataclass
class AgentNode(FlowNode):
    """Agent実行ノード."""

    agent: AgentProtocol | None = None
    input_mapper: Callable[["FlowContext"], dict[str, Any]] | None = None

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.AGENT)

    async def execute(self, ctx: "FlowContext") -> NodeResult:
        """Agentを実行."""
        try:
            inputs = self.input_mapper(ctx) if self.input_mapper else ctx.get_inputs()
            self._logger.debug(f"Agentを実行: {self.id}")

            result = await self.agent.run(inputs)
            ctx.set_result(self.id, result)

            return NodeResult(success=True, data=result, action=NextAction.CONTINUE)
        except Exception as e:
            self._logger.error(f"Agent実行失敗: {e}")
            return NodeResult(
                success=False,
                data={"error": str(e), "error_type": type(e).__name__},
                action=NextAction.STOP,
            )


@dataclass
class GateNode(FlowNode):
    """ゲートノード：条件を満たさない場合は早期リターン."""

    agent: AgentProtocol | None = None
    check: Callable[[dict[str, Any]], bool] | None = None
    on_fail: Callable[["FlowContext"], dict[str, Any] | Any] | None = None

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.GATE)

    async def execute(self, ctx: "FlowContext") -> NodeResult:
        """ゲートチェックを実行."""
        try:
            inputs = ctx.get_inputs()
            result = await self.agent.run(inputs)
            ctx.set_result(self.id, result)

            # 条件をチェック
            if self.check:
                passed = self.check(result)
            else:
                passed = result.get("proceed", result.get("is_acceptable", True))

            if passed:
                self._logger.debug(f"ゲート {self.id} 通過")
                return NodeResult(success=True, data=result, action=NextAction.CONTINUE)

            self._logger.info(f"ゲート {self.id} インターセプト")
            early_data = self.on_fail(ctx) if self.on_fail else None
            return NodeResult(
                success=True,
                data=result,
                action=NextAction.EARLY_RETURN,
                early_return_data=early_data,
            )
        except Exception as e:
            self._logger.error(f"ゲート実行失敗: {e}")
            return NodeResult(success=False, data={"error": str(e)}, action=NextAction.STOP)


@dataclass
class ParallelNode(FlowNode):
    """並列実行ノード：複数Agentを同時実行."""

    agents: list[tuple[str, AgentProtocol]] = field(default_factory=list)
    input_mappers: dict[str, Callable[["FlowContext"], dict[str, Any]]] = field(default_factory=dict)

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.PARALLEL)

    async def execute(self, ctx: "FlowContext") -> NodeResult:
        """すべてのAgentを並列実行."""
        try:
            async def run_one(agent_id: str, agent: AgentProtocol) -> tuple[str, dict]:
                mapper = self.input_mappers.get(agent_id)
                inputs = mapper(ctx) if mapper else ctx.get_inputs()
                result = await agent.run(inputs)
                ctx.set_result(agent_id, result)
                return agent_id, result

            tasks = [run_one(aid, agent) for aid, agent in self.agents]
            results_list = await asyncio.gather(*tasks, return_exceptions=True)

            combined: dict[str, Any] = {}
            for item in results_list:
                if isinstance(item, Exception):
                    self._logger.error(f"並列実行失敗: {item}")
                else:
                    aid, res = item
                    combined[aid] = res

            return NodeResult(success=True, data=combined, action=NextAction.CONTINUE)
        except Exception as e:
            self._logger.error(f"並列ノード失敗: {e}")
            return NodeResult(success=False, data={"error": str(e)}, action=NextAction.STOP)


@dataclass
class ReviewNode(FlowNode):
    """レビューノード：判定結果に基づいてPASS/REVISE/REJECTを決定."""

    agent: AgentProtocol | None = None
    input_mapper: Callable[["FlowContext"], dict[str, Any]] | None = None
    on_pass: Callable[["FlowContext"], dict[str, Any]] | None = None
    on_reject: Callable[["FlowContext"], dict[str, Any]] | None = None
    retry_from: str | None = None  # REVISE時にロールバックするノード
    max_revisions: int = 2
    verdict_key: str = "overall_verdict"  # 判定結果フィールド名

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.REVIEW)

    async def execute(self, ctx: "FlowContext") -> NodeResult:
        """レビューを実行."""
        try:
            inputs = self.input_mapper(ctx) if self.input_mapper else ctx.get_all_results()
            result = await self.agent.run(inputs)
            ctx.set_result(self.id, result)

            # 判定を取得
            verdict_raw = result.get(self.verdict_key, "PASS")
            if isinstance(verdict_raw, ReviewVerdict):
                verdict = verdict_raw
            else:
                verdict = ReviewVerdict(str(verdict_raw).upper())

            self._logger.info(f"レビュー判定: {verdict.value}")

            if verdict == ReviewVerdict.PASS:
                final_data = self.on_pass(ctx) if self.on_pass else result
                return NodeResult(success=True, data=final_data, action=NextAction.STOP)

            if verdict == ReviewVerdict.REJECT:
                reject_data = self.on_reject(ctx) if self.on_reject else result
                return NodeResult(
                    success=True,
                    data=reject_data,
                    action=NextAction.EARLY_RETURN,
                    early_return_data={"status": "rejected", "reason": result.get("findings")},
                )

            # REVISE
            if ctx.revision_count >= self.max_revisions:
                self._logger.warning(f"最大リビジョン回数に達しました: {self.max_revisions}")
                return NodeResult(
                    success=True,
                    data=result,
                    action=NextAction.STOP,
                    early_return_data={"status": "max_revisions_reached"},
                )

            ctx.increment_revision()
            return NodeResult(
                success=True,
                data=result,
                action=NextAction.RETRY_FROM,
                retry_from=self.retry_from,
            )
        except Exception as e:
            self._logger.error(f"レビュー実行失敗: {e}")
            return NodeResult(success=False, data={"error": str(e)}, action=NextAction.STOP)


__all__ = ["FlowNode", "AgentNode", "GateNode", "ParallelNode", "ReviewNode"]

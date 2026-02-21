"""Flowノード定義.

フローグラフ内の各種ノード型を定義:
- FlowNode: 抽象基底クラス
- AgentNode: Agent実行ノード
- GateNode: ゲートノード（条件インターセプト）
- ParallelNode: 並列実行ノード
- ReviewNode: レビューノード（PASS/REVISE/COACH）

設計原則:
- 単一責任：各ノードは1つのロジックのみを処理
- 開放閉鎖：継承による拡張
"""

from __future__ import annotations

import asyncio
import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from agentflow.core.type_safe import safe_enum
from agentflow.flow.types import (
    AgentProtocol,
    NextAction,
    NodeResult,
    NodeType,
    ReviewVerdict,
)


# 後方互換: LLM が旧 "REJECT" を返した場合 → COACH にマッピング
_VERDICT_ALIASES: dict[str, str] = {"REJECT": "COACH"}


if TYPE_CHECKING:
    from collections.abc import Callable

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
    async def execute(self, ctx: FlowContext) -> NodeResult:
        """ノードを実行."""
        ...


@dataclass
class AgentNode(FlowNode):
    """Agent実行ノード."""

    agent: AgentProtocol | None = None
    input_mapper: Callable[[FlowContext], dict[str, Any]] | None = None

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.AGENT)

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """Agentを実行."""
        try:
            inputs = self.input_mapper(ctx) if self.input_mapper else ctx.get_inputs()
            self._logger.debug(f"Agentを実行: {self.id}")

            agent = self.agent
            if agent is None:
                msg = "Agent is not configured"
                raise RuntimeError(msg)
            result = await agent.run(inputs)
            ctx.set_result(self.id, result)

            return NodeResult(success=True, data=result, action=NextAction.CONTINUE)
        except Exception as e:
            self._logger.exception(f"Agent実行失敗（後続ノードは継続）: {e}")
            # エラー情報を context に保存し、後続ノードが実行できるよう CONTINUE を返す
            error_data: dict[str, Any] = {
                "error": str(e),
                "error_type": type(e).__name__,
            }
            ctx.set_result(self.id, error_data)
            return NodeResult(
                success=False,
                data=error_data,
                action=NextAction.CONTINUE,
            )


@dataclass
class GateNode(FlowNode):
    """ゲートノード：条件を満たさない場合は早期リターン."""

    agent: AgentProtocol | None = None
    input_mapper: Callable[[FlowContext], dict[str, Any]] | None = None
    check: Callable[[dict[str, Any]], bool] | None = None
    on_fail: Callable[[FlowContext], dict[str, Any] | Any] | None = None

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.GATE)

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """ゲートチェックを実行."""
        try:
            # input_mapperがあれば使用、なければ元の入力を使用
            inputs = self.input_mapper(ctx) if self.input_mapper else ctx.get_inputs()
            agent = self.agent
            if agent is None:
                msg = "Agent is not configured"
                raise RuntimeError(msg)
            result = await agent.run(inputs)
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
            self._logger.exception(f"ゲート実行失敗: {e}")
            return NodeResult(success=False, data={"error": str(e)}, action=NextAction.STOP)


@dataclass
class ParallelNode(FlowNode):
    """並列実行ノード：複数Agentを同時実行."""

    agents: list[tuple[str, AgentProtocol]] = field(default_factory=list)
    input_mappers: dict[str, Callable[[FlowContext], dict[str, Any]]] = field(default_factory=dict)

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.PARALLEL)

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """すべてのAgentを並列実行."""
        try:

            async def run_one(agent_id: str, agent: AgentProtocol) -> tuple[str, dict[str, Any]]:
                mapper = self.input_mappers.get(agent_id)
                inputs = mapper(ctx) if mapper else ctx.get_inputs()
                result = await agent.run(inputs)
                ctx.set_result(agent_id, result)
                return agent_id, result

            tasks = [run_one(aid, agent) for aid, agent in self.agents]
            results_list = await asyncio.gather(*tasks, return_exceptions=True)

            combined: dict[str, Any] = {}
            for item in results_list:
                if isinstance(item, BaseException):
                    self._logger.error(f"並列実行失敗: {item}")
                else:
                    aid, res = item
                    combined[aid] = res

            return NodeResult(success=True, data=combined, action=NextAction.CONTINUE)
        except Exception as e:
            self._logger.exception(f"並列ノード失敗: {e}")
            return NodeResult(success=False, data={"error": str(e)}, action=NextAction.STOP)


@dataclass
class ReviewNode(FlowNode):
    """レビューノード：判定結果に基づいてPASS/REVISE/COACHを決定."""

    agent: AgentProtocol | None = None
    input_mapper: Callable[[FlowContext], dict[str, Any]] | None = None
    on_pass: Callable[[FlowContext], dict[str, Any]] | None = None
    on_coach: Callable[[FlowContext], dict[str, Any]] | None = None
    retry_from: str | None = None  # REVISE時にロールバックするノード
    max_revisions: int = 2
    verdict_key: str = "overall_verdict"  # 判定結果フィールド名

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.REVIEW)

    @staticmethod
    def _extract_finding_summary(findings: list[Any], max_items: int = 3) -> str | None:
        """所見リストからユーザー向け重大課題サマリーを生成."""
        if not findings:
            return None

        summaries: list[str] = []
        for finding in findings:
            if not isinstance(finding, dict):
                continue

            severity = str(finding.get("severity", "")).upper()
            is_critical = severity.endswith("CRITICAL")
            if not is_critical:
                continue

            raw_text = finding.get("description") or finding.get("failure_point") or finding.get("impact_scope") or ""
            text = str(raw_text).strip()
            if text and text not in summaries:
                summaries.append(text)
            if len(summaries) >= max_items:
                break

        if not summaries:
            return None

        return "重大課題: " + " / ".join(summaries)

    def _build_coach_payload(self, result: dict[str, Any]) -> dict[str, Any]:
        """Review COACH 時のコーチング情報を生成."""
        findings_raw = result.get("findings", [])
        findings = findings_raw if isinstance(findings_raw, list) else []

        warnings_raw = result.get("final_warnings", [])
        final_warnings = warnings_raw if isinstance(warnings_raw, list) else []

        default_reason = (
            f"ReviewAgentが{len(findings)}件の重要所見を検出" if findings else "ReviewAgentが実行計画の重大な課題を検出"
        )
        rejection_reason = str(result.get("rejection_reason") or result.get("reason") or default_reason)

        message_raw = result.get("rejection_message")
        findings_summary = self._extract_finding_summary(findings)
        rejection_message = (
            message_raw
            if isinstance(message_raw, str) and message_raw.strip()
            else findings_summary or "重大課題が検出されました。findings を確認してください。"
        )

        suggest_raw = result.get("suggested_rephrase")
        suggested_rephrase = (
            suggest_raw
            if isinstance(suggest_raw, str) and suggest_raw.strip()
            else "検証で指摘された所見（findings）を修正して再実行してください。"
        )

        return {
            "status": "coach",
            "stage": self.id,
            "source": "review",
            "verdict": "COACH",
            "rejection_message": rejection_message,
            "rejection_reason": rejection_reason,
            "suggested_rephrase": suggested_rephrase,
            "findings": findings,
            "final_warnings": final_warnings,
        }

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """レビューを実行."""
        try:
            inputs = self.input_mapper(ctx) if self.input_mapper else ctx.get_all_results()
            agent = self.agent
            if agent is None:
                msg = "Agent is not configured"
                raise RuntimeError(msg)
            result = await agent.run(inputs)
            ctx.set_result(self.id, result)

            # 判定を取得（防御的パーシング: 未知値はクラッシュせずフォールバック）
            verdict_raw = result.get(self.verdict_key, "PASS")
            if isinstance(verdict_raw, ReviewVerdict):
                verdict = verdict_raw
            else:
                # LLMが "REVIEWVERDICT.REVISE" のような形式で返す場合に対応
                verdict_str = str(verdict_raw).strip().upper()
                if verdict_str.startswith("REVIEWVERDICT."):
                    verdict_str = verdict_str.replace("REVIEWVERDICT.", "")
                verdict = safe_enum(
                    ReviewVerdict,
                    verdict_str,
                    ReviewVerdict.REVISE,
                    aliases=_VERDICT_ALIASES,
                )

            self._logger.info(f"レビュー判定: {verdict.value}")

            if verdict == ReviewVerdict.PASS:
                final_data = self.on_pass(ctx) if self.on_pass else result
                return NodeResult(success=True, data=final_data, action=NextAction.STOP)

            if verdict == ReviewVerdict.COACH:
                # コーチング型改善指導: 即終了せず、レポート生成を継続
                coach_data = self.on_coach(ctx) if self.on_coach else result
                return NodeResult(
                    success=True,
                    data=coach_data,
                    action=NextAction.STOP,
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
            self._logger.exception(f"レビュー実行失敗: {e}")
            return NodeResult(success=False, data={"error": str(e)}, action=NextAction.STOP)


__all__ = ["AgentNode", "FlowNode", "GateNode", "ParallelNode", "ReviewNode"]

"""Flow実行エンジン.

フローの実際の実行を担当、以下を含む:
- ノードの順次実行
- 条件分岐の処理（Gateインターセプト）
- ロールバックロジックの処理（REVISE）
- 進捗イベントの発行
- ミドルウェアチェーン（ガバナンス・監査・安全チェック）

設計原則:
- ステートマシンパターン：明確な状態遷移
- 中断可能：早期リターンをサポート
- 観測可能：イベントを自動発行
- ミドルウェア拡張：FlowMiddleware で実行前後に介入可能
"""

from __future__ import annotations

import logging
import time
from typing import TYPE_CHECKING, Any

from contracts.flow.contracts import FlowMiddleware, MiddlewareDecision, MiddlewareResult
from kernel.flow.context import FlowContext
from kernel.flow.progress import ProgressTracker
from kernel.flow.types import NextAction, NodeResult, NodeType
from kernel.protocols.agui_events import (
    FlowCompleteEvent,
    FlowErrorEvent,
    NodeCompleteEvent,
    NodeErrorEvent,
    to_legacy_dict,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator, Sequence

    from kernel.flow.graph import FlowGraph


class FlowExecutor:
    """フロー実行エンジン.

    Example:
        >>> executor = FlowExecutor(graph, config)
        >>> result = await executor.execute({"question": "..."})
        >>> # ミドルウェア付き実行
        >>> executor = FlowExecutor(graph, middlewares=[governance_mw, audit_mw])
        >>> result = await executor.execute({"question": "..."})
    """

    def __init__(
        self,
        graph: FlowGraph,
        *,
        enable_progress: bool = True,
        max_revisions: int = 2,
        honor_termination: bool = False,
        middlewares: Sequence[FlowMiddleware] | None = None,
    ) -> None:
        """初期化.

        Args:
            graph: フローグラフ
            enable_progress: 進捗追跡を有効化するか
            max_revisions: 最大リビジョン回数
            honor_termination: 終了判定を尊重して中断するか
            middlewares: ノード実行前後に介入するミドルウェアチェーン
        """
        self._logger = logging.getLogger("kernel.flow.executor")
        self._graph = graph
        self._enable_progress = enable_progress
        self._max_revisions = max_revisions
        self._honor_termination = honor_termination
        self._middlewares: list[FlowMiddleware] = list(middlewares or [])

    def add_middleware(self, middleware: FlowMiddleware) -> None:
        """ミドルウェアを追加.

        Args:
            middleware: 追加するミドルウェア
        """
        self._middlewares.append(middleware)

    async def _run_before_middlewares(
        self,
        node_id: str,
        node_name: str,
        inputs: dict[str, Any],
    ) -> MiddlewareResult:
        """全ミドルウェアの before_node を順次実行.

        最初に DENY を返したミドルウェアで停止する。
        """
        for mw in self._middlewares:
            try:
                result = await mw.before_node(node_id, node_name, inputs)
                if result.decision != MiddlewareDecision.ALLOW:
                    self._logger.info(
                        "ミドルウェア %s がノード %s を %s: %s",
                        mw.name,
                        node_name,
                        result.decision,
                        result.reason,
                    )
                    return result
            except Exception:
                self._logger.exception("ミドルウェア %s の before_node で例外", mw.name)
        return MiddlewareResult(decision=MiddlewareDecision.ALLOW)

    async def _run_after_middlewares(
        self,
        node_id: str,
        node_name: str,
        result_data: dict[str, Any],
        success: bool,
    ) -> MiddlewareResult:
        """全ミドルウェアの after_node を順次実行.

        modified_result があれば後続ミドルウェアにも伝播する。
        """
        current_data = result_data
        final_metadata: dict[str, Any] = {}
        for mw in self._middlewares:
            try:
                mw_result = await mw.after_node(node_id, node_name, current_data, success)
                final_metadata.update(mw_result.metadata)
                if mw_result.modified_result is not None:
                    current_data = mw_result.modified_result
                if mw_result.decision == MiddlewareDecision.DENY:
                    self._logger.warning(
                        "ミドルウェア %s がノード %s の結果を拒否: %s",
                        mw.name,
                        node_name,
                        mw_result.reason,
                    )
                    return MiddlewareResult(
                        decision=MiddlewareDecision.DENY,
                        reason=mw_result.reason,
                        metadata=final_metadata,
                    )
            except Exception:
                self._logger.exception("ミドルウェア %s の after_node で例外", mw.name)
        return MiddlewareResult(
            decision=MiddlewareDecision.ALLOW,
            modified_result=current_data if current_data is not result_data else None,
            metadata=final_metadata,
        )

    async def execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """フローを同期実行.

        Args:
            inputs: 入力データ

        Returns:
            最終結果
        """
        ctx = FlowContext()
        ctx.set_inputs(inputs)

        result: dict[str, Any] = {}
        async for event in self._execute_internal(ctx):
            if event.get("type") == "flow_complete":
                result = event.get("result", {})
            elif event.get("type") == "early_return":
                result = event.get("data", {})

        return result

    async def execute_stream(
        self,
        inputs: dict[str, Any],
    ) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行、イベントを発行.

        Args:
            inputs: 入力データ

        Yields:
            AG-UIプロトコルイベント
        """
        ctx = FlowContext()
        ctx.set_inputs(inputs)

        async for event in self._execute_internal(ctx):
            yield event

    async def _execute_internal(
        self,
        ctx: FlowContext,
    ) -> AsyncIterator[dict[str, Any]]:
        """内部実行ロジック."""
        tracker = ProgressTracker(self._graph, ctx.flow_id) if self._enable_progress else None

        # Flow開始
        if tracker:
            yield tracker.on_flow_start(ctx.inputs)

        try:
            # 最初のノードから開始
            nodes = list(self._graph)
            idx = 0

            while idx < len(nodes):
                node = nodes[idx]
                ctx.set_current_node(node.id)

                # ノード開始イベント
                if tracker:
                    yield tracker.on_node_start(node)

                # ミドルウェア before_node チェック
                if self._middlewares:
                    mw_before = await self._run_before_middlewares(
                        node.id,
                        node.name,
                        ctx.inputs,
                    )
                    if mw_before.decision == MiddlewareDecision.DENY:
                        # ガバナンスにより拒否 → ノードをスキップしエラーイベント発行
                        deny_msg = f"ミドルウェアにより拒否: {mw_before.reason}"
                        if tracker:
                            yield tracker.on_node_error(node, deny_msg, "GovernanceDenied")
                        else:
                            error_event = NodeErrorEvent(
                                timestamp=time.time(),
                                flow_id=ctx.flow_id,
                                node_id=node.id,
                                node_name=node.name,
                                error_message=deny_msg,
                                error_type="GovernanceDenied",
                            )
                            yield to_legacy_dict(error_event)
                        yield {
                            "type": "middleware_denied",
                            "data": {
                                "node_id": node.id,
                                "reason": mw_before.reason,
                                "metadata": mw_before.metadata,
                            },
                        }
                        idx += 1
                        continue
                    if mw_before.decision == MiddlewareDecision.APPROVAL_REQUIRED:
                        # fail-closed: 承認なしでは実行しない
                        yield {
                            "type": "approval_required",
                            "data": {
                                "node_id": node.id,
                                "node_name": node.name,
                                "reason": mw_before.reason,
                                "metadata": mw_before.metadata,
                            },
                        }
                        # ノードをスキップ（承認済みで再実行される前提）
                        deny_msg = f"承認が必要: {mw_before.reason}"
                        if tracker:
                            yield tracker.on_node_error(node, deny_msg, "ApprovalRequired")
                        idx += 1
                        continue

                # ノードを実行
                result = await node.execute(ctx)

                # ミドルウェア after_node チェック
                if self._middlewares and result.success:
                    mw_after = await self._run_after_middlewares(
                        node.id,
                        node.name,
                        result.data,
                        result.success,
                    )
                    if mw_after.decision == MiddlewareDecision.DENY:
                        # 実行後ガバナンスにより結果を拒否
                        deny_msg = f"ミドルウェアにより結果拒否: {mw_after.reason}"
                        if tracker:
                            yield tracker.on_node_error(node, deny_msg, "GovernanceDenied")
                        else:
                            error_event = NodeErrorEvent(
                                timestamp=time.time(),
                                flow_id=ctx.flow_id,
                                node_id=node.id,
                                node_name=node.name,
                                error_message=deny_msg,
                                error_type="GovernanceDenied",
                            )
                            yield to_legacy_dict(error_event)
                        idx += 1
                        continue
                    if mw_after.modified_result is not None:
                        # ミドルウェアが結果を変換した場合、NodeResultを再構築
                        result = NodeResult(
                            success=result.success,
                            data=mw_after.modified_result,
                            action=result.action,
                            retry_from=result.retry_from,
                            early_return_data=result.early_return_data,
                        )

                # ノード失敗時はエラーイベントを発行
                if not result.success:
                    error_msg = result.data.get("error", "Unknown error")
                    error_type = result.data.get("error_type", "AgentError")
                    if tracker:
                        yield tracker.on_node_error(node, error_msg, error_type)
                    else:
                        error_event = NodeErrorEvent(
                            timestamp=time.time(),
                            flow_id=ctx.flow_id,
                            node_id=node.id,
                            node_name=node.name,
                            error_message=error_msg,
                            error_type=error_type,
                        )
                        yield to_legacy_dict(error_event)
                    # 失敗でも CONTINUE なら後続ノードを実行するためここでは return しない
                elif result.action in (NextAction.CONTINUE, NextAction.STOP) or (
                    result.action == NextAction.EARLY_RETURN and not self._honor_termination
                ):
                    # ノード正常完了時のみ完了イベントを発行
                    # EARLY_RETURN（Gate失敗）や RETRY_FROM（REVISE）では発行しない
                    if tracker:
                        yield tracker.on_node_complete(node, result.data, success=True)
                    else:
                        complete_event = NodeCompleteEvent(
                            timestamp=time.time(),
                            flow_id=ctx.flow_id,
                            node_id=node.id,
                            node_name=node.name,
                            data={"success": True},
                        )
                        yield to_legacy_dict(complete_event)

                # 実行結果を処理
                if result.action == NextAction.CONTINUE:
                    idx += 1
                    continue

                if result.action == NextAction.EARLY_RETURN and not self._honor_termination:
                    if node.node_type == NodeType.GATE:
                        yield {
                            "type": "gate_rejected",
                            "data": {
                                **(result.early_return_data or result.data or {}),
                                "non_blocking": True,
                            },
                        }
                    if node.node_type == NodeType.REVIEW:
                        er_data = result.early_return_data or result.data or {}
                        yield {
                            "type": "review_verdict",
                            "data": {"verdict": er_data.get("verdict", "COACH")},
                        }
                    idx += 1
                    continue

                if result.action == NextAction.STOP:
                    # Review PASS の場合は review_verdict イベントを発行
                    if node.node_type == NodeType.REVIEW and result.success:
                        review_data = result.data if isinstance(result.data, dict) else {}
                        verdict = str(
                            review_data.get("overall_verdict") or review_data.get("verdict") or "PASS"
                        ).upper()
                        if verdict == "REJECT":
                            verdict = "COACH"
                        yield {
                            "type": "review_verdict",
                            "data": {"verdict": verdict},
                        }

                    if result.success:
                        # 正常終了 - 全ノードの結果を返す
                        final_result = ctx.get_all_results()
                        if tracker:
                            yield tracker.on_flow_complete(final_result)
                        else:
                            flow_complete_event = FlowCompleteEvent(
                                timestamp=time.time(),
                                flow_id=ctx.flow_id,
                                result=final_result,
                                include_result=True,
                            )
                            yield to_legacy_dict(flow_complete_event)
                    else:
                        # エラー終了
                        error_msg = result.data.get("error", "Agent execution failed")
                        if tracker:
                            yield tracker.on_flow_error(Exception(error_msg))
                        else:
                            flow_error_event = FlowErrorEvent(
                                timestamp=time.time(),
                                flow_id=ctx.flow_id,
                                error_message=error_msg,
                                error_type="AgentError",
                            )
                            yield to_legacy_dict(flow_error_event)
                    return

                if result.action == NextAction.EARLY_RETURN:
                    # Review の場合は review_verdict イベントを発行
                    if node.node_type == NodeType.REVIEW:
                        er_data = result.early_return_data or {}
                        yield {
                            "type": "review_verdict",
                            "data": {"verdict": er_data.get("verdict", "COACH")},
                        }

                    # 早期リターン（Gateインターセプト）
                    yield {
                        "type": "early_return",
                        "data": result.early_return_data or result.data,
                    }
                    return

                if result.action == NextAction.RETRY_FROM:
                    # REVISEロールバック
                    retry_id = result.retry_from
                    if not retry_id:
                        self._logger.error("RETRY_FROMでロールバックノードが指定されていません")
                        break

                    # ロールバックノードのインデックスを検索
                    retry_idx = self._graph.get_node_index(retry_id)
                    if retry_idx < 0:
                        self._logger.error(f"ロールバックノードが見つかりません: {retry_id}")
                        break

                    # REVISE イベントを発行（フロントエンドにロールバックを通知）
                    # retry_from はノードのインデックス（フロントエンドで使用）
                    yield {
                        "type": "review_verdict",
                        "data": {"verdict": "REVISE"},
                    }
                    yield {
                        "type": "revise",
                        "data": {"retry_from": retry_idx, "retry_node_id": retry_id},
                    }

                    # ロールバックノードから開始する結果をクリア
                    ctx.clear_results_from(retry_id)
                    if tracker:
                        tracker.reset()

                    self._logger.info(f"REVISEロールバック: {retry_id} (第{ctx.revision_count}回)")
                    idx = retry_idx
                    continue

            # すべてのノードを正常に実行完了
            final_result = ctx.get_all_results()
            if tracker:
                yield tracker.on_flow_complete(final_result)
            else:
                yield {
                    "event_type": "flow.complete",
                    "type": "flow_complete",  # 後方互換
                    "result": final_result,
                }

        except Exception as e:
            self._logger.exception(f"フロー実行失敗: {e}")
            if tracker:
                yield tracker.on_flow_error(e)
            else:
                yield {
                    "event_type": "flow.error",
                    "type": "flow_error",  # 後方互換
                    "error_message": str(e),
                    "message": str(e),  # フロントエンド fallback
                }
            raise


__all__ = ["FlowExecutor"]

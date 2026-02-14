"""Flow実行エンジン.

フローの実際の実行を担当、以下を含む:
- ノードの順次実行
- 条件分岐の処理（Gateインターセプト）
- ロールバックロジックの処理（REVISE）
- 進捗イベントの発行

設計原則:
- ステートマシンパターン：明確な状態遷移
- 中断可能：早期リターンをサポート
- 観測可能：イベントを自動発行
"""

from __future__ import annotations

import logging
import time
from typing import TYPE_CHECKING, Any

from agentflow.flow.context import FlowContext
from agentflow.flow.progress import ProgressTracker
from agentflow.flow.types import NextAction, NodeType
from agentflow.protocols.agui_events import (
    FlowCompleteEvent,
    FlowErrorEvent,
    NodeCompleteEvent,
    NodeErrorEvent,
    to_legacy_dict,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from agentflow.flow.graph import FlowGraph


class FlowExecutor:
    """フロー実行エンジン.

    Example:
        >>> executor = FlowExecutor(graph, config)
        >>> result = await executor.execute({"question": "..."})
        >>> # またはストリーム実行
        >>> async for event in executor.execute_stream(inputs):
        ...     print(event)
    """

    def __init__(
        self,
        graph: FlowGraph,
        *,
        enable_progress: bool = True,
        max_revisions: int = 2,
    ) -> None:
        """初期化.

        Args:
            graph: フローグラフ
            enable_progress: 進捗追跡を有効化するか
            max_revisions: 最大リビジョン回数
        """
        self._logger = logging.getLogger("agentflow.flow.executor")
        self._graph = graph
        self._enable_progress = enable_progress
        self._max_revisions = max_revisions

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

                # ノードを実行
                result = await node.execute(ctx)

                # ノード失敗時はエラーイベントを発行
                if not result.success:
                    error_msg = result.data.get("error", "Unknown error")
                    error_type = result.data.get("error_type", "AgentError")
                    if tracker:
                        yield tracker.on_node_error(node, error_msg, error_type)
                    else:
                        event = NodeErrorEvent(
                            timestamp=time.time(),
                            flow_id=ctx.flow_id,
                            node_id=node.id,
                            node_name=node.name,
                            error_message=error_msg,
                            error_type=error_type,
                        )
                        yield to_legacy_dict(event)
                    # 失敗でも CONTINUE なら後続ノードを実行するためここでは return しない
                elif result.action in (NextAction.CONTINUE, NextAction.STOP):
                    # ノード正常完了時のみ完了イベントを発行
                    # EARLY_RETURN（Gate失敗）や RETRY_FROM（REVISE）では発行しない
                    if tracker:
                        yield tracker.on_node_complete(node, result.data, success=True)
                    else:
                        event = NodeCompleteEvent(
                            timestamp=time.time(),
                            flow_id=ctx.flow_id,
                            node_id=node.id,
                            node_name=node.name,
                            data={"success": True},
                        )
                        yield to_legacy_dict(event)

                # 実行結果を処理
                if result.action == NextAction.CONTINUE:
                    idx += 1
                    continue

                if result.action == NextAction.STOP:
                    # Review PASS の場合は review_verdict イベントを発行
                    if node.node_type == NodeType.REVIEW and result.success:
                        yield {
                            "type": "review_verdict",
                            "data": {"verdict": "PASS"},
                        }

                    if result.success:
                        # 正常終了 - 全ノードの結果を返す
                        final_result = ctx.get_all_results()
                        if tracker:
                            yield tracker.on_flow_complete(final_result)
                        else:
                            event = FlowCompleteEvent(
                                timestamp=time.time(),
                                flow_id=ctx.flow_id,
                                result=final_result,
                                include_result=True,
                            )
                            yield to_legacy_dict(event)
                    else:
                        # エラー終了
                        error_msg = result.data.get("error", "Agent execution failed")
                        if tracker:
                            yield tracker.on_flow_error(Exception(error_msg))
                        else:
                            event = FlowErrorEvent(
                                timestamp=time.time(),
                                flow_id=ctx.flow_id,
                                error_message=error_msg,
                                error_type="AgentError",
                            )
                            yield to_legacy_dict(event)
                    return

                if result.action == NextAction.EARLY_RETURN:
                    # Review REJECT の場合は review_verdict イベントを発行
                    if node.node_type == NodeType.REVIEW:
                        yield {
                            "type": "review_verdict",
                            "data": {"verdict": "REJECT"},
                        }

                    # 早期リターン（GateインターセプトまたはREJECT）
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

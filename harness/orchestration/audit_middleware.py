"""監査ミドルウェア - フロー実行の全ステップを記録.

FlowMiddleware プロトコルを実装し、ノード実行の開始・完了・エラーを
AuditEvent として記録する。
"""

from __future__ import annotations

import logging
import time
from typing import Any

from contracts.flow.contracts import MiddlewareDecision, MiddlewareResult
from harness.governance.audit import AuditEvent, AuditLogger

_logger = logging.getLogger(__name__)


class AuditMiddleware:
    """監査ミドルウェア.

    全ノード実行の開始・完了・エラーを AuditEvent として記録する。
    判定には一切関与せず、常に ALLOW を返す（記録専用）。

    Attributes:
        name: ミドルウェア名（ログ・トレース用）
    """

    def __init__(
        self,
        audit_logger: AuditLogger,
        *,
        flow_id: str | None = None,
        run_id: str | None = None,
    ) -> None:
        """初期化.

        Args:
            audit_logger: 監査ロガー
            flow_id: フローID（トレース用）
            run_id: 実行ID（トレース用）
        """
        self._audit_logger = audit_logger
        self._flow_id = flow_id
        self._run_id = run_id
        self._start_times: dict[str, float] = {}

    @property
    def name(self) -> str:
        """ミドルウェア名."""
        return "AuditMiddleware"

    async def before_node(
        self,
        node_id: str,
        node_name: str,
        inputs: dict[str, Any],
    ) -> MiddlewareResult:
        """ノード実行前: 開始イベントを記録.

        Args:
            node_id: ノード識別子
            node_name: ノード表示名
            inputs: ノードへの入力データ

        Returns:
            常に ALLOW（記録専用）
        """
        self._start_times[node_id] = time.monotonic()

        event = AuditEvent(
            tool_name=node_name,
            decision="node_start",
            reason=f"ノード '{node_name}' の実行を開始",
            operation_type="node_start",
            flow_id=self._flow_id,
            run_id=self._run_id,
            metadata={
                "node_id": node_id,
                "input_keys": list(inputs.keys()),
            },
        )
        self._audit_logger.log_event(event)
        _logger.debug("監査: ノード開始 node_id=%s, node_name=%s", node_id, node_name)

        return MiddlewareResult(decision=MiddlewareDecision.ALLOW)

    async def after_node(
        self,
        node_id: str,
        node_name: str,
        result: dict[str, Any],
        success: bool,
    ) -> MiddlewareResult:
        """ノード実行後: 完了/エラーイベントを記録.

        Args:
            node_id: ノード識別子
            node_name: ノード表示名
            result: ノード実行結果
            success: 実行成功フラグ

        Returns:
            常に ALLOW（記録専用）
        """
        elapsed_ms = 0.0
        start = self._start_times.pop(node_id, None)
        if start is not None:
            elapsed_ms = (time.monotonic() - start) * 1000

        operation_type = "node_complete" if success else "node_error"

        event = AuditEvent(
            tool_name=node_name,
            decision=operation_type,
            reason=f"ノード '{node_name}' {'完了' if success else 'エラー'}",
            operation_type=operation_type,
            flow_id=self._flow_id,
            run_id=self._run_id,
            metadata={
                "node_id": node_id,
                "success": success,
                "elapsed_ms": round(elapsed_ms, 2),
                "result_keys": list(result.keys()) if result else [],
            },
        )
        self._audit_logger.log_event(event)
        _logger.debug(
            "監査: ノード%s node_id=%s, elapsed=%.1fms",
            "完了" if success else "エラー",
            node_id,
            elapsed_ms,
        )

        return MiddlewareResult(decision=MiddlewareDecision.ALLOW)


__all__ = ["AuditMiddleware"]

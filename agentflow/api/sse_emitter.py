"""SSE Emitter - 統一 Server-Sent Events 発信器.

設計原則:
- 統一形式: 全アプリで同じイベント構造
- 型安全: Pydantic によるイベント定義
- 拡張性: カスタムイベント種別対応

使用例:
    >>> from agentflow.api import SSEEmitter, SSEEvent
    >>>
    >>> emitter = SSEEmitter()
    >>>
    >>> # イベント発信
    >>> async def generate():
    ...     yield emitter.progress(10, "開始...")
    ...     yield emitter.data({"result": "ok"})
    ...     yield emitter.complete()
    >>>
    >>> return StreamingResponse(generate(), media_type="text/event-stream")
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any


class SSEEventType(str, Enum):
    """SSE イベント種別."""

    # 進捗
    PROGRESS = "progress"

    # データ
    DATA = "data"
    PARTIAL = "partial"
    CHUNK = "chunk"

    # 状態
    START = "start"
    COMPLETE = "complete"
    ERROR = "error"

    # Agent 関連
    AGENT_START = "agent_start"
    AGENT_COMPLETE = "agent_complete"
    TOOL_CALL = "tool_call"
    TOOL_RESULT = "tool_result"

    # HITL
    CLARIFICATION = "clarification"
    APPROVAL_REQUIRED = "approval_required"


@dataclass
class SSEEvent:
    """SSE イベント."""

    event: str
    data: dict[str, Any]
    id: str | None = None
    retry: int | None = None

    def to_sse(self) -> str:
        """SSE 形式文字列に変換."""
        lines = []
        if self.id:
            lines.append(f"id: {self.id}")
        if self.retry:
            lines.append(f"retry: {self.retry}")
        lines.append(f"event: {self.event}")
        lines.append(f"data: {json.dumps(self.data, ensure_ascii=False, default=str)}")
        return "\n".join(lines) + "\n\n"

    def __str__(self) -> str:
        """文字列表現."""
        return self.to_sse()


@dataclass
class SSEConfig:
    """SSE 設定."""

    # 再接続間隔（ミリ秒）
    retry_interval: int = 3000

    # ハートビート間隔（秒）
    heartbeat_interval: float = 30.0

    # イベントIDプレフィックス
    event_id_prefix: str = "evt"


class SSEEmitter:
    """SSE イベント発信器.

    統一された SSE イベント構造を提供します。

    使用例:
        >>> emitter = SSEEmitter()
        >>>
        >>> async def stream():
        ...     yield emitter.start()
        ...     yield emitter.progress(50, "処理中...")
        ...     yield emitter.data({"result": "ok"})
        ...     yield emitter.complete()
    """

    def __init__(self, config: SSEConfig | None = None) -> None:
        """初期化.

        Args:
            config: SSE 設定
        """
        self._config = config or SSEConfig()
        self._event_counter = 0

    def _next_id(self) -> str:
        """次のイベントIDを生成."""
        self._event_counter += 1
        return f"{self._config.event_id_prefix}-{self._event_counter}"

    def _create_event(
        self,
        event_type: SSEEventType | str,
        data: dict[str, Any],
        include_id: bool = True,
    ) -> str:
        """SSE イベントを作成.

        Args:
            event_type: イベント種別
            data: イベントデータ
            include_id: イベントIDを含めるか

        Returns:
            SSE 形式文字列
        """
        event = SSEEvent(
            event=event_type.value if isinstance(event_type, SSEEventType) else event_type,
            data={
                **data,
                "timestamp": datetime.now().isoformat(),
            },
            id=self._next_id() if include_id else None,
            retry=self._config.retry_interval,
        )
        return event.to_sse()

    # =========================================================================
    # 便利メソッド
    # =========================================================================

    def start(self, message: str = "処理を開始します", **kwargs: Any) -> str:
        """開始イベント."""
        return self._create_event(
            SSEEventType.START,
            {"message": message, **kwargs},
        )

    def progress(
        self,
        value: int,
        message: str = "",
        step: str | None = None,
        **kwargs: Any,
    ) -> str:
        """進捗イベント.

        Args:
            value: 進捗値（0-100）
            message: 進捗メッセージ
            step: 現在のステップ名
        """
        return self._create_event(
            SSEEventType.PROGRESS,
            {"value": value, "message": message, "step": step, **kwargs},
        )

    def data(self, data: dict[str, Any]) -> str:
        """データイベント."""
        return self._create_event(SSEEventType.DATA, {"payload": data})

    def partial(self, content: str, index: int = 0) -> str:
        """部分データイベント（ストリーミング用）."""
        return self._create_event(
            SSEEventType.PARTIAL,
            {"content": content, "index": index},
        )

    def chunk(self, content: str) -> str:
        """チャンクイベント（LLM ストリーミング用）."""
        return self._create_event(
            SSEEventType.CHUNK,
            {"content": content},
        )

    def complete(self, data: dict[str, Any] | None = None) -> str:
        """完了イベント."""
        return self._create_event(
            SSEEventType.COMPLETE,
            {"payload": data or {}},
        )

    def error(
        self,
        message: str,
        code: str = "ERROR",
        details: dict[str, Any] | None = None,
    ) -> str:
        """エラーイベント."""
        return self._create_event(
            SSEEventType.ERROR,
            {"message": message, "code": code, "details": details or {}},
        )

    def agent_start(self, agent_name: str, **kwargs: Any) -> str:
        """Agent 開始イベント."""
        return self._create_event(
            SSEEventType.AGENT_START,
            {"agent": agent_name, **kwargs},
        )

    def agent_complete(
        self,
        agent_name: str,
        result: dict[str, Any] | None = None,
    ) -> str:
        """Agent 完了イベント."""
        return self._create_event(
            SSEEventType.AGENT_COMPLETE,
            {"agent": agent_name, "result": result or {}},
        )

    def tool_call(
        self,
        tool_name: str,
        arguments: dict[str, Any] | None = None,
    ) -> str:
        """ツール呼び出しイベント."""
        return self._create_event(
            SSEEventType.TOOL_CALL,
            {"tool": tool_name, "arguments": arguments or {}},
        )

    def tool_result(
        self,
        tool_name: str,
        result: dict[str, Any] | None = None,
    ) -> str:
        """ツール結果イベント."""
        return self._create_event(
            SSEEventType.TOOL_RESULT,
            {"tool": tool_name, "result": result or {}},
        )

    def clarification(self, question: str, options: list[str] | None = None) -> str:
        """HITL 明確化要求イベント."""
        return self._create_event(
            SSEEventType.CLARIFICATION,
            {"question": question, "options": options or []},
        )

    def heartbeat(self) -> str:
        """ハートビート（接続維持用）."""
        return ": heartbeat\n\n"


__all__ = [
    "SSEConfig",
    "SSEEmitter",
    "SSEEvent",
    "SSEEventType",
]

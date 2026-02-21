"""interrupt 関数モジュール.

ワークフローを一時停止し、人間の入力を待つためのコア機能。
LangGraph の interrupt() パターンを参考に実装。

重要な注意事項:
    - interrupt() 呼び出し前に非冪等操作（DB書き込み等）を行わない
    - 再開時にノードは最初から再実行される
    - checkpointer が設定されていない場合はエラー
"""

from __future__ import annotations

import contextvars
import logging
import uuid
from datetime import datetime, timedelta
from typing import TYPE_CHECKING, Any

from agentflow.hitl.types import (
    ApprovalRequest,
    ApprovalResponse,
    ApprovalStatus,
    Command,
    CommandType,
    InterruptPayload,
    InterruptType,
)


if TYPE_CHECKING:
    from agentflow.hitl.checkpointer import Checkpointer

logger = logging.getLogger(__name__)

# コンテキスト変数: 現在の割り込み状態を追跡
_current_interrupt: contextvars.ContextVar[InterruptPayload | None] = contextvars.ContextVar(
    "current_interrupt", default=None
)
_current_checkpointer: contextvars.ContextVar[Checkpointer | None] = contextvars.ContextVar(
    "current_checkpointer", default=None
)
_current_thread_id: contextvars.ContextVar[str | None] = contextvars.ContextVar("current_thread_id", default=None)


class InterruptError(Exception):
    """割り込み関連のエラー."""


class InterruptTimeoutError(InterruptError):
    """割り込みタイムアウトエラー."""


def get_current_interrupt() -> InterruptPayload | None:
    """現在の割り込みペイロードを取得."""
    return _current_interrupt.get()


def is_interrupted() -> bool:
    """現在割り込み中かどうかを確認."""
    return _current_interrupt.get() is not None


def set_checkpointer(checkpointer: Checkpointer) -> None:
    """現在のコンテキストに Checkpointer を設定."""
    _current_checkpointer.set(checkpointer)


def set_thread_id(thread_id: str) -> None:
    """現在のコンテキストにスレッドIDを設定."""
    _current_thread_id.set(thread_id)


async def interrupt(
    request: ApprovalRequest | str,
    *,
    interrupt_type: InterruptType = InterruptType.APPROVAL,
    options: list[str] | None = None,
    timeout_seconds: int | None = None,
    node_id: str | None = None,
    flow_id: str | None = None,
    state: dict[str, Any] | None = None,
) -> ApprovalResponse:
    """ワークフローを一時停止し、人間の入力を待つ.

    Args:
        request: 承認リクエストまたはプロンプト文字列
        interrupt_type: 割り込みタイプ
        options: 選択肢（INPUT タイプ用）
        timeout_seconds: タイムアウト秒数
        node_id: 中断ノードID
        flow_id: フローID
        state: 中断時の状態

    Returns:
        承認レスポンス

    Raises:
        InterruptError: Checkpointer が設定されていない場合
        InterruptTimeoutError: タイムアウトした場合

    Example:
        >>> response = await interrupt(
        ...     ApprovalRequest(
        ...         action="delete_user",
        ...         resource_id="user-123",
        ...         reason="ユーザー削除は不可逆操作です",
        ...     )
        ... )
        >>> if response.approved:
        ...     await do_delete()
    """
    checkpointer = _current_checkpointer.get()
    thread_id = _current_thread_id.get()

    if checkpointer is None:
        msg = (
            "Checkpointer が設定されていません。"
            "Engine に checkpointer を設定するか、set_checkpointer() を呼び出してください。"
        )
        raise InterruptError(msg)

    if thread_id is None:
        thread_id = str(uuid.uuid4())
        _current_thread_id.set(thread_id)
        logger.warning(f"thread_id が未設定のため自動生成: {thread_id}")

    # リクエストの正規化
    if isinstance(request, str):
        approval_request = ApprovalRequest(
            action="user_input",
            reason=request,
        )
        prompt = request
    else:
        approval_request = request
        prompt = request.reason

    # タイムアウトの設定
    effective_timeout = timeout_seconds or approval_request.timeout_seconds or 3600
    expires_at = datetime.utcnow() + timedelta(seconds=effective_timeout)
    approval_request.expires_at = expires_at

    # 割り込みペイロードを作成
    payload = InterruptPayload(
        interrupt_type=interrupt_type,
        request=approval_request,
        prompt=prompt,
        options=options,
        node_id=node_id,
        flow_id=flow_id,
        state=state or {},
    )

    # コンテキストに設定
    _current_interrupt.set(payload)

    logger.info(f"Interrupt triggered: {payload.id} (type={interrupt_type.value}, action={approval_request.action})")

    # InterruptSignal を発生させて上位レイヤーに通知
    raise InterruptSignal(payload)


class InterruptSignal(Exception):
    """割り込みシグナル.

    Engine がキャッチして処理するための例外。
    通常のエラーではなく、制御フロー用。
    """

    def __init__(self, payload: InterruptPayload) -> None:
        """初期化.

        Args:
            payload: 割り込みペイロード
        """
        self.payload = payload
        super().__init__(f"Interrupt: {payload.id}")


def clear_interrupt() -> None:
    """現在の割り込み状態をクリア."""
    _current_interrupt.set(None)


async def resume_with_command(
    command: Command,
    checkpointer: Checkpointer,
    checkpoint_id: str,
) -> ApprovalResponse:
    """コマンドでワークフローを再開.

    Args:
        command: 再開コマンド
        checkpointer: チェックポインター
        checkpoint_id: 再開するチェックポイントID

    Returns:
        承認レスポンス
    """
    checkpoint = await checkpointer.load(checkpoint_id)
    if checkpoint is None:
        msg = f"チェックポイントが見つかりません: {checkpoint_id}"
        raise InterruptError(msg)

    payload_data = checkpoint.interrupt_payload
    if payload_data is None:
        msg = "このチェックポイントには割り込みペイロードがありません"
        raise InterruptError(msg)

    payload = InterruptPayload(**payload_data)
    request_id = payload.request.id if payload.request else "unknown"

    # コマンドタイプに応じてレスポンスを生成
    if command.type == CommandType.APPROVE:
        return ApprovalResponse(
            request_id=request_id,
            status=ApprovalStatus.APPROVED,
            approved=True,
            approver=command.issuer,
            modifications=command.value if isinstance(command.value, dict) else {},
        )
    if command.type == CommandType.REJECT:
        return ApprovalResponse(
            request_id=request_id,
            status=ApprovalStatus.REJECTED,
            approved=False,
            approver=command.issuer,
            rejection_reason=str(command.value) if command.value else None,
        )
    if command.type == CommandType.CANCEL:
        return ApprovalResponse(
            request_id=request_id,
            status=ApprovalStatus.CANCELLED,
            approved=False,
            approver=command.issuer,
            rejection_reason="Cancelled by user",
        )
    # UPDATE や RETRY の場合は承認として扱う
    return ApprovalResponse(
        request_id=request_id,
        status=ApprovalStatus.APPROVED,
        approved=True,
        approver=command.issuer,
        modifications=command.value if isinstance(command.value, dict) else {},
    )

"""HITL (Human-in-the-Loop) Engine Mixin.

BaseEngine から分離した HITL 関連ロジックを提供する。
Harness 層の関心事（承認・チェックポイント・割り込み）を
Kernel の Engine に opt-in で合成するための mixin。

使用例:
    >>> from kernel.engines.base import BaseEngine
    >>> from harness.engines.hitl_mixin import HITLEngineMixin, HITLEngineConfig
    >>>
    >>> class MyEngine(HITLEngineMixin, BaseEngine):
    ...     async def _initialize(self) -> None: ...
    ...     async def _execute(self, inputs: dict) -> dict: ...
"""

from __future__ import annotations

import logging
import time
import uuid
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from harness.approval.checkpointer import CheckpointData
    from harness.approval.types import ApprovalResponse, Command


class HITLEngineMixin:
    """HITL 機能を Engine に合成する mixin.

    BaseEngine のサブクラスに多重継承で合成して使用する。
    このクラスは harness 層に所属し、kernel の BaseEngine を直接変更せずに
    HITL 機能（承認、チェックポイント、割り込み）を追加する。

    前提: 合成先のクラスが以下の属性を持つこと:
    - _config: EngineConfig (hitl フィールドを含む)
    - _thread_id: str | None
    - _run_id: str
    - _flow_id: str
    - _logger: logging.Logger
    - _is_resuming: bool
    - _resume_checkpoint_id: str | None
    """

    _config: Any
    _thread_id: str | None
    _run_id: str
    _flow_id: str
    _logger: logging.Logger
    _is_resuming: bool
    _resume_checkpoint_id: str | None

    def _setup_hitl_context(self) -> None:
        """HITL コンテキストを設定."""
        hitl_config = getattr(getattr(self, "_config", None), "hitl", None)
        if hitl_config is None or not hitl_config.enabled:
            return

        from harness.approval.interrupt import set_checkpointer, set_thread_id

        if hitl_config.checkpointer:
            set_checkpointer(hitl_config.checkpointer)

        thread_id = getattr(self, "_thread_id", None)
        if thread_id:
            set_thread_id(thread_id)

    def _cleanup_hitl_context(self) -> None:
        """HITL コンテキストをクリーンアップ."""
        hitl_config = getattr(getattr(self, "_config", None), "hitl", None)
        if hitl_config is None or not hitl_config.enabled:
            return

        from harness.approval.interrupt import clear_interrupt

        clear_interrupt()

    def _is_interrupt_signal(self, exc: Exception) -> bool:
        """例外が InterruptSignal かどうかを判定."""
        from harness.approval.interrupt import InterruptSignal

        return isinstance(exc, InterruptSignal)

    async def _handle_interrupt(
        self,
        exc: Exception,
        inputs: dict[str, Any],
    ) -> None:
        """割り込みを処理し、状態を保存."""
        from harness.approval.checkpointer import CheckpointCursor, CheckpointData
        from harness.approval.interrupt import InterruptSignal

        if not isinstance(exc, InterruptSignal):
            return

        hitl_config = getattr(getattr(self, "_config", None), "hitl", None)
        checkpointer = hitl_config.checkpointer if hitl_config else None
        logger = getattr(self, "_logger", logging.getLogger(__name__))

        if checkpointer is None:
            logger.warning("Checkpointer が未設定のため、状態を保存できません")
            return

        flow_id = getattr(self, "_flow_id", "")
        thread_id = getattr(self, "_thread_id", None)
        run_id = getattr(self, "_run_id", "")
        resume_checkpoint_id = getattr(self, "_resume_checkpoint_id", None)

        payload = exc.payload
        cursor = CheckpointCursor(
            node_id=payload.node_id,
            flow_id=payload.flow_id or flow_id,
            thread_id=thread_id,
            run_id=run_id,
        )
        checkpoint = CheckpointData(
            checkpoint_id=f"cp-{uuid.uuid4().hex[:12]}",
            thread_id=thread_id or "",
            flow_id=flow_id,
            node_id=payload.node_id,
            schema_version=2,
            cursor=cursor,
            run_id=run_id,
            state=payload.state,
            inputs=inputs,
            interrupt_payload=payload.model_dump(),
            parent_checkpoint_id=resume_checkpoint_id,
        )

        await checkpointer.save(checkpoint)
        logger.info(f"Checkpoint saved: {checkpoint.checkpoint_id}")

    async def resume(
        self,
        thread_id: str,
        command: Command,
    ) -> dict[str, Any]:
        """中断されたワークフローを再開.

        Args:
            thread_id: スレッドID
            command: 再開コマンド（approve/reject/update）

        Returns:
            実行結果

        Raises:
            InterruptError: チェックポイントが見つからない場合
        """
        from harness.approval.interrupt import InterruptError, resume_with_command

        hitl_config = getattr(getattr(self, "_config", None), "hitl", None)
        checkpointer = hitl_config.checkpointer if hitl_config else None
        logger = getattr(self, "_logger", logging.getLogger(__name__))

        if checkpointer is None:
            msg = "Checkpointer が設定されていません"
            raise InterruptError(msg)

        checkpoint = await checkpointer.load_latest(thread_id)
        if checkpoint is None:
            msg = f"チェックポイントが見つかりません: {thread_id}"
            raise InterruptError(msg)

        logger.info(f"Resuming from checkpoint: {checkpoint.checkpoint_id} (command: {command.type.value})")

        schema_version = checkpoint.schema_version or 1
        if schema_version not in {1, 2}:
            msg = f"未対応のチェックポイントスキーマ: {schema_version}"
            raise InterruptError(msg)

        if schema_version >= 2 and checkpoint.cursor is None:
            logger.warning("カーソル情報が欠落しているため、旧式の再開処理にフォールバックします")

        response = await resume_with_command(
            command=command,
            checkpointer=checkpointer,
            checkpoint_id=checkpoint.checkpoint_id,
        )

        # 合成先 Engine の属性を設定
        if hasattr(self, "_is_resuming"):
            self._is_resuming = True
        if hasattr(self, "_resume_checkpoint_id"):
            self._resume_checkpoint_id = checkpoint.checkpoint_id

        inputs = self._rehydrate_inputs(checkpoint, response, command)

        # 合成先の run() を呼び出す
        result_obj: object = await self.run(inputs, thread_id=thread_id)  # type: ignore[attr-defined]
        if isinstance(result_obj, dict):
            return {str(key): value for key, value in result_obj.items()}
        return {"result": result_obj}

    def _rehydrate_inputs(
        self,
        checkpoint: CheckpointData,
        response: ApprovalResponse,
        command: Command,
    ) -> dict[str, Any]:
        """再開用の入力データを再構成."""
        inputs = checkpoint.inputs.copy()
        inputs["_hitl_response"] = response.model_dump()
        inputs["_hitl_command"] = command.model_dump()
        if checkpoint.schema_version >= 2:
            if checkpoint.cursor:
                inputs["_hitl_cursor"] = checkpoint.cursor.model_dump()
            if checkpoint.run_id:
                inputs["_hitl_run_id"] = checkpoint.run_id
            inputs["_hitl_schema_version"] = checkpoint.schema_version
        return inputs

    def _emit_approval_required(
        self,
        request_id: str,
        action: str,
        reason: str,
        context: dict[str, Any],
    ) -> dict[str, Any] | None:
        """承認要求イベントを発行."""
        config = getattr(self, "_config", None)
        enable_events = getattr(config, "enable_events", True) if config else True
        flow_id = getattr(self, "_flow_id", "")

        if enable_events:
            return {
                "event_type": "approval_required",
                "timestamp": time.time(),
                "flow_id": flow_id,
                "data": {
                    "request_id": request_id,
                    "action": action,
                    "reason": reason,
                    "context": context,
                },
            }
        return None

    @property
    def is_hitl_enabled(self) -> bool:
        """HITL が有効かどうか."""
        hitl_config = getattr(getattr(self, "_config", None), "hitl", None)
        return hitl_config.enabled if hitl_config else False

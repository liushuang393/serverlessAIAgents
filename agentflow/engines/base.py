"""BaseEngine - Engine抽象基底クラス.

すべてのEngine Patternの基底クラス、統一インターフェースを定義：
- run(): 同期実行、最終結果を返却
- run_stream(): ストリーム実行、AG-UIイベントをyield
- configure(): 実行時設定調整

設計原則：
- create_flow()ベースで構築
- 統一されたイベント発行メカニズム（ProgressEmitter活用）
- プラガブルなコンポーネント（Agent、Report Generator等）
"""

from __future__ import annotations

import logging
import time
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, TypeVar, cast

from pydantic import BaseModel

from agentflow.run import MemoryRunStore, RunRecord


# AG-UI イベント（遅延インポートで循環依存回避）
if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from agentflow.hitl.checkpointer import Checkpointer
    from agentflow.patterns.progress_emitter import AgentMeta, ProgressEmitter


@dataclass
class HITLEngineConfig:
    """HITL 関連の Engine 設定.

    Attributes:
        enabled: HITL を有効にするか
        checkpointer: チェックポインター（状態永続化）
        interrupt_before: 指定ノードの実行前に割り込み
        interrupt_after: 指定ノードの実行後に割り込み
        approval_required_for: 承認が必要なアクションパターン
        default_timeout_seconds: デフォルト承認タイムアウト
    """

    enabled: bool = False
    checkpointer: Checkpointer | None = None
    interrupt_before: list[str] = field(default_factory=list)
    interrupt_after: list[str] = field(default_factory=list)
    approval_required_for: list[str] = field(default_factory=list)
    default_timeout_seconds: int = 3600


@dataclass
class EngineConfig:
    """Engine設定.

    Attributes:
        name: Engineインスタンス名
        enable_events: AG-UIイベントを発行するか
        enable_memory: コンテキストメモリを有効化するか
        max_retries: グローバル最大リトライ回数
        timeout_seconds: グローバルタイムアウト時間
        llm_config: LLM設定（model、temperature等）
        hitl: HITL（Human-in-the-Loop）設定
        extra: 追加設定
    """

    name: str = "engine"
    enable_events: bool = True
    enable_memory: bool = True
    max_retries: int = 2
    timeout_seconds: int = 300
    llm_config: dict[str, Any] = field(default_factory=dict)
    hitl: HITLEngineConfig = field(default_factory=HITLEngineConfig)
    run_store: object = field(default_factory=MemoryRunStore)
    extra: dict[str, Any] = field(default_factory=dict)


# ジェネリック型
InputT = TypeVar("InputT", bound=BaseModel)
OutputT = TypeVar("OutputT", bound=BaseModel)


class BaseEngine(ABC):
    """Engine抽象基底クラス.

    すべてのEngine Patternはこのクラスを継承し、以下を実装する必要がある：
    - _build_flow(): 内部フローを構築
    - _execute(): コアロジックを実行

    Example:
        >>> class MyEngine(BaseEngine):
        ...     def _build_flow(self):
        ...         return create_flow([self._agent])
        ...
        ...     async def _execute(self, inputs):
        ...         return await self._flow.run(inputs)
    """

    def __init__(self, config: EngineConfig | None = None) -> None:
        """Engineを初期化.

        Args:
            config: Engine設定、Noneの場合はデフォルト設定を使用
        """
        self._config = config or EngineConfig()
        self._logger = logging.getLogger(f"agentflow.engines.{self._config.name}")
        self._flow_id: str | None = None
        self._initialized = False
        self._progress_emitter: ProgressEmitter | None = None
        self._thread_id: str | None = None
        self._run_id: str | None = None
        self._run_store = cast("MemoryRunStore", self._config.run_store)
        self._is_resuming: bool = False
        self._resume_checkpoint_id: str | None = None

    @property
    def config(self) -> EngineConfig:
        """現在の設定を取得."""
        return self._config

    def configure(self, **kwargs: Any) -> BaseEngine:
        """実行時に設定を調整.

        Args:
            **kwargs: 設定項目のキー値ペア

        Returns:
            self（チェーンメソッド呼び出しをサポート）
        """
        for key, value in kwargs.items():
            if hasattr(self._config, key):
                setattr(self._config, key, value)
            else:
                self._config.extra[key] = value
        return self

    @abstractmethod
    async def _initialize(self) -> None:
        """内部コンポーネント（Agent、Flow等）を初期化.

        サブクラスで実装必須、初回run()時に自動呼び出し。
        """
        ...

    @abstractmethod
    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """コアロジックを実行.

        Args:
            inputs: 入力データ

        Returns:
            出力結果
        """
        ...

    async def run(
        self,
        inputs: dict[str, Any],
        *,
        thread_id: str | None = None,
    ) -> dict[str, Any]:
        """Engineを同期実行.

        Args:
            inputs: 入力データ
            thread_id: スレッドID（HITL用、省略時は自動生成）

        Returns:
            最終結果（dict）

        Raises:
            InterruptSignal: HITL 割り込みが発生した場合
        """
        if not self._initialized:
            await self._initialize()
            self._initialized = True

        self._flow_id = f"{self._config.name}-{uuid.uuid4().hex[:8]}"
        self._thread_id = thread_id or f"thread-{uuid.uuid4().hex[:8]}"
        self._run_id = f"run-{uuid.uuid4().hex[:12]}"
        started_at = time.time()
        self._logger.info(f"Engine run started: {self._flow_id} (thread: {self._thread_id})")

        from agentflow.integrations.context_bridge import get_current_context

        context = get_current_context()
        run_record = RunRecord(
            run_id=self._run_id,
            flow_id=self._flow_id,
            thread_id=self._thread_id,
            trace_id=context.trace_id if context else None,
            tenant_id=context.tenant_id if context else None,
            status="running",
            started_at=started_at,
            completed_at=None,
            metrics={},
        )
        await self._run_store.save(run_record)

        # HITL コンテキストを設定
        self._setup_hitl_context()

        try:
            result = await self._execute(inputs)
            self._logger.info(f"Engine run completed: {self._flow_id}")
            run_record.status = "completed"
            return result
        except Exception as e:
            # InterruptSignal の場合は状態を保存
            if self._is_interrupt_signal(e):
                run_record.status = "interrupted"
                await self._handle_interrupt(e, inputs)
            else:
                run_record.status = "failed"
            self._logger.exception(f"Engine run failed: {self._flow_id}")
            raise
        finally:
            run_record.completed_at = time.time()
            run_record.metrics["duration_ms"] = (
                run_record.completed_at - run_record.started_at
            ) * 1000
            await self._run_store.save(run_record)
            self._cleanup_hitl_context()

    async def run_stream(self, inputs: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """Engineをストリーム実行、AG-UIイベントをyield.

        Args:
            inputs: 入力データ

        Yields:
            AG-UIイベント（dict形式）
        """
        if not self._initialized:
            await self._initialize()
            self._initialized = True

        self._flow_id = f"{self._config.name}-{uuid.uuid4().hex[:8]}"

        # 運行時インポート（循環依存回避）
        from agentflow.patterns.progress_emitter import ProgressEmitter as PE
        from agentflow.protocols.agui_events import (
            FlowCompleteEvent,
            FlowErrorEvent,
            FlowStartEvent,
        )

        # ProgressEmitter を初期化
        self._progress_emitter = PE(
            flow_id=self._flow_id,
            total_agents=0,
        )

        # Flow開始イベント（to_dict() で event_type を文字列に変換）
        if self._config.enable_events:
            yield FlowStartEvent(
                timestamp=time.time(),
                flow_id=self._flow_id,
                data={"engine": self.__class__.__name__},
            ).to_dict()

        try:
            # 実行してイベントを収集
            async for event in self._execute_stream(inputs):
                yield event

            # Flow完了イベント
            if self._config.enable_events:
                yield FlowCompleteEvent(
                    timestamp=time.time(),
                    flow_id=self._flow_id,
                    data={},
                ).to_dict()

        except Exception as e:
            self._logger.exception(f"Engine stream failed: {self._flow_id}")
            if self._config.enable_events:
                yield FlowErrorEvent(
                    timestamp=time.time(),
                    flow_id=self._flow_id or "",
                    error_message=str(e),
                    error_type=type(e).__name__,
                    data={},
                ).to_dict()
            raise

    async def _execute_stream(self, inputs: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行のコアロジック（サブクラスでオーバーライド可能）.

        デフォルト実装：_execute()を呼び出し、単一の結果イベントをyield。
        サブクラスでオーバーライドしてより細かい粒度のイベントストリームを提供可能。
        """
        result = await self._execute(inputs)
        yield {"type": "result", "data": result}

    def _setup_progress_emitter(self, agent_metas: list[AgentMeta]) -> None:
        """進捗エミッターを設定.

        Args:
            agent_metas: Agent メタデータリスト
        """
        if self._progress_emitter:
            self._progress_emitter.register_agents(agent_metas)

    def _emit_node_start(self, node_name: str) -> dict[str, Any] | None:
        """ノード開始イベントを発行."""
        if self._config.enable_events:
            from agentflow.protocols.agui_events import NodeStartEvent

            return NodeStartEvent(
                timestamp=time.time(),
                node_id=node_name,
                node_name=node_name,
                flow_id=self._flow_id or "",
                data={},
            ).to_dict()  # to_dict() で event_type を文字列に変換
        return None

    def _emit_node_complete(self, node_name: str, result: dict[str, Any]) -> dict[str, Any] | None:
        """ノード完了イベントを発行.

        Args:
            node_name: ノード名
            result: Agent結果（思考過程含む）

        Returns:
            AG-UIイベント辞書（完全な結果データ付き）
        """
        if self._config.enable_events:
            from agentflow.protocols.agui_events import NodeCompleteEvent

            # 結果をシリアライズ可能な形式に変換
            serialized_result = self._serialize_result(result)

            return NodeCompleteEvent(
                timestamp=time.time(),
                node_id=node_name,
                node_name=node_name,
                flow_id=self._flow_id or "",
                data=serialized_result,  # 完全な結果データを含む
            ).to_dict()
        return None

    def _serialize_result(self, result: dict[str, Any]) -> dict[str, Any]:
        """結果をJSONシリアライズ可能な形式に変換.

        Pydantic モデル、Enum、その他の複雑なオブジェクトを処理。

        Args:
            result: Agent結果辞書

        Returns:
            シリアライズ可能な辞書
        """
        from enum import Enum

        from pydantic import BaseModel

        def serialize_value(value: Any) -> Any:
            if value is None:
                return None
            if isinstance(value, BaseModel):
                return value.model_dump()
            if isinstance(value, Enum):
                return value.value
            if isinstance(value, dict):
                return {k: serialize_value(v) for k, v in value.items()}
            if isinstance(value, list):
                return [serialize_value(v) for v in value]
            # 基本型（str, int, float, bool）はそのまま
            return value

        return {k: serialize_value(v) for k, v in result.items()}

    @property
    def progress_emitter(self) -> ProgressEmitter | None:
        """現在の ProgressEmitter を取得."""
        return self._progress_emitter

    # =========================================================================
    # HITL (Human-in-the-Loop) サポート
    # =========================================================================

    def _setup_hitl_context(self) -> None:
        """HITL コンテキストを設定."""
        if not self._config.hitl.enabled:
            return

        from agentflow.hitl import set_checkpointer, set_thread_id

        if self._config.hitl.checkpointer:
            set_checkpointer(self._config.hitl.checkpointer)

        if self._thread_id:
            set_thread_id(self._thread_id)

    def _cleanup_hitl_context(self) -> None:
        """HITL コンテキストをクリーンアップ."""
        if not self._config.hitl.enabled:
            return

        from agentflow.hitl import clear_interrupt

        clear_interrupt()

    def _is_interrupt_signal(self, exc: Exception) -> bool:
        """例外が InterruptSignal かどうかを判定."""
        from agentflow.hitl import InterruptSignal

        return isinstance(exc, InterruptSignal)

    async def _handle_interrupt(
        self,
        exc: Exception,
        inputs: dict[str, Any],
    ) -> None:
        """割り込みを処理し、状態を保存."""
        from agentflow.hitl import InterruptSignal
        from agentflow.hitl.checkpointer import CheckpointCursor, CheckpointData

        if not isinstance(exc, InterruptSignal):
            return

        checkpointer = self._config.hitl.checkpointer
        if checkpointer is None:
            self._logger.warning("Checkpointer が未設定のため、状態を保存できません")
            return

        payload = exc.payload
        cursor = CheckpointCursor(
            node_id=payload.node_id,
            flow_id=payload.flow_id or self._flow_id,
            thread_id=self._thread_id,
            run_id=self._run_id,
        )
        checkpoint = CheckpointData(
            checkpoint_id=f"cp-{uuid.uuid4().hex[:12]}",
            thread_id=self._thread_id or "",
            flow_id=self._flow_id,
            node_id=payload.node_id,
            schema_version=2,
            cursor=cursor,
            run_id=self._run_id,
            state=payload.state,
            inputs=inputs,
            interrupt_payload=payload.model_dump(),
            parent_checkpoint_id=self._resume_checkpoint_id,
        )

        await checkpointer.save(checkpoint)
        self._logger.info(f"Checkpoint saved: {checkpoint.checkpoint_id}")

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
        from agentflow.hitl import InterruptError, resume_with_command

        checkpointer = self._config.hitl.checkpointer
        if checkpointer is None:
            msg = "Checkpointer が設定されていません"
            raise InterruptError(msg)

        # 最新のチェックポイントを取得
        checkpoint = await checkpointer.load_latest(thread_id)
        if checkpoint is None:
            msg = f"チェックポイントが見つかりません: {thread_id}"
            raise InterruptError(msg)

        self._logger.info(
            f"Resuming from checkpoint: {checkpoint.checkpoint_id} (command: {command.type.value})"
        )

        # スキーマバージョンの検証
        schema_version = checkpoint.schema_version or 1
        if schema_version not in {1, 2}:
            msg = f"未対応のチェックポイントスキーマ: {schema_version}"
            raise InterruptError(msg)

        if schema_version >= 2 and checkpoint.cursor is None:
            self._logger.warning(
                "カーソル情報が欠落しているため、旧式の再開処理にフォールバックします"
            )

        # コマンドに基づいて承認レスポンスを生成
        response = await resume_with_command(
            command=command,
            checkpointer=checkpointer,
            checkpoint_id=checkpoint.checkpoint_id,
        )

        self._is_resuming = True
        self._resume_checkpoint_id = checkpoint.checkpoint_id

        # 入力データを復元して再実行
        # 注意: 現段階ではカーソルを付与するだけの最小実装
        inputs = self._rehydrate_inputs(checkpoint, response, command)

        return await self.run(inputs, thread_id=thread_id)

    def _rehydrate_inputs(
        self,
        checkpoint: CheckpointData,
        response: ApprovalResponse,
        command: Command,
    ) -> dict[str, Any]:
        """再開用の入力データを再構成.

        カーソル情報を付与し、最小限の決定論的な再開を支援する。
        """
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
        if self._config.enable_events:
            return {
                "event_type": "approval_required",
                "timestamp": time.time(),
                "flow_id": self._flow_id or "",
                "data": {
                    "request_id": request_id,
                    "action": action,
                    "reason": reason,
                    "context": context,
                },
            }
        return None

    @property
    def thread_id(self) -> str | None:
        """現在のスレッドIDを取得."""
        return self._thread_id

    @property
    def is_hitl_enabled(self) -> bool:
        """HITL が有効かどうか."""
        return self._config.hitl.enabled


# Command のインポート用型ヒント
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from agentflow.hitl import ApprovalResponse, Command
    from agentflow.hitl.checkpointer import CheckpointData

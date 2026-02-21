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
from typing import TYPE_CHECKING, Any, TypeVar

from pydantic import BaseModel

from agentflow.run import (
    LightningRuntimeConfig,
    LightningStore,
    LightningTracer,
    LightningTrainingRequest,
    LightningTrainingResult,
    MemoryRunStore,
    PromptRewardSample,
    RunRecord,
    RunStore,
    TrajectoryAdapter,
    resolve_lightning_store,
    train_with_lightning_backend,
)


# AG-UI イベント（遅延インポートで循環依存回避）
if TYPE_CHECKING:
    from collections.abc import AsyncIterator, Callable

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
        run_store: 実行記録ストア
        lightning: Lightning 実行/学習設定
        lightning_store: 標準化イベント/報酬ストア（学習連携用）
        reward_evaluator: 結果から報酬を計算する関数
        extra: 追加設定
    """

    name: str = "engine"
    enable_events: bool = True
    enable_memory: bool = True
    max_retries: int = 2
    timeout_seconds: int = 300
    llm_config: dict[str, Any] = field(default_factory=dict)
    hitl: HITLEngineConfig = field(default_factory=HITLEngineConfig)
    run_store: RunStore = field(default_factory=MemoryRunStore)
    lightning: LightningRuntimeConfig = field(default_factory=LightningRuntimeConfig)
    lightning_store: LightningStore | None = None
    reward_evaluator: Callable[[dict[str, Any]], float | None] | None = None
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
        self._run_store = self._config.run_store
        if self._config.lightning_store is None:
            self._config.lightning_store = resolve_lightning_store(self._config.lightning)
        self._lightning_tracer = (
            LightningTracer(self._config.lightning_store) if self._config.lightning_store is not None else None
        )
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
        await self._trace_custom_event(
            "engine.run.start",
            {"mode": "run", "input_keys": sorted(str(key) for key in inputs)},
        )

        # HITL コンテキストを設定
        self._setup_hitl_context()

        try:
            result = await self._execute(inputs)
            self._logger.info(f"Engine run completed: {self._flow_id}")
            run_record.status = "completed"
            await self._trace_custom_event("engine.run.result", {"result": self._serialize_result(result)})
            await self._record_reward_signal(result=result, source="run")
            return result
        except Exception as e:
            # InterruptSignal の場合は状態を保存
            if self._is_interrupt_signal(e):
                run_record.status = "interrupted"
                await self._handle_interrupt(e, inputs)
            else:
                run_record.status = "failed"
            await self._trace_custom_event(
                "engine.run.error",
                {
                    "error_message": str(e),
                    "error_type": type(e).__name__,
                },
                status="error",
            )
            self._logger.exception(f"Engine run failed: {self._flow_id}")
            raise
        finally:
            run_record.completed_at = time.time()
            run_record.metrics["duration_ms"] = (run_record.completed_at - run_record.started_at) * 1000
            await self._run_store.save(run_record)
            await self._trace_custom_event(
                "engine.run.end",
                {
                    "status": run_record.status,
                    "metrics": run_record.metrics,
                },
                status="ok" if run_record.status == "completed" else "error",
            )
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
        self._thread_id = f"thread-{uuid.uuid4().hex[:8]}"
        self._run_id = f"run-{uuid.uuid4().hex[:12]}"
        started_at = time.time()

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
        await self._trace_custom_event(
            "engine.run.start",
            {"mode": "run_stream", "input_keys": sorted(str(key) for key in inputs)},
        )

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
        latest_result: dict[str, Any] = {}

        self._setup_hitl_context()

        # Flow開始イベント（to_dict() で event_type を文字列に変換）
        if self._config.enable_events:
            start_event = FlowStartEvent(
                timestamp=time.time(),
                flow_id=self._flow_id,
                data={"engine": self.__class__.__name__},
            ).to_dict()
            await self._trace_event(start_event)
            yield start_event

        try:
            # 実行してイベントを収集
            async for event in self._execute_stream(inputs):
                await self._trace_event(event)
                result_candidate = self._extract_result_candidate(event)
                if result_candidate is not None:
                    latest_result = result_candidate
                yield event

            # Flow完了イベント
            run_record.status = "completed"
            await self._record_reward_signal(result=latest_result, source="run_stream")
            if self._config.enable_events:
                complete_event = FlowCompleteEvent(
                    timestamp=time.time(),
                    flow_id=self._flow_id,
                    data={},
                ).to_dict()
                await self._trace_event(complete_event)
                yield complete_event

        except Exception as e:
            if self._is_interrupt_signal(e):
                run_record.status = "interrupted"
                await self._handle_interrupt(e, inputs)
            else:
                run_record.status = "failed"
            self._logger.exception(f"Engine stream failed: {self._flow_id}")
            await self._trace_custom_event(
                "engine.run.error",
                {
                    "error_message": str(e),
                    "error_type": type(e).__name__,
                },
                status="error",
            )
            if self._config.enable_events:
                error_event = FlowErrorEvent(
                    timestamp=time.time(),
                    flow_id=self._flow_id or "",
                    error_message=str(e),
                    error_type=type(e).__name__,
                    data={},
                ).to_dict()
                await self._trace_event(error_event)
                yield error_event
            raise
        finally:
            run_record.completed_at = time.time()
            run_record.metrics["duration_ms"] = (run_record.completed_at - run_record.started_at) * 1000
            await self._run_store.save(run_record)
            await self._trace_custom_event(
                "engine.run.end",
                {
                    "status": run_record.status,
                    "metrics": run_record.metrics,
                },
                status="ok" if run_record.status == "completed" else "error",
            )
            self._cleanup_hitl_context()

    async def _execute_stream(self, inputs: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行のコアロジック（サブクラスでオーバーライド可能）.

        デフォルト実装：_execute()を呼び出し、単一の結果イベントをyield。
        サブクラスでオーバーライドしてより細かい粒度のイベントストリームを提供可能。
        """
        result = await self._execute(inputs)
        yield {"type": "result", "data": result}

    async def _trace_event(self, event: dict[str, Any]) -> None:
        """イベントを LightningStore へ記録."""
        if self._lightning_tracer is None:
            return
        if self._run_id is None or self._flow_id is None:
            return
        await self._lightning_tracer.record_event(
            run_id=self._run_id,
            flow_id=self._flow_id,
            event=event,
        )

    async def _trace_custom_event(
        self,
        event_type: str,
        payload: dict[str, Any] | None = None,
        *,
        node_id: str | None = None,
        node_name: str | None = None,
        status: str = "ok",
    ) -> None:
        """任意イベントを LightningStore へ記録."""
        if self._lightning_tracer is None:
            return
        if self._run_id is None or self._flow_id is None:
            return
        await self._lightning_tracer.record_custom_event(
            run_id=self._run_id,
            flow_id=self._flow_id,
            event_type=event_type,
            payload=payload,
            node_id=node_id,
            node_name=node_name,
            status=status,
        )

    async def _record_reward_signal(
        self,
        *,
        result: dict[str, Any],
        source: str,
    ) -> None:
        """報酬評価関数に基づく報酬信号を記録."""
        evaluator = self._config.reward_evaluator
        if evaluator is None or self._lightning_tracer is None:
            return
        if self._run_id is None or self._flow_id is None:
            return

        try:
            reward = evaluator(result)
        except Exception as exc:
            self._logger.warning("reward_evaluator failed: %s", exc)
            return
        if reward is None:
            return

        await self._lightning_tracer.record_reward(
            run_id=self._run_id,
            flow_id=self._flow_id,
            value=reward,
            source=source,
            metadata={"thread_id": self._thread_id or ""},
        )

    async def train_lightning(
        self,
        request: LightningTrainingRequest | None = None,
    ) -> LightningTrainingResult:
        """蓄積済みトレースから Lightning 学習/最適化を実行."""
        store = self._config.lightning_store
        if store is None:
            return LightningTrainingResult(
                success=False,
                backend="none",
                trained=False,
                optimized=False,
                num_samples=0,
                message="lightning_store is not configured",
            )

        req = request or LightningTrainingRequest()
        runtime = self._config.lightning.model_copy(deep=True)
        if req.backend is not None:
            runtime.backend = req.backend
        if req.algorithm is not None:
            runtime.algorithm = req.algorithm

        target_run_id = req.run_id
        if target_run_id is None:
            run_ids = await store.list_run_ids()
            if not run_ids:
                return LightningTrainingResult(
                    success=True,
                    backend=runtime.backend,
                    trained=False,
                    optimized=False,
                    num_samples=0,
                    message="no recorded runs",
                )
            target_run_id = run_ids[-1]

        events = await store.list_events(target_run_id)
        rewards = await store.list_rewards(target_run_id)
        samples = TrajectoryAdapter.to_prompt_reward_samples(events=events, rewards=rewards)
        samples = self._trim_training_samples(samples=samples, max_samples=req.max_samples)

        result = await train_with_lightning_backend(samples=samples, runtime=runtime)
        if req.apply_optimized_profile and result.optimized_llm_profile:
            self._apply_optimized_profile(result.optimized_llm_profile)
        return result

    def _extract_result_candidate(self, event: dict[str, Any]) -> dict[str, Any] | None:
        """イベントから結果候補を抽出."""
        event_type = event.get("type")
        if event_type == "result":
            data = event.get("data")
            if isinstance(data, dict):
                return data

        flow_result = event.get("result")
        if isinstance(flow_result, dict):
            return flow_result
        return None

    def _trim_training_samples(
        self,
        *,
        samples: list[PromptRewardSample],
        max_samples: int,
    ) -> list[PromptRewardSample]:
        """学習サンプル数を上限制御."""
        if len(samples) <= max_samples:
            return samples
        return samples[-max_samples:]

    def _apply_optimized_profile(self, profile: dict[str, Any]) -> None:
        """最適化プロファイルを Engine の LLM 設定へ反映."""
        self._config.llm_config = dict(self._config.llm_config)
        self._config.llm_config["optimized_profile"] = profile

        if "temperature" in profile:
            self._config.llm_config["temperature"] = profile["temperature"]
        if "max_tokens" in profile:
            self._config.llm_config["max_tokens"] = profile["max_tokens"]

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

        self._logger.info(f"Resuming from checkpoint: {checkpoint.checkpoint_id} (command: {command.type.value})")

        # スキーマバージョンの検証
        schema_version = checkpoint.schema_version or 1
        if schema_version not in {1, 2}:
            msg = f"未対応のチェックポイントスキーマ: {schema_version}"
            raise InterruptError(msg)

        if schema_version >= 2 and checkpoint.cursor is None:
            self._logger.warning("カーソル情報が欠落しているため、旧式の再開処理にフォールバックします")

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

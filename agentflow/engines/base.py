# -*- coding: utf-8 -*-
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
from collections.abc import AsyncIterator
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, TypeVar

from pydantic import BaseModel

# AG-UI イベント（遅延インポートで循環依存回避）
if TYPE_CHECKING:
    from agentflow.patterns.progress_emitter import AgentMeta, ProgressEmitter
    from agentflow.protocols.agui_events import AGUIEvent


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
        extra: 追加設定
    """
    name: str = "engine"
    enable_events: bool = True
    enable_memory: bool = True
    max_retries: int = 2
    timeout_seconds: int = 300
    llm_config: dict[str, Any] = field(default_factory=dict)
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

    @property
    def config(self) -> EngineConfig:
        """現在の設定を取得."""
        return self._config

    def configure(self, **kwargs: Any) -> "BaseEngine":
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

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Engineを同期実行.
        
        Args:
            inputs: 入力データ
            
        Returns:
            最終結果（dict）
        """
        if not self._initialized:
            await self._initialize()
            self._initialized = True

        self._flow_id = f"{self._config.name}-{uuid.uuid4().hex[:8]}"
        self._logger.info(f"Engine run started: {self._flow_id}")

        try:
            result = await self._execute(inputs)
            self._logger.info(f"Engine run completed: {self._flow_id}")
            return result
        except Exception:
            self._logger.exception(f"Engine run failed: {self._flow_id}")
            raise

    async def run_stream(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
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

    async def _execute_stream(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行のコアロジック（サブクラスでオーバーライド可能）.

        デフォルト実装：_execute()を呼び出し、単一の結果イベントをyield。
        サブクラスでオーバーライドしてより細かい粒度のイベントストリームを提供可能。
        """
        result = await self._execute(inputs)
        yield {"type": "result", "data": result}

    def _setup_progress_emitter(self, agent_metas: list["AgentMeta"]) -> None:
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

    def _emit_node_complete(
        self, node_name: str, result: dict[str, Any]
    ) -> dict[str, Any] | None:
        """ノード完了イベントを発行."""
        if self._config.enable_events:
            from agentflow.protocols.agui_events import NodeCompleteEvent
            return NodeCompleteEvent(
                timestamp=time.time(),
                node_id=node_name,
                node_name=node_name,
                flow_id=self._flow_id or "",
                data={"result_keys": list(result.keys())},
            ).to_dict()  # to_dict() で event_type を文字列に変換
        return None

    @property
    def progress_emitter(self) -> "ProgressEmitter | None":
        """現在の ProgressEmitter を取得."""
        return self._progress_emitter


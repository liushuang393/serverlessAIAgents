"""A2A Agent 実行コンテキストとエグゼキュータ.

- A2ARequestContext: Agent の process() 内から利用可能な A2A コンテキスト
- A2AAgentExecutor: python_a2a SDK の AgentExecutor に相当する抽象クラス
- ResilientAgentExecutor: 既存 ResilientAgent を A2A 対応にラップするアダプタ
"""

from __future__ import annotations

import logging
from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any

from agentflow.protocols.a2a.types import (
    A2ATaskState,
    Artifact,
    DataPart,
    Message,
    TextPart,
)


if TYPE_CHECKING:
    from agentflow.core.resilient_agent import ResilientAgent
    from agentflow.protocols.a2a.task_manager import A2ATaskManager


class A2ARequestContext:
    """A2A 実行コンテキスト.

    Agent の process() 内から emit_status() / emit_artifact() / is_cancelled を利用できる。
    Agent 開発者が直接インスタンス化することはない（Hub/Executor が生成する）。
    """

    def __init__(
        self,
        *,
        task_id: str,
        context_id: str,
        message: Message,
        task_manager: A2ATaskManager,
    ) -> None:
        """初期化.

        Args:
            task_id: タスク ID
            context_id: 会話/セッション ID
            message: 受信メッセージ
            task_manager: タスクマネージャー
        """
        self.task_id = task_id
        self.context_id = context_id
        self.message = message
        self.task_manager = task_manager
        self._cancelled = False

    async def emit_status(self, state: A2ATaskState, text: str | None = None) -> None:
        """タスク状態を更新（AG-UI/A2UI に自動変換される）.

        Args:
            state: 新しい状態
            text: 状態メッセージ
        """
        await self.task_manager.update_task_status(self.task_id, state, text)

    async def emit_artifact(self, artifact: Artifact) -> None:
        """アーティファクトを発行（AG-UI/A2UI に自動変換される）.

        Args:
            artifact: 発行するアーティファクト
        """
        await self.task_manager.add_artifact(self.task_id, artifact)

    async def emit_text(self, text: str) -> None:
        """テキストアーティファクトを簡易発行.

        Args:
            text: テキスト内容
        """
        await self.task_manager.add_text_artifact(self.task_id, text)

    async def emit_data(self, data: dict[str, Any]) -> None:
        """構造化データアーティファクトを簡易発行.

        Args:
            data: 構造化データ
        """
        await self.task_manager.add_data_artifact(self.task_id, data)

    @property
    def is_cancelled(self) -> bool:
        """キャンセル要求があるかどうか."""
        return self._cancelled

    def request_cancel(self) -> None:
        """キャンセルを要求（外部から呼ばれる）."""
        self._cancelled = True


class A2AAgentExecutor(ABC):
    """A2A Agent エグゼキュータ（抽象クラス）.

    python_a2a SDK の handle_message / handle_task に相当する統一インターフェース。
    """

    @abstractmethod
    async def execute(self, context: A2ARequestContext) -> None:
        """タスクを実行.

        context.emit_status() / emit_artifact() でイベントを発火する。
        """

    @abstractmethod
    async def cancel(self, context: A2ARequestContext) -> None:
        """実行中のタスクをキャンセル."""


class ResilientAgentExecutor(A2AAgentExecutor):
    """既存 ResilientAgent を A2A 対応にラップするアダプタ.

    Message → dict 変換 → agent.run() → dict → Artifact 変換の橋渡しを行う。
    """

    def __init__(self, agent: ResilientAgent[Any, Any]) -> None:
        """初期化.

        Args:
            agent: ラップする ResilientAgent インスタンス
        """
        self._agent = agent
        self._logger = logging.getLogger(f"agentflow.a2a.executor.{agent.name}")

    async def execute(self, context: A2ARequestContext) -> None:
        """A2A コンテキスト付きで Agent を実行.

        1. Message.parts[] → dict に変換
        2. context を agent に注入
        3. agent.run(dict) 呼び出し
        4. dict 結果 → Artifact に変換
        5. 状態遷移イベントを発火
        """
        try:
            # 状態: working
            await context.emit_status(A2ATaskState.WORKING)

            # Message → dict 変換
            input_data = self._message_to_dict(context.message)

            # Agent にコンテキストを注入して実行
            if hasattr(self._agent, "run_with_context"):
                result = await self._agent.run_with_context(input_data, context)
            else:
                # フォールバック: コンテキストなしで実行
                result = await self._agent.run(input_data)

            # 結果 → Artifact
            artifact = Artifact.from_data(result, name=f"{self._agent.name}_result")
            await context.emit_artifact(artifact)

            # 状態: completed
            await context.emit_status(A2ATaskState.COMPLETED)

        except Exception as e:
            self._logger.exception(f"Agent 実行失敗: {self._agent.name}")
            await context.emit_status(A2ATaskState.FAILED, str(e))
            raise

    async def cancel(self, context: A2ARequestContext) -> None:
        """キャンセルフラグを設定."""
        context.request_cancel()

    @staticmethod
    def _message_to_dict(message: Message) -> dict[str, Any]:
        """Message を Agent 入力 dict に変換.

        TextPart → "text" キー、DataPart → データをマージ。
        """
        result: dict[str, Any] = {}

        texts: list[str] = []
        for part in message.parts:
            if isinstance(part, TextPart):
                texts.append(part.text)
            elif isinstance(part, DataPart):
                result.update(part.data)

        if texts:
            result["text"] = "\n".join(texts)

        # メタデータもマージ
        if message.metadata:
            result["_metadata"] = message.metadata

        return result

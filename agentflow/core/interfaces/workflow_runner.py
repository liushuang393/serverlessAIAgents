"""IWorkflowRunner - ワークフロー実行器インターフェース.

Workflow を実際に実行するインターフェース（Preview 機能用）。

このインターフェースは安定しており、変更は慎重に行う必要があります。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from agentflow.core.interfaces.types import (
        DebugEvent,
        ExecutionEvent,
        WorkflowDefinition,
    )


@runtime_checkable
class IWorkflowRunner(Protocol):
    """ワークフロー実行器インターフェース.

    Studio の Preview 機能や実際のワークフロー実行に使用します。

    責務:
    - ワークフローの同期実行
    - ストリーム実行（リアルタイム進捗）
    - デバッグ実行（ブレークポイントサポート）

    実装要件:
    - run(): 同期実行、結果を返す
    - run_stream(): ストリーム実行、イベントを yield
    - run_debug(): デバッグモード実行

    使用例:
        >>> runner = WorkflowRunner()
        >>> # 同期実行
        >>> result = await runner.run(workflow, {"input": "..."})
        >>> # ストリーム実行（Preview 用）
        >>> async for event in runner.run_stream(workflow, inputs):
        ...     print(f"{event.type}: {event.message}")
    """

    async def run(
        self,
        workflow: WorkflowDefinition,
        inputs: dict[str, Any],
    ) -> dict[str, Any]:
        """ワークフローを同期実行.

        Args:
            workflow: ワークフロー定義
            inputs: 入力データ

        Returns:
            実行結果

        Raises:
            ExecutionError: 実行失敗
        """
        ...

    async def run_stream(
        self,
        workflow: WorkflowDefinition,
        inputs: dict[str, Any],
    ) -> AsyncIterator[ExecutionEvent]:
        """ワークフローをストリーム実行.

        リアルタイムで進捗イベントを発行しながら実行します。
        Studio の Preview 機能で使用されます。

        Args:
            workflow: ワークフロー定義
            inputs: 入力データ

        Yields:
            実行イベント

        Raises:
            ExecutionError: 実行失敗
        """
        ...

    async def run_debug(
        self,
        workflow: WorkflowDefinition,
        inputs: dict[str, Any],
        breakpoints: list[str] | None = None,
    ) -> AsyncIterator[DebugEvent]:
        """ワークフローをデバッグモードで実行.

        ブレークポイントでの一時停止、変数の検査などをサポートします。

        Args:
            workflow: ワークフロー定義
            inputs: 入力データ
            breakpoints: ブレークポイントを設定するノード ID のリスト

        Yields:
            デバッグイベント

        Raises:
            ExecutionError: 実行失敗
        """
        ...

    async def validate(
        self,
        workflow: WorkflowDefinition,
    ) -> list[str]:
        """ワークフローを検証.

        Args:
            workflow: ワークフロー定義

        Returns:
            エラーメッセージのリスト（空の場合は有効）
        """
        ...

    async def get_node_result(
        self,
        execution_id: str,
        node_id: str,
    ) -> dict[str, Any] | None:
        """特定ノードの実行結果を取得.

        Args:
            execution_id: 実行 ID
            node_id: ノード ID

        Returns:
            ノードの実行結果、または None
        """
        ...


__all__ = ["IWorkflowRunner"]

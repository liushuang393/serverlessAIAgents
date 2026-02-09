"""Agent Service - 統一Agent実行サービス.

API / CLI / Studio 全てが使用する Agent 実行サービス。

使用例:
    >>> # API から
    >>> service = AgentService()
    >>> result = await service.execute(agent_id="MyAgent", input_data={"text": "Hello"})
    >>>
    >>> # CLI から（進捗表示付き）
    >>> def on_progress(pct, msg):
    ...     print(f"[{pct:.0f}%] {msg}")
    >>> result = await service.execute_with_callback(
    ...     agent_id="MyAgent",
    ...     input_data={"text": "Hello"},
    ...     on_progress=on_progress,
    ... )
    >>>
    >>> # Studio から（WebSocket）
    >>> async for event in service.execute_stream(agent_id="MyAgent", input_data=data):
    ...     await ws.send(event.to_json())
"""

from __future__ import annotations

import time
from pathlib import Path
from typing import TYPE_CHECKING, Any

from agentflow.services.base import (
    LogLevel,
    ProgressEvent,
    ServiceBase,
    ServiceError,
    ServiceEvent,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


class AgentService(ServiceBase[dict[str, Any]]):
    """Agent実行サービス.

    全交互モード（API/CLI/Studio）で共通のAgent実行ロジックを提供。

    機能:
    - Agent の検出と読み込み
    - 実行と進捗追跡
    - エラーハンドリング
    - HITL サポート
    """

    def __init__(
        self,
        agents_dir: Path | None = None,
    ) -> None:
        """初期化.

        Args:
            agents_dir: Agentディレクトリ
        """
        super().__init__()
        self._agents_dir = agents_dir

    async def _execute_internal(
        self,
        execution_id: str,
        *,
        agent_id: str = "",
        agent_path: str | Path | None = None,
        input_data: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """Agent実行（内部）.

        Args:
            execution_id: 実行ID
            agent_id: Agent ID（@agent デコレータ名）
            agent_path: Agent パス（agent.yaml ベース）
            input_data: 入力データ
            **kwargs: 追加オプション

        Yields:
            実行イベント
        """
        start_time = time.time()
        input_data = input_data or {}

        # フェーズ1: Agent 読み込み
        yield self._emit_progress(
            execution_id, 10.0, "Loading agent...", phase="load"
        )

        try:
            agent = await self._load_agent(agent_id, agent_path)
        except Exception as e:
            yield self._emit_error(
                execution_id, "agent_not_found", f"Failed to load agent: {e}"
            )
            return

        yield self._emit_log(
            execution_id, f"Agent loaded: {agent_id or agent_path}", LogLevel.INFO
        )

        # フェーズ2: 入力検証
        yield self._emit_progress(
            execution_id, 20.0, "Validating input...", phase="validate"
        )

        # フェーズ3: 実行
        yield self._emit_progress(
            execution_id, 30.0, "Executing agent...", phase="execute"
        )

        try:
            # ストリームモードの場合
            if hasattr(agent, "run_stream"):
                progress = 30.0
                async for chunk in agent.run_stream(input_data):
                    progress = min(90.0, progress + 5.0)
                    yield ProgressEvent(
                        execution_id=execution_id,
                        progress=progress,
                        message=f"Processing: {chunk.get('type', 'step')}",
                        phase="execute",
                        data=chunk,
                    )
                result = chunk.get("result", {})
            # 通常実行
            elif hasattr(agent, "run"):
                result = await agent.run(input_data)
            elif hasattr(agent, "invoke"):
                result = await agent.invoke(input_data)
            else:
                msg = "invalid_agent"
                raise ServiceError(
                    msg,
                    "Agent has no run/invoke method",
                )

        except ServiceError:
            raise
        except Exception as e:
            self._logger.exception("Agent execution error")
            yield self._emit_error(
                execution_id, "execution_error", f"Agent execution failed: {e}"
            )
            return

        # フェーズ4: 完了
        duration_ms = (time.time() - start_time) * 1000
        yield self._emit_progress(
            execution_id, 100.0, "Completed", phase="complete"
        )
        yield self._emit_result(execution_id, result, duration_ms)

    async def _load_agent(
        self,
        agent_id: str,
        agent_path: str | Path | None,
    ) -> Any:
        """Agent を読み込み.

        Args:
            agent_id: Agent ID
            agent_path: Agent パス

        Returns:
            Agent インスタンス
        """
        # 方式1: @agent デコレータ
        if agent_id:
            try:
                from agentflow.agent_decorator import AgentClient
                return AgentClient.get(agent_id)
            except Exception as e:
                self._logger.warning(f"Failed to load decorated agent: {e}")

        # 方式2: agent.yaml ベース
        if agent_path:
            path = Path(agent_path)
            if path.is_dir():
                agent_yaml = path / "agent.yaml"
                if agent_yaml.exists():
                    from agentflow.core.schemas import SchemaLoader
                    loader = SchemaLoader()
                    loader.load_from_file(agent_yaml)
                    # TODO: メタデータからAgentをロード
                    msg = "agent.yaml based loading"
                    raise NotImplementedError(msg)

        msg = "agent_not_found"
        raise ServiceError(
            msg,
            f"Agent not found: {agent_id or agent_path}",
        )

    # =========================================================================
    # 追加API（Agent固有）
    # =========================================================================

    async def list_agents(self) -> list[dict[str, Any]]:
        """利用可能なAgentを一覧取得.

        Returns:
            Agent情報リスト
        """
        agents = []

        # @agent デコレータで定義されたAgent
        try:
            from agentflow.agent_decorator import AgentClient
            for name in AgentClient.list_agents():
                agents.append({
                    "id": name,
                    "type": "decorator",
                    "description": f"Decorator-based agent: {name}",
                })
        except Exception:
            pass

        # agent.yaml ベース（将来）
        # TODO: registry から取得

        return agents

    async def get_agent_info(self, agent_id: str) -> dict[str, Any] | None:
        """Agent詳細情報を取得.

        Args:
            agent_id: Agent ID

        Returns:
            Agent情報、または None
        """
        try:
            from agentflow.agent_decorator import AgentClient
            client = AgentClient.get(agent_id)
            return {
                "id": agent_id,
                "type": "decorator",
                "skills": getattr(client, "skills", []),
            }
        except Exception:
            return None

    async def get_agent_schema(self, agent_id: str) -> dict[str, Any] | None:
        """Agent入出力スキーマを取得.

        Args:
            agent_id: Agent ID

        Returns:
            スキーマ情報
        """
        try:
            from agentflow.agent_decorator import AgentClient
            client = AgentClient.get(agent_id)
            return {
                "input": getattr(client, "input_schema", {}),
                "output": getattr(client, "output_schema", {}),
            }
        except Exception:
            return None


# =============================================================================
# エクスポート
# =============================================================================

__all__ = ["AgentService"]

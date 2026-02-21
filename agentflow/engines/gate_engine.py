"""GateEngine - Gateインターセプト + Agent処理パターン.

前置チェック付きのEngine Pattern、以下に適用：
- コンプライアンスチェック後の処理
- 権限検証後の実行
- 条件満足後の処理

フロー: Gate Agent → (通過) → Main Agent → Response
              ↓ (拒否)
           拒否レスポンスを返却

使用例:
    >>> from agentflow.engines import GateEngine
    >>>
    >>> engine = GateEngine(
    ...     gate_agent=ComplianceChecker,
    ...     main_agent=ApprovalAgent,
    ...     gate_check=lambda r: r.get("compliant", False),
    ... )
    >>> result = await engine.run({"request": "..."})
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, cast

from agentflow.engines.base import BaseEngine, EngineConfig


if TYPE_CHECKING:
    from collections.abc import AsyncIterator, Callable


class GateEngine(BaseEngine):
    """Gateインターセプト + Agent処理エンジン.

    特徴：
    - 前置Gateチェック
    - Gate通過時のみメインAgentを実行
    - Gate拒否時はカスタムレスポンスを返却

    Attributes:
        gate_agent: Gate Agent（チェッカー）
        main_agent: メインAgent（プロセッサー）
        gate_check: Gate通過条件関数
        on_reject: 拒否時のコールバック/レスポンス
    """

    def __init__(
        self,
        gate_agent: type | Any,
        main_agent: type | Any,
        *,
        gate_check: Callable[[dict[str, Any]], bool] | None = None,
        on_reject: Callable[[dict[str, Any]], dict[str, Any]] | None = None,
        config: EngineConfig | None = None,
    ) -> None:
        """GateEngineを初期化.

        Args:
            gate_agent: Gate Agentクラスまたはインスタンス
            main_agent: メインAgentクラスまたはインスタンス
            gate_check: Gate通過条件（デフォルトはresult["passed"]をチェック）
            on_reject: 拒否時のレスポンスジェネレーター
            config: Engine設定
        """
        super().__init__(config)
        self._gate_cls = gate_agent
        self._main_cls = main_agent
        self._gate_instance: Any = None
        self._main_instance: Any = None
        self._gate_check = gate_check or (lambda r: r.get("passed", False))
        self._on_reject = on_reject or self._default_reject_response
        self._logger = logging.getLogger("agentflow.engines.gate")

    def _default_reject_response(self, gate_result: dict[str, Any]) -> dict[str, Any]:
        """デフォルト拒否レスポンス."""
        return {
            "status": "rejected",
            "reason": gate_result.get("reason", "Gate check failed"),
            "gate_result": gate_result,
        }

    async def _initialize(self) -> None:
        """2つのAgentを初期化."""
        # Gate Agent
        if isinstance(self._gate_cls, type):
            self._gate_instance = self._gate_cls()
        else:
            self._gate_instance = self._gate_cls

        # Main Agent
        if isinstance(self._main_cls, type):
            self._main_instance = self._main_cls()
        else:
            self._main_instance = self._main_cls

        # 初期化
        for agent in [self._gate_instance, self._main_instance]:
            if hasattr(agent, "initialize"):
                await agent.initialize()

        self._logger.info("GateEngine initialized")

    async def _run_agent(self, agent: Any, inputs: dict[str, Any]) -> dict[str, Any]:
        """単一Agentを実行."""
        if hasattr(agent, "run"):
            result = await agent.run(inputs)
        elif hasattr(agent, "invoke"):
            result = await agent.invoke(inputs)
        elif hasattr(agent, "process"):
            result = await agent.process(inputs)
        else:
            msg = f"Agent {agent} has no run/invoke/process method"
            raise AttributeError(msg)

        if isinstance(result, dict):
            return result
        if hasattr(result, "model_dump"):
            return cast("dict[str, Any]", result.model_dump())
        return {"result": result}

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Gate → Mainフローを実行."""
        # Step 1: Gateチェック
        gate_result = await self._run_agent(self._gate_instance, inputs)

        # Step 2: 通過判定
        if not self._gate_check(gate_result):
            self._logger.info("Gate rejected request")
            return self._on_reject(gate_result)

        # Step 3: メインAgentを実行
        self._logger.info("Gate passed, executing main agent")
        main_result = await self._run_agent(self._main_instance, inputs)

        return {
            "status": "success",
            "gate_result": gate_result,
            "result": main_result,
        }

    async def _execute_stream(self, inputs: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行."""
        gate_name = getattr(self._gate_instance, "name", self._gate_instance.__class__.__name__)
        main_name = getattr(self._main_instance, "name", self._main_instance.__class__.__name__)

        # Gate開始
        if event := self._emit_node_start(gate_name):
            yield event

        gate_result = await self._run_agent(self._gate_instance, inputs)

        # Gate完了
        if event := self._emit_node_complete(gate_name, gate_result):
            yield event

        # Gate結果をチェック
        if not self._gate_check(gate_result):
            yield {"type": "gate_rejected", "data": gate_result}
            yield {"type": "result", "data": self._on_reject(gate_result)}
            return

        # Main開始
        if event := self._emit_node_start(main_name):
            yield event

        main_result = await self._run_agent(self._main_instance, inputs)

        # Main完了
        if event := self._emit_node_complete(main_name, main_result):
            yield event

        yield {
            "type": "result",
            "data": {
                "status": "success",
                "gate_result": gate_result,
                "result": main_result,
            },
        }

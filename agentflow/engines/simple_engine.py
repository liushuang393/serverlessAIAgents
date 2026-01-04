# -*- coding: utf-8 -*-
"""SimpleEngine - 単一Agent質問応答パターン.

最もシンプルなEngine Pattern、以下に適用：
- 単一ターン質問応答
- カスタマーサービスBot
- シンプルなAIアシスタント

使用例:
    >>> from agentflow.engines import SimpleEngine
    >>> from agentflow import agent
    >>>
    >>> @agent
    ... class QAAgent:
    ...     system_prompt = "あなたはアシスタントです"
    ...     async def process(self, inputs: dict) -> dict:
    ...         return {"answer": f"回答: {inputs['question']}"}
    >>>
    >>> engine = SimpleEngine(agent=QAAgent)
    >>> result = await engine.run({"question": "こんにちは"})
    >>> print(result)  # {"answer": "回答: こんにちは"}
"""

from __future__ import annotations

import logging
from collections.abc import AsyncIterator
from typing import Any

from agentflow.engines.base import BaseEngine, EngineConfig


class SimpleEngine(BaseEngine):
    """単一Agent質問応答エンジン.
    
    特徴：
    - 最もシンプルなパターン
    - 単一Agentでリクエストを処理
    - Agent出力を直接返却
    
    Attributes:
        agent: Agentクラスまたはインスタンス
        skills: バインドするSkillsリスト
        tools: バインドするToolsリスト
    """

    def __init__(
        self,
        agent: type | Any,
        *,
        skills: list[str] | None = None,
        tools: list[str] | None = None,
        config: EngineConfig | None = None,
    ) -> None:
        """SimpleEngineを初期化.
        
        Args:
            agent: Agentクラスまたはインスタンス
            skills: Skills名リスト
            tools: Tools名リスト
            config: Engine設定
        """
        super().__init__(config)
        self._agent_cls = agent
        self._agent_instance: Any = None
        self._skills = skills or []
        self._tools = tools or []
        self._logger = logging.getLogger("agentflow.engines.simple")

    async def _initialize(self) -> None:
        """Agentインスタンスを初期化."""
        if isinstance(self._agent_cls, type):
            # クラスの場合、インスタンス化が必要
            self._agent_instance = self._agent_cls()
        else:
            # 既にインスタンスの場合
            self._agent_instance = self._agent_cls

        # Agentにinitializeメソッドがある場合、呼び出す
        if hasattr(self._agent_instance, "initialize"):
            await self._agent_instance.initialize()

        self._logger.info(f"SimpleEngine initialized with {self._agent_instance}")

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Agentを実行.
        
        Args:
            inputs: 入力データ
            
        Returns:
            Agent出力結果
        """
        # Agentを呼び出し
        if hasattr(self._agent_instance, "run"):
            result = await self._agent_instance.run(inputs)
        elif hasattr(self._agent_instance, "invoke"):
            result = await self._agent_instance.invoke(inputs)
        elif hasattr(self._agent_instance, "process"):
            result = await self._agent_instance.process(inputs)
        else:
            raise AttributeError(
                f"Agent {self._agent_instance} has no run/invoke/process method"
            )

        # dictを返却することを保証
        if isinstance(result, dict):
            return result
        elif hasattr(result, "model_dump"):
            return result.model_dump()
        else:
            return {"result": result}

    async def _execute_stream(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行（ノードイベントを発行）."""
        agent_name = getattr(
            self._agent_instance, "name", self._agent_instance.__class__.__name__
        )

        # ノード開始
        event = self._emit_node_start(agent_name)
        if event:
            yield event

        # 実行
        result = await self._execute(inputs)

        # ノード完了
        event = self._emit_node_complete(agent_name, result)
        if event:
            yield event

        # 結果
        yield {"type": "result", "data": result}


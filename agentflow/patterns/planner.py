"""Dynamic Planner - LLM 駆動の動的計画生成.

業界最佳実践に基づいた計画生成:
- Chain-of-thought prompting
- タスク分解
- 依存関係分析
- 変数参照メカニズム（ReWOO スタイル）

参考:
- LangChain Plan-and-Execute
- ReWOO: Reasoning Without Observations
"""

from __future__ import annotations

import logging
from datetime import UTC, datetime
from typing import Any

from pydantic import BaseModel, Field


class Step(BaseModel):
    """計画ステップ.

    各ステップは実行可能なアクションを表し、他のステップの結果を参照できます。

    Example:
        >>> step = Step(
        ...     step_id="E1",
        ...     description="Search for information",
        ...     tool="search",
        ...     parameters={"query": "AI agents"},
        ...     dependencies=[]
        ... )
    """

    step_id: str = Field(..., description="ステップ ID（例: E1, E2）")
    description: str = Field(..., description="ステップの説明")
    tool: str = Field(..., description="使用するツール")
    parameters: dict[str, Any] = Field(
        default_factory=dict,
        description="ツールパラメータ（#E1, #E2 などの変数参照を含む）",
    )
    dependencies: list[str] = Field(
        default_factory=list,
        description="依存するステップ ID のリスト",
    )


class Plan(BaseModel):
    """実行計画.

    タスクを達成するための一連のステップを含みます。

    Example:
        >>> plan = Plan(
        ...     task="Find AI agent frameworks",
        ...     steps=[step1, step2],
        ...     created_at=datetime.now(UTC).isoformat()
        ... )
    """

    task: str = Field(..., description="元のタスク")
    steps: list[Step] = Field(..., description="実行ステップリスト")
    created_at: str = Field(..., description="作成時刻（ISO 8601 形式）")
    reasoning: str = Field(
        default="",
        description="計画の推論プロセス",
    )


class DynamicPlanner:
    """LLM 駆動の動的プランナー.

    業界最佳実践に基づいた計画生成:
    - Chain-of-thought prompting
    - タスク分解
    - 依存関係分析
    - 変数参照メカニズム（#E1, #E2）

    Example:
        >>> planner = DynamicPlanner(llm=my_llm, available_tools=["search", "calculate"])
        >>> plan = await planner.create_plan("What is 2+2 and what is the square of that?")
        >>> print(plan.steps[0].step_id)  # E1
        >>> print(plan.steps[1].parameters)  # {"number": "#E1"}
    """

    def __init__(
        self,
        llm: Any,
        available_tools: list[str],
        max_steps: int = 10,
        logger: logging.Logger | None = None,
    ) -> None:
        """動的プランナーを初期化.

        Args:
            llm: LLM インスタンス（generate メソッドを持つ）
            available_tools: 利用可能なツールリスト
            max_steps: 最大ステップ数（デフォルト: 10）
            logger: ロガーインスタンス（オプション）
        """
        self._llm = llm
        self._available_tools = available_tools
        self._max_steps = max_steps
        self._logger = logger or logging.getLogger(__name__)

    async def create_plan(
        self,
        task: str,
        context: dict[str, Any] | None = None,
    ) -> Plan:
        """タスクの実行計画を生成.

        Args:
            task: タスク説明
            context: 追加コンテキスト（オプション）

        Returns:
            実行計画

        Example:
            >>> plan = await planner.create_plan(
            ...     "Find the capital of France and its population",
            ...     context={"language": "en"}
            ... )
        """
        self._logger.info(f"Creating plan for task: {task}")

        # プロンプトを構築
        prompt = self._build_planning_prompt(task, context or {})

        # LLM で計画を生成
        response = await self._llm.generate(prompt)

        # 計画をパース
        plan = self._parse_plan(response, task)

        self._logger.info(f"Plan created with {len(plan.steps)} steps")
        return plan

    def _build_planning_prompt(self, task: str, context: dict[str, Any]) -> str:
        """計画生成用のプロンプトを構築.

        Args:
            task: タスク説明
            context: 追加コンテキスト

        Returns:
            プロンプト文字列
        """
        tools_str = "\n".join(f"- {tool}" for tool in self._available_tools)
        context_str = "\n".join(f"{k}: {v}" for k, v in context.items()) if context else "None"

        prompt = f"""You are a task planning assistant. Given a task, create a detailed execution plan.

Available tools:
{tools_str}

Context:
{context_str}

Task: {task}

Create a plan with the following format:
Plan: [reasoning about the task]
E1: [tool_name]([parameters]) - [description]
Plan: [reasoning about next step]
E2: [tool_name]([parameters, can reference #E1]) - [description]
...

Rules:
1. Each step should have a unique ID (E1, E2, E3, ...)
2. You can reference previous step results using #E1, #E2, etc.
3. Keep the plan focused and efficient
4. Maximum {self._max_steps} steps
5. Use only the available tools listed above

Example:
Task: What are the stats for the quarterbacks of the super bowl contenders this year?

Plan: I need to know the teams playing in the superbowl this year
E1: search("Who is competing in the superbowl?") - Find current superbowl teams
Plan: I need to know the quarterbacks for each team
E2: llm("Quarterback for the first team of #E1") - Extract first team's QB
Plan: I need to know the quarterbacks for each team
E3: llm("Quarterback for the second team of #E1") - Extract second team's QB
Plan: I need to look up stats for the first quarterback
E4: search("Stats for #E2") - Get stats for first QB
Plan: I need to look up stats for the second quarterback
E5: search("Stats for #E3") - Get stats for second QB

Now create a plan for the given task:
"""
        return prompt

    def _parse_plan(self, response: str, task: str) -> Plan:
        """LLM レスポンスから計画をパース.

        Args:
            response: LLM レスポンス
            task: 元のタスク

        Returns:
            パースされた計画
        """
        steps: list[Step] = []
        reasoning_parts: list[str] = []

        lines = response.strip().split("\n")
        for line in lines:
            line = line.strip()
            if not line:
                continue

            # Plan: で始まる行は推論
            if line.startswith("Plan:"):
                reasoning = line[5:].strip()
                reasoning_parts.append(reasoning)
                continue

            # E1:, E2: などで始まる行はステップ
            if line[0] == "E" and ":" in line:
                try:
                    step = self._parse_step_line(line)
                    steps.append(step)
                except Exception as e:
                    self._logger.warning(f"Failed to parse step line: {line}, error: {e}")
                    continue

        if not steps:
            # フォールバック: 単一ステップの計画
            self._logger.warning("No steps parsed, creating fallback plan")
            steps = [
                Step(
                    step_id="E1",
                    description=task,
                    tool=self._available_tools[0] if self._available_tools else "unknown",
                    parameters={"task": task},
                    dependencies=[],
                )
            ]

        return Plan(
            task=task,
            steps=steps,
            created_at=datetime.now(UTC).isoformat(),
            reasoning="\n".join(reasoning_parts),
        )

    def _parse_step_line(self, line: str) -> Step:
        """ステップ行をパース.

        Args:
            line: ステップ行（例: "E1: search('query') - Find information"）

        Returns:
            パースされたステップ
        """
        # E1: search("query") - description の形式をパース
        step_id, rest = line.split(":", 1)
        step_id = step_id.strip()

        # ツール名とパラメータを抽出
        if " - " in rest:
            tool_part, description = rest.split(" - ", 1)
        else:
            tool_part = rest
            description = ""

        tool_part = tool_part.strip()
        description = description.strip()

        # ツール名を抽出
        if "(" in tool_part:
            tool_name = tool_part[: tool_part.index("(")].strip()
            params_str = tool_part[tool_part.index("(") + 1 : tool_part.rindex(")")].strip()
        else:
            tool_name = tool_part
            params_str = ""

        # パラメータをパース（簡易実装）
        parameters: dict[str, Any] = {}
        if params_str:
            # 簡易的なパース（"key": "value" 形式）
            parameters = {"query": params_str.strip('"').strip("'")}

        # 依存関係を検出（#E1, #E2 などの参照）
        dependencies: list[str] = []
        import re

        refs = re.findall(r"#(E\d+)", params_str)
        dependencies.extend(refs)

        return Step(
            step_id=step_id,
            description=description or tool_part,
            tool=tool_name,
            parameters=parameters,
            dependencies=dependencies,
        )


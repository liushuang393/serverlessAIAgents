"""
Tool Calling アーキテクチャパターン

このモジュールは、LLMが必要に応じて外部ツールを呼び出し、
その結果を使用して最終的な回答を生成するパターンを実装します。
"""

import json
import re
import time
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Optional

from ..core import ToolInterface
from ..core.models import Message, MessageRole, ToolDefinition, ToolResult
from ..utils.logging import get_logger
from .base import Agent

logger = get_logger(__name__)


class ToolCallStatus(str, Enum):
    """ツール呼び出しの状態"""

    PENDING = "pending"  # 待機中
    EXECUTING = "executing"  # 実行中
    COMPLETED = "completed"  # 完了
    FAILED = "failed"  # 失敗


@dataclass
class ToolCall:
    """ツール呼び出しのデータクラス"""

    id: str
    tool_name: str
    parameters: Dict[str, Any]
    status: ToolCallStatus = ToolCallStatus.PENDING
    result: Optional[ToolResult] = None
    start_time: Optional[float] = None
    end_time: Optional[float] = None

    @property
    def execution_time(self) -> Optional[float]:
        """実行時間を取得する"""
        if self.start_time and self.end_time:
            return self.end_time - self.start_time
        return None


class ToolCallingAgent(Agent):
    """ツール呼び出し機能を持つエージェント"""

    def __init__(
        self,
        name: str = "ToolCallingAgent",
        llm_provider: Optional[Any] = None,
        tool_manager: Optional[ToolInterface] = None,
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        Tool Callingエージェントを初期化する

        Args:
            name: エージェント名
            llm_provider: LLMプロバイダー
            tool_manager: ツールマネージャー
            config: 設定辞書
        """
        super().__init__(name, config)
        self.llm = llm_provider
        self.tools = tool_manager
        self.config = config if config is not None else {}
        # 設定値
        self.max_tool_calls = self.config.get("max_tool_calls", 5)
        self.max_iterations = self.config.get("max_iterations", 3)
        self.tool_call_timeout = self.config.get("tool_call_timeout", 30.0)
        self.enable_parallel_calls = self.config.get("enable_parallel_calls", True)

        if not self.llm:
            raise ValueError("LLMプロバイダーが必要です")
        if not self.tools:
            raise ValueError("ツールマネージャーが必要です")

        logger.info(f"ToolCallingAgent '{self.name}' を初期化しました")

    async def process(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        入力を処理してツールを使用した応答を生成する

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            str: 処理結果
        """
        context = context or {}
        conversation_history = []
        tool_calls_made = 0

        # 初期システムメッセージ
        system_message = self._create_system_message()
        conversation_history.append(system_message)

        # ユーザーメッセージ
        user_message = Message(role=MessageRole.USER, content=input_text)
        conversation_history.append(user_message)

        try:
            for iteration in range(self.max_iterations):
                logger.info(f"反復 {iteration + 1}/{self.max_iterations} を開始")

                # LLMに応答を生成させる
                if self.llm is None:
                    raise ValueError("LLMプロバイダーが設定されていません")
                response = await self.llm.generate(conversation_history)
                assistant_message = Message(
                    role=MessageRole.ASSISTANT, content=response.content
                )
                conversation_history.append(assistant_message)

                # ツール呼び出しが必要かチェック
                tool_calls = self._extract_tool_calls(response.content)

                if not tool_calls:
                    # ツール呼び出しが不要な場合、最終回答として返す
                    logger.info("ツール呼び出しが不要です。最終回答を返します")
                    return self._extract_final_answer(response.content)

                # ツール呼び出し数の制限チェック
                if tool_calls_made + len(tool_calls) > self.max_tool_calls:
                    logger.warning(f"ツール呼び出し数の上限に達しました ({self.max_tool_calls})")
                    break

                # ツールを実行
                tool_results = await self._execute_tool_calls(tool_calls)
                tool_calls_made += len(tool_calls)

                # ツール結果を会話履歴に追加
                for tool_call, result in zip(tool_calls, tool_results):
                    tool_message = Message(
                        role=MessageRole.TOOL,
                        content=self._format_tool_result(tool_call, result),
                    )
                    conversation_history.append(tool_message)

                # 全てのツールが成功した場合、次の反復へ
                if all(result.success for result in tool_results):
                    continue
                else:
                    # 一部のツールが失敗した場合、エラー情報を含めて継続
                    logger.warning("一部のツール呼び出しが失敗しました")

            # 最大反復数に達した場合、最後の応答を返す
            logger.info("最大反復数に達しました。最後の応答を返します")
            return self._extract_final_answer(conversation_history[-1].content)

        except Exception as e:
            logger.error(f"ツール呼び出し処理エラー: {e}")
            return f"申し訳ありませんが、処理中にエラーが発生しました: {str(e)}"

    def _create_system_message(self) -> Message:
        """システムメッセージを作成する"""
        if self.tools is None:
            raise ValueError("ツールマネージャーが設定されていません")
        available_tools = self.tools.get_available_tools()
        tools_description = self._format_tools_description(available_tools)

        system_content = f"""あなたは様々なツールを使用できる高度なAIアシスタントです。

利用可能なツール:
{tools_description}

ツールを使用する場合は、以下の形式で呼び出してください:
```tool_call
{{
    "tool_name": "ツール名",
    "parameters": {{
        "param1": "値1",
        "param2": "値2"
    }}
}}
```

複数のツールを同時に呼び出すことも可能です。
ツールの結果を受け取った後、それを基に最終的な回答を生成してください。

ツールが不要な場合は、直接回答してください。
"""

        return Message(role=MessageRole.SYSTEM, content=system_content)

    def _format_tools_description(self, tools: List[ToolDefinition]) -> str:
        """ツールの説明を整形する"""
        descriptions = []
        for tool in tools:
            desc = f"- **{tool.name}**: {tool.description}"
            if tool.parameters:
                params = ", ".join(f"{k}: {v}" for k, v in tool.parameters.items())
                desc += f"\n  パラメータ: {params}"
            descriptions.append(desc)

        return "\n".join(descriptions)

    def _extract_tool_calls(self, content: str) -> List[ToolCall]:
        """応答からツール呼び出しを抽出する"""
        tool_calls = []

        # tool_callブロックを検索
        pattern = r"```tool_call\s*\n(.*?)\n```"
        matches = re.findall(pattern, content, re.DOTALL)

        for i, match in enumerate(matches):
            try:
                call_data = json.loads(match.strip())
                tool_call = ToolCall(
                    id=f"call_{int(time.time())}_{i}",
                    tool_name=call_data["tool_name"],
                    parameters=call_data.get("parameters", {}),
                )
                tool_calls.append(tool_call)

            except (json.JSONDecodeError, KeyError) as e:
                logger.warning(f"ツール呼び出しの解析に失敗: {e}")
                continue

        return tool_calls

    async def _execute_tool_calls(self, tool_calls: List[ToolCall]) -> List[ToolResult]:
        """ツール呼び出しを実行する"""
        results: List[ToolResult] = []

        if self.enable_parallel_calls and len(tool_calls) > 1:
            # 並列実行
            import asyncio

            tasks = [self._execute_single_tool_call(call) for call in tool_calls]
            results = await asyncio.gather(*tasks, return_exceptions=True)

            # 例外を処理
            processed_results: List[ToolResult] = []
            for i, result in enumerate(results):
                if isinstance(result, Exception):
                    tool_calls[i].status = ToolCallStatus.FAILED
                    error_result = ToolResult(
                        success=False,
                        result=None,
                        error_message=str(result),
                        execution_time=0.0,
                    )
                    tool_calls[i].result = error_result
                    processed_results.append(error_result)
                else:
                    processed_results.append(result)
            results = processed_results
        else:
            # 順次実行
            for tool_call in tool_calls:
                result = await self._execute_single_tool_call(tool_call)
                results.append(result)

        return results

    async def _execute_single_tool_call(self, tool_call: ToolCall) -> ToolResult:
        """単一のツール呼び出しを実行する"""
        tool_call.status = ToolCallStatus.EXECUTING
        tool_call.start_time = time.time()

        try:
            logger.info(f"ツール '{tool_call.tool_name}' を実行中...")

            if self.tools is None:
                raise ValueError("ツールマネージャーが設定されていません")
            result = await self.tools.execute(tool_call.tool_name, tool_call.parameters)

            tool_call.status = ToolCallStatus.COMPLETED
            tool_call.result = result
            tool_call.end_time = time.time()

            logger.info(
                f"ツール '{tool_call.tool_name}' の実行完了 "
                f"(実行時間: {tool_call.execution_time:.2f}秒)"
            )

            return result

        except Exception as e:
            tool_call.status = ToolCallStatus.FAILED
            tool_call.end_time = time.time()

            error_result = ToolResult(
                success=False,
                result=None,
                error_message=str(e),
                execution_time=tool_call.execution_time or 0.0,
            )
            tool_call.result = error_result

            logger.error(f"ツール '{tool_call.tool_name}' の実行に失敗: {e}")

            return error_result

    def _format_tool_result(self, tool_call: ToolCall, result: ToolResult) -> str:
        """ツール結果を整形する"""
        if result.success:
            return f"ツール '{tool_call.tool_name}' の実行結果:\n{result.result}"
        else:
            return f"ツール '{tool_call.tool_name}' の実行に失敗しました: {result.error_message}"

    def _extract_final_answer(self, content: str) -> str:
        """最終回答を抽出する"""
        # tool_callブロックを除去
        content = re.sub(r"```tool_call\s*\n.*?\n```", "", content, flags=re.DOTALL)

        # 空行を整理
        lines = [line.strip() for line in content.split("\n") if line.strip()]

        return "\n".join(lines)


class FunctionCallingAgent(ToolCallingAgent):
    """関数呼び出し形式のツール呼び出しエージェント"""

    def _create_system_message(self) -> Message:
        """関数呼び出し形式のシステムメッセージを作成する"""
        if self.tools is None:
            raise ValueError("ツールマネージャーが設定されていません")
        available_tools = self.tools.get_available_tools()
        functions_schema = self._create_functions_schema(available_tools)

        system_content = f"""あなたは関数を呼び出すことができるAIアシスタントです。

利用可能な関数:
{json.dumps(functions_schema, indent=2, ensure_ascii=False)}

関数を呼び出す場合は、以下の形式を使用してください:
```function_call
{{
    "name": "関数名",
    "arguments": {{
        "arg1": "値1",
        "arg2": "値2"
    }}
}}
```

関数の結果を受け取った後、それを基に回答を生成してください。
"""

        return Message(role=MessageRole.SYSTEM, content=system_content)

    def _create_functions_schema(
        self, tools: List[ToolDefinition]
    ) -> List[Dict[str, Any]]:
        """関数スキーマを作成する"""
        functions = []
        for tool in tools:
            function_schema = {
                "name": tool.name,
                "description": tool.description,
                "parameters": {
                    "type": "object",
                    "properties": tool.parameters or {},
                    "required": list(tool.parameters.keys()) if tool.parameters else [],
                },
            }
            functions.append(function_schema)

        return functions

    def _extract_tool_calls(self, content: str) -> List[ToolCall]:
        """関数呼び出し形式からツール呼び出しを抽出する"""
        tool_calls = []

        # function_callブロックを検索
        pattern = r"```function_call\s*\n(.*?)\n```"
        matches = re.findall(pattern, content, re.DOTALL)

        for i, match in enumerate(matches):
            try:
                call_data = json.loads(match.strip())
                tool_call = ToolCall(
                    id=f"func_call_{int(time.time())}_{i}",
                    tool_name=call_data["name"],
                    parameters=call_data.get("arguments", {}),
                )
                tool_calls.append(tool_call)

            except (json.JSONDecodeError, KeyError) as e:
                logger.warning(f"関数呼び出しの解析に失敗: {e}")
                continue

        return tool_calls

"""
Augmented LLM アーキテクチャパターン

このモジュールは、LLM + Memory + Tool を組み合わせた基本的なエージェントパターンを実装します。
これは最も基本的で汎用性の高いアーキテクチャパターンです。
"""

import json
import time
from typing import Any, Dict, List, Optional

from ..core import MemoryInterface, ToolInterface
from ..utils.logging import get_logger
from .base import Agent

logger = get_logger(__name__)


class AugmentedLLM(Agent):
    """LLMにMemoryとToolを組み合わせた基本的なエージェント"""

    def __init__(
        self,
        llm_provider: Any,
        memory: Optional[MemoryInterface] = None,
        tool_manager: Optional[ToolInterface] = None,
        name: str = "AugmentedLLM",
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        Augmented LLMを初期化する

        Args:
            llm_provider: LLMプロバイダー
            memory: メモリコンポーネント
            tool_manager: ツールマネージャー
            name: エージェント名
            config: 設定辞書
        """
        super().__init__(name, config)
        self.llm = llm_provider
        self.memory = memory
        self.tools = tool_manager
        self.config = config if config is not None else {}
        # 設定値
        self.max_memory_items = self.config.get("max_memory_items", 5)
        self.memory_threshold = self.config.get("memory_threshold", 0.7)
        self.enable_tool_use = self.config.get("enable_tool_use", True)
        self.max_tool_calls = self.config.get("max_tool_calls", 3)

        logger.info(f"Augmented LLM '{self.name}' を初期化しました")

    async def process(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        入力を処理して応答を生成する

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            str: 処理結果
        """
        if not input_text.strip():
            return "申し訳ありませんが、入力が空です。何かご質問やご依頼はありますか？"

        context = context if context is not None else {}
        try:
            # 1. 関連する記憶を検索
            relevant_memories = await self._search_memory(input_text)
            # 2. ツール実行
            tool_results = await self._execute_tools_if_needed(input_text, context)
            # 3. LLMで応答生成
            response = await self._generate_response(
                input_text, relevant_memories, tool_results, context
            )
            # 4. 応答を記憶に保存
            await self._store_interaction(input_text, response)
            return response
        except Exception as e:
            logger.error(f"処理中にエラーが発生しました: {e}")
            return f"申し訳ありませんが、処理中にエラーが発生しました: {str(e)}"

    async def _search_memory(self, query: str) -> List[Dict[str, Any]]:
        """
        関連する記憶を検索する

        Args:
            query: 検索クエリ

        Returns:
            List[Dict[str, Any]]: 関連する記憶のリスト
        """
        if not self.memory:
            return []
        try:
            memory_items = await self.memory.search(
                query, limit=self.max_memory_items, threshold=self.memory_threshold
            )
            memories = []
            for item in memory_items:
                memories.append(
                    {
                        "content": item.content,
                        "similarity": item.similarity_score,
                        "metadata": item.metadata,
                    }
                )
            logger.debug(f"記憶検索結果: {len(memories)}件")
            return memories
        except Exception as e:
            logger.warning(f"記憶検索中にエラーが発生しました: {e}")
            return []

    async def _execute_tools_if_needed(
        self, input_text: str, context: Dict[str, Any]
    ) -> List[Dict[str, Any]]:
        """
        必要に応じてツールを実行する

        Args:
            input_text: 入力テキスト
            context: コンテキスト

        Returns:
            List[Dict[str, Any]]: ツール実行結果のリスト
        """
        if not self.tools or not self.enable_tool_use:
            return []

        try:
            # LLMにツール使用の必要性を判定させる
            tool_plan = await self._plan_tool_usage(input_text, context)

            if not tool_plan:
                return []

            # ツールを実行
            tool_results = []
            for i, tool_call in enumerate(tool_plan):
                if i >= self.max_tool_calls:
                    logger.warning(f"最大ツール呼び出し回数({self.max_tool_calls})に達しました")
                    break

                result = await self.tools.execute(
                    tool_call["name"], tool_call.get("parameters", {})
                )

                tool_results.append(
                    {
                        "tool": tool_call["name"],
                        "parameters": tool_call.get("parameters", {}),
                        "result": result.result if result.success else None,
                        "success": result.success,
                        "error": result.error_message,
                    }
                )

            logger.debug(f"ツール実行結果: {len(tool_results)}件")
            return tool_results

        except Exception as e:
            logger.warning(f"ツール実行中にエラーが発生しました: {e}")
            return []

    async def _plan_tool_usage(
        self, input_text: str, context: Dict[str, Any]
    ) -> List[Dict[str, Any]]:
        """
        ツール使用計画を立てる

        Args:
            input_text: 入力テキスト
            context: コンテキスト

        Returns:
            List[Dict[str, Any]]: ツール呼び出し計画のリスト
        """
        if not self.llm:
            return []

        # 利用可能なツールを取得
        if not self.tools:
            return []

        available_tools = self.tools.get_available_tools()
        if not available_tools:
            return []

        # ツール情報を整理
        tools_info = []
        for tool in available_tools:
            tools_info.append(
                {
                    "name": tool.name,
                    "description": tool.description,
                    "parameters": tool.parameters,
                }
            )

        # LLMにツール使用計画を依頼
        prompt = f"""
以下の入力に対して、必要なツールがあれば使用計画を立ててください。

入力: "{input_text}"

利用可能なツール:
{json.dumps(tools_info, ensure_ascii=False, indent=2)}

ツールが必要な場合は、以下の形式でJSONレスポンスを返してください:
[
    {{
        "name": "ツール名",
        "parameters": {{
            "パラメータ名": "値"
        }},
        "reason": "使用理由"
    }}
]

ツールが不要な場合は空の配列 [] を返してください。
"""

        try:
            response = await self.llm.generate(prompt)
            tool_plan = json.loads(response.strip())

            if isinstance(tool_plan, list):
                return tool_plan
            else:
                return []

        except (json.JSONDecodeError, Exception) as e:
            logger.warning(f"ツール計画の解析に失敗しました: {e}")
            return []

    async def _generate_response(
        self,
        input_text: str,
        memories: List[Dict[str, Any]],
        tool_results: List[Dict[str, Any]],
        context: Dict[str, Any],
    ) -> str:
        """
        LLMで応答を生成する

        Args:
            input_text: 入力テキスト
            memories: 関連する記憶
            tool_results: ツール実行結果
            context: コンテキスト

        Returns:
            str: 生成された応答
        """
        if not self.llm:
            return "LLMプロバイダーが設定されていません。"

        # プロンプトを構築
        prompt_parts = [f"ユーザーの入力: {input_text}"]

        # 記憶情報を追加
        if memories:
            memory_text = "\n".join(
                [
                    f"- {memory['content']} (類似度: {memory['similarity']:.2f})"
                    for memory in memories
                ]
            )
            prompt_parts.append(f"\n関連する記憶:\n{memory_text}")

        # ツール結果を追加
        if tool_results:
            tool_text = "\n".join(
                [
                    f"- {result['tool']}: "
                    f"{result['result'] if result['success'] else result['error']}"
                    for result in tool_results
                ]
            )
            prompt_parts.append(f"\nツール実行結果:\n{tool_text}")

        # コンテキスト情報を追加
        if context:
            context_text = json.dumps(context, ensure_ascii=False, indent=2)
            prompt_parts.append(f"\n追加コンテキスト:\n{context_text}")

        # 指示を追加
        prompt_parts.append(
            """
上記の情報を参考にして、ユーザーの入力に対して適切で有用な応答を生成してください。
記憶やツール結果がある場合は、それらを活用して回答の質を向上させてください。
"""
        )

        prompt = "\n".join(prompt_parts)

        try:
            response = await self.llm.generate(prompt)
            return str(response).strip()

        except Exception as e:
            logger.error(f"LLM応答生成中にエラーが発生しました: {e}")
            return f"申し訳ありませんが、応答生成中にエラーが発生しました: {str(e)}"

    async def _store_interaction(self, input_text: str, response: str) -> None:
        """
        対話を記憶に保存する

        Args:
            input_text: 入力テキスト
            response: 応答テキスト
        """
        if not self.memory:
            return

        try:
            # 入力と応答をペアで保存
            interaction = f"ユーザー: {input_text}\nアシスタント: {response}"

            metadata = {
                "type": "interaction",
                "timestamp": time.time(),
                "input_length": len(input_text),
                "response_length": len(response),
            }

            await self.memory.store(interaction, metadata)
            logger.debug("対話を記憶に保存しました")

        except Exception as e:
            logger.warning(f"記憶保存中にエラーが発生しました: {e}")

    async def add_knowledge(
        self, knowledge: str, metadata: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        知識を記憶に追加する

        Args:
            knowledge: 追加する知識
            metadata: 知識のメタデータ

        Returns:
            str: 保存された記憶のID
        """
        if not self.memory:
            raise RuntimeError("メモリコンポーネントが設定されていません")

        metadata = metadata or {}
        metadata.update({"type": "knowledge", "timestamp": time.time()})

        memory_id = await self.memory.store(knowledge, metadata)
        logger.info(f"知識を記憶に追加しました（ID: {memory_id[:8]}...）")

        return memory_id

    async def get_status(self) -> Dict[str, Any]:
        """
        エージェントの状態を取得する

        Returns:
            Dict[str, Any]: 状態情報
        """
        status = {
            "name": self.name,
            "config": self.config,
            "metrics": self.get_metrics(),
            "components": {
                "llm": self.llm is not None,
                "memory": self.memory is not None,
                "tools": self.tools is not None,
            },
        }

        # メモリ情報を追加
        if self.memory:
            try:
                memory_count = await self.memory.count()
                status["memory_count"] = memory_count  # type: ignore
            except Exception:
                status["memory_count"] = "unknown"

        # ツール情報を追加
        if self.tools:
            try:
                available_tools = self.tools.get_available_tools()
                status["available_tools"] = [tool.name for tool in available_tools]
            except Exception:
                status["available_tools"] = []

        return status

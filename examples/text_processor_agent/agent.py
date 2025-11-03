"""テキスト処理エージェントの例.

このエージェントは AgentBlock を継承して、テキスト処理機能を提供します。
"""

from typing import Any

from agentflow.core.agent_block import AgentBlock


class TextProcessorAgent(AgentBlock):
    """テキスト処理エージェント.

    このエージェントは以下の機能を提供します：
    - テキストの大文字変換
    - テキストの小文字変換
    - テキストの長さカウント
    - 単語数カウント
    """

    async def initialize(self) -> None:
        """エージェントを初期化."""
        await super().initialize()
        print("TextProcessorAgent initialized")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """テキストを処理.

        Args:
            input_data: 入力データ
                - text (str): 処理するテキスト
                - operation (str): 実行する操作 (upper, lower, length, word_count)

        Returns:
            出力データ
                - result (str | int): 処理結果
                - original_text (str): 元のテキスト
                - operation (str): 実行した操作
        """
        text = input_data.get("text", "")
        operation = input_data.get("operation", "upper")

        result: str | int

        if operation == "upper":
            result = text.upper()
        elif operation == "lower":
            result = text.lower()
        elif operation == "length":
            result = len(text)
        elif operation == "word_count":
            result = len(text.split())
        else:
            result = text

        return {
            "result": result,
            "original_text": text,
            "operation": operation,
        }

    async def cleanup(self) -> None:
        """エージェントをクリーンアップ."""
        await super().cleanup()
        print("TextProcessorAgent cleaned up")


# 使用例
async def main() -> None:
    """メイン関数."""
    # エージェントを作成
    agent = TextProcessorAgent(metadata_path="agent.yaml")

    # 初期化
    await agent.initialize()

    # 実行
    result = await agent.run({
        "text": "Hello World",
        "operation": "upper",
    })
    print(f"Result: {result}")

    # クリーンアップ
    await agent.cleanup()


# 非同期コンテキストマネージャーを使用した例
async def main_with_context() -> None:
    """コンテキストマネージャーを使用したメイン関数."""
    async with TextProcessorAgent(metadata_path="agent.yaml") as agent:
        result = await agent.run({
            "text": "Hello World",
            "operation": "word_count",
        })
        print(f"Word count: {result['result']}")


if __name__ == "__main__":
    import asyncio

    asyncio.run(main())
    asyncio.run(main_with_context())


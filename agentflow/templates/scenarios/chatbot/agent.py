"""{{ agent_name | title }} エージェント.

{{ agent_description }}

Author: {{ author }} <{{ email }}>
"""

from __future__ import annotations

from typing import Any

from agentflow.core.agent_block import AgentBlock


class {{ agent_name | replace('-', '_') | title | replace('_', '') }}Agent(AgentBlock):
    """{{ agent_name | title }} エージェント.
    
    対話型チャットボットエージェント。
    """

    async def initialize(self) -> None:
        """エージェントを初期化.
        
        LLM クライアントと会話履歴ストレージをセットアップします。
        """
        await super().initialize()
        
        # LLM 設定
        self.llm_provider = "{{ llm_provider }}"
        self.model_name = "{{ model_name }}"
        self.enable_memory = {{ enable_memory | lower }}
        self.max_history_length = {{ max_history_length }}
        
        # 会話履歴
        if self.enable_memory:
            self.conversations: dict[str, list[dict[str, str]]] = {}
        
        print(f"Initialized {self.metadata.name} agent")
        print(f"LLM Provider: {self.llm_provider}")
        print(f"Model: {self.model_name}")
        print(f"Memory enabled: {self.enable_memory}")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """エージェントを実行.
        
        Args:
            input_data: 入力データ
                - message: ユーザーメッセージ
                - session_id: セッション ID（会話履歴用）
        
        Returns:
            実行結果
                - response: ボットの応答
                - metadata: メタデータ
        """
        message = input_data.get("message")
        session_id = input_data.get("session_id", "default")
        
        if not message:
            raise ValueError("message is required")
        
        # 会話履歴を取得
        history = []
        if self.enable_memory:
            history = self.conversations.get(session_id, [])
        
        # LLM を呼び出す
        response, metadata = await self._generate_response(message, history)
        
        # 会話履歴を更新
        if self.enable_memory:
            self._update_history(session_id, message, response)
        
        return {
            "response": response,
            "metadata": metadata,
        }

    async def _generate_response(
        self,
        message: str,
        history: list[dict[str, str]],
    ) -> tuple[str, dict[str, Any]]:
        """LLM を使用して応答を生成.
        
        Args:
            message: ユーザーメッセージ
            history: 会話履歴
        
        Returns:
            応答とメタデータのタプル
        """
        # TODO: 実際の LLM 呼び出しを実装
        print(f"Generating response for: {message}")
        print(f"History length: {len(history)}")
        
        # ダミー応答
        response = f"You said: {message}. This is a response from {self.model_name}."
        metadata = {
            "model": self.model_name,
            "tokens_used": 50,
            "confidence": 0.95,
        }
        
        return response, metadata

    def _update_history(
        self,
        session_id: str,
        user_message: str,
        bot_response: str,
    ) -> None:
        """会話履歴を更新.
        
        Args:
            session_id: セッション ID
            user_message: ユーザーメッセージ
            bot_response: ボットの応答
        """
        if session_id not in self.conversations:
            self.conversations[session_id] = []
        
        # ユーザーメッセージを追加
        self.conversations[session_id].append({
            "role": "user",
            "content": user_message,
        })
        
        # ボット応答を追加
        self.conversations[session_id].append({
            "role": "assistant",
            "content": bot_response,
        })
        
        # 履歴長を制限
        if len(self.conversations[session_id]) > self.max_history_length * 2:
            self.conversations[session_id] = self.conversations[session_id][
                -self.max_history_length * 2:
            ]

    async def cleanup(self) -> None:
        """エージェントをクリーンアップ.
        
        リソースを解放します。
        """
        print(f"Cleaning up {self.metadata.name} agent")
        if self.enable_memory:
            print(f"Active sessions: {len(self.conversations)}")
        await super().cleanup()


# エージェントのエントリーポイント
async def main():
    """エージェントを実行."""
    agent = {{ agent_name | replace('-', '_') | title | replace('_', '') }}Agent()
    
    async with agent:
        # 会話例
        result1 = await agent.run({
            "message": "Hello!",
            "session_id": "user123",
        })
        print("Response 1:", result1["response"])
        
        result2 = await agent.run({
            "message": "How are you?",
            "session_id": "user123",
        })
        print("Response 2:", result2["response"])


if __name__ == "__main__":
    import asyncio
    asyncio.run(main())


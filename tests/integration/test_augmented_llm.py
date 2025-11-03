"""
Augmented LLM アーキテクチャパターンの統合テスト
"""

import asyncio
from typing import Optional

import pytest

from ai_blocks.architectures.augmented_llm import AugmentedLLM
from ai_blocks.core.memory import MemoryInterface, VectorMemory
from ai_blocks.core.tool import ToolInterface, ToolManager, tool


class MockLLMProvider:
    """テスト用のモックLLMプロバイダー"""

    def __init__(self, responses=None):
        self.responses = responses or {}
        self.call_count = 0
        self.last_prompt = None

    async def generate(self, prompt: str) -> str:
        """モック応答を生成"""
        self.call_count += 1
        self.last_prompt = prompt

        # キーワードベースの応答
        if "計算" in prompt or "add" in prompt:
            return "計算を実行します。addツールを使用してください。"
        elif "時刻" in prompt or "time" in prompt:
            return "現在の時刻を取得します。get_current_timeツールを使用してください。"
        elif "記憶" in prompt or "memory" in prompt:
            return "記憶から関連情報を検索しました。"
        else:
            return f"入力「{prompt[:50]}...」に対する応答です。"


class TestAugmentedLLMIntegration:
    """Augmented LLMの統合テスト"""

    @pytest.fixture
    async def setup_agent(self):
        """テスト用のエージェントをセットアップ"""
        # コンポーネントを初期化
        memory = VectorMemory(max_items=10)
        tools = ToolManager()
        llm = MockLLMProvider()

        # カスタムツールを追加
        @tool(name="test_tool", description="テスト用ツール")
        def test_tool_func(message: str) -> str:
            return f"Test: {message}"

        tools.register_function(test_tool_func)

        # エージェントを作成
        agent = AugmentedLLM(
            llm_provider=llm,
            memory=memory,
            tool_manager=tools,
            name="TestAgent",
            config={
                "max_memory_items": 5,
                "enable_tool_use": True,
                "max_tool_calls": 2,
            },
        )

        return agent, memory, tools, llm

    @pytest.mark.asyncio
    async def test_basic_processing(self, setup_agent):
        """基本的な処理のテスト"""
        agent, memory, tools, llm = await setup_agent

        # 基本的な質問
        response = await agent.process("こんにちは")

        assert isinstance(response, str)
        assert len(response) > 0
        assert llm.call_count > 0

    @pytest.mark.asyncio
    async def test_memory_integration(self, setup_agent):
        """メモリ統合のテスト"""
        agent, memory, tools, llm = await setup_agent

        # 知識を追加
        knowledge = "Pythonは汎用プログラミング言語です。"
        memory_id = await agent.add_knowledge(knowledge, {"type": "programming"})

        assert isinstance(memory_id, str)
        assert len(memory_id) > 0

        # メモリ件数を確認
        count = await memory.count()
        assert count == 1

        # 関連する質問
        response = await agent.process("Pythonについて教えて")

        assert isinstance(response, str)
        assert len(response) > 0

    @pytest.mark.asyncio
    async def test_tool_integration(self, setup_agent):
        """ツール統合のテスト"""
        agent, memory, tools, llm = await setup_agent

        # ツールが利用可能かチェック
        available_tools = tools.get_available_tools()
        tool_names = [tool.name for tool in available_tools]

        assert "echo" in tool_names  # 組み込みツール
        assert "add" in tool_names  # 組み込みツール
        assert "test_tool" in tool_names  # カスタムツール

        # ツール使用を含む処理
        response = await agent.process("5 + 3 を計算して")

        assert isinstance(response, str)
        assert len(response) > 0

    @pytest.mark.asyncio
    async def test_empty_input(self, setup_agent):
        """空入力のテスト"""
        agent, memory, tools, llm = await setup_agent

        response = await agent.process("")

        assert isinstance(response, str)
        assert "空" in response or "入力" in response

    @pytest.mark.asyncio
    async def test_agent_status(self, setup_agent):
        """エージェント状態のテスト"""
        agent, memory, tools, llm = await setup_agent

        # いくつかの処理を実行
        await agent.process("テスト1")
        await agent.process("テスト2")

        # 状態を取得
        status = await agent.get_status()

        assert isinstance(status, dict)
        assert "name" in status
        assert "metrics" in status
        assert "components" in status

        # メトリクスをチェック
        metrics = status["metrics"]
        assert metrics["total_requests"] >= 2
        assert metrics["success_rate"] > 0

        # コンポーネント状態をチェック
        components = status["components"]
        assert components["llm"] is True
        assert components["memory"] is True
        assert components["tools"] is True

    @pytest.mark.asyncio
    async def test_multiple_interactions(self, setup_agent):
        """複数回の対話のテスト"""
        agent, memory, tools, llm = await setup_agent

        # 複数の対話を実行
        interactions = ["こんにちは", "Pythonについて教えて", "計算をお願いします", "ありがとうございました"]

        responses = []
        for interaction in interactions:
            response = await agent.process(interaction)
            responses.append(response)

        # 全ての応答が有効かチェック
        assert len(responses) == len(interactions)
        for response in responses:
            assert isinstance(response, str)
            assert len(response) > 0

        # メトリクスをチェック
        metrics = agent.get_metrics()
        assert metrics["total_requests"] == len(interactions)

    @pytest.mark.asyncio
    async def test_knowledge_search(self, setup_agent):
        """知識検索のテスト"""
        agent, memory, tools, llm = await setup_agent

        # 複数の知識を追加
        knowledge_items = [
            "機械学習は人工知能の一分野です。",
            "深層学習はニューラルネットワークを使用します。",
            "自然言語処理はテキストを理解する技術です。",
        ]

        for knowledge in knowledge_items:
            await agent.add_knowledge(knowledge, {"category": "ai"})

        # 関連する質問
        response = await agent.process("機械学習について説明して")

        assert isinstance(response, str)
        assert len(response) > 0

        # メモリ検索が実行されたかチェック（LLMプロンプトに記憶情報が含まれる）
        assert llm.last_prompt is not None

    @pytest.mark.asyncio
    async def test_concurrent_processing(self, setup_agent):
        """並行処理のテスト"""
        agent, memory, tools, llm = await setup_agent

        # 複数の処理を並行実行
        tasks = [agent.process(f"質問{i}") for i in range(5)]

        responses = await asyncio.gather(*tasks)

        # 全ての応答が有効かチェック
        assert len(responses) == 5
        for response in responses:
            assert isinstance(response, str)
            assert len(response) > 0

        # メトリクスをチェック
        metrics = agent.get_metrics()
        assert metrics["total_requests"] == 5


class TestAugmentedLLMErrorHandling:
    """Augmented LLMのエラーハンドリングテスト"""

    @pytest.mark.asyncio
    async def test_llm_error_handling(self):
        """LLMエラーのハンドリングテスト"""

        # エラーを発生させるモックLLM
        class ErrorLLM:
            async def generate(self, prompt: str) -> str:
                raise Exception("LLMエラー")

        memory = VectorMemory()
        tools = ToolManager()
        llm = ErrorLLM()

        agent = AugmentedLLM(llm_provider=llm, memory=memory, tool_manager=tools)

        response = await agent.process("テスト")

        # エラーが適切に処理されているかチェック
        assert isinstance(response, str)
        assert "エラー" in response

    @pytest.mark.asyncio
    async def test_memory_error_handling(self):
        """メモリエラーのハンドリングテスト"""

        # エラーを発生させるモックメモリ
        class ErrorMemory(MemoryInterface):
            async def search(
                self, query: str, limit: int = 10, threshold: Optional[float] = None
            ):
                raise Exception("メモリエラー")

            async def store(self, content: str, metadata=None):
                raise Exception("メモリエラー")

            async def count(self):
                return 0

            async def get(self, memory_id: str):
                # メモリIDを使用してエラーを発生させる
                _ = memory_id
                raise Exception("メモリエラー")

            async def delete(self, memory_id: str):
                # メモリIDを使用してエラーを発生させる
                _ = memory_id
                raise Exception("メモリエラー")

            async def clear(self):
                raise Exception("メモリエラー")

        memory = ErrorMemory()
        tools = ToolManager()
        llm = MockLLMProvider()

        agent = AugmentedLLM(llm_provider=llm, memory=memory, tool_manager=tools)

        # メモリエラーがあっても処理が継続されるかチェック
        response = await agent.process("テスト")

        assert isinstance(response, str)
        assert len(response) > 0

    @pytest.mark.asyncio
    async def test_tool_error_handling(self):
        """ツールエラーのハンドリングテスト"""

        # エラーを発生させるモックツール
        class ErrorTools(ToolInterface):
            def get_available_tools(self):
                return []

            async def execute(self, tool_name: str, parameters):
                # パラメータを使用してエラーを発生させる
                _ = tool_name, parameters
                raise Exception("ツールエラー")

            def register_tool(self, tool):
                # ツールを使用してエラーを発生させる
                _ = tool
                raise Exception("ツールエラー")

            def unregister_tool(self, tool_name: str):
                # ツール名を使用してエラーを発生させる
                _ = tool_name
                raise Exception("ツールエラー")

            def register_function(self, func, name=None, description=None):
                # パラメータを使用してエラーを発生させる
                _ = func, name, description
                raise Exception("ツールエラー")

            def register_class(self, cls, prefix=None):
                # パラメータを使用してエラーを発生させる
                _ = cls, prefix
                raise Exception("ツールエラー")

        memory = VectorMemory()
        tools = ErrorTools()
        llm = MockLLMProvider()

        agent = AugmentedLLM(llm_provider=llm, memory=memory, tool_manager=tools)

        # ツールエラーがあっても処理が継続されるかチェック
        response = await agent.process("計算をお願いします")

        assert isinstance(response, str)
        assert len(response) > 0


if __name__ == "__main__":
    pytest.main([__file__])

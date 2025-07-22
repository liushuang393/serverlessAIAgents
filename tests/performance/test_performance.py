"""
パフォーマンステスト

このモジュールは、AI Blocksの各コンポーネントとアーキテクチャパターンの
パフォーマンスを測定するためのテストを提供します。
"""

import asyncio
import statistics
import time
from typing import Dict

import pytest

from ai_blocks.architectures.augmented_llm import AugmentedLLM
from ai_blocks.architectures.parallel_agents import ParallelAgents
from ai_blocks.architectures.prompt_chaining import PromptChain, SpecializedAgent
from ai_blocks.core.chunker import SmartChunker
from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.tool import ToolManager


class MockLLMProvider:
    """テスト用のモックLLMプロバイダー"""

    def __init__(self, delay: float = 0.05):
        self.delay = delay

    async def generate(self, prompt: str) -> str:
        """モック応答を生成（遅延あり）"""
        await asyncio.sleep(self.delay)
        return f"応答: {prompt[:50]}..."


class PerformanceMetrics:
    """パフォーマンスメトリクスを収集・分析するクラス"""

    def __init__(self):
        self.execution_times = []
        self.start_time = None

    def start(self):
        """計測開始"""
        self.start_time = time.time()

    def stop(self):
        """計測終了"""
        if self.start_time is not None:
            execution_time = time.time() - self.start_time
            self.execution_times.append(execution_time)
            self.start_time = None

    def get_metrics(self) -> Dict[str, float]:
        """メトリクスを取得"""
        if not self.execution_times:
            return {
                "min": 0,
                "max": 0,
                "avg": 0,
                "median": 0,
                "p95": 0,
                "total": 0,
                "count": 0,
            }

        return {
            "min": min(self.execution_times),
            "max": max(self.execution_times),
            "avg": statistics.mean(self.execution_times),
            "median": statistics.median(self.execution_times),
            "p95": sorted(self.execution_times)[int(len(self.execution_times) * 0.95)],
            "total": sum(self.execution_times),
            "count": len(self.execution_times),
        }

    def reset(self):
        """メトリクスをリセット"""
        self.execution_times = []
        self.start_time = None


@pytest.mark.slow
class TestMemoryPerformance:
    """Memoryコンポーネントのパフォーマンステスト"""

    @pytest.mark.asyncio
    async def test_memory_store_performance(self):
        """メモリ保存のパフォーマンステスト"""
        memory = VectorMemory(max_items=1000)
        metrics = PerformanceMetrics()

        # 100件のアイテムを保存
        for i in range(100):
            content = f"テストコンテンツ {i}: " + "a" * 100

            metrics.start()
            await memory.store(content, {"index": i})
            metrics.stop()

        # メトリクスを表示
        results = metrics.get_metrics()
        print("\nメモリ保存パフォーマンス (100件):")
        print(f"  平均時間: {results['avg']:.6f}秒")
        print(f"  最大時間: {results['max']:.6f}秒")
        print(f"  最小時間: {results['min']:.6f}秒")
        print(f"  95パーセンタイル: {results['p95']:.6f}秒")

        # 基本的な検証
        assert results["count"] == 100
        assert await memory.count() == 100

    @pytest.mark.asyncio
    async def test_memory_search_performance(self):
        """メモリ検索のパフォーマンステスト"""
        memory = VectorMemory(max_items=1000)
        metrics = PerformanceMetrics()

        # テストデータを準備
        categories = ["技術", "科学", "歴史", "文化", "芸術"]
        for i in range(100):
            category = categories[i % len(categories)]
            content = f"{category}に関する情報 {i}: " + "a" * 100
            await memory.store(content, {"category": category})

        # 検索クエリ
        queries = ["技術", "科学", "歴史", "情報", "データ"]

        # 各クエリで検索を実行
        for query in queries:
            for _ in range(5):  # 各クエリを5回実行
                metrics.start()
                await memory.search(query, limit=10)
                metrics.stop()

        # メトリクスを表示
        metrics_results = metrics.get_metrics()
        print("\nメモリ検索パフォーマンス (25回):")
        print(f"  平均時間: {metrics_results['avg']:.6f}秒")
        print(f"  最大時間: {metrics_results['max']:.6f}秒")
        print(f"  最小時間: {metrics_results['min']:.6f}秒")
        print(f"  95パーセンタイル: {metrics_results['p95']:.6f}秒")

        # 基本的な検証
        assert metrics_results["count"] == 25


@pytest.mark.slow
class TestChunkerPerformance:
    """Chunkerコンポーネントのパフォーマンステスト"""

    @pytest.mark.asyncio
    async def test_chunker_performance(self):
        """チャンカーのパフォーマンステスト"""
        chunker = SmartChunker()
        metrics = PerformanceMetrics()

        # テストテキストを生成（サイズ別）
        texts = [
            "a" * 1000,  # 1KB
            "a" * 10000,  # 10KB
            "a" * 100000,  # 100KB
        ]

        # 各テキストでチャンク分割を実行
        for i, text in enumerate(texts):
            metrics.start()
            chunks = await chunker.chunk(text, chunk_size=1000, overlap=100)
            metrics.stop()

            print(f"\nテキスト{i+1} ({len(text)}文字) → {len(chunks)}チャンク")

        # メトリクスを表示
        results = metrics.get_metrics()
        print("\nチャンカーパフォーマンス:")
        print(f"  平均時間: {results['avg']:.6f}秒")
        print(f"  最大時間: {results['max']:.6f}秒")
        print(f"  最小時間: {results['min']:.6f}秒")

        # 基本的な検証
        assert results["count"] == len(texts)


@pytest.mark.slow
class TestAugmentedLLMPerformance:
    """Augmented LLMのパフォーマンステスト"""

    @pytest.mark.asyncio
    async def test_augmented_llm_performance(self):
        """Augmented LLMのパフォーマンステスト"""
        # コンポーネントを初期化
        memory = VectorMemory(max_items=100)
        tools = ToolManager()
        llm = MockLLMProvider(delay=0.05)  # 50ms遅延

        agent = AugmentedLLM(
            llm_provider=llm, memory=memory, tool_manager=tools, name="PerfTestAgent"
        )

        # 知識を追加
        for i in range(10):
            await agent.add_knowledge(f"テスト知識 {i}: " + "a" * 100, {"index": i})

        metrics = PerformanceMetrics()

        # 複数のクエリを処理
        queries = ["こんにちは", "テスト知識について教えて", "計算をお願いします", "情報を検索して"]

        for query in queries:
            metrics.start()
            response = await agent.process(query)
            metrics.stop()

            assert isinstance(response, str)
            assert len(response) > 0

        # メトリクスを表示
        results = metrics.get_metrics()
        print("\nAugmented LLMパフォーマンス:")
        print(f"  平均時間: {results['avg']:.6f}秒")
        print(f"  最大時間: {results['max']:.6f}秒")
        print(f"  最小時間: {results['min']:.6f}秒")

        # 基本的な検証
        assert results["count"] == len(queries)


@pytest.mark.slow
class TestParallelAgentsPerformance:
    """Parallel Agentsのパフォーマンステスト"""

    @pytest.mark.asyncio
    async def test_parallel_agents_performance(self):
        """Parallel Agentsのパフォーマンステスト"""
        # モックLLM（異なる遅延）
        llm1 = MockLLMProvider(delay=0.05)  # 50ms
        llm2 = MockLLMProvider(delay=0.10)  # 100ms
        llm3 = MockLLMProvider(delay=0.15)  # 150ms

        # 特化Agent
        agent1 = SpecializedAgent("Agent1", "タスク1", llm1)
        agent2 = SpecializedAgent("Agent2", "タスク2", llm2)
        agent3 = SpecializedAgent("Agent3", "タスク3", llm3)

        # Parallel Agents
        parallel = ParallelAgents(
            agents=[agent1, agent2, agent3], name="PerfTestParallel"
        )

        metrics = PerformanceMetrics()

        # 複数回実行
        for i in range(5):
            metrics.start()
            response = await parallel.process(f"テスト{i}")
            metrics.stop()

            assert isinstance(response, str)
            assert len(response) > 0

        # メトリクスを表示
        results = metrics.get_metrics()
        print("\nParallel Agentsパフォーマンス:")
        print(f"  平均時間: {results['avg']:.6f}秒")
        print(f"  最大時間: {results['max']:.6f}秒")
        print(f"  最小時間: {results['min']:.6f}秒")

        # 基本的な検証
        assert results["count"] == 5

        # 並列実行の効果を検証（最も遅いAgentの時間に近いはず）
        assert results["avg"] < 0.20  # 最も遅いAgentは0.15秒


@pytest.mark.slow
class TestPromptChainPerformance:
    """Prompt Chainのパフォーマンステスト"""

    @pytest.mark.asyncio
    async def test_prompt_chain_performance(self):
        """Prompt Chainのパフォーマンステスト"""
        # モックLLM（異なる遅延）
        llm1 = MockLLMProvider(delay=0.05)  # 50ms
        llm2 = MockLLMProvider(delay=0.10)  # 100ms

        # 特化Agent
        agent1 = SpecializedAgent("Agent1", "タスク1", llm1)
        agent2 = SpecializedAgent("Agent2", "タスク2", llm2)

        # Prompt Chain
        chain = PromptChain(agents=[agent1, agent2], name="PerfTestChain")

        metrics = PerformanceMetrics()

        # 複数回実行
        for i in range(5):
            metrics.start()
            result = await chain.execute(f"テスト{i}")
            metrics.stop()

            assert result.success is True
            assert isinstance(result.final_output, str)

        # メトリクスを表示
        results = metrics.get_metrics()
        print("\nPrompt Chainパフォーマンス:")
        print(f"  平均時間: {results['avg']:.6f}秒")
        print(f"  最大時間: {results['max']:.6f}秒")
        print(f"  最小時間: {results['min']:.6f}秒")

        # 基本的な検証
        assert results["count"] == 5

        # 直列実行の効果を検証（Agentの時間の合計に近いはず）
        assert results["avg"] > 0.14  # 0.05 + 0.10 = 0.15秒


@pytest.mark.slow
class TestConcurrencyPerformance:
    """並行処理のパフォーマンステスト"""

    @pytest.mark.asyncio
    async def test_concurrency_performance(self):
        """並行処理のパフォーマンステスト"""
        # モックLLM
        llm = MockLLMProvider(delay=0.05)  # 50ms

        # Augmented LLM
        agent = AugmentedLLM(
            llm_provider=llm,
            memory=VectorMemory(),
            tool_manager=ToolManager(),
            name="ConcurrencyTestAgent",
        )

        metrics = PerformanceMetrics()

        # 並行処理数
        concurrency_levels = [1, 5, 10, 20]

        for concurrency in concurrency_levels:
            print(f"\n並行処理数: {concurrency}")

            # 複数のタスクを並行実行
            metrics.reset()
            metrics.start()

            tasks = [agent.process(f"テスト{i}") for i in range(concurrency)]

            responses = await asyncio.gather(*tasks)

            metrics.stop()

            # 全ての応答をチェック
            for response in responses:
                assert isinstance(response, str)
                assert len(response) > 0

            # メトリクスを表示
            results = metrics.get_metrics()
            print(f"  総時間: {results['total']:.6f}秒")
            print(f"  平均時間/リクエスト: {results['total'] / concurrency:.6f}秒")

            # 並行処理の効果を検証
            if concurrency > 1:
                # 完全な線形スケーリングではないが、並行処理の効果があるはず
                assert results["total"] < 0.05 * concurrency * 1.5


if __name__ == "__main__":
    pytest.main(["-v", __file__])

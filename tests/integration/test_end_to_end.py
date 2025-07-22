"""
エンドツーエンド統合テスト

このモジュールは、AI Blocksの全体的なワークフローをテストします。
実際のユースケースに基づいた統合テストを提供します。
"""

import asyncio
import time

import pytest

from ai_blocks.config.dynamic import config_context, get_config_manager
from ai_blocks.core.chunker import SimpleChunker
from ai_blocks.core.evaluator import EvaluationCriteria, RuleBasedEvaluator
from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.models import Message, MessageRole, RouteDefinition
from ai_blocks.core.parser import TextParser
from ai_blocks.core.router import RuleBasedRouter
from ai_blocks.core.thread import SimpleThread
from ai_blocks.core.tool import ToolManager, tool
from ai_blocks.utils.observability import MetricType, get_observability_manager


# テスト用のカスタムツール
@tool(name="calculate", description="数学計算を実行する")
async def calculate_tool(expression: str) -> str:
    """安全な数学計算ツール"""
    try:
        # 安全な評価のため、基本的な演算のみ許可
        allowed_chars = set("0123456789+-*/.() ")
        if not all(c in allowed_chars for c in expression):
            return "エラー: 許可されていない文字が含まれています"

        result = eval(expression)
        return f"計算結果: {expression} = {result}"
    except Exception as e:
        return f"計算エラー: {str(e)}"


@tool(name="text_analysis", description="テキスト分析を実行する")
async def text_analysis_tool(text: str) -> str:
    """テキスト分析ツール"""
    word_count = len(text.split())
    char_count = len(text)
    sentence_count = text.count(".") + text.count("!") + text.count("?")

    return f"テキスト分析結果:\n文字数: {char_count}\n単語数: {word_count}\n文数: {sentence_count}"


class TestEndToEndIntegration:
    """エンドツーエンド統合テストクラス"""

    @pytest.fixture(autouse=True)
    async def setup_and_teardown(self):
        """テストのセットアップとクリーンアップ"""
        # 観測可能性システムを初期化
        self.observability = await get_observability_manager()

        # 動的設定管理を初期化
        self.config_manager = await get_config_manager()

        yield

        # クリーンアップ
        await self.observability.cleanup()
        await self.config_manager.cleanup()

    @pytest.mark.asyncio
    async def test_basic_memory_workflow(self):
        """基本的なメモリワークフローテスト"""
        with self.observability.trace("basic_memory_workflow"):
            # メモリコンポーネントを初期化
            memory = VectorMemory(max_items=100)

            # 知識ベースを構築
            knowledge_items = [
                "人工知能は機械が人間の知能を模倣する技術です",
                "機械学習は人工知能の一分野で、データから学習します",
                "深層学習はニューラルネットワークを使用した機械学習手法です",
                "自然言語処理は人間の言語をコンピュータが理解する技術です",
                "コンピュータビジョンは画像や動画を解析する技術です",
            ]

            # 知識を保存
            memory_ids = []
            for item in knowledge_items:
                memory_id = await memory.store(item, {"category": "ai_knowledge"})
                memory_ids.append(memory_id)

            assert len(memory_ids) == len(knowledge_items)

            # 検索テスト
            search_queries = [("人工知能", 2), ("機械学習", 2), ("画像", 1)]

            for query, expected_min_results in search_queries:
                results = await memory.search(query, limit=5)
                assert len(results) >= expected_min_results

                # 関連性の確認
                for result in results:
                    assert query in result.content or any(
                        related in result.content
                        for related in ["AI", "人工知能", "機械学習", "深層学習"]
                    )

    @pytest.mark.asyncio
    async def test_tool_integration_workflow(self):
        """ツール統合ワークフローテスト"""
        with self.observability.trace("tool_integration_workflow"):
            # ツールマネージャーを初期化
            tool_manager = ToolManager()

            # カスタムツールを登録
            tool_manager.register_function(calculate_tool)
            tool_manager.register_function(text_analysis_tool)

            # 利用可能なツールを確認
            available_tools = tool_manager.get_available_tools()
            tool_names = [tool.name for tool in available_tools]

            assert "calculate" in tool_names
            assert "text_analysis" in tool_names
            assert "echo" in tool_names  # 組み込みツール

            # 計算ツールのテスト
            calc_result = await tool_manager.execute(
                "calculate", {"expression": "10 + 20 * 3"}
            )
            assert calc_result.success is True
            assert "70" in calc_result.result

            # テキスト分析ツールのテスト
            sample_text = "これはテストです。AI Blocksは素晴らしいライブラリです！"
            analysis_result = await tool_manager.execute(
                "text_analysis", {"text": sample_text}
            )
            assert analysis_result.success is True
            assert "文字数" in analysis_result.result
            assert "単語数" in analysis_result.result

    @pytest.mark.asyncio
    async def test_document_processing_workflow(self):
        """ドキュメント処理ワークフローテスト"""
        with self.observability.trace("document_processing_workflow"):
            # パーサーとチャンカーを初期化
            parser = TextParser()
            chunker = SimpleChunker()

            # サンプルドキュメント
            sample_document = """
            # AI Blocks ドキュメント

            AI Blocksは軽量で柔軟なAIエージェント構築ライブラリです。

            ## 主な特徴

            1. モジュラー設計: 各コンポーネントは独立して開発・テスト可能
            2. 型安全性: Pydanticを使用した堅牢なデータモデル
            3. 観測可能性: 分散トレーシングとメトリクス収集
            4. ホットスワップ: 実行時でのコンポーネント交換

            ## 使用例

            ```python
            from ai_blocks import AugmentedLLM

            agent = AugmentedLLM()
            response = await agent.process("こんにちは")
            ```

            詳細については公式ドキュメントを参照してください。
            """

            # ドキュメントをパース
            parsed_doc = await parser.parse(
                sample_document.encode("utf-8"),
                "text/markdown",
                {"source": "test_document.md"},
            )

            assert parsed_doc.text is not None
            assert len(parsed_doc.text) > 0
            assert parsed_doc.source_type == "text/markdown"

            # ドキュメントをチャンクに分割
            chunks = await chunker.chunk(
                parsed_doc.text,
                chunk_size=200,
                overlap=50,
                metadata={"document_id": "test_doc"},
            )

            assert len(chunks) > 0

            # 各チャンクの検証
            for chunk in chunks:
                assert len(chunk.text) <= 250  # オーバーラップを考慮
                assert chunk.metadata["document_id"] == "test_doc"
                assert chunk.chunk_id is not None

    @pytest.mark.asyncio
    async def test_conversation_workflow(self):
        """会話ワークフローテスト"""
        with self.observability.trace("conversation_workflow"):
            # スレッドを初期化
            thread = SimpleThread(max_history=50)

            # 会話をシミュレート
            conversation_flow = [
                (MessageRole.USER, "こんにちは"),
                (MessageRole.ASSISTANT, "こんにちは！何かお手伝いできることはありますか？"),
                (MessageRole.USER, "AI Blocksについて教えて"),
                (MessageRole.ASSISTANT, "AI Blocksは軽量で柔軟なAIエージェント構築ライブラリです。"),
                (MessageRole.USER, "主な特徴は？"),
                (MessageRole.ASSISTANT, "モジュラー設計、型安全性、観測可能性、ホットスワップ機能が主な特徴です。"),
            ]

            # メッセージを順次追加
            for role, content in conversation_flow:
                message = Message(role=role, content=content)
                await thread.add_message(message)

            # 履歴を確認
            history = await thread.get_history()
            assert len(history) == len(conversation_flow)

            # 履歴の内容を確認
            for i, (expected_role, expected_content) in enumerate(conversation_flow):
                assert history[i].role == expected_role
                assert history[i].content == expected_content

            # 状態管理のテスト
            await thread.update_state(
                {
                    "user_name": "テストユーザー",
                    "conversation_topic": "AI Blocks",
                    "message_count": len(conversation_flow),
                }
            )

            state = await thread.get_state()
            assert state["user_name"] == "テストユーザー"
            assert state["conversation_topic"] == "AI Blocks"
            assert state["message_count"] == len(conversation_flow)

    @pytest.mark.asyncio
    async def test_routing_workflow(self):
        """ルーティングワークフローテスト"""
        with self.observability.trace("routing_workflow"):
            # ルーターを初期化
            router = RuleBasedRouter()

            # ルートを定義
            routes = [
                RouteDefinition(
                    pattern=r"計算|数学|足し算|引き算|掛け算|割り算",
                    target="calculator_agent",
                    priority=1,
                    conditions={"type": "math"},
                    description="数学計算関連のクエリ",
                ),
                RouteDefinition(
                    pattern=r"分析|解析|統計|データ",
                    target="analysis_agent",
                    priority=1,
                    conditions={"type": "analysis"},
                    description="データ分析関連のクエリ",
                ),
                RouteDefinition(
                    pattern=r".*",
                    target="general_agent",
                    priority=0,
                    conditions={"type": "general"},
                    description="一般的なクエリ",
                ),
            ]

            # ルートを登録
            for route in routes:
                router.register_route(route)

            # ルーティングテスト
            test_cases = [
                ("10 + 20を計算して", "calculator_agent"),
                ("データを分析してください", "analysis_agent"),
                ("こんにちは", "general_agent"),
                ("数学の問題を解いて", "calculator_agent"),
            ]

            for input_text, expected_target in test_cases:
                result = await router.route(input_text)
                assert result.target == expected_target
                assert result.confidence > 0

    @pytest.mark.asyncio
    async def test_evaluation_workflow(self):
        """評価ワークフローテスト"""
        with self.observability.trace("evaluation_workflow"):
            # 評価器を初期化
            evaluator = RuleBasedEvaluator()

            # 評価基準を定義
            criteria = [
                EvaluationCriteria.RELEVANCE.value,
                EvaluationCriteria.ACCURACY.value,
                EvaluationCriteria.COMPLETENESS.value,
            ]

            # テストケース
            test_cases: list[dict[str, str | tuple[float, float]]] = [
                {
                    "input": "AI Blocksの主な特徴は？",
                    "output": "AI Blocksの主な特徴は、モジュラー設計、型安全性、観測可能性、ホットスワップ機能です。",
                    "expected_score_range": (0.8, 1.0),
                },
                {
                    "input": "天気はどうですか？",
                    "output": "申し訳ありませんが、現在の天気情報は提供できません。",
                    "expected_score_range": (0.6, 0.8),
                },
                {
                    "input": "計算してください",
                    "output": "何を計算しますか？",
                    "expected_score_range": (0.4, 0.7),
                },
            ]

            for test_case in test_cases:
                result = await evaluator.evaluate(
                    output=str(test_case["output"]),
                    criteria=criteria,
                    context={"query": str(test_case["input"])},
                )

                assert result.passed is True
                score_range = test_case["expected_score_range"]
                assert isinstance(score_range, tuple)
                min_score, max_score = score_range
                assert min_score <= result.score <= max_score

    @pytest.mark.asyncio
    async def test_dynamic_configuration_workflow(self):
        """動的設定ワークフローテスト"""
        with self.observability.trace("dynamic_configuration_workflow"):
            # 設定の動的変更テスト
            original_debug = await self.config_manager.get("debug")
            original_max_tokens = await self.config_manager.get("max_tokens")

            # 一時的な設定変更
            async with config_context(debug=True, max_tokens=2000):
                # 設定が変更されていることを確認
                assert await self.config_manager.get("debug") is True
                assert await self.config_manager.get("max_tokens") == 2000

                # 設定に依存する処理をシミュレート
                memory = VectorMemory(
                    max_items=await self.config_manager.get("max_tokens")
                )
                assert memory.max_items == 2000

            # 設定が元に戻っていることを確認
            assert await self.config_manager.get("debug") == original_debug
            assert await self.config_manager.get("max_tokens") == original_max_tokens

    @pytest.mark.asyncio
    async def test_performance_under_load(self):
        """負荷下でのパフォーマンステスト"""
        with self.observability.trace("performance_under_load"):
            # 複数のコンポーネントを同時に使用
            memory = VectorMemory(max_items=1000)
            tool_manager = ToolManager()

            # 並列処理のテスト
            async def memory_task(i: int):
                await memory.store(f"並列テストデータ {i}")
                results = await memory.search("並列", limit=5)
                return len(results)

            async def tool_task(i: int):
                result = await tool_manager.execute("echo", {"text": f"並列ツールテスト {i}"})
                return result.success

            # 大量の並列タスクを実行
            memory_tasks = [memory_task(i) for i in range(50)]
            tool_tasks = [tool_task(i) for i in range(50)]

            start_time = time.time()
            memory_results = await asyncio.gather(*memory_tasks)
            tool_results = await asyncio.gather(*tool_tasks)
            end_time = time.time()

            # 結果の検証
            assert all(result > 0 for result in memory_results)
            assert all(result is True for result in tool_results)

            # パフォーマンスの確認
            total_time = end_time - start_time
            assert total_time < 10.0  # 10秒以内で完了することを期待

            print(f"並列処理テスト完了: {total_time:.2f}秒")

    @pytest.mark.asyncio
    async def test_error_handling_and_recovery(self):
        """エラーハンドリングと回復テスト"""
        with self.observability.trace("error_handling_and_recovery"):
            tool_manager = ToolManager()

            # 存在しないツールの実行
            result = await tool_manager.execute("nonexistent_tool", {})
            assert result.success is False
            assert result.error_message is not None
            assert "見つかりません" in result.error_message

            # 無効なパラメータでのツール実行
            result = await tool_manager.execute("add", {"a": "invalid", "b": 20})
            assert result.success is False

            # メモリの制限テスト
            memory = VectorMemory(max_items=5)

            # 制限を超えてデータを保存
            for i in range(10):
                await memory.store(f"テストデータ {i}")

            # メモリが制限内に収まっていることを確認
            count = await memory.count()
            assert count <= 5

    @pytest.mark.asyncio
    async def test_observability_integration(self):
        """観測可能性統合テスト"""
        # トレーシングとメトリクスの統合テスト
        with self.observability.trace("observability_integration_test"):
            # メトリクスを記録
            self.observability.record_metric("test_counter", 1.0, MetricType.COUNTER)
            self.observability.record_metric("test_gauge", 42.0, MetricType.GAUGE)
            self.observability.record_metric(
                "test_histogram", 1.5, MetricType.HISTOGRAM
            )

            # 複数の操作を実行してトレースを生成
            from ai_blocks.core.memory import VectorMemory

            memory = VectorMemory()
            await memory.store("観測可能性テスト")
            await memory.search("観測可能性")

            # メトリクスが記録されていることを確認
            metrics = self.observability.metrics_collector.get_metrics()
            assert len(metrics) > 0

            metric_names = [m.name for m in metrics]
            assert "test_counter" in metric_names
            assert "test_gauge" in metric_names
            assert "test_histogram" in metric_names

        # 観測可能性の概要を確認
        summary = self.observability.get_observability_summary()
        assert summary["service_name"] == "ai-blocks"
        assert summary["traces"]["total_count"] > 0
        assert summary["metrics"]["total_count"] > 0

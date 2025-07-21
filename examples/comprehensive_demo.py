"""
AI Blocks 包括的デモンストレーション

このスクリプトは、実装した全ての機能を統合的にデモンストレーションします：
1. ホットスワップ機能
2. 動的設定管理
3. 高度な観測可能性
4. 統合テスト機能
"""

import asyncio
import time
from pathlib import Path

from ai_blocks.config.dynamic import config_context, get_config_manager
from ai_blocks.core.factory import builder, get_factory
from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.registry import DeploymentStrategy, get_registry
from ai_blocks.core.tool import ToolManager, tool
from ai_blocks.utils.observability import get_observability_manager, trace


@tool(name="demo_calculator", description="デモ用計算機")
async def demo_calculator(expression: str) -> str:
    """デモ用の安全な計算機"""
    try:
        # 基本的な演算のみ許可
        allowed_chars = set("0123456789+-*/.() ")
        if not all(c in allowed_chars for c in expression):
            return "エラー: 許可されていない文字が含まれています"

        result = eval(expression)
        return f"計算結果: {expression} = {result}"
    except Exception as e:
        return f"計算エラー: {str(e)}"


async def demo_section_header(title: str, description: str = ""):
    """デモセクションのヘッダーを表示"""
    print("\n" + "=" * 80)
    print(f"🎯 {title}")
    if description:
        print(f"   {description}")
    print("=" * 80)


async def demo_hotswap_capabilities():
    """ホットスワップ機能のデモ"""
    await demo_section_header("ホットスワップ機能デモ", "実行時でのコンポーネント交換とバージョン管理")

    registry = get_registry()
    factory = get_factory()

    # v1.0.0のメモリコンポーネントを作成
    print("\n1️⃣ v1.0.0 メモリコンポーネントの作成と使用")
    memory_v1 = await factory.create("memory", "vector", "1.0.0")

    # データを保存
    test_data = [
        "AI Blocksは軽量で柔軟なライブラリです",
        "ホットスワップ機能により実行時交換が可能です",
        "観測可能性機能で詳細な監視ができます",
    ]

    for data in test_data:
        await memory_v1.store(data)

    results = await memory_v1.search("AI Blocks")
    print(f"   v1.0.0 検索結果: {len(results)}件")

    # v2.0.0へのホットスワップ
    print("\n2️⃣ v2.0.0 へのホットスワップ実行")

    # 改良版メモリコンポーネントを登録（実際の実装では別ファイルから）
    from examples.hotswap_demo import EnhancedVectorMemory

    registry.register_component(
        component_type="memory",
        component_name="vector",
        component_class=EnhancedVectorMemory,
        version="2.0.0",
        deployment_strategy=DeploymentStrategy.CANARY,
    )

    # カナリアデプロイメントを実行
    success = await registry.deploy_version(
        "memory",
        "vector",
        "2.0.0",
        strategy=DeploymentStrategy.CANARY,
        canary_percentage=0.1,
    )

    if success:
        print("   ✅ ホットスワップ成功！")

        # 新バージョンでテスト
        memory_v2 = await factory.create("memory", "vector", "2.0.0")
        await memory_v2.store("v2.0.0の新機能テスト")

        results = await memory_v2.search("新機能")
        print(
            f"   v2.0.0 検索結果: {len(results)}件（改良された類似度: {results[0].similarity_score if results else 'N/A'}）"
        )
    else:
        print("   ❌ ホットスワップ失敗")

    # レジストリメトリクスを表示
    print("\n3️⃣ レジストリメトリクス")
    metrics = registry.get_metrics()
    print(f"   総コンポーネント数: {metrics['total_components']}")
    print(f"   総インスタンス数: {metrics['total_instances']}")


async def demo_dynamic_configuration():
    """動的設定管理のデモ"""
    await demo_section_header("動的設定管理デモ", "実行時での設定変更と環境別管理")

    config_manager = await get_config_manager()

    # 現在の設定を表示
    print("\n1️⃣ 現在の設定状況")
    current_debug = await config_manager.get("debug")
    current_max_tokens = await config_manager.get("max_tokens")
    current_temperature = await config_manager.get("temperature")

    print(f"   debug: {current_debug}")
    print(f"   max_tokens: {current_max_tokens}")
    print(f"   temperature: {current_temperature}")

    # 動的設定変更
    print("\n2️⃣ 動的設定変更")
    await config_manager.set("debug", True, reason="デモ用デバッグ有効化")
    await config_manager.set("max_tokens", 2000, reason="トークン数増加")

    print(f"   debug: {await config_manager.get('debug')}")
    print(f"   max_tokens: {await config_manager.get('max_tokens')}")

    # コンテキストマネージャーでの一時的変更
    print("\n3️⃣ 一時的設定変更（コンテキストマネージャー）")
    async with config_context(temperature=1.5, max_tokens=3000):
        print(f"   コンテキスト内 temperature: {await config_manager.get('temperature')}")
        print(f"   コンテキスト内 max_tokens: {await config_manager.get('max_tokens')}")

    print(f"   コンテキスト外 temperature: {await config_manager.get('temperature')}")
    print(f"   コンテキスト外 max_tokens: {await config_manager.get('max_tokens')}")

    # 変更履歴
    print("\n4️⃣ 設定変更履歴")
    history = config_manager.get_change_history(limit=5)
    for i, change in enumerate(history[-3:], 1):  # 最新3件
        print(f"   {i}. {change.key}: {change.old_value} -> {change.new_value}")
        print(f"      理由: {change.reason or 'なし'}")


async def demo_observability_features():
    """観測可能性機能のデモ"""
    await demo_section_header("観測可能性機能デモ", "分散トレーシング、メトリクス収集、パフォーマンス監視")

    observability = await get_observability_manager()

    # 分散トレーシングのデモ
    print("\n1️⃣ 分散トレーシング")

    @trace("demo_complex_operation")
    async def complex_operation():
        """複雑な操作をシミュレート"""
        memory = VectorMemory(max_items=50)
        tool_manager = ToolManager()
        tool_manager.register_function(demo_calculator)

        # 複数の操作を実行
        with observability.trace("memory_operations"):
            await memory.store("トレーシングテストデータ")
            results = await memory.search("トレーシング")

        with observability.trace("tool_operations"):
            calc_result = await tool_manager.execute(
                "demo_calculator", {"expression": "10 + 20"}
            )
            echo_result = await tool_manager.execute("echo", {"text": "トレーシングテスト"})

        return len(results), calc_result.success, echo_result.success

    # 複雑な操作を実行
    start_time = time.time()
    memory_results, calc_success, echo_success = await complex_operation()
    end_time = time.time()

    print(f"   操作完了: {end_time - start_time:.3f}秒")
    print(f"   メモリ検索結果: {memory_results}件")
    print(f"   計算ツール成功: {calc_success}")
    print(f"   エコーツール成功: {echo_success}")

    # メトリクス収集のデモ
    print("\n2️⃣ メトリクス収集")

    # 様々なメトリクスを記録
    observability.record_metric(
        "demo_requests_total", 1.0, "counter", {"endpoint": "/api/demo"}
    )
    observability.record_metric("demo_active_users", 42.0, "gauge")
    observability.record_metric(
        "demo_response_time", 0.123, "histogram", {"method": "GET"}
    )

    metrics = observability.metrics_collector.get_metrics()
    print(f"   収集されたメトリクス数: {len(metrics)}")

    # Prometheus形式での出力例
    prometheus_output = observability.metrics_collector.get_prometheus_format()
    lines = prometheus_output.split("\n")[:10]  # 最初の10行
    print(f"   Prometheus形式出力例:")
    for line in lines[:5]:  # 最初の5行のみ表示
        if line.strip():
            print(f"     {line}")

    # パフォーマンス監視
    print("\n3️⃣ パフォーマンス監視")
    current_perf = observability.performance_monitor.get_current_metrics()

    if current_perf:
        print(f"   CPU使用率: {current_perf.cpu_usage:.1f}%")
        print(f"   メモリ使用率: {current_perf.memory_usage:.1f}%")
    else:
        print("   パフォーマンスメトリクスは収集中です...")

    # 観測可能性の概要
    print("\n4️⃣ 観測可能性概要")
    summary = observability.get_observability_summary()
    print(f"   サービス名: {summary['service_name']}")
    print(f"   総トレース数: {summary['traces']['total_count']}")
    print(f"   総メトリクス数: {summary['metrics']['total_count']}")


async def demo_integrated_workflow():
    """統合ワークフローのデモ"""
    await demo_section_header("統合ワークフローデモ", "全機能を組み合わせた実用的なワークフロー")

    observability = await get_observability_manager()
    config_manager = await get_config_manager()
    factory = get_factory()

    # 統合ワークフローを実行
    with observability.trace("integrated_workflow_demo"):
        print("\n1️⃣ 動的設定に基づくコンポーネント作成")

        # 設定を動的に調整
        await config_manager.set("memory_max_items", 200, reason="統合デモ用に増加")
        max_items = await config_manager.get("memory_max_items")

        # 設定に基づいてコンポーネントを作成
        memory = await factory.create("memory", "vector", "2.0.0", max_items=max_items)

        print(f"   メモリコンポーネント作成完了（最大アイテム数: {max_items}）")

        print("\n2️⃣ 知識ベース構築とクエリ処理")

        # 知識ベースを構築
        knowledge_base = [
            "AI Blocksは積木式設計を採用しています",
            "ホットスワップ機能により実行時交換が可能です",
            "動的設定管理で柔軟な運用ができます",
            "観測可能性機能で詳細な監視が可能です",
            "統合テストで品質を保証しています",
        ]

        # メトリクスを記録しながら知識を保存
        for i, knowledge in enumerate(knowledge_base):
            await memory.store(knowledge, {"category": "features", "index": i})
            observability.record_metric("knowledge_items_stored", 1.0, "counter")

        # 複数のクエリを処理
        queries = ["積木式", "ホットスワップ", "観測可能性"]

        for query in queries:
            with observability.trace(f"query_processing_{query}"):
                results = await memory.search(query, limit=3)

                print(f"   クエリ '{query}': {len(results)}件の結果")
                if results:
                    print(f"     最高類似度: {results[0].similarity_score:.3f}")

                # クエリメトリクスを記録
                observability.record_metric(
                    "query_results_count", len(results), "gauge", {"query": query}
                )

        print("\n3️⃣ パフォーマンス分析")

        # 負荷テストをシミュレート
        start_time = time.time()

        tasks = []
        for i in range(20):
            task = asyncio.create_task(memory.search(f"テスト{i % 3}", limit=2))
            tasks.append(task)

        results = await asyncio.gather(*tasks)
        end_time = time.time()

        total_results = sum(len(r) for r in results)
        avg_time = (end_time - start_time) / len(tasks)

        print(f"   20件の並列クエリ実行完了")
        print(f"   総実行時間: {end_time - start_time:.3f}秒")
        print(f"   平均クエリ時間: {avg_time:.3f}秒")
        print(f"   総結果数: {total_results}")

        # パフォーマンスメトリクスを記録
        observability.record_metric(
            "batch_query_duration", end_time - start_time, "histogram"
        )
        observability.record_metric("avg_query_duration", avg_time, "gauge")


async def demo_error_handling_and_resilience():
    """エラーハンドリングと耐障害性のデモ"""
    await demo_section_header("エラーハンドリングと耐障害性デモ", "システムの堅牢性と回復力")

    observability = await get_observability_manager()

    print("\n1️⃣ エラーハンドリングテスト")

    # 意図的にエラーを発生させてハンドリングをテスト
    tool_manager = ToolManager()

    # 存在しないツールの実行
    with observability.trace("error_handling_test"):
        result = await tool_manager.execute("nonexistent_tool", {})
        print(f"   存在しないツール実行結果: 成功={result.success}")
        if not result.success:
            print(f"   エラーメッセージ: {result.error_message}")

    # 無効なパラメータでの実行
    result = await tool_manager.execute("add", {"a": "invalid", "b": 20})
    print(f"   無効パラメータ実行結果: 成功={result.success}")

    print("\n2️⃣ 制限とリソース管理")

    # メモリ制限のテスト
    limited_memory = VectorMemory(max_items=5)

    # 制限を超えてデータを保存
    for i in range(10):
        await limited_memory.store(f"制限テストデータ {i}")

    final_count = await limited_memory.count()
    print(f"   制限5件に対して10件保存 -> 実際の件数: {final_count}")

    print("\n3️⃣ 回復力テスト")

    # 一時的な障害をシミュレート
    error_count = 0
    success_count = 0

    for i in range(20):
        try:
            # 10%の確率で人工的なエラーを発生
            if i % 10 == 0:
                raise Exception("シミュレートされた一時的障害")

            await limited_memory.store(f"回復力テスト {i}")
            success_count += 1

        except Exception as e:
            error_count += 1
            # エラーメトリクスを記録
            observability.record_metric("simulated_errors", 1.0, "counter")

    print(f"   成功操作: {success_count}")
    print(f"   エラー操作: {error_count}")
    print(f"   成功率: {success_count/(success_count+error_count):.1%}")


async def main():
    """メインデモ実行関数"""
    print("🚀 AI Blocks 包括的機能デモンストレーション")
    print("=" * 80)
    print("実装された全ての機能を統合的にデモンストレーションします")

    start_time = time.time()

    try:
        # 各機能のデモを順次実行
        await demo_hotswap_capabilities()
        await demo_dynamic_configuration()
        await demo_observability_features()
        await demo_integrated_workflow()
        await demo_error_handling_and_resilience()

        end_time = time.time()

        # 最終サマリー
        await demo_section_header("デモ完了サマリー")
        print(f"\n🎉 全ての機能デモが正常に完了しました！")
        print(f"⏱️  総実行時間: {end_time - start_time:.2f}秒")

        # 最終的な観測可能性レポート
        observability = await get_observability_manager()
        summary = observability.get_observability_summary()

        print(f"\n📊 最終観測可能性レポート:")
        print(f"   総トレース数: {summary['traces']['total_count']}")
        print(f"   総メトリクス数: {summary['metrics']['total_count']}")

        if summary["performance"]:
            perf = summary["performance"]
            print(f"   最終CPU使用率: {perf['cpu_usage']:.1f}%")
            print(f"   最終メモリ使用率: {perf['memory_usage']:.1f}%")

        print(f"\n✨ AI Blocks の積木式設計により、全ての機能が")
        print(f"   シームレスに連携して動作することが確認できました！")

    except Exception as e:
        print(f"\n❌ デモ実行中にエラーが発生しました: {e}")
        import traceback

        traceback.print_exc()

    finally:
        # クリーンアップ
        try:
            observability = await get_observability_manager()
            await observability.cleanup()

            config_manager = await get_config_manager()
            await config_manager.cleanup()
        except:
            pass  # クリーンアップエラーは無視


if __name__ == "__main__":
    asyncio.run(main())

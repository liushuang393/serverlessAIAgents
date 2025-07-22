"""
高度な観測可能性機能のデモンストレーション

このサンプルは、分散トレーシング、リアルタイムメトリクス、パフォーマンス監視機能を示します。
OpenTelemetry準拠のトレーシング、Prometheus形式のメトリクス、構造化ログを含みます。
"""

import asyncio
import random
import time
from typing import Any, Dict, List, Tuple

from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.tool import ToolManager
from ai_blocks.utils.observability import (
    SpanStatus,
    get_observability_manager,
    record_counter,
    record_gauge,
    record_histogram,
    trace,
)


@trace("simulate_llm_request")
async def simulate_llm_request(prompt: str, model: str = "gpt-3.5-turbo") -> str:
    """LLMリクエストをシミュレートする"""
    manager = await get_observability_manager()

    # リクエストカウンターを増加
    await record_counter("llm_requests_total", labels={"model": model})

    # 処理時間をシミュレート
    processing_time = random.uniform(0.5, 3.0)

    with manager.trace(
        "llm_processing", {"model": model, "prompt_length": len(prompt)}
    ):
        await asyncio.sleep(processing_time)

        # 応答時間をヒストグラムに記録
        await record_histogram(
            "llm_response_time_seconds", processing_time, {"model": model}
        )

        # トークン数をゲージに記録
        token_count = len(prompt.split()) * 1.3  # 概算
        await record_gauge("llm_tokens_processed", token_count, {"model": model})

        # エラーをランダムにシミュレート
        if random.random() < 0.1:  # 10%の確率でエラー
            await record_counter(
                "llm_errors_total", labels={"model": model, "error_type": "timeout"}
            )
            raise Exception("LLMタイムアウトエラー")

        return f"LLM応答: {prompt[:50]}... (処理時間: {processing_time:.2f}秒)"


@trace("memory_operations")
async def simulate_memory_operations() -> None:
    """メモリ操作をシミュレートする"""
    manager = await get_observability_manager()

    memory = VectorMemory(max_items=100)

    # データ保存操作
    with manager.trace("memory_store_batch"):
        documents = [
            "人工知能は現代技術の重要な分野です",
            "機械学習アルゴリズムは日々進歩しています",
            "深層学習は画像認識に革命をもたらしました",
            "自然言語処理は人間とコンピュータの橋渡しです",
            "強化学習はゲームAIで大きな成果を上げています",
        ]

        for i, doc in enumerate(documents):
            store_time = time.time()
            await memory.store(doc, {"batch_id": "demo", "index": i})
            store_duration = time.time() - store_time

            await record_histogram("memory_store_duration_seconds", store_duration)
            await record_counter(
                "memory_operations_total", labels={"operation": "store"}
            )

    # 検索操作
    with manager.trace("memory_search_batch"):
        queries = ["人工知能", "機械学習", "深層学習"]

        for query in queries:
            search_time = time.time()
            results = await memory.search(query, limit=3)
            search_duration = time.time() - search_time

            await record_histogram("memory_search_duration_seconds", search_duration)
            await record_gauge(
                "memory_search_results_count",
                len(results),
                {"query_type": "similarity"},
            )
            await record_counter(
                "memory_operations_total", labels={"operation": "search"}
            )

    # メモリ使用量を記録
    total_items = await memory.count()
    await record_gauge("memory_items_total", total_items)


@trace("tool_execution_simulation")
async def simulate_tool_execution() -> None:
    """ツール実行をシミュレートする"""
    manager = await get_observability_manager()

    tool_manager = ToolManager()

    # 複数のツールを実行
    tools_to_test: List[Tuple[str, Dict[str, Any]]] = [
        ("echo", {"text": "Hello, World!"}),
        ("add", {"a": 10, "b": 20}),
        ("multiply", {"a": 5, "b": 7}),
        ("get_current_time", {}),
    ]

    for tool_name, params in tools_to_test:
        with manager.trace(f"tool_execution_{tool_name}", {"tool": tool_name}):
            execution_time = time.time()

            try:
                result = await tool_manager.execute(tool_name, params)
                execution_duration = time.time() - execution_time

                # 成功メトリクス
                await record_histogram(
                    "tool_execution_duration_seconds",
                    execution_duration,
                    {"tool": tool_name},
                )
                await record_counter(
                    "tool_executions_total",
                    labels={"tool": tool_name, "status": "success"},
                )

                if result.success:
                    await record_gauge(
                        "tool_last_execution_success", 1, {"tool": tool_name}
                    )
                else:
                    await record_gauge(
                        "tool_last_execution_success", 0, {"tool": tool_name}
                    )
                    await record_counter(
                        "tool_errors_total",
                        labels={"tool": tool_name, "error_type": "execution_failed"},
                    )

            except Exception:
                execution_duration = time.time() - execution_time

                # エラーメトリクス
                await record_histogram(
                    "tool_execution_duration_seconds",
                    execution_duration,
                    {"tool": tool_name},
                )
                await record_counter(
                    "tool_executions_total",
                    labels={"tool": tool_name, "status": "error"},
                )
                await record_counter(
                    "tool_errors_total",
                    labels={"tool": tool_name, "error_type": "exception"},
                )
                await record_gauge(
                    "tool_last_execution_success", 0, {"tool": tool_name}
                )


async def demo_distributed_tracing():
    """分散トレーシングのデモ"""
    print("\n" + "=" * 60)
    print("🔍 分散トレーシングデモ")
    print("=" * 60)

    manager = await get_observability_manager()

    # 複雑な処理フローをトレース
    trace = manager.tracer.start_trace("complex_ai_workflow")

    try:
        # 並列処理をシミュレート
        tasks = [
            simulate_llm_request("人工知能について教えて", "gpt-3.5-turbo"),
            simulate_llm_request("機械学習の基礎を説明して", "gpt-4"),
            simulate_memory_operations(),
            simulate_tool_execution(),
        ]

        results = await asyncio.gather(*tasks, return_exceptions=True)

        # 結果を処理
        for i, result in enumerate(results):
            if isinstance(result, Exception):
                print(f"   タスク {i+1} でエラーが発生: {result}")
            else:
                print(f"   タスク {i+1} 完了")

    finally:
        manager.tracer.finish_trace(trace)

    # トレース結果を表示
    print("\n📊 トレース結果:")
    print(f"   トレースID: {trace.trace_id}")
    print(f"   総実行時間: {trace.duration:.3f}秒")
    print(f"   スパン数: {len(trace.spans)}")

    # スパンの詳細
    print("\n📋 スパン詳細:")
    for span in trace.spans.values():
        status_icon = "✅" if span.status == SpanStatus.OK else "❌"
        print(f"   {status_icon} {span.operation_name}: {span.duration:.3f}秒")
        if span.tags:
            for key, value in list(span.tags.items())[:3]:  # 最初の3つのタグを表示
                print(f"      {key}: {value}")


async def demo_metrics_collection():
    """メトリクス収集のデモ"""
    print("\n" + "=" * 60)
    print("📊 メトリクス収集デモ")
    print("=" * 60)

    manager = await get_observability_manager()

    # 様々なメトリクスを生成
    print("\n1️⃣ メトリクス生成中...")

    # カウンターメトリクス
    for i in range(10):
        await record_counter(
            "demo_requests_total", labels={"endpoint": f"/api/v{i%3+1}"}
        )
        await record_counter("demo_errors_total", labels={"error_type": "validation"})

    # ゲージメトリクス
    await record_gauge("demo_active_users", random.randint(50, 200))
    await record_gauge("demo_queue_size", random.randint(0, 50))
    await record_gauge("demo_cache_hit_ratio", random.uniform(0.7, 0.95))

    # ヒストグラムメトリクス
    for _ in range(20):
        await record_histogram(
            "demo_request_duration_seconds", random.uniform(0.1, 2.0)
        )
        await record_histogram("demo_payload_size_bytes", random.uniform(100, 10000))

    # 収集されたメトリクスを表示
    metrics = manager.metrics_collector.get_metrics()
    print(f"\n2️⃣ 収集されたメトリクス ({len(metrics)}件):")

    for metric in metrics[:10]:  # 最初の10件を表示
        labels_str = (
            ", ".join(f"{k}={v}" for k, v in metric.labels.items())
            if metric.labels
            else "なし"
        )
        print(
            f"   {metric.name} ({metric.type}): {metric.value:.3f} [ラベル: {labels_str}]"
        )

    # Prometheus形式での出力
    print("\n3️⃣ Prometheus形式出力（抜粋）:")
    prometheus_output = manager.metrics_collector.get_prometheus_format()
    lines = prometheus_output.split("\n")[:15]  # 最初の15行を表示
    for line in lines:
        print(f"   {line}")
    if len(prometheus_output.split("\n")) > 15:
        print("   ...")


async def demo_performance_monitoring():
    """パフォーマンス監視のデモ"""
    print("\n" + "=" * 60)
    print("🏥 パフォーマンス監視デモ")
    print("=" * 60)

    manager = await get_observability_manager()

    # 現在のパフォーマンスメトリクスを取得
    current_metrics = manager.performance_monitor.get_current_metrics()

    if current_metrics:
        print("\n📈 現在のパフォーマンス:")
        print(f"   CPU使用率: {current_metrics.cpu_usage:.1f}%")
        print(f"   メモリ使用率: {current_metrics.memory_usage:.1f}%")
        print(f"   リクエスト数: {current_metrics.request_count}")
        print(f"   エラー数: {current_metrics.error_count}")
        print(f"   平均応答時間: {current_metrics.avg_response_time:.3f}秒")
        print(f"   アクティブ接続数: {current_metrics.active_connections}")
    else:
        print("   パフォーマンスメトリクスはまだ利用できません")

    # 履歴を取得
    history = manager.performance_monitor.get_metrics_history(limit=5)

    if history:
        print(f"\n📊 パフォーマンス履歴 (最新{len(history)}件):")
        for i, metrics in enumerate(history, 1):
            timestamp = time.strftime("%H:%M:%S", time.localtime(metrics.timestamp))
            print(
                f"   {i}. {timestamp} - CPU: {metrics.cpu_usage:.1f}%, メモリ: {metrics.memory_usage:.1f}%"
            )

    # 負荷テストをシミュレート
    print("\n⚡ 負荷テストシミュレーション:")

    # 高負荷処理をシミュレート
    tasks = []
    for i in range(5):
        task = asyncio.create_task(simulate_llm_request(f"負荷テスト {i}", "gpt-3.5-turbo"))
        tasks.append(task)

    start_time = time.time()
    results = await asyncio.gather(*tasks, return_exceptions=True)
    end_time = time.time()

    successful_requests = sum(1 for r in results if not isinstance(r, Exception))
    failed_requests = len(results) - successful_requests

    print(f"   総実行時間: {end_time - start_time:.2f}秒")
    print(f"   成功リクエスト: {successful_requests}")
    print(f"   失敗リクエスト: {failed_requests}")


async def demo_observability_summary():
    """観測可能性の概要デモ"""
    print("\n" + "=" * 60)
    print("📋 観測可能性概要デモ")
    print("=" * 60)

    manager = await get_observability_manager()

    # 概要を取得
    summary = manager.get_observability_summary()

    print("\n🎯 サービス概要:")
    print(f"   サービス名: {summary['service_name']}")
    print(
        f"   タイムスタンプ: {time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(summary['timestamp']))}"
    )

    print("\n🔍 トレーシング:")
    print(f"   総トレース数: {summary['traces']['total_count']}")
    print(f"   アクティブトレース数: {summary['traces']['active_count']}")

    print("\n📊 メトリクス:")
    print(f"   総メトリクス数: {summary['metrics']['total_count']}")
    for metric_type, count in summary["metrics"]["types"].items():
        print(f"   {metric_type}: {count}件")

    if summary["performance"]:
        print("\n🏥 パフォーマンス:")
        perf = summary["performance"]
        print(f"   CPU使用率: {perf['cpu_usage']:.1f}%")
        print(f"   メモリ使用率: {perf['memory_usage']:.1f}%")
        print(f"   平均応答時間: {perf['avg_response_time']:.3f}秒")


async def main():
    """メインデモ実行関数"""
    print("🎯 AI Blocks 高度な観測可能性機能デモ")
    print("=" * 80)

    try:
        # 各デモを順次実行
        await demo_distributed_tracing()
        await demo_metrics_collection()
        await demo_performance_monitoring()
        await demo_observability_summary()

        print("\n" + "=" * 80)
        print("🎉 全ての観測可能性デモが完了しました！")
        print("=" * 80)

    except Exception as e:
        print(f"\n❌ デモ実行中にエラーが発生しました: {e}")
        import traceback

        traceback.print_exc()

    finally:
        # クリーンアップ
        manager = await get_observability_manager()
        await manager.cleanup()


if __name__ == "__main__":
    asyncio.run(main())

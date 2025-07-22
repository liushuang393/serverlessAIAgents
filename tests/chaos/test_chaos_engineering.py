"""
カオスエンジニアリングテスト

このモジュールは、システムの耐障害性をテストするためのカオステストを提供します。
ランダムな障害注入、リソース制限、ネットワーク分断などをシミュレートします。
"""

import asyncio
import random
import time
from typing import Any, Dict, List

import pytest

from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.thread import SimpleThread
from ai_blocks.core.tool import ToolManager
from ai_blocks.utils.observability import MetricType, get_observability_manager


class ChaosInjector:
    """カオス注入器"""

    def __init__(self, failure_rate: float = 0.1):
        self.failure_rate = failure_rate
        self.active_failures: Dict[str, bool] = {}

    def should_fail(self, operation: str) -> bool:
        """操作が失敗すべきかどうかを判定する"""
        return random.random() < self.failure_rate

    def inject_latency(self, min_delay: float = 0.1, max_delay: float = 2.0) -> float:
        """レイテンシを注入する"""
        return random.uniform(min_delay, max_delay)

    def inject_memory_pressure(self) -> bool:
        """メモリ圧迫をシミュレートする"""
        return random.random() < 0.05  # 5%の確率

    def inject_network_partition(self, service: str) -> bool:
        """ネットワーク分断をシミュレートする"""
        return self.active_failures.get(f"network_{service}", False)

    def activate_failure(self, failure_type: str) -> None:
        """障害を有効化する"""
        self.active_failures[failure_type] = True

    def deactivate_failure(self, failure_type: str) -> None:
        """障害を無効化する"""
        self.active_failures[failure_type] = False


class ChaosMemory(VectorMemory):
    """カオス注入付きメモリコンポーネント"""

    def __init__(self, chaos_injector: ChaosInjector, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.chaos_injector = chaos_injector

    async def store(self, content: str, metadata: Dict[str, Any] = None) -> str:
        """カオス注入付きストア操作"""
        # レイテンシ注入
        if self.chaos_injector.should_fail("latency"):
            delay = self.chaos_injector.inject_latency()
            await asyncio.sleep(delay)

        # 障害注入
        if self.chaos_injector.should_fail("store"):
            raise Exception("カオス注入: ストア操作が失敗しました")

        # メモリ圧迫シミュレート
        if self.chaos_injector.inject_memory_pressure():
            raise MemoryError("カオス注入: メモリ不足")

        return await super().store(content, metadata)

    async def search(
        self, query: str, limit: int = 10, threshold: float = None
    ) -> List:
        """カオス注入付き検索操作"""
        # ネットワーク分断シミュレート
        if self.chaos_injector.inject_network_partition("memory"):
            raise ConnectionError("カオス注入: ネットワーク分断")

        # レイテンシ注入
        if self.chaos_injector.should_fail("latency"):
            delay = self.chaos_injector.inject_latency(0.5, 3.0)
            await asyncio.sleep(delay)

        # 部分的な結果返却
        if self.chaos_injector.should_fail("partial_results"):
            results = await super().search(query, limit, threshold)
            return results[: max(1, len(results) // 2)]  # 結果の半分のみ返却

        return await super().search(query, limit, threshold)


class ChaosToolManager(ToolManager):
    """カオス注入付きツールマネージャー"""

    def __init__(self, chaos_injector: ChaosInjector, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.chaos_injector = chaos_injector

    async def execute(self, tool_name: str, parameters: Dict[str, Any]):
        """カオス注入付きツール実行"""
        # タイムアウトシミュレート
        if self.chaos_injector.should_fail("timeout"):
            await asyncio.sleep(self._timeout + 1)  # タイムアウトを超過

        # 実行失敗シミュレート
        if self.chaos_injector.should_fail("execution"):
            from ai_blocks.core.models import ToolResult

            return ToolResult(
                success=False,
                result=None,
                error_message="カオス注入: ツール実行が失敗しました",
                execution_time=0.0,
            )

        # レイテンシ注入
        if self.chaos_injector.should_fail("latency"):
            delay = self.chaos_injector.inject_latency()
            await asyncio.sleep(delay)

        return await super().execute(tool_name, parameters)


class TestChaosEngineering:
    """カオスエンジニアリングテストクラス"""

    @pytest.fixture(autouse=True)
    async def setup_and_teardown(self):
        """テストのセットアップとクリーンアップ"""
        self.chaos_injector = ChaosInjector(failure_rate=0.2)  # 20%の障害率
        self.observability = await get_observability_manager()

        yield

        # クリーンアップ
        await self.observability.cleanup()

    @pytest.mark.asyncio
    async def test_memory_chaos_resilience(self):
        """メモリコンポーネントのカオス耐性テスト"""
        chaos_memory = ChaosMemory(self.chaos_injector, max_items=100)

        successful_stores = 0
        failed_stores = 0
        successful_searches = 0
        failed_searches = 0

        # 大量の操作を実行して障害に対する耐性をテスト
        for i in range(50):
            try:
                # ストア操作
                _ = await chaos_memory.store(f"カオステストデータ {i}")
                successful_stores += 1

                # 検索操作
                _ = await chaos_memory.search("カオス", limit=5)
                successful_searches += 1

            except Exception as e:
                if "ストア" in str(e):
                    failed_stores += 1
                else:
                    failed_searches += 1

        # 結果の検証
        total_operations = (
            successful_stores + failed_stores + successful_searches + failed_searches
        )
        success_rate = (successful_stores + successful_searches) / total_operations

        print("メモリカオステスト結果:")
        print(f"  成功ストア: {successful_stores}")
        print(f"  失敗ストア: {failed_stores}")
        print(f"  成功検索: {successful_searches}")
        print(f"  失敗検索: {failed_searches}")
        print(f"  成功率: {success_rate:.2%}")

        # 最低限の成功率を確保
        assert success_rate > 0.5  # 50%以上の成功率を期待

    @pytest.mark.asyncio
    async def test_tool_chaos_resilience(self):
        """ツールマネージャーのカオス耐性テスト"""
        chaos_tool_manager = ChaosToolManager(self.chaos_injector)

        successful_executions = 0
        failed_executions = 0
        timeout_executions = 0

        # 様々なツールを実行
        tools_to_test: List[tuple[str, Dict[str, Any]]] = [
            ("echo", {"text": "カオステスト"}),
            ("add", {"a": 10, "b": 20}),
            ("multiply", {"a": 5, "b": 7}),
            ("get_current_time", {}),
        ]

        for _ in range(20):  # 各ツールを複数回実行
            for tool_name, params in tools_to_test:
                try:
                    start_time = time.time()
                    result = await asyncio.wait_for(
                        chaos_tool_manager.execute(tool_name, params), timeout=5.0
                    )
                    _ = time.time() - start_time

                    if result.success:
                        successful_executions += 1
                    else:
                        failed_executions += 1

                except asyncio.TimeoutError:
                    timeout_executions += 1
                except Exception:
                    failed_executions += 1

        total_executions = (
            successful_executions + failed_executions + timeout_executions
        )
        success_rate = successful_executions / total_executions

        print("ツールカオステスト結果:")
        print(f"  成功実行: {successful_executions}")
        print(f"  失敗実行: {failed_executions}")
        print(f"  タイムアウト: {timeout_executions}")
        print(f"  成功率: {success_rate:.2%}")

        # 最低限の成功率を確保
        assert success_rate > 0.4  # 40%以上の成功率を期待

    @pytest.mark.asyncio
    async def test_concurrent_chaos_operations(self):
        """並行カオス操作テスト"""
        chaos_memory = ChaosMemory(self.chaos_injector, max_items=200)
        chaos_tool_manager = ChaosToolManager(self.chaos_injector)

        async def memory_worker(worker_id: int):
            """メモリワーカー"""
            results = {"success": 0, "failure": 0}

            for i in range(10):
                try:
                    _ = await chaos_memory.store(f"ワーカー{worker_id}データ{i}")
                    _ = await chaos_memory.search(f"ワーカー{worker_id}")
                    results["success"] += 1
                except Exception:
                    results["failure"] += 1

            return results

        async def tool_worker(worker_id: int):
            """ツールワーカー"""
            results = {"success": 0, "failure": 0}

            for i in range(10):
                try:
                    result = await chaos_tool_manager.execute(
                        "echo", {"text": f"ワーカー{worker_id}_{i}"}
                    )
                    if result.success:
                        results["success"] += 1
                    else:
                        results["failure"] += 1
                except Exception:
                    results["failure"] += 1

            return results

        # 複数のワーカーを並行実行
        memory_workers = [memory_worker(i) for i in range(5)]
        tool_workers = [tool_worker(i) for i in range(5)]

        start_time = time.time()
        memory_results = await asyncio.gather(*memory_workers, return_exceptions=True)
        tool_results = await asyncio.gather(*tool_workers, return_exceptions=True)
        end_time = time.time()

        # 結果の集計
        total_memory_success = sum(
            r["success"] for r in memory_results if isinstance(r, dict)
        )
        total_memory_failure = sum(
            r["failure"] for r in memory_results if isinstance(r, dict)
        )
        total_tool_success = sum(
            r["success"] for r in tool_results if isinstance(r, dict)
        )
        total_tool_failure = sum(
            r["failure"] for r in tool_results if isinstance(r, dict)
        )

        print(f"並行カオステスト結果 (実行時間: {end_time - start_time:.2f}秒):")
        print(f"  メモリ成功: {total_memory_success}, 失敗: {total_memory_failure}")
        print(f"  ツール成功: {total_tool_success}, 失敗: {total_tool_failure}")

        # システムが完全に停止していないことを確認
        assert total_memory_success > 0 or total_tool_success > 0

    @pytest.mark.asyncio
    async def test_network_partition_simulation(self):
        """ネットワーク分断シミュレーションテスト"""
        chaos_memory = ChaosMemory(self.chaos_injector, max_items=100)

        # 正常状態でのテスト
        _ = await chaos_memory.store("分断テスト前")
        results = await chaos_memory.search("分断")
        assert len(results) > 0

        # ネットワーク分断を有効化
        self.chaos_injector.activate_failure("network_memory")

        # 分断中の操作テスト
        partition_failures = 0
        for _ in range(10):
            try:
                await chaos_memory.search("分断テスト")
            except ConnectionError:
                partition_failures += 1

        # 分断を無効化
        self.chaos_injector.deactivate_failure("network_memory")

        # 復旧後のテスト
        results = await chaos_memory.search("分断")
        assert len(results) > 0

        print("ネットワーク分断テスト結果:")
        print(f"  分断中の失敗: {partition_failures}/10")

        # 分断が実際に発生していることを確認
        assert partition_failures > 0

    @pytest.mark.asyncio
    async def test_resource_exhaustion_simulation(self):
        """リソース枯渇シミュレーションテスト"""
        # メモリ制限の厳しいコンポーネントを作成
        limited_memory = ChaosMemory(self.chaos_injector, max_items=10)

        memory_errors = 0
        successful_stores = 0

        # 大量のデータを保存してメモリ圧迫をテスト
        for i in range(50):
            try:
                await limited_memory.store(f"大量データ {i}" * 100)  # 大きなデータ
                successful_stores += 1
            except MemoryError:
                memory_errors += 1
            except Exception:
                pass  # その他のカオス障害は無視

        print("リソース枯渇テスト結果:")
        print(f"  成功ストア: {successful_stores}")
        print(f"  メモリエラー: {memory_errors}")

        # メモリ制限が機能していることを確認
        total_items = await limited_memory.count()
        assert total_items <= 10  # 制限内に収まっている

    @pytest.mark.asyncio
    async def test_cascading_failure_resilience(self):
        """カスケード障害耐性テスト"""
        # 複数のコンポーネントを連携させる
        chaos_memory = ChaosMemory(self.chaos_injector, max_items=100)
        chaos_tool_manager = ChaosToolManager(self.chaos_injector)
        thread = SimpleThread(max_history=50)

        # 高い障害率を設定
        high_chaos_injector = ChaosInjector(failure_rate=0.5)
        high_chaos_memory = ChaosMemory(high_chaos_injector, max_items=100)

        async def complex_workflow(workflow_id: int):
            """複雑なワークフローをシミュレート"""
            try:
                # 1. メモリにデータを保存
                _ = await chaos_memory.store(f"ワークフロー{workflow_id}データ")

                # 2. ツールを実行
                _ = await chaos_tool_manager.execute(
                    "echo", {"text": f"ワークフロー{workflow_id}"}
                )

                # 3. 高障害率メモリで検索
                _ = await high_chaos_memory.search(f"ワークフロー{workflow_id}")

                # 4. スレッドにメッセージを追加
                from ai_blocks.core.models import Message, MessageRole

                message = Message(
                    role=MessageRole.USER, content=f"ワークフロー{workflow_id}完了"
                )
                await thread.add_message(message)

                return True

            except Exception:
                return False

        # 複数のワークフローを並行実行
        workflows = [complex_workflow(i) for i in range(20)]
        results = await asyncio.gather(*workflows, return_exceptions=True)

        successful_workflows = sum(1 for r in results if r is True)
        failed_workflows = len(results) - successful_workflows

        print("カスケード障害テスト結果:")
        print(f"  成功ワークフロー: {successful_workflows}")
        print(f"  失敗ワークフロー: {failed_workflows}")
        print(f"  成功率: {successful_workflows/len(results):.2%}")

        # 一部のワークフローは成功することを確認
        assert successful_workflows > 0

    @pytest.mark.asyncio
    async def test_recovery_after_chaos(self):
        """カオス後の回復テスト"""
        chaos_memory = ChaosMemory(self.chaos_injector, max_items=100)

        # 高い障害率でカオスを実行
        original_failure_rate = self.chaos_injector.failure_rate
        self.chaos_injector.failure_rate = 0.8  # 80%の障害率

        # カオス期間中の操作
        chaos_operations = 0
        chaos_successes = 0

        for i in range(20):
            try:
                await chaos_memory.store(f"カオス期間データ {i}")
                chaos_successes += 1
            except Exception:
                pass
            chaos_operations += 1

        # 障害率を元に戻す（回復）
        self.chaos_injector.failure_rate = 0.1  # 10%の障害率

        # 回復期間中の操作
        recovery_operations = 0
        recovery_successes = 0

        for i in range(20):
            try:
                await chaos_memory.store(f"回復期間データ {i}")
                recovery_successes += 1
            except Exception:
                pass
            recovery_operations += 1

        # 障害率を元に戻す
        self.chaos_injector.failure_rate = original_failure_rate

        chaos_success_rate = chaos_successes / chaos_operations
        recovery_success_rate = recovery_successes / recovery_operations

        print("回復テスト結果:")
        print(f"  カオス期間成功率: {chaos_success_rate:.2%}")
        print(f"  回復期間成功率: {recovery_success_rate:.2%}")

        # 回復期間の成功率がカオス期間より高いことを確認
        assert recovery_success_rate > chaos_success_rate
        assert recovery_success_rate > 0.7  # 70%以上の回復を期待

    @pytest.mark.asyncio
    async def test_observability_under_chaos(self):
        """カオス下での観測可能性テスト"""
        chaos_memory = ChaosMemory(self.chaos_injector, max_items=100)

        # 観測可能性システムが障害中も動作することを確認
        with self.observability.trace("chaos_observability_test"):
            operations_attempted = 0
            operations_successful = 0

            for i in range(30):
                operations_attempted += 1

                try:
                    # メトリクスを記録
                    self.observability.record_metric(
                        "chaos_test_operations",
                        1.0,
                        MetricType.COUNTER,
                        {"operation": "store", "attempt": str(i)},
                    )

                    # 実際の操作を実行
                    await chaos_memory.store(f"観測可能性テスト {i}")
                    operations_successful += 1

                    # 成功メトリクスを記録
                    self.observability.record_metric(
                        "chaos_test_success",
                        1.0,
                        MetricType.COUNTER,
                        {"operation": "store"},
                    )

                except Exception:
                    # 失敗メトリクスを記録
                    self.observability.record_metric(
                        "chaos_test_failures",
                        1.0,
                        MetricType.COUNTER,
                        {"operation": "store"},
                    )

        # 観測可能性システムが機能していることを確認
        summary = self.observability.get_observability_summary()
        assert summary["traces"]["total_count"] > 0
        assert summary["metrics"]["total_count"] > 0

        # メトリクスが記録されていることを確認
        metrics = self.observability.metrics_collector.get_metrics()
        metric_names = [m.name for m in metrics]
        assert "chaos_test_operations" in metric_names

        success_rate = operations_successful / operations_attempted
        print("カオス下観測可能性テスト結果:")
        print(f"  成功率: {success_rate:.2%}")
        print(f"  記録されたメトリクス数: {len(metrics)}")

        # 観測可能性システムが障害に影響されていないことを確認
        assert len(metrics) > 0

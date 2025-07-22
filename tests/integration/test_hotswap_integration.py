"""
ホットスワップ機能の統合テスト

このモジュールは、ホットスワップ機能の統合テストを提供します。
実際のコンポーネント交換、バージョン管理、デプロイメント戦略をテストします。
"""

import asyncio
import time
from typing import Any, Dict

import pytest

from ai_blocks.core.factory import get_factory
from ai_blocks.core.memory import MemoryInterface, VectorMemory
from ai_blocks.core.models import MemoryItem
from ai_blocks.core.registry import (
    DeploymentStrategy,
    HealthCheckResult,
    get_registry,
    register_component,
)


# テスト用の改良版メモリコンポーネント
@register_component(
    component_type="memory",
    component_name="test_vector",
    version="2.0.0",
    deployment_strategy=DeploymentStrategy.CANARY,
)
class TestEnhancedVectorMemory(MemoryInterface):
    """テスト用改良版ベクトル記憶実装"""

    def __init__(
        self, max_items: int = 1000, similarity_threshold: float = 0.7, **kwargs
    ):
        self.max_items = max_items
        self.similarity_threshold = similarity_threshold
        self._items: Dict[str, MemoryItem] = {}
        self._version = "2.0.0"
        self._performance_boost = True  # v2.0.0の新機能

    async def store(self, content: str, metadata: Dict[str, Any] = None) -> str:
        """コンテンツを記憶に保存する（改良版）"""
        import uuid
        from datetime import datetime

        memory_id = str(uuid.uuid4())
        item = MemoryItem(
            id=memory_id,
            content=content,
            metadata={**(metadata or {}), "version": self._version},
            similarity_score=None,
            created_at=datetime.now(),
        )

        self._items[memory_id] = item
        return memory_id

    async def search(
        self, query: str, limit: int = 10, threshold: float = None
    ) -> list[MemoryItem]:
        """類似性検索を実行する（改良版）"""
        results = []
        for item in self._items.values():
            if query.lower() in item.content.lower():
                item.similarity_score = (
                    0.95 if self._performance_boost else 0.8
                )  # 改良版では高い類似度
                results.append(item)

        return results[:limit]

    async def get(self, memory_id: str) -> MemoryItem:
        """IDで記憶を取得する"""
        item = self._items.get(memory_id)
        if item is None:
            raise KeyError(f"Memory item with ID '{memory_id}' not found")
        return item

    async def delete(self, memory_id: str) -> bool:
        """記憶を削除する"""
        if memory_id in self._items:
            del self._items[memory_id]
            return True
        return False

    async def count(self) -> int:
        """記憶の総数を取得する"""
        return len(self._items)

    async def clear(self) -> None:
        """全ての記憶をクリアする"""
        self._items.clear()

    async def health_check(self) -> HealthCheckResult:
        """ヘルスチェック（改良版）"""
        try:
            test_id = await self.store("ヘルスチェック v2.0.0", {"test": True})
            await self.search("ヘルスチェック", limit=1)
            await self.delete(test_id)

            return HealthCheckResult(
                healthy=True,
                message="Enhanced Vector Memory v2.0.0 は正常に動作しています",
                metrics={
                    "total_items": len(self._items),
                    "version": self._version,
                    "performance_boost": self._performance_boost,
                },
            )
        except Exception as e:
            return HealthCheckResult(
                healthy=False,
                message=f"Enhanced Vector Memory v2.0.0 でエラー: {str(e)}",
                metrics={"error": str(e), "version": self._version},
            )


class TestHotswapIntegration:
    """ホットスワップ統合テストクラス"""

    @pytest.fixture(autouse=True)
    async def setup_and_teardown(self):
        """テストのセットアップとクリーンアップ"""
        # セットアップ
        self.registry = get_registry()
        self.factory = get_factory()

        # v1.0.0のコンポーネントを登録
        self.registry.register_component(
            component_type="memory",
            component_name="test_vector",
            component_class=VectorMemory,
            version="1.0.0",
            deployment_strategy=DeploymentStrategy.IMMEDIATE,
        )

        yield

        # クリーンアップ
        self.factory.clear_cache()

    @pytest.mark.asyncio
    async def test_basic_component_creation(self):
        """基本的なコンポーネント作成テスト"""
        # v1.0.0のコンポーネントを作成
        memory_v1 = await self.factory.create("memory", "test_vector", "1.0.0")
        assert memory_v1 is not None

        # データを保存してテスト
        memory_id = await memory_v1.store("テストデータ v1.0.0")
        assert memory_id is not None

        # 検索テスト
        results = await memory_v1.search("テスト")
        assert len(results) > 0
        assert results[0].content == "テストデータ v1.0.0"

    @pytest.mark.asyncio
    async def test_version_deployment(self):
        """バージョンデプロイメントテスト"""
        # 初期状態の確認
        registration = self.registry.get_registration("memory", "test_vector")
        assert registration is not None
        assert registration.current_version == "1.0.0"

        # v2.0.0にデプロイ
        success = await self.registry.deploy_version(
            "memory", "test_vector", "2.0.0", strategy=DeploymentStrategy.IMMEDIATE
        )
        assert success is True

        # バージョンが更新されたことを確認
        registration = self.registry.get_registration("memory", "test_vector")
        assert registration is not None
        assert registration.current_version == "2.0.0"

        # 新しいバージョンでコンポーネントを作成
        memory_v2 = await self.factory.create("memory", "test_vector", "2.0.0")
        assert memory_v2 is not None

        # v2.0.0の機能をテスト
        await memory_v2.store("テストデータ v2.0.0")
        results = await memory_v2.search("テスト")
        assert len(results) > 0
        assert results[0].similarity_score == 0.95  # v2.0.0の改良された類似度

    @pytest.mark.asyncio
    async def test_canary_deployment(self):
        """カナリアデプロイメントテスト"""
        # カナリアデプロイメントを実行
        success = await self.registry.deploy_version(
            "memory",
            "test_vector",
            "2.0.0",
            strategy=DeploymentStrategy.CANARY,
            canary_percentage=0.1,
        )
        assert success is True

        # デプロイメント後の状態確認
        registration = self.registry.get_registration("memory", "test_vector")
        assert registration is not None
        assert registration.current_version == "2.0.0"

        # 両方のバージョンが利用可能であることを確認
        memory_v1 = await self.factory.create("memory", "test_vector", "1.0.0")
        memory_v2 = await self.factory.create("memory", "test_vector", "2.0.0")

        assert memory_v1 is not None
        assert memory_v2 is not None

    @pytest.mark.asyncio
    async def test_health_check_integration(self):
        """ヘルスチェック統合テスト"""
        # v1.0.0のヘルスチェック
        memory_v1 = await self.factory.create("memory", "test_vector", "1.0.0")
        health_v1 = await memory_v1.health_check()
        assert health_v1.healthy is True

        # v2.0.0にデプロイ
        await self.registry.deploy_version("memory", "test_vector", "2.0.0")

        # v2.0.0のヘルスチェック
        memory_v2 = await self.factory.create("memory", "test_vector", "2.0.0")
        health_v2 = await memory_v2.health_check()
        assert health_v2.healthy is True
        assert health_v2.metrics["version"] == "2.0.0"
        assert health_v2.metrics["performance_boost"] is True

    @pytest.mark.asyncio
    async def test_rollback_functionality(self):
        """ロールバック機能テスト"""
        # v2.0.0にデプロイ
        await self.registry.deploy_version("memory", "test_vector", "2.0.0")

        # 現在のバージョンを確認
        registration = self.registry.get_registration("memory", "test_vector")
        assert registration is not None
        assert registration.current_version == "2.0.0"

        # 問題のあるバージョンをシミュレート（手動でロールバック）
        await self.registry._rollback(registration, "1.0.0")

        # ロールバック後の状態確認
        registration = self.registry.get_registration("memory", "test_vector")
        assert registration is not None
        assert registration.current_version == "1.0.0"

        # v1.0.0が正常に動作することを確認
        memory_v1 = await self.factory.create("memory", "test_vector", "1.0.0")
        await memory_v1.store("ロールバックテスト")
        results = await memory_v1.search("ロールバック")
        assert len(results) > 0

    @pytest.mark.asyncio
    async def test_concurrent_access(self):
        """同時アクセステスト"""
        # 複数のコンポーネントインスタンスを同時に作成
        tasks = []
        for i in range(10):
            task = asyncio.create_task(
                self.factory.create("memory", "test_vector", "1.0.0")
            )
            tasks.append(task)

        instances = await asyncio.gather(*tasks)

        # 全てのインスタンスが正常に作成されたことを確認
        assert len(instances) == 10
        for instance in instances:
            assert instance is not None

        # 各インスタンスが独立して動作することを確認
        for i, instance in enumerate(instances):
            await instance.store(f"同時アクセステスト {i}")
            results = await instance.search("同時アクセス")
            assert len(results) > 0

    @pytest.mark.asyncio
    async def test_deployment_strategies_comparison(self):
        """デプロイメント戦略比較テスト"""
        strategies = [
            DeploymentStrategy.IMMEDIATE,
            DeploymentStrategy.CANARY,
            DeploymentStrategy.BLUE_GREEN,
            DeploymentStrategy.ROLLING,
        ]

        deployment_times = {}

        for strategy in strategies:
            # 各戦略でのデプロイメント時間を測定
            start_time = time.time()

            success = await self.registry.deploy_version(
                "memory", "test_vector", "2.0.0", strategy=strategy
            )

            end_time = time.time()
            deployment_times[strategy] = end_time - start_time

            assert success is True

            # v1.0.0にロールバック（次のテストのため）
            registration = self.registry.get_registration("memory", "test_vector")
            assert registration is not None
            await self.registry._rollback(registration, "1.0.0")

        # デプロイメント時間の結果を確認
        print("\nデプロイメント戦略別実行時間:")
        for strategy, duration in deployment_times.items():
            print(f"  {strategy}: {duration:.3f}秒")

        # 即座デプロイメントが最も高速であることを確認
        assert deployment_times[DeploymentStrategy.IMMEDIATE] <= min(
            deployment_times.values()
        )

    @pytest.mark.asyncio
    async def test_registry_metrics(self):
        """レジストリメトリクステスト"""
        # 初期メトリクスを取得
        initial_metrics = self.registry.get_metrics()

        # 複数のコンポーネントインスタンスを作成
        for i in range(5):
            await self.factory.create("memory", "test_vector", "1.0.0")

        # メトリクスの変化を確認
        updated_metrics = self.registry.get_metrics()

        assert updated_metrics["total_instances"] >= initial_metrics["total_instances"]
        assert "memory:test_vector" in updated_metrics["registrations"]

        registration_info = updated_metrics["registrations"]["memory:test_vector"]
        assert registration_info["current_version"] == "1.0.0"
        assert registration_info["total_versions"] >= 1

    @pytest.mark.asyncio
    async def test_factory_dependency_resolution(self):
        """ファクトリー依存関係解決テスト"""
        # 依存関係を持つコンポーネントのテスト（シンプルな例）
        memory = await self.factory.create(
            "memory", "test_vector", "1.0.0", max_items=500, similarity_threshold=0.8
        )

        assert memory is not None
        assert memory.max_items == 500
        assert memory.similarity_threshold == 0.8

        # 設定が正しく適用されていることを確認
        await memory.store("依存関係テスト")
        results = await memory.search("依存関係")
        assert len(results) > 0

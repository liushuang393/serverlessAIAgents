"""
ホットスワップ機能のデモンストレーション

このサンプルは、実行時でのコンポーネント交換機能を示します。
バージョン管理、段階的デプロイメント、ロールバック機能を含みます。
"""

import asyncio
import time
import uuid
from datetime import datetime
from typing import Any, Dict, Optional

from ai_blocks.core.factory import builder, component_context, get_factory
from ai_blocks.core.memory import MemoryInterface, VectorMemory
from ai_blocks.core.models import MemoryItem
from ai_blocks.core.registry import (
    DeploymentStrategy,
    HealthCheckResult,
    get_registry,
    register_component,
)


# 改良版メモリコンポーネント（v2.0.0）
@register_component(
    component_type="memory",
    component_name="vector",
    version="2.0.0",
    deployment_strategy=DeploymentStrategy.CANARY,
)
class EnhancedVectorMemory(MemoryInterface):
    """改良版ベクトル記憶実装（v2.0.0）"""

    def __init__(
        self, max_items: int = 1000, similarity_threshold: float = 0.7, **kwargs
    ):
        self.max_items = max_items
        self.similarity_threshold = similarity_threshold
        self._items: Dict[str, MemoryItem] = {}
        self._performance_metrics = {
            "total_searches": 0,
            "avg_search_time": 0.0,
            "cache_hits": 0,
        }
        print("🚀 Enhanced Vector Memory v2.0.0 初期化完了（改良版機能付き）")

    async def store(
        self, content: str, metadata: Optional[Dict[str, Any]] = None
    ) -> str:
        """コンテンツを記憶に保存する（改良版）"""
        memory_id = str(uuid.uuid4())
        item = MemoryItem(
            id=memory_id,
            content=content,
            metadata=metadata or {},
            similarity_score=None,
            created_at=datetime.now(),
        )

        self._items[memory_id] = item
        print(f"📝 [v2.0.0] 改良版保存: {content[:50]}... (ID: {memory_id[:8]})")
        return memory_id

    async def search(
        self, query: str, limit: int = 10, threshold: Optional[float] = None
    ) -> list[MemoryItem]:
        """類似性検索を実行する（改良版）"""
        start_time = time.time()

        # 改良版検索アルゴリズム（シンプルな文字列マッチング）
        results = []
        for item in self._items.values():
            if query.lower() in item.content.lower():
                item.similarity_score = 0.9  # 改良版では高い類似度
                results.append(item)

        # パフォーマンスメトリクス更新
        search_time = time.time() - start_time
        self._performance_metrics["total_searches"] += 1
        self._performance_metrics["avg_search_time"] = (
            self._performance_metrics["avg_search_time"]
            * (self._performance_metrics["total_searches"] - 1)
            + search_time
        ) / self._performance_metrics["total_searches"]

        results = results[:limit]
        print(f"🔍 [v2.0.0] 改良版検索: '{query}' -> {len(results)}件 ({search_time:.3f}秒)")
        return results

    async def get(self, memory_id: str) -> Optional[MemoryItem]:
        """IDで記憶を取得する"""
        return self._items.get(memory_id)

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
            _ = await self.search("ヘルスチェック", limit=1)
            await self.delete(test_id)

            return HealthCheckResult(
                healthy=True,
                message="Enhanced Vector Memory v2.0.0 は正常に動作しています",
                metrics={
                    **self._performance_metrics,
                    "total_items": len(self._items),
                    "version": "2.0.0",
                },
            )
        except Exception as e:
            return HealthCheckResult(
                healthy=False,
                message=f"Enhanced Vector Memory v2.0.0 でエラー: {str(e)}",
                metrics={"error": str(e), "version": "2.0.0"},
            )


async def demo_basic_hotswap():
    """基本的なホットスワップのデモ"""
    print("\n" + "=" * 60)
    print("🔄 基本的なホットスワップデモ")
    print("=" * 60)

    registry = get_registry()
    factory = get_factory()

    # v1.0.0のメモリコンポーネントを作成
    print("\n1️⃣ v1.0.0 メモリコンポーネントを作成")
    memory_v1 = await factory.create("memory", "vector", "1.0.0")

    # データを保存
    await memory_v1.store("Python は素晴らしいプログラミング言語です")
    await memory_v1.store("AI Blocks は軽量で柔軟なライブラリです")

    # 検索テスト
    results = await memory_v1.search("Python")
    print(f"v1.0.0 検索結果: {len(results)}件")

    # v2.0.0にアップグレード
    print("\n2️⃣ v2.0.0 にホットスワップ")
    success = await registry.deploy_version(
        "memory",
        "vector",
        "2.0.0",
        strategy=DeploymentStrategy.CANARY,
        canary_percentage=0.1,
    )

    if success:
        print("✅ ホットスワップ成功！")

        # 新しいバージョンでテスト
        memory_v2 = await factory.create("memory", "vector", "2.0.0")
        await memory_v2.store("新しいバージョンでのテストデータ")

        results = await memory_v2.search("テスト")
        print(f"v2.0.0 検索結果: {len(results)}件")
    else:
        print("❌ ホットスワップ失敗")


async def demo_deployment_strategies():
    """デプロイメント戦略のデモ"""
    print("\n" + "=" * 60)
    print("🚀 デプロイメント戦略デモ")
    print("=" * 60)

    registry = get_registry()

    # 各種デプロイメント戦略をテスト
    strategies = [
        (DeploymentStrategy.IMMEDIATE, "即座デプロイメント"),
        (DeploymentStrategy.CANARY, "カナリアデプロイメント"),
        (DeploymentStrategy.BLUE_GREEN, "ブルーグリーンデプロイメント"),
        (DeploymentStrategy.ROLLING, "ローリングアップデート"),
    ]

    for strategy, description in strategies:
        print(f"\n📋 {description} テスト中...")

        # 一時的なコンポーネントを登録
        test_name = f"test_{strategy.value}"
        registry.register_component(
            component_type="memory",
            component_name=test_name,
            component_class=VectorMemory,
            version="1.0.0",
            deployment_strategy=strategy,
        )

        # v2.0.0を登録
        registry.register_component(
            component_type="memory",
            component_name=test_name,
            component_class=EnhancedVectorMemory,
            version="2.0.0",
            deployment_strategy=strategy,
        )

        # デプロイメント実行
        start_time = time.time()
        success = await registry.deploy_version(
            "memory", test_name, "2.0.0", strategy=strategy
        )
        deploy_time = time.time() - start_time

        status = "✅ 成功" if success else "❌ 失敗"
        print(f"   結果: {status} (実行時間: {deploy_time:.2f}秒)")


async def demo_component_builder():
    """コンポーネントビルダーのデモ"""
    print("\n" + "=" * 60)
    print("🏗️ コンポーネントビルダーデモ")
    print("=" * 60)

    # Fluent Interfaceでコンポーネントを構築
    print("\n1️⃣ ビルダーパターンでメモリコンポーネントを作成")
    memory = await (
        builder()
        .component("memory", "vector")
        .version("2.0.0")
        .config(max_items=500, similarity_threshold=0.8)
        .singleton(True)
        .build()
    )

    await memory.store("ビルダーパターンで作成されたデータ")
    results = await memory.search("ビルダー")
    print(f"ビルダー作成メモリ検索結果: {len(results)}件")

    # コンテキストマネージャーでの使用
    print("\n2️⃣ コンテキストマネージャーでの使用")
    async with component_context(
        "memory", "vector", "2.0.0", max_items=100
    ) as temp_memory:
        await temp_memory.store("一時的なデータ")
        results = await temp_memory.search("一時的")
        print(f"一時メモリ検索結果: {len(results)}件")

    print("   コンテキスト終了時に自動クリーンアップされました")


async def demo_health_monitoring():
    """ヘルスモニタリングのデモ"""
    print("\n" + "=" * 60)
    print("🏥 ヘルスモニタリングデモ")
    print("=" * 60)

    registry = get_registry()
    # レジストリ表示
    print(registry)
    factory = get_factory()

    # 複数バージョンのヘルスチェック
    versions = ["1.0.0", "2.0.0"]

    for version in versions:
        print(f"\n🔍 v{version} ヘルスチェック実行中...")

        memory = await factory.create("memory", "vector", version)
        health_result = await memory.health_check()

        status = "✅ 健全" if health_result.healthy else "❌ 異常"
        print(f"   状態: {status}")
        print(f"   メッセージ: {health_result.message}")
        print(f"   メトリクス: {health_result.metrics}")


async def demo_registry_metrics():
    """レジストリメトリクスのデモ"""
    print("\n" + "=" * 60)
    print("📊 レジストリメトリクスデモ")
    print("=" * 60)

    registry = get_registry()

    # レジストリの状態を表示
    metrics = registry.get_metrics()

    print("📈 レジストリ統計:")
    print(f"   総コンポーネント数: {metrics['total_components']}")
    print(f"   総インスタンス数: {metrics['total_instances']}")
    print(f"   アクティブヘルスチェック数: {metrics['active_health_checks']}")

    print("\n📋 登録済みコンポーネント:")
    for key, info in metrics["registrations"].items():
        print(f"   {key}:")
        print(f"     現在バージョン: {info['current_version']}")
        print(f"     総バージョン数: {info['total_versions']}")
        print(f"     デプロイメント戦略: {info['deployment_strategy']}")
        if info["canary_percentage"] > 0:
            print(f"     カナリア割合: {info['canary_percentage']:.1%}")


async def main():
    """メインデモ実行関数"""
    print("🎯 AI Blocks ホットスワップ機能デモ")
    print("=" * 80)

    try:
        # 各デモを順次実行
        await demo_basic_hotswap()
        await demo_deployment_strategies()
        await demo_component_builder()
        await demo_health_monitoring()
        await demo_registry_metrics()

        print("\n" + "=" * 80)
        print("🎉 全てのホットスワップデモが完了しました！")
        print("=" * 80)

    except Exception as e:
        print(f"\n❌ デモ実行中にエラーが発生しました: {e}")
        import traceback

        traceback.print_exc()


if __name__ == "__main__":
    asyncio.run(main())

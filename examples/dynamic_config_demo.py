"""
動的設定管理システムのデモンストレーション

このサンプルは、実行時での設定変更、環境別設定管理、設定バリデーション機能を示します。
設定の変更通知、ロールバック、監査ログ機能を含みます。
"""

import asyncio
import json
import tempfile
from pathlib import Path
from typing import Any

from ai_blocks.config.dynamic import (
    get_config_manager, config_context, ConfigSource, ConfigValidationError
)


async def demo_basic_config_operations():
    """基本的な設定操作のデモ"""
    print("\n" + "="*60)
    print("⚙️ 基本的な設定操作デモ")
    print("="*60)
    
    manager = await get_config_manager()
    
    # 現在の設定を表示
    print("\n1️⃣ 現在の設定:")
    current_settings = manager.get_current_settings()
    for key, value in list(current_settings.items())[:5]:  # 最初の5つを表示
        print(f"   {key}: {value}")
    
    # 設定を動的に変更
    print("\n2️⃣ 設定の動的変更:")
    
    # デバッグモードを有効化
    old_debug = await manager.get('debug')
    await manager.set('debug', True, reason="デモ用にデバッグモードを有効化")
    new_debug = await manager.get('debug')
    print(f"   debug: {old_debug} -> {new_debug}")
    
    # ログレベルを変更
    old_log_level = await manager.get('log_level')
    await manager.set('log_level', 'DEBUG', reason="詳細ログを有効化")
    new_log_level = await manager.get('log_level')
    print(f"   log_level: {old_log_level} -> {new_log_level}")
    
    # 数値設定を変更
    old_max_tokens = await manager.get('max_tokens')
    await manager.set('max_tokens', 2000, reason="トークン数を増加")
    new_max_tokens = await manager.get('max_tokens')
    print(f"   max_tokens: {old_max_tokens} -> {new_max_tokens}")


async def demo_validation():
    """設定バリデーションのデモ"""
    print("\n" + "="*60)
    print("✅ 設定バリデーションデモ")
    print("="*60)
    
    manager = await get_config_manager()
    
    # 正常なバリデーション
    print("\n1️⃣ 正常なバリデーション:")
    try:
        await manager.set('temperature', 0.5)
        print("   ✅ temperature = 0.5 (正常)")
    except Exception as e:
        print(f"   ❌ エラー: {e}")
    
    # バリデーションエラーのテスト
    print("\n2️⃣ バリデーションエラーのテスト:")
    
    # 範囲外の値
    try:
        await manager.set('temperature', 3.0)  # 範囲外（0.0-2.0）
        print("   ❌ 範囲外の値が受け入れられました")
    except ConfigValidationError as e:
        print(f"   ✅ 範囲外の値が正しく拒否されました: {e}")
    
    # 無効なログレベル
    try:
        await manager.set('log_level', 'INVALID')
        print("   ❌ 無効なログレベルが受け入れられました")
    except ConfigValidationError as e:
        print(f"   ✅ 無効なログレベルが正しく拒否されました: {e}")
    
    # 負の値
    try:
        await manager.set('max_tokens', -100)
        print("   ❌ 負の値が受け入れられました")
    except ConfigValidationError as e:
        print(f"   ✅ 負の値が正しく拒否されました: {e}")


async def demo_change_listeners():
    """設定変更リスナーのデモ"""
    print("\n" + "="*60)
    print("👂 設定変更リスナーデモ")
    print("="*60)
    
    manager = await get_config_manager()
    
    # 変更通知を受け取るリスナー
    change_log = []
    
    def config_change_listener(key: str, old_value: Any, new_value: Any):
        change_log.append(f"設定変更: {key} = {old_value} -> {new_value}")
        print(f"   📢 通知: {key} が {old_value} から {new_value} に変更されました")
    
    # リスナーを登録
    manager.add_change_listener(config_change_listener)
    
    print("\n1️⃣ リスナー登録後の設定変更:")
    
    # 複数の設定を変更
    await manager.set('enable_caching', False, reason="キャッシュを無効化")
    await manager.set('cache_ttl', 1800, reason="TTLを短縮")
    await manager.set('max_concurrent_requests', 20, reason="同時リクエスト数を増加")
    
    print(f"\n2️⃣ 変更ログ（{len(change_log)}件）:")
    for log_entry in change_log:
        print(f"   {log_entry}")
    
    # リスナーを削除
    manager.remove_change_listener(config_change_listener)
    print("\n3️⃣ リスナー削除後の設定変更（通知されない）:")
    await manager.set('enable_metrics', False, reason="メトリクスを無効化")


async def demo_rollback():
    """設定ロールバックのデモ"""
    print("\n" + "="*60)
    print("🔄 設定ロールバックデモ")
    print("="*60)
    
    manager = await get_config_manager()
    
    # 現在の設定を記録
    original_debug = await manager.get('debug')
    original_log_level = await manager.get('log_level')
    original_temperature = await manager.get('temperature')
    
    print(f"\n1️⃣ 元の設定:")
    print(f"   debug: {original_debug}")
    print(f"   log_level: {original_log_level}")
    print(f"   temperature: {original_temperature}")
    
    # 複数の設定を変更
    print(f"\n2️⃣ 設定を変更:")
    await manager.set('debug', True, reason="デバッグモード有効化")
    await manager.set('log_level', 'DEBUG', reason="詳細ログ有効化")
    await manager.set('temperature', 1.5, reason="温度を上げる")
    
    print(f"   debug: {await manager.get('debug')}")
    print(f"   log_level: {await manager.get('log_level')}")
    print(f"   temperature: {await manager.get('temperature')}")
    
    # 1ステップロールバック
    print(f"\n3️⃣ 1ステップロールバック:")
    await manager.rollback(1)
    print(f"   debug: {await manager.get('debug')}")
    print(f"   log_level: {await manager.get('log_level')}")
    print(f"   temperature: {await manager.get('temperature')}")
    
    # 2ステップロールバック
    print(f"\n4️⃣ 2ステップロールバック:")
    await manager.rollback(2)
    print(f"   debug: {await manager.get('debug')}")
    print(f"   log_level: {await manager.get('log_level')}")
    print(f"   temperature: {await manager.get('temperature')}")


async def demo_file_operations():
    """ファイル操作のデモ"""
    print("\n" + "="*60)
    print("📁 ファイル操作デモ")
    print("="*60)
    
    manager = await get_config_manager()
    
    # 一時ファイルを作成
    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
        temp_config = {
            "debug": True,
            "log_level": "DEBUG",
            "max_tokens": 1500,
            "temperature": 0.8
        }
        json.dump(temp_config, f, indent=2)
        temp_file_path = Path(f.name)
    
    print(f"\n1️⃣ 設定をファイルに保存:")
    try:
        await manager.save_to_file(temp_file_path)
        print(f"   ✅ 設定を保存しました: {temp_file_path}")
        
        # ファイル内容を表示
        with open(temp_file_path, 'r') as f:
            content = f.read()
        print(f"   ファイル内容（最初の200文字）:")
        print(f"   {content[:200]}...")
        
    except Exception as e:
        print(f"   ❌ 保存に失敗しました: {e}")
    finally:
        # 一時ファイルを削除
        if temp_file_path.exists():
            temp_file_path.unlink()


async def demo_context_manager():
    """コンテキストマネージャーのデモ"""
    print("\n" + "="*60)
    print("🔒 コンテキストマネージャーデモ")
    print("="*60)
    
    manager = await get_config_manager()
    
    # 元の設定を記録
    original_debug = await manager.get('debug')
    original_max_tokens = await manager.get('max_tokens')
    
    print(f"\n1️⃣ コンテキスト外の設定:")
    print(f"   debug: {original_debug}")
    print(f"   max_tokens: {original_max_tokens}")
    
    # 一時的な設定変更
    print(f"\n2️⃣ コンテキスト内での一時的な設定変更:")
    async with config_context(debug=True, max_tokens=3000):
        print(f"   debug: {await manager.get('debug')}")
        print(f"   max_tokens: {await manager.get('max_tokens')}")
        
        # コンテキスト内での処理をシミュレート
        await asyncio.sleep(0.1)
    
    # コンテキスト終了後
    print(f"\n3️⃣ コンテキスト終了後（自動復元）:")
    print(f"   debug: {await manager.get('debug')}")
    print(f"   max_tokens: {await manager.get('max_tokens')}")


async def demo_change_history():
    """変更履歴のデモ"""
    print("\n" + "="*60)
    print("📜 変更履歴デモ")
    print("="*60)
    
    manager = await get_config_manager()
    
    # いくつかの設定変更を実行
    await manager.set('debug', True, user="admin", reason="デバッグセッション開始")
    await manager.set('log_level', 'DEBUG', user="admin", reason="詳細ログ有効化")
    await manager.set('temperature', 0.9, user="user1", reason="創造性を向上")
    await manager.set('max_tokens', 2500, user="user2", reason="長い応答を許可")
    
    # 変更履歴を取得
    history = manager.get_change_history(limit=10)
    
    print(f"\n📋 最近の変更履歴（{len(history)}件）:")
    for i, change in enumerate(history[-5:], 1):  # 最新5件を表示
        print(f"   {i}. {change.timestamp.strftime('%H:%M:%S')} - {change.key}")
        print(f"      {change.old_value} -> {change.new_value}")
        print(f"      ユーザー: {change.user or 'システム'}, 理由: {change.reason or 'なし'}")
        print(f"      ソース: {change.source}")
        print()


async def main():
    """メインデモ実行関数"""
    print("🎯 AI Blocks 動的設定管理システムデモ")
    print("=" * 80)
    
    try:
        # 各デモを順次実行
        await demo_basic_config_operations()
        await demo_validation()
        await demo_change_listeners()
        await demo_rollback()
        await demo_file_operations()
        await demo_context_manager()
        await demo_change_history()
        
        print("\n" + "="*80)
        print("🎉 全ての動的設定管理デモが完了しました！")
        print("="*80)
        
    except Exception as e:
        print(f"\n❌ デモ実行中にエラーが発生しました: {e}")
        import traceback
        traceback.print_exc()
    
    finally:
        # クリーンアップ
        manager = await get_config_manager()
        await manager.cleanup()


if __name__ == "__main__":
    asyncio.run(main())

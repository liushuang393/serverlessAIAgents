#!/usr/bin/env python3
"""
ユーティリティテスト実行スクリプト

このスクリプトは、作成したユーティリティファイルのテストを実行します。
"""

import unittest
import sys
import os
from io import StringIO

# テストモジュールのパスを追加
sys.path.insert(0, os.path.dirname(__file__))

def run_all_tests():
    """すべてのテストを実行"""
    print("=" * 60)
    print("ユーティリティテスト実行開始")
    print("=" * 60)
    
    # テストスイートを作成
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # テストモジュールを追加
    test_modules = [
        'test_embedding_utils',
        'test_text_utils',
        'test_vector_utils',
        'test_websearch_utils',
        'test_audio_utils',
        'test_viz_utils'
    ]
    
    total_tests = 0
    total_failures = 0
    total_errors = 0
    
    for module_name in test_modules:
        print(f"\n{'-' * 40}")
        print(f"テストモジュール: {module_name}")
        print(f"{'-' * 40}")
        
        try:
            # モジュールをインポート
            module = __import__(module_name)
            
            # テストスイートを作成
            module_suite = loader.loadTestsFromModule(module)
            
            # テストを実行
            stream = StringIO()
            runner = unittest.TextTestRunner(
                stream=stream,
                verbosity=2,
                buffer=True
            )
            result = runner.run(module_suite)
            
            # 結果を表示
            output = stream.getvalue()
            print(output)
            
            # 統計を更新
            total_tests += result.testsRun
            total_failures += len(result.failures)
            total_errors += len(result.errors)
            
            # モジュール別サマリー
            print(f"\n{module_name} 結果:")
            print(f"  実行テスト数: {result.testsRun}")
            print(f"  失敗: {len(result.failures)}")
            print(f"  エラー: {len(result.errors)}")
            
            if result.failures:
                print(f"  失敗詳細:")
                for test, traceback in result.failures:
                    print(f"    - {test}: {traceback.split('AssertionError:')[-1].strip()}")
            
            if result.errors:
                print(f"  エラー詳細:")
                for test, traceback in result.errors:
                    print(f"    - {test}: {traceback.split('Exception:')[-1].strip()}")
                    
        except ImportError as e:
            print(f"モジュール {module_name} のインポートに失敗しました: {e}")
            total_errors += 1
        except Exception as e:
            print(f"モジュール {module_name} の実行中にエラーが発生しました: {e}")
            total_errors += 1
    
    # 全体サマリー
    print("\n" + "=" * 60)
    print("テスト実行結果サマリー")
    print("=" * 60)
    print(f"総実行テスト数: {total_tests}")
    print(f"総失敗数: {total_failures}")
    print(f"総エラー数: {total_errors}")
    
    success_rate = ((total_tests - total_failures - total_errors) / total_tests * 100) if total_tests > 0 else 0
    print(f"成功率: {success_rate:.1f}%")
    
    if total_failures == 0 and total_errors == 0:
        print("\n🎉 すべてのテストが成功しました！")
        return True
    else:
        print(f"\n⚠️  {total_failures + total_errors} 件の問題が発見されました。")
        return False


def run_specific_test(test_module):
    """特定のテストモジュールを実行"""
    print(f"テストモジュール '{test_module}' を実行中...")
    
    try:
        # モジュールをインポート
        module = __import__(test_module)
        
        # テストスイートを作成
        loader = unittest.TestLoader()
        suite = loader.loadTestsFromModule(module)
        
        # テストを実行
        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)
        
        return result.wasSuccessful()
        
    except ImportError as e:
        print(f"モジュール {test_module} のインポートに失敗しました: {e}")
        return False
    except Exception as e:
        print(f"テスト実行中にエラーが発生しました: {e}")
        return False


def check_dependencies():
    """依存関係をチェック"""
    print("依存関係をチェック中...")
    
    required_modules = [
        'unittest',
        'unittest.mock',
        'numpy'
    ]
    
    missing_modules = []
    
    for module_name in required_modules:
        try:
            __import__(module_name)
            print(f"✓ {module_name}")
        except ImportError:
            print(f"✗ {module_name} (不足)")
            missing_modules.append(module_name)
    
    if missing_modules:
        print(f"\n警告: 以下のモジュールが不足しています: {', '.join(missing_modules)}")
        print("一部のテストが失敗する可能性があります。")
        return False
    else:
        print("\n✓ すべての依存関係が満たされています。")
        return True


def main():
    """メイン関数"""
    if len(sys.argv) > 1:
        # 特定のテストモジュールを実行
        test_module = sys.argv[1]
        if not test_module.startswith('test_'):
            test_module = f'test_{test_module}'
        
        success = run_specific_test(test_module)
        sys.exit(0 if success else 1)
    else:
        # 依存関係チェック
        check_dependencies()
        
        # すべてのテストを実行
        success = run_all_tests()
        sys.exit(0 if success else 1)


if __name__ == '__main__':
    main()

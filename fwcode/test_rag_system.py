#!/usr/bin/env python3
"""
RAGシステムのテストスクリプト
"""

import asyncio
import os
import sys
from pathlib import Path


# テスト用のサンプルテキストファイルを作成
def create_sample_documents() -> Path:
    """テスト用のサンプルドキュメントを作成"""
    documents_dir = Path("test_documents")
    documents_dir.mkdir(exist_ok=True)

    # サンプルテキストファイル1
    sample1_content = """
    人工知能（AI）技術の発展について

    人工知能技術は近年急速に発展しており、様々な分野で活用されています。
    機械学習、深層学習、自然言語処理などの技術が中核となっています。

    主な応用分野：
    1. 画像認識と画像処理
    2. 自然言語処理と翻訳
    3. 音声認識と音声合成
    4. 自動運転技術
    5. 医療診断支援

    今後の展望：
    - より高度な推論能力の実現
    - 人間とAIの協調作業の拡大
    - 倫理的なAI開発の重要性
    """

    with open(documents_dir / "ai_report.txt", "w", encoding="utf-8") as f:
        f.write(sample1_content)

    # サンプルテキストファイル2
    sample2_content = """
    市場分析レポート

    2024年のテクノロジー市場は以下の特徴を示しています：

    成長分野：
    - クラウドコンピューティング：前年比25%成長
    - AI・機械学習：前年比40%成長
    - IoTデバイス：前年比15%成長

    市場動向：
    1. デジタルトランスフォーメーションの加速
    2. リモートワーク技術の普及
    3. サイバーセキュリティ需要の増加

    予測：
    - 2025年にはAI市場が2倍に拡大
    - クラウドファーストの企業が80%に到達
    - エッジコンピューティングの本格普及
    """

    with open(documents_dir / "market_analysis.txt", "w", encoding="utf-8") as f:
        f.write(sample2_content)

    print(f"✅ サンプルドキュメントを作成しました: {documents_dir}")
    return documents_dir


async def test_server_import() -> bool:
    """サーバーモジュールのインポートテスト"""
    try:
        print("=== サーバーモジュールインポートテスト ===")

        # パスを追加
        sys.path.insert(0, str(Path(__file__).parent))

        # サーバーモジュールをインポート
        import mcp_rag_server

        print("✅ mcp_rag_server モジュールのインポートに成功")

        # アプリケーションの作成テスト
        app = mcp_rag_server.create_app()
        print("✅ FastAPIアプリケーションの作成に成功")

        return True

    except ImportError as e:
        print(f"❌ インポートエラー: {e}")
        print("必要な依存関係がインストールされていない可能性があります")
        return False
    except Exception as e:
        print(f"❌ 予期しないエラー: {e}")
        return False


async def test_client_import() -> bool:
    """クライアントモジュールのインポートテスト"""
    try:
        print("\n=== クライアントモジュールインポートテスト ===")

        # クライアントモジュールをインポート
        import mcp_rag_client

        print("✅ mcp_rag_client モジュールのインポートに成功")

        # クライアントの作成テスト
        client = mcp_rag_client.RAGClient()
        print("✅ RAGClientの作成に成功")

        await client.close()
        return True

    except ImportError as e:
        print(f"❌ インポートエラー: {e}")
        return False
    except Exception as e:
        print(f"❌ 予期しないエラー: {e}")
        return False


def test_config_files() -> bool:
    """設定ファイルの存在と形式をテスト"""
    print("\n=== 設定ファイルテスト ===")

    config_files = ["mcp_config.json", "doc_config.json"]

    all_valid = True

    for config_file in config_files:
        config_path = Path(config_file)
        if config_path.exists():
            try:
                import json

                with open(config_path, "r", encoding="utf-8") as f:
                    config = json.load(f)
                print(f"✅ {config_file} は有効なJSONファイルです")
            except json.JSONDecodeError as e:
                print(f"❌ {config_file} のJSON形式が無効です: {e}")
                all_valid = False
        else:
            print(f"❌ {config_file} が見つかりません")
            all_valid = False

    return all_valid


def test_docker_files() -> bool:
    """Dockerファイルの存在をテスト"""
    print("\n=== Dockerファイルテスト ===")

    docker_files = ["Dockerfile", "docker-compose.yml", "requirements.txt"]

    all_exist = True

    for docker_file in docker_files:
        docker_path = Path(docker_file)
        if docker_path.exists():
            print(f"✅ {docker_file} が存在します")
        else:
            print(f"❌ {docker_file} が見つかりません")
            all_exist = False

    return all_exist


async def main() -> bool:
    """メインテスト関数"""
    print("🚀 RAGシステムテストを開始します\n")

    # テスト結果を記録
    test_results = []

    # サンプルドキュメントを作成
    create_sample_documents()

    # 各テストを実行
    test_results.append(("設定ファイル", test_config_files()))
    test_results.append(("Dockerファイル", test_docker_files()))
    test_results.append(("サーバーインポート", await test_server_import()))
    test_results.append(("クライアントインポート", await test_client_import()))

    # 結果をまとめて表示
    print("\n" + "=" * 50)
    print("📊 テスト結果サマリー")
    print("=" * 50)

    passed = 0
    total = len(test_results)

    for test_name, result in test_results:
        status = "✅ PASS" if result else "❌ FAIL"
        print(f"{test_name:<20}: {status}")
        if result:
            passed += 1

    print(f"\n合計: {passed}/{total} テストが成功")

    if passed == total:
        print("🎉 すべてのテストが成功しました！")
        print("\n次のステップ:")
        print("1. 'python mcp_rag_server.py' でサーバーを起動")
        print("2. 別のターミナルで 'python mcp_rag_client.py' でクライアントをテスト")
        print("3. または 'docker-compose up' でDockerコンテナとして起動")
    else:
        print("⚠️  一部のテストが失敗しました。上記のエラーを確認してください。")

    return passed == total


if __name__ == "__main__":
    success = asyncio.run(main())
    sys.exit(0 if success else 1)

"""
フロントエンド移行システムテストスイート

このモジュールは、フロントエンド移行システムの包括的なテストスイートを提供します。
単体テスト、統合テスト、E2Eテストを含み、品質保証を確実にします。

テスト構成:
- unit/: 各エージェントとツールの単体テスト
- integration/: エージェント間連携の統合テスト
- e2e/: エンドツーエンドの移行プロセステスト
"""

import asyncio
from pathlib import Path

import pytest

# テスト用の共通設定
TEST_DATA_DIR = Path(__file__).parent / "test_data"
TEST_OUTPUT_DIR = Path(__file__).parent / "test_output"


# テスト用のサンプルデータを確保
def ensure_test_data():
    """テスト用データディレクトリを作成"""
    TEST_DATA_DIR.mkdir(exist_ok=True)
    TEST_OUTPUT_DIR.mkdir(exist_ok=True)


# pytest設定
def pytest_configure(config):
    """pytest設定の初期化"""
    ensure_test_data()


# 非同期テスト用のイベントループ設定
@pytest.fixture(scope="session")
def event_loop():
    """セッション全体で使用するイベントループ"""
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()


__all__ = [
    "TEST_DATA_DIR",
    "TEST_OUTPUT_DIR",
    "ensure_test_data",
]

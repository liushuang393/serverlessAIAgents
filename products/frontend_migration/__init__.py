"""
フロントエンド移行システム

このモジュールは、レガシーフロントエンドコードを現代的なレスポンシブデザインに
自動変換するためのAIエージェントシステムを提供します。

主要コンポーネント:
- agents: 各種変換エージェント
- tools: 専用変換ツール
- workflows: ワークフロー定義
- config: 設定管理

使用例:
    from products.frontend_migration import MigrationOrchestrator

    orchestrator = MigrationOrchestrator()
    result = await orchestrator.migrate_project("/path/to/legacy/project")
"""

__version__ = "0.1.0"
__author__ = "AI Blocks Team"

from .workflows.migration_workflow import MigrationOrchestrator

__all__ = [
    "MigrationOrchestrator",
    "__version__",
    "__author__",
]

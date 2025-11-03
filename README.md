# AgentFlow

<div align="center">

**軽量 AI エージェント開発フレームワーク**

*PocketFlow ベースの統一プロトコルインターフェース*

[![Python 3.13+](https://img.shields.io/badge/python-3.13+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-208%20passed-brightgreen.svg)](tests/)
[![Coverage](https://img.shields.io/badge/coverage-90.28%25-brightgreen.svg)](htmlcov/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Code style: ruff](https://img.shields.io/badge/code%20style-ruff-000000.svg)](https://github.com/astral-sh/ruff)

[Documentation](https://github.com/liushuang393/serverlessAIAgents/tree/main/docs) | [Examples](https://github.com/liushuang393/serverlessAIAgents/tree/main/examples) | [Contributing](CONTRIBUTING.md)

</div>

---

## � AgentFlow とは

AgentFlow は、**MCP（Model Context Protocol）**、**A2A（Agent-to-Agent）**、**AG-UI（Agent-UI）** の 3 つのオープンプロトコルを統一インターフェースで提供する軽量 AI エージェント開発フレームワークです。

### ✨ 主な特徴

| 特徴 | 説明 | 利点 |
|------|------|------|
| 🚀 **軽量設計** | コアコード ~500 行 | 高速起動、低メモリ使用量 |
| 🔌 **3 プロトコル統合** | MCP / A2A / AG-UI | 1 つのコードで複数のプロトコルに対応 |
| 🎨 **自動アダプター** | `@auto_adapt` デコレーター | プロトコル変換を自動化 |
| 📦 **CLI ツール** | 包括的なコマンドラインツール | プロジェクト管理を簡素化 |
| 🏪 **マーケットプレイス** | エージェント共有プラットフォーム | 再利用可能なエージェントを検索・インストール |
| 🧪 **高品質** | 208 テスト、90.28% カバレッジ | 本番環境対応の信頼性 |
| 🔒 **型安全** | 100% 型アノテーション、mypy strict | 開発時エラーを早期発見 |
| ⚡ **非同期ファースト** | 完全非同期 I/O | 高スループット処理 |

### 🎁 AgentFlow の利点

- **学習コストが低い**: シンプルな API、豊富なサンプル、包括的なドキュメント
- **プロトコル非依存**: 1 つのエージェントで複数のプロトコルに対応
- **拡張性**: モジュラー設計で簡単にカスタマイズ可能
- **本番環境対応**: 高いテストカバレッジと型安全性
- **アクティブな開発**: 継続的な改善とコミュニティサポート

## 📦 インストール

### PyPI からインストール

```bash
pip install agentflow
```

### ソースからインストール

```bash
git clone https://github.com/liushuang393/serverlessAIAgents.git
cd serverlessAIAgents
pip install -e ".[dev]"
```

### 動作確認

```bash
agentflow --version
# agentflow, version 1.0.0
```

---

## 🚀 クイックスタート

### 5 分でエージェントを作成

```bash
# 1. プロジェクト作成
agentflow init my-agent && cd my-agent

# 2. エージェント実装（agent.py を編集）
cat > agent.py << 'EOF'
from agentflow.core.agent_block import AgentBlock
from typing import Any

class MyAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        return {"result": input_data.get("text", "").upper()}
EOF

# 3. 実行
agentflow run . --input '{"text": "hello"}'
# Output: {"result": "HELLO"}
```

詳細は [クイックスタートガイド](docs/quickstart.md) を参照してください。

---

## 🎯 機能一覧

### コア機能

| 機能 | 説明 | ドキュメント |
|------|------|-------------|
| **AgentBlock** | エージェント基底クラス | [API リファレンス](docs/api.md#agentblock) |
| **@auto_adapt** | プロトコル自動変換デコレーター | [API リファレンス](docs/api.md#auto-adapt) |
| **AgentFlowEngine** | PocketFlow ベースのワークフローエンジン | [API リファレンス](docs/api.md#engine) |
| **CLI** | コマンドラインツール | [CLI リファレンス](docs/cli.md) |
| **Marketplace** | エージェント共有プラットフォーム | [API リファレンス](docs/api.md#marketplace) |
| **Template System** | プロジェクトテンプレート | [テンプレートガイド](docs/templates.md) |

### プロトコルサポート

| プロトコル | 説明 | Python バージョン | ドキュメント |
|-----------|------|------------------|-------------|
| **MCP** | Model Context Protocol（ツール接続） | 3.10+ | [プロトコルガイド](docs/protocols.md#mcp) |
| **A2A** | Agent-to-Agent（エージェント協調） | 3.9+ | [プロトコルガイド](docs/protocols.md#a2a) |
| **AG-UI** | Agent-UI（フロントエンド連携） | 3.13+ | [プロトコルガイド](docs/protocols.md#ag-ui) |

### CLI コマンド

```bash
agentflow init <project>        # プロジェクト初期化
agentflow create agent <name>   # エージェント作成
agentflow run <path>            # エージェント実行
agentflow search [query]        # マーケットプレイス検索
agentflow install <agent-id>    # エージェントインストール
agentflow template list         # テンプレート一覧
```

詳細は [CLI リファレンス](docs/cli.md) を参照してください。

---

## 📚 ドキュメント

### 入門ガイド

- [クイックスタート](docs/quickstart.md) - 10 分で最初のエージェントを作成
- [実装ガイド](docs/implementation-guide.md) - 各層の実装方法とベストプラクティス
- [サンプル集](examples/) - 5 つの実用的なエージェント例

### リファレンス

- [API リファレンス](docs/api.md) - 完全な API ドキュメント
- [プロトコルガイド](docs/protocols.md) - MCP/A2A/AG-UI の詳細
- [CLI リファレンス](docs/cli.md) - すべてのコマンドの説明
- [アーキテクチャ](docs/architecture.md) - システム設計と設計哲学

### 開発者向け

- [開発ガイド](docs/development.md) - 開発環境のセットアップと貢献方法
- [貢献ガイドライン](CONTRIBUTING.md) - コーディング規約とプルリクエストプロセス
- [変更履歴](CHANGELOG.md) - バージョン履歴と変更内容

---

## 🏗️ アーキテクチャ

AgentFlow は 4 層のモジュラーアーキテクチャを採用：

```
┌──────────────────────────────────────────┐
│  UI Layer (Optional)                     │  ← Visual Studio (React)
├──────────────────────────────────────────┤
│  Protocol Layer                          │  ← MCP / A2A / AG-UI
├──────────────────────────────────────────┤
│  Engine Layer                            │  ← AgentFlowEngine (PocketFlow)
├──────────────────────────────────────────┤
│  Tool Layer                              │  ← LLM / Database / External APIs
└──────────────────────────────────────────┘
```

詳細は [アーキテクチャドキュメント](docs/architecture.md) を参照してください。

---

## 🤝 貢献

AgentFlow への貢献を歓迎します！

### 貢献方法

1. **リポジトリをフォーク**
   ```bash
   # GitHub で Fork ボタンをクリック
   ```

2. **開発環境をセットアップ**
   ```bash
   git clone https://github.com/YOUR_USERNAME/serverlessAIAgents.git
   cd serverlessAIAgents
   python -m venv .venv
   source .venv/bin/activate  # Windows: .venv\Scripts\activate
   pip install -e ".[dev]"
   ```

3. **ブランチを作成**
   ```bash
   git checkout -b feature/your-feature-name
   ```

4. **変更を加える**
   - コーディング規約に従う
   - テストを追加
   - ドキュメントを更新

5. **品質チェック**
   ```bash
   ruff format .           # フォーマット
   ruff check .            # リント
   mypy agentflow          # 型チェック
   pytest tests/ --cov     # テスト
   ```

6. **プルリクエストを作成**
   ```bash
   git push origin feature/your-feature-name
   # GitHub で Pull Request を作成
   ```

詳細は [開発ガイド](docs/development.md) と [貢献ガイドライン](CONTRIBUTING.md) を参照してください。

### 行動規範

すべての貢献者は [行動規範](CONTRIBUTING.md#行動規範) に従うことが求められます。

---

## 📄 ライセンス

AgentFlow は [MIT License](LICENSE) の下で公開されています。

```
MIT License

Copyright (c) 2025 AgentFlow Team

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction...
```

---

## 🙏 謝辞

AgentFlow は以下のオープンソースプロジェクトとコミュニティに支えられています：

### コアライブラリ

- **[PocketFlow](https://github.com/pocketflow/pocketflow)** - 軽量ワークフローエンジンの基盤
- **[Pydantic](https://github.com/pydantic/pydantic)** - データ検証と設定管理
- **[Click](https://github.com/pallets/click)** - CLI フレームワーク
- **[Rich](https://github.com/Textualize/rich)** - 美しいターミナル出力
- **[FastAPI](https://github.com/tiangolo/fastapi)** - 高性能 Web フレームワーク
- **[Ruff](https://github.com/astral-sh/ruff)** - 高速 Python リンター・フォーマッター

### プロトコル

- **[MCP (Model Context Protocol)](https://modelcontextprotocol.io/)** - Anthropic による LLM ツール接続プロトコル
- **[A2A (Agent-to-Agent Protocol)](https://a2a.dev/)** - エージェント間通信の標準プロトコル
- **[AG-UI](https://github.com/ag-ui/ag-ui)** - エージェント UI 統合プロトコル

### 開発ツール

- **[pytest](https://github.com/pytest-dev/pytest)** - テストフレームワーク
- **[mypy](https://github.com/python/mypy)** - 静的型チェッカー
- **[pre-commit](https://github.com/pre-commit/pre-commit)** - Git フックフレームワーク

### コミュニティ

- **Python コミュニティ** - 素晴らしい言語とエコシステム
- **オープンソースコントリビューター** - すべての貢献者に感謝

---

## 📞 サポート

### コミュニティ

- 💬 **Discussions**: [GitHub Discussions](https://github.com/liushuang393/serverlessAIAgents/discussions) - 質問、アイデア、フィードバック
- 🐛 **Issues**: [GitHub Issues](https://github.com/liushuang393/serverlessAIAgents/issues) - バグ報告、機能リクエスト
- 📖 **Documentation**: [docs/](https://github.com/liushuang393/serverlessAIAgents/tree/main/docs) - 包括的なドキュメント

### 連絡先

- 📧 **Email**: 115070984+liushuang393@users.noreply.github.com
- � **GitHub**: [@liushuang393](https://github.com/liushuang393)
- � **Repository**: [serverlessAIAgents](https://github.com/liushuang393/serverlessAIAgents)

---

## 🌟 スター履歴

[![Star History Chart](https://api.star-history.com/svg?repos=liushuang393/serverlessAIAgents&type=Date)](https://star-history.com/#liushuang393/serverlessAIAgents&Date)

---

<div align="center">

**AgentFlow で AI エージェント開発を加速しましょう！**

[始める](docs/quickstart.md) | [ドキュメント](docs/) | [サンプル](examples/) | [貢献](CONTRIBUTING.md)

Made with ❤️ by the AgentFlow Team

</div>

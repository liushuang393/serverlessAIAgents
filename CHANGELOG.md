# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-11-03

### Added

#### Phase 1: Core Framework & Protocol Integration
- **AgentFlow Engine**: PocketFlow ベースの軽量ワークフローエンジン
- **MCP Client**: Model Context Protocol クライアント実装
- **A2A Server/Client**: Agent-to-Agent プロトコル実装
- **AG-UI Event Emitter**: フロントエンド統合用イベントエミッター
- **Agent Metadata Schema**: エージェントメタデータの YAML スキーマ定義
- **Type Safety**: 100% 型カバレッジ、mypy strict モード対応
- **Test Coverage**: 90.28% カバレッジ、208 テスト

#### Phase 2: CLI & Marketplace
- **CLI Framework**: Click ベースのコマンドラインインターフェース
- **Init Command**: プロジェクト初期化コマンド (`agentflow init`)
- **Create Commands**: エージェント作成コマンド (`agentflow create`)
- **Marketplace Client**: エージェントマーケットプレイス統合
- **Search & Install**: エージェント検索とインストール機能
- **Run & Test Commands**: エージェント実行とテストコマンド

#### Phase 3: Auto-Adapter & Integration
- **Protocol Adapter Generator**: MCP/A2A/AG-UI アダプター自動生成
- **@auto_adapt Decorator**: プロトコルメソッド自動注入デコレーター
- **AgentBlock Base Class**: エージェント基底クラス
- **Lifecycle Management**: initialize/run/cleanup ライフサイクル管理
- **Context Manager**: コンテキストマネージャーサポート
- **End-to-End Tests**: 統合テスト 13 件

#### Phase 4: Visual Studio (Optional)
- **Studio Backend API**: FastAPI ベースの REST API
- **Agent Management API**: エージェント管理エンドポイント
- **Marketplace API**: マーケットプレイス統合 API
- **Workflow API**: ワークフロー管理 API
- **WebSocket Support**: リアルタイム通信サポート
- **Studio Frontend**: React 18 + TypeScript フロントエンド
- **Visual Canvas**: React Flow ベースのワークフローキャンバス
- **Drag & Drop**: ドラッグ&ドロップ UI
- **State Management**: Zustand 状態管理
- **Undo/Redo**: 操作の取り消し/やり直し機能

#### Phase 5: Templates & Documentation
- **Template System**: Jinja2 ベースのテンプレートシステム
- **Template Manager**: テンプレート管理クラス
- **Scenario Templates**: 3 つのシナリオテンプレート
  - Invoice Processor: PDF 請求書処理エージェント
  - Chatbot: 対話型チャットボットエージェント
  - Data Pipeline: データ処理パイプラインエージェント
- **Template CLI**: テンプレート管理コマンド
- **Documentation**: 包括的なドキュメント
  - Quick Start Guide (クイックスタートガイド)
  - API Reference (API リファレンス)
  - Protocol Guide (プロトコルガイド)
  - CLI Reference (CLI リファレンス)
  - Architecture Documentation (アーキテクチャドキュメント)
  - Contributing Guide (貢献ガイド)
- **Example Agents**: 5 つのサンプルエージェント
  - Text Processor Agent: テキスト処理エージェント
  - Sample Agent: 基本的なサンプルエージェント
  - Weather Agent: 天気情報取得エージェント
  - Translator Agent: 翻訳エージェント
  - Calculator Agent: 計算機エージェント

### Technical Details

#### Dependencies
- **Python**: 3.13+ (LTS until Oct 2029)
- **Core**: pydantic>=2.0, pyyaml>=6.0, httpx>=0.27.0
- **CLI**: click>=8.1.0, rich>=13.0.0
- **Template**: jinja2>=3.1.0
- **Studio**: fastapi>=0.115.0, uvicorn>=0.32.0, websockets>=13.0
- **Dev**: pytest>=8.0.0, pytest-cov>=5.0.0, pytest-asyncio>=0.24.0, ruff>=0.7.0, mypy>=1.13.0

#### Code Quality
- **Linter**: Ruff (unified linter and formatter)
- **Type Checker**: mypy (strict mode)
- **Test Framework**: pytest + pytest-asyncio
- **Coverage**: 90.28% (208 tests, all passing)
- **Code Style**: Google-style docstrings, 100% type annotations

#### Architecture
- **4-Layer Architecture**:
  1. UI Layer (Optional) - Visual Studio (React)
  2. Protocol Layer - MCP, A2A, AG-UI
  3. Engine Layer - AgentFlowEngine (PocketFlow)
  4. Tool Layer - LLM, Database, External API Tools
- **Design Principles**:
  - Lightweight (~500 lines core code)
  - Modular (pluggable protocols)
  - Type-safe (100% type coverage)
  - Async-first (all I/O operations)
  - Developer-friendly (CLI, templates, docs)

### Breaking Changes
- None (initial release)

### Deprecated
- None (initial release)

### Removed
- None (initial release)

### Fixed
- None (initial release)

### Security
- Safe expression evaluation using AST (Calculator Agent)
- No use of `eval()` or `exec()`
- Input validation using Pydantic
- Type safety with mypy strict mode

## [Unreleased]

### Planned
- Additional protocol integrations
- More scenario templates
- Performance optimizations
- Enhanced error messages
- Internationalization (i18n)

---

[1.0.0]: https://github.com/liushuang393/serverlessAIAgents/releases/tag/v1.0.0


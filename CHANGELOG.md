# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0] - 2026-01-03

### Added

#### 🧠 Knowledge Base Integration

- **RAG Pipeline**: 完整的检索增强生成（RAG）管道实现
  - `RAGPipeline`: 统一的 RAG 接口，支持文档索引和语义查询
  - `RAGConfig`: 可配置的 RAG 参数（top_k、相似度阈值、提示模板）
  - 支持流式响应

- **Document Loaders**: 多格式文档加载器
  - `TextLoader`: 纯文本文件
  - `MarkdownLoader`: Markdown 文件（按章节分割）
  - `PDFLoader`: PDF 文件（支持 pdfplumber/pypdf）
  - `CSVLoader`: CSV 文件（可指定内容列和元数据列）
  - `JSONLoader`: JSON/JSONL 文件
  - `HTMLLoader`: HTML 文件（自动清理标签）
  - `UniversalLoader`: 自动检测格式的统一加载器

- **Vector Search Hooks**: React Hooks 风格的向量搜索 API
  - `use_vector_search()`: 向量相似度搜索
  - `use_rag()`: RAG 查询接口

#### 📊 Observability

- **Structured Logging**: JSON 格式的结构化日志
  - `AgentFlowLogger`: 带上下文的日志记录器
  - `JSONFormatter`: JSON 格式输出
  - 敏感信息自动掩码

- **Metrics Collection**: Prometheus 兼容的指标收集
  - `Counter`: 单调递增计数器
  - `Gauge`: 可增减的量表
  - `Histogram`: 分布统计
  - `MetricsCollector`: 指标管理器

- **Distributed Tracing**: 分布式追踪
  - `Tracer`: 追踪器（支持 span 嵌套）
  - `Span`: 追踪单元（支持属性和事件）
  - 装饰器支持：`@tracer.trace()`

- **Sentry Integration**: 错误追踪集成
  - `setup_sentry()`: Sentry 初始化
  - `capture_exception()`: 异常捕获
  - 性能监控支持

#### 🔐 Security Layer

- **API Key Management**: API 密钥管理
  - `APIKeyManager`: 密钥的创建、验证、吊销
  - `generate_api_key()`: 安全的密钥生成
  - 基于范围（scope）的访问控制
  - 密钥哈希存储

- **Rate Limiting**: 请求速率限制
  - `RateLimiter`: 滑动窗口限流器
  - 支持分钟/小时/天级别限制
  - `RateLimitExceeded` 异常

- **Authentication Middleware**: 认证中间件
  - `AuthMiddleware`: JWT 和 API Key 认证
  - `JWTConfig`: JWT 配置
  - `@require_auth`: 认证装饰器
  - `@require_permission`: 权限装饰器

- **RBAC**: 基于角色的访问控制
  - `RBACManager`: 角色管理器
  - `Role`: 角色定义（支持权限继承）
  - `Permission`: 权限定义（支持通配符）

#### 🧪 Testing Tools

- **Mock LLM Provider**: 测试用 LLM 模拟
  - `MockLLMProvider`: 可配置响应的模拟 LLM
  - 模式匹配响应
  - 序列响应
  - 调用记录追踪

- **Agent Test Framework**: Agent 测试框架
  - `AgentTestCase`: 测试用例基类
  - `AgentTestRunner`: 测试运行器
  - 自动 Mock 注入
  - 断言辅助方法

- **Test Fixtures**: Pytest 夹具
  - `mock_llm_fixture`: Mock LLM 夹具
  - `agent_fixture`: Agent 夹具
  - `clean_env_fixture`: 清洁环境夹具

#### 📦 Deployment Tools

- **Docker Templates**: Docker 部署模板
  - `Dockerfile`: 多阶段构建，安全最佳实践
  - `docker-compose.yml`: 包含 Redis、PostgreSQL
  - `.dockerignore`: 优化构建

- **Serverless Deployment**: 无服务器部署
  - `vercel.json`: Vercel 配置
  - `serverless.yml`: AWS Lambda 配置
  - `handler.py`: Lambda 入口模板

- **CI/CD Templates**: CI/CD 模板
  - `.github/workflows/ci-cd.yml`: GitHub Actions
  - `.gitlab-ci.yml`: GitLab CI
  - `.pre-commit-config.yaml`: 预提交钩子

- **Environment Templates**: 环境配置模板
  - `.env.example`: 环境变量模板

### Usage Examples

```python
# Knowledge Base / RAG
from agentflow.knowledge import RAGPipeline, use_vector_search

async with RAGPipeline() as rag:
    await rag.add_documents("./docs/")
    result = await rag.query("What is AgentFlow?")
    print(result.answer)

# Observability
from agentflow.observability import setup_observability, get_tracer

setup_observability(service_name="my-agent", sentry_dsn="...")
tracer = get_tracer()

with tracer.span("process-request"):
    # your code

# Security
from agentflow.security import APIKeyManager, RateLimiter

api_keys = APIKeyManager()
key, api_key = api_keys.create_key("my-key", scopes=["read"])

limiter = RateLimiter(requests_per_minute=60)
if await limiter.check(user_id):
    # process request

# Testing
from agentflow.testing import MockLLMProvider

mock = MockLLMProvider()
mock.set_response("Test response")
mock.add_pattern_response(r"hello", "Hello!")

# Deploy
from agentflow.deploy import generate_all

generate_all("./deploy", app_name="my-agent", docker=True, github_actions=True)
```

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

## [1.1.0] - 2025-12-30

### Changed

#### ドキュメント整理

- **docs/** ディレクトリを大幅に整理
  - `architecture.md` を更新（新アーキテクチャ図、コアコンセプト追加）
  - 重複ファイルを削除（DEVELOPMENT_STANDARDS の中英版など）
  - `design/` ディレクトリを削除（Phase1-4 設計書）
  - `deployment/` ディレクトリを削除（CI/CD、本番チェックリストなど）
  - `memory/` ディレクトリを整理（MEMORY_SYSTEM_DESIGN.md のみ保持）

#### 保持されたドキュメント

- `architecture.md` - アーキテクチャ設計書（更新済み）
- `protocols.md` - プロトコル詳細
- `api.md` - API リファレンス
- `cli.md` - CLI リファレンス
- `quickstart.md` - クイックスタート
- `getting-started-ja.md` - 入門ガイド
- `DEVELOPMENT_STANDARDS_JA.md` - 開発規範（日本語版のみ）
- `memory/MEMORY_SYSTEM_DESIGN.md` - 記憶システム設計
- `examples/` - サンプルコード

### Removed

- `docs/DEVELOPMENT_STANDARDS.md` - 中文版（日本語版に統一）
- `docs/DEVELOPMENT_STANDARDS_EN.md` - 英語版（日本語版に統一）
- `docs/ERROR_HANDLING_BEST_PRACTICES.md`
- `docs/development.md`
- `docs/implementation-guide.md`
- `docs/quality-checks.md`
- `docs/security-hardening.md`
- `docs/templates.md`
- `docs/design/` ディレクトリ全体
- `docs/deployment/` ディレクトリ全体
- `docs/memory/PRODUCTION_DEPLOYMENT.md`
- `docs/memory/USAGE_EXAMPLES.md`

---

## [Unreleased]

### Planned

- Additional protocol integrations
- More scenario templates
- Performance optimizations
- Enhanced error messages
- Internationalization (i18n)

---

[1.1.0]: https://github.com/liushuang393/serverlessAIAgents/releases/tag/v1.1.0
[1.0.0]: https://github.com/liushuang393/serverlessAIAgents/releases/tag/v1.0.0

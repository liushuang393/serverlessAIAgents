# AgentFlow ドキュメント目次

> **AgentFlow** - 軽量 AI エージェント開発フレームワーク

---

## 🚀 クイックスタート

| ドキュメント | 説明 |
|-------------|------|
| **[QUICKSTART.md](./QUICKSTART.md)** | ⭐ **まずここから** - 5分で動かす |
| [getting-started-ja.md](./getting-started-ja.md) | 日本語入門ガイド |
| [quickstart.md](./quickstart.md) | 英語版クイックスタート |

---

## 📖 ユーザーガイド

### Agent 開発

| ドキュメント | 説明 |
|-------------|------|
| [guide-coding.md](./guide-coding.md) | Agent 開発コーディングガイド |
| [PATTERNS_GUIDE.md](./PATTERNS_GUIDE.md) | デザインパターン解説 |
| [guide-skills.md](./guide-skills.md) | Skills システム詳細 |
| [guide-builtin-skills.md](./guide-builtin-skills.md) | 組み込み Skills 一覧 |

### CLI / Studio

| ドキュメント | 説明 |
|-------------|------|
| [guide-cli.md](./guide-cli.md) | CLI コマンドリファレンス |
| [cli.md](./cli.md) | CLI 詳細仕様 |
| [guide-studio-ui.md](./guide-studio-ui.md) | Studio Web UI 使用ガイド |

### 高度な機能

| ドキュメント | 説明 |
|-------------|------|
| **[context-engineering.md](./context-engineering.md)** | ⭐ **Context Engineering** - 上下文予算管理 |
| [guide-llm-router.md](./guide-llm-router.md) | LLM ルーター設定 |
| [protocols.md](./protocols.md) | MCP / A2A / AG-UI プロトコル |

---

## 🏗️ アーキテクチャ / 設計

| ドキュメント | 説明 |
|-------------|------|
| [architecture.md](./architecture.md) | 全体アーキテクチャ |
| [api.md](./api.md) | API リファレンス |
| [ja/PLATFORM_RUNTIME_GUIDE.md](./ja/PLATFORM_RUNTIME_GUIDE.md) | Platform Runtime（マルチテナントSaaS運用） |
| [design/DEEP_AGENT_COORDINATOR_DESIGN.md](./design/DEEP_AGENT_COORDINATOR_DESIGN.md) | DeepAgent 設計 |
| [design/DEEP_AGENT_IMPLEMENTATION_GUIDE_JA.md](./design/DEEP_AGENT_IMPLEMENTATION_GUIDE_JA.md) | DeepAgent 実装ガイド |
| [memory/MEMORY_SYSTEM_DESIGN.md](./memory/MEMORY_SYSTEM_DESIGN.md) | メモリシステム設計 |

---

## 🔧 拡張 / 開発者向け

| ドキュメント | 説明 |
|-------------|------|
| [DEVELOPMENT_STANDARDS_JA.md](./DEVELOPMENT_STANDARDS_JA.md) | 開発規約 |
| [FRAMEWORK_ABSTRACTION_PLAN.md](./FRAMEWORK_ABSTRACTION_PLAN.md) | フレームワーク抽象化計画 |
| [design/APP_REFACTORING_PROPOSAL_JA.md](./design/APP_REFACTORING_PROPOSAL_JA.md) | アプリ リファクタリング提案 |
| [design/DATALAKE_INTEGRATION_DESIGN.md](./design/DATALAKE_INTEGRATION_DESIGN.md) | DataLake 統合設計 |

---

## 📦 サンプル

| サンプル | 説明 |
|---------|------|
| [examples/README.md](./examples/README.md) | サンプル一覧 |
| [examples/calculator_agent/](./examples/calculator_agent/) | 計算機 Agent |
| [examples/weather_agent/](./examples/weather_agent/) | 天気 Agent |
| [examples/translator_agent/](./examples/translator_agent/) | 翻訳 Agent |
| [examples/text_processor_agent/](./examples/text_processor_agent/) | テキスト処理 Agent |

---

## 🧪 Apps（実験的プロジェクト）

| プロジェクト | 説明 |
|-------------|------|
| `apps/code_migration_assistant/` | COBOL → Java コード移行 |
| `apps/decision_governance_engine/` | 意思決定支援エンジン |
| `apps/market_trend_monitor/` | 市場トレンド監視 |

---

## 🔍 API クイックリファレンス

### Engine パターン（推奨）

```python
from agentflow import SimpleEngine, PipelineEngine, GateEngine, RAGEngine

# SimpleEngine - 単一 Agent
engine = SimpleEngine(agent=MyAgent)
result = await engine.run({"question": "..."})

# PipelineEngine - 複数ステージ
engine = PipelineEngine(stages=[...])
async for event in engine.run_stream(inputs):
    print(event)
```

### @agent デコレータ

```python
from agentflow import agent, tool, AgentClient

@agent
class MyAgent:
    system_prompt = "..."
    
    @tool
    def search(self, query: str) -> list:
        return []

result = await AgentClient.get("MyAgent").invoke({"question": "..."})
```

### Provider（LLM / DB）

```python
from agentflow import get_llm, get_db, get_vectordb

llm = get_llm()  # 環境変数から自動検出
response = await llm.chat([{"role": "user", "content": "hello"}])
```

### CLI

```bash
agentflow create my-agent --template chatbot
agentflow run my-agent --interactive
agentflow studio --port 8000
```

---

## 📝 更新履歴

| バージョン | 変更内容 |
|-----------|---------|
| v0.2.0 | Engine パターン導入、HITL サポート |
| v0.1.0 | 初回リリース |

---

> 💡 **ヒント**: まずは [QUICKSTART.md](./QUICKSTART.md) を読んで、5分で最初の Agent を作成してみてください！

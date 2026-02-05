# 変更履歴

AgentFlow フレームワークの変更履歴。

---

## [2026-02-05] - Auto-Agent + Skills 統合 (v1.8.1)

### ✨ 新機能

#### Skills 統合

- `ToolDiscoveryService.discover_skills_from_engine()`: SkillEngine からスキルを自動発見
- `@agent` デコレータが AgentRegistry に自動登録
- `SimpleEngine` がスキルをツールとして自動バインド
- ルートレベル `skills/` ディレクトリ構成

**使用例:**

```python
from agentflow import agent
from agentflow.engines import SimpleEngine
from agentflow.core.tool_discovery import ToolDiscoveryService

# Skills を発見
service = ToolDiscoveryService(get_global_tool_registry())
await service.discover_skills_from_engine()

# @agent で Agent を定義（AgentRegistry に自動登録）
@agent(skills=["rag"])
class RAGAgent:
    system_prompt = "RAG検索Agent"

# SimpleEngine で実行（スキルを自動バインド）
engine = SimpleEngine(agent=RAGAgent, skills=["rag"])
result = await engine.run({"query": "質問"})
```

### 🧪 テスト

- `tests/unit/skills/test_skill_loader_root.py`: ルートスキルディレクトリテスト
- `tests/unit/test_agent_decorator_registry.py`: @agent + AgentRegistry 統合テスト
- `tests/unit/engines/test_simple_engine_binding.py`: SimpleEngine + ToolBinder テスト
- `tests/integration/test_auto_agent_skills_integration.py`: 完全統合テスト

---

## [2026-02-05] - Auto-Agent アーキテクチャ (v1.8.0)

### ✨ 新機能

#### 1. 統一ツール・Agent レジストリ（Auto-Agent Architecture）

未来志向のAI快速開発基盤。統一されたツール・Agentレジストリを通じて、
自律的なAgent分析、自動Agent生成、MCP/Skills機能のバインディングを実現。

**設計原則:**

| 原則 | 説明 |
|------|------|
| **高度抽象化** | ツールソース（MCP/Skills/Builtin）を統一インターフェースで表現 |
| **低結合** | レジストリはインターフェースであり、具体実装に依存しない |
| **高凝集** | 各モジュールは単一責任を持つ |
| **拡張容易** | 新しいツールソースは `ToolDefinition.from_*()` を実装するだけ |

**コアコンポーネント:**

| コンポーネント | クラス | 役割 |
|--------------|--------|------|
| **ToolDefinition** | `ToolDefinition` | 統一ツール表現（URI、スキーマ、メタデータ） |
| **ToolRegistry** | `ToolRegistry` | ツール登録・検索・フィルタリング |
| **AgentCapabilitySpec** | `AgentCapabilitySpec` | Agent能力宣言（ツール/LLM要件） |
| **AgentRegistry** | `AgentRegistry` | Agent能力登録・マッチング・ファクトリ |
| **ToolBinder** | `ToolBinder` | ランタイムツールバインディング |
| **ToolDiscoveryService** | `ToolDiscoveryService` | 全ソースからツール発見 |

**使用例:**

```python
from agentflow import (
    get_global_tool_registry,
    get_global_agent_registry,
    ToolDiscoveryService,
    AgentCapabilitySpec,
    CapabilityRequirement,
    ToolBinder,
)

# Step 1: ツール発見・登録
tool_registry = get_global_tool_registry()
service = ToolDiscoveryService(tool_registry)
service.register_builtin(name="search", description="検索", input_schema={...})

# Step 2: Agent能力定義・登録
agent_registry = get_global_agent_registry()
capability = AgentCapabilitySpec(
    id="search_agent",
    name="Search Agent",
    description="ドキュメントを検索",
    tags=["search"],
    required_tools=["tool://builtin/search"],
)
agent_registry.register("SearchAgent", capability, lambda: SearchAgent())

# Step 3: タスク要件でAgent検索（能力ベースマッチング）
requirement = CapabilityRequirement(
    description="ドキュメントを検索",
    required_tags=["search"],
)
matches = agent_registry.find_matching(requirement)

# Step 4: ツール自動バインド
binder = ToolBinder(tool_registry)
agent = agent_registry.get_factory(matches[0][0])()
bound_agent = await binder.bind_for_capability(agent, capability)
```

**ファイル構成:**

```
agentflow/core/
├── tool_definition.py     # ToolDefinition, ToolSource
├── tool_registry.py       # ToolRegistry, get_global_tool_registry
├── capability_spec.py     # AgentCapabilitySpec, CapabilityRequirement
├── agent_registry.py      # AgentRegistry, get_global_agent_registry
├── tool_binding.py        # ToolBinder, BoundTools, ToolExecutor
└── tool_discovery.py      # ToolDiscoveryService
```

---

### 📝 ドキュメント

- `docs/auto-agent-architecture.md`: Auto-Agent アーキテクチャ設計書（NEW）
- `docs/architecture.md`: アーキテクチャ設計書（Auto-Agent層追加）
- `README.md`: Auto-Agent機能追加

---

### 🧪 テスト

- `tests/unit/core/test_tool_definition.py`: ToolDefinition 単体テスト (8 件)
- `tests/unit/core/test_tool_registry.py`: ToolRegistry 単体テスト (12 件)
- `tests/unit/core/test_capability_spec.py`: AgentCapabilitySpec 単体テスト (8 件)
- `tests/unit/core/test_agent_registry.py`: AgentRegistry 単体テスト (11 件)
- `tests/unit/core/test_tool_binding.py`: ToolBinder 単体テスト (8 件)
- `tests/unit/core/test_tool_discovery.py`: ToolDiscoveryService 単体テスト (6 件)
- `tests/integration/test_auto_agent_flow.py`: 統合テスト (5 件)

---

## [2026-01-15] - 新機能追加

### ✨ 新機能

#### 1. フロントエンド富文本レンダリングコンポーネント (`RichContentRenderer`)

バックエンド A2UI `RichResponse` をフロントエンドで美しくレンダリングするコンポーネント群。

**対応コンポーネント:**

| タイプ | コンポーネント | 説明 |
|--------|---------------|------|
| `markdown` | `MarkdownRenderer` | Markdown テキストを HTML に変換 |
| `code_block` | `CodeBlockRenderer` | シンタックスハイライト付きコード表示 |
| `alert` | `AlertRenderer` | 情報・警告・エラーアラート |
| `data_table` | `DataTableRenderer` | ソート・ページネーション付きテーブル |
| `citation` | `CitationRenderer` | 引用元情報の表示 |
| `collapsible` | `CollapsibleRenderer` | 折りたたみセクション |
| `tabs` | `TabsRenderer` | タブ付きコンテンツ |

**使用例:**

```tsx
import { RichContentRenderer } from '@/components/rich-content';

function ResultPanel({ data }) {
  return (
    <RichContentRenderer
      response={data}
      theme="dark"
      className="p-4"
    />
  );
}
```

**ファイル構成:**

```
studio/src/components/rich-content/
├── index.ts                    # エクスポート
├── RichContentRenderer.tsx     # メインレンダラー
├── types.ts                    # 型定義
└── renderers/
    ├── MarkdownRenderer.tsx
    ├── CodeBlockRenderer.tsx
    ├── DataTableRenderer.tsx
    ├── AlertRenderer.tsx
    ├── CitationRenderer.tsx
    ├── CollapsibleRenderer.tsx
    └── TabsRenderer.tsx
```

---

#### 2. Agent 発見機構 (`AgentDiscovery`)

大規模デプロイメント向けの Agent 自動発見・登録・負荷分散機構。

**主な機能:**

- **動的登録/解除**: Agent の自動登録・発見
- **ヘルスチェック**: ハートビートによる生存確認
- **能力検索**: 特定能力を持つ Agent の検索
- **負荷分散**: 複数戦略対応（Round Robin / Random / Weighted）

**使用例:**

```python
from agentflow.discovery import AgentDiscovery, AgentEntry, AgentStatus

# 初期化
discovery = AgentDiscovery()

# Agent 登録
await discovery.register(AgentEntry(
    agent_id="agent-001",
    name="ResearchAgent",
    endpoint="http://localhost:8001",
    capabilities=["research", "summarize"],
    status=AgentStatus.HEALTHY,
))

# 能力による検索
agents = await discovery.discover(capability="research")

# 負荷分散で選択
agent = await discovery.select("research")

# ハートビート送信
await discovery.heartbeat("agent-001")
```

**負荷分散戦略:**

| 戦略 | 説明 |
|------|------|
| `ROUND_ROBIN` | 順番に選択（デフォルト） |
| `RANDOM` | ランダム選択 |
| `WEIGHTED` | 重み付き選択 |
| `LEAST_CONNECTIONS` | 最小接続数（将来実装） |

**ファイル構成:**

```
agentflow/discovery/
├── __init__.py     # エクスポート
├── base.py         # 基底クラス・型定義
├── registry.py     # InMemoryRegistry 実装
└── health.py       # ヘルスチェッカー
```

---

### 📝 ドキュメント

- `docs/design/RICH_CONTENT_RENDERER_DESIGN.md`: フロントエンド設計書
- `docs/design/AGENT_DISCOVERY_DESIGN.md`: Agent 発見機構設計書
- `docs/CHANGELOG_JA.md`: このファイル

---

### 🧪 テスト

- `tests/unit/test_agent_discovery.py`: Agent Discovery 単体テスト (10 件)

---

### 🔧 修正

- `datetime.utcnow()` の非推奨警告を修正（`datetime.now(UTC)` に変更）

---

## 今後の予定

1. **Chart レンダラー**: ECharts を使用したグラフ表示
2. **Math レンダラー**: KaTeX を使用した数式表示
3. **Redis ベース Registry**: 本番環境向け分散レジストリ
4. **Consul/etcd 連携**: 外部サービスディスカバリとの統合


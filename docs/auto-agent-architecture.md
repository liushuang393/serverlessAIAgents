# Auto-Agent アーキテクチャ

> **バージョン**: 1.1.0
> **更新日**: 2026-02-05

---

## 📋 概要

Auto-Agent アーキテクチャは、統一されたツール・Agent レジストリを通じて、
自律的な Agent 分析、自動 Agent 生成、MCP/Skills 機能のバインディングを実現する
未来志向の AI 開発フレームワーク基盤です。

### 設計哲学

| 原則 | 説明 |
|------|------|
| **高度抽象化** | ツールソース（MCP/Skills/Builtin）を統一インターフェースで表現 |
| **低結合** | レジストリはインターフェースであり、具体実装に依存しない |
| **高凝集** | 各モジュールは単一責任を持つ |
| **拡張容易** | 新しいツールソースは `ToolDefinition.from_*()` を実装するだけ |
| **可読性** | 日本語コメント付きで、設計意図を明確に |

---

## 🏗️ アーキテクチャ概要

```
┌─────────────────────────────────────────────────────────────────────────┐
│                     Auto-Agent Architecture                              │
├─────────────────────────────────────────────────────────────────────────┤
│  📊 統一レジストリ層                                                     │
│     ├── ToolRegistry: 全ソースのツールを統一管理                        │
│     ├── AgentRegistry: Agent能力とファクトリを統一管理                   │
│     └── グローバルシングルトン: get_global_tool/agent_registry()        │
├─────────────────────────────────────────────────────────────────────────┤
│  🔧 ツール定義層                                                         │
│     ├── ToolDefinition: 統一ツール表現                                  │
│     ├── ToolSource: builtin / mcp / skill / dynamic                     │
│     └── ファクトリメソッド: from_mcp(), from_skill(), from_builtin()    │
├─────────────────────────────────────────────────────────────────────────┤
│  🤖 Agent能力層                                                          │
│     ├── AgentCapabilitySpec: 能力宣言（ツール/LLM要件）                 │
│     ├── CapabilityRequirement: タスク要件宣言                           │
│     └── マッチングスコア計算: capability.matches(requirement)           │
├─────────────────────────────────────────────────────────────────────────┤
│  🔗 バインディング層                                                     │
│     ├── ToolBinder: ランタイムツールアタッチ                            │
│     ├── BoundTools: バインド済みツールコンテナ                          │
│     └── ToolExecutor: ツール実行管理                                    │
├─────────────────────────────────────────────────────────────────────────┤
│  🔍 発見層                                                               │
│     ├── ToolDiscoveryService: 全ソースからツール発見                    │
│     └── discover_all() / discover_skills() / discover_mcp_tools()       │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## 📦 コアコンポーネント

### ToolDefinition - 統一ツール定義

すべてのツールソースを統一的に表現するモデル。

```python
from agentflow import ToolDefinition, ToolSource

# MCPツールから作成
tool = ToolDefinition.from_mcp(
    {"name": "read_file", "description": "ファイル読み取り", "inputSchema": {}},
    server_name="filesystem"
)
# URI: tool://mcp/filesystem/read_file

# Skillから作成
tool = ToolDefinition.from_skill({
    "name": "summarize",
    "description": "テキストを要約",
    "parameters": {"text": {"type": "string"}}
})
# URI: tool://skill/summarize

# ビルトイン（@tool デコレータ）から作成
tool = ToolDefinition.from_builtin(
    name="calculator",
    description="計算を実行",
    input_schema={"type": "object", "properties": {"expr": {"type": "string"}}}
)
# URI: tool://builtin/calculator

# MCP形式に変換（LLMに渡す用）
mcp_format = tool.to_mcp()
```

### ToolRegistry - 統一ツールレジストリ

```python
from agentflow import ToolRegistry, get_global_tool_registry

# グローバルレジストリを取得
registry = get_global_tool_registry()

# ツールを登録
registry.register(tool)

# URIで取得
tool = registry.get("tool://mcp/filesystem/read_file")

# クエリで検索（関連性スコア順）
results = registry.search("ファイル")

# ソースでフィルタ
mcp_tools = registry.filter_by_source(ToolSource.MCP)
```

### AgentCapabilitySpec - Agent能力仕様

```python
from agentflow import AgentCapabilitySpec, CapabilityRequirement

# Agent能力を宣言
capability = AgentCapabilitySpec(
    id="pdf_analyzer_v1",
    name="PDF Analyzer",
    description="PDF文書を分析して情報を抽出",
    required_tools=["tool://mcp/filesystem/read_file", "tool://builtin/ocr"],
    tags=["pdf", "analysis", "extraction"],
    llm_requirements=LLMRequirements(
        model="claude-3-opus",
        temperature=0.3,
    ),
)

# タスク要件を宣言
requirement = CapabilityRequirement(
    description="PDFレポートを分析して要約を作成",
    required_tags=["pdf"],
    required_tools=["tool://mcp/filesystem/read_file"],
)

# マッチングスコア計算
score = capability.matches(requirement)  # 0.0 〜 1.0
```

### AgentRegistry - Agentレジストリ

```python
from agentflow import AgentRegistry, get_global_agent_registry

# グローバルレジストリを取得
registry = get_global_agent_registry()

# Agentを登録
registry.register(
    agent_id="PDFAnalyzer",
    capability=capability,
    factory=lambda: PDFAnalyzerAgent(),
)

# タスク要件でマッチング
matches = registry.find_matching(requirement)
# [("PDFAnalyzer", 0.85), ("TextAnalyzer", 0.42), ...]

# Agentインスタンスを取得
factory = registry.get_factory("PDFAnalyzer")
agent = factory()
```

### ToolBinder - ツールバインディング

```python
from agentflow import ToolBinder

# バインダーを作成
binder = ToolBinder(tool_registry)

# Agent能力が必要とするツールをバインド
bound_agent = await binder.bind_for_capability(agent, capability)

# バインドされたツールをMCP形式で取得（LLMに渡す用）
mcp_tools = bound_agent._tools.to_mcp_format()
```

### ToolDiscoveryService - ツール発見

```python
from agentflow import ToolDiscoveryService

service = ToolDiscoveryService(tool_registry)

# Skillsを発見
await service.discover_skills([
    {"name": "code_review", "description": "コードレビュー"},
    {"name": "summarize", "description": "テキスト要約"},
])

# MCPサーバーツールを発見
await service.discover_mcp_tools("filesystem", mcp_tools_list)

# 手動でビルトインを登録
service.register_builtin(
    name="echo",
    description="入力をそのまま返す",
    input_schema={"type": "object", "properties": {"text": {"type": "string"}}},
)
```

---

## 🔄 完全ワークフロー

```python
from agentflow import (
    get_global_tool_registry,
    get_global_agent_registry,
    ToolDiscoveryService,
    AgentCapabilitySpec,
    CapabilityRequirement,
    ToolBinder,
)

# Step 1: ツールを発見・登録
tool_registry = get_global_tool_registry()
service = ToolDiscoveryService(tool_registry)
service.register_builtin(
    name="search",
    description="ドキュメント検索",
    input_schema={"type": "object", "properties": {"query": {"type": "string"}}},
)

# Step 2: Agent能力を定義・登録
agent_registry = get_global_agent_registry()
capability = AgentCapabilitySpec(
    id="search_agent",
    name="Search Agent",
    description="ドキュメントを検索して情報を取得",
    tags=["search", "document"],
    required_tools=["tool://builtin/search"],
)
agent_registry.register("SearchAgent", capability, lambda: SearchAgent())

# Step 3: タスク要件でAgent検索
requirement = CapabilityRequirement(
    description="ドキュメントを検索",
    required_tags=["search"],
)
matches = agent_registry.find_matching(requirement)
best_agent_id = matches[0][0]  # "SearchAgent"

# Step 4: Agentインスタンスを取得してツールをバインド
factory = agent_registry.get_factory(best_agent_id)
agent = factory()
binder = ToolBinder(tool_registry)
bound_agent = await binder.bind_for_capability(agent, capability)

# Step 5: バインドされたツールをLLMに渡す
mcp_tools = bound_agent._tools.to_mcp_format()
```

---

## 📁 ファイル構成

```
agentflow/core/
├── tool_definition.py     # ToolDefinition, ToolSource
├── tool_registry.py       # ToolRegistry, get_global_tool_registry
├── capability_spec.py     # AgentCapabilitySpec, CapabilityRequirement, LLMRequirements
├── agent_registry.py      # AgentRegistry, get_global_agent_registry
├── tool_binding.py        # ToolBinder, BoundTools, ToolExecutor
└── tool_discovery.py      # ToolDiscoveryService
```

---

## 🧪 テスト

```bash
# ユニットテスト
pytest tests/unit/core/ -v

# 統合テスト
pytest tests/integration/test_auto_agent_flow.py -v
```

---

## 🔗 Skills 統合 (v1.1.0 NEW)

### Skills をツールとして発見

```python
from agentflow.core.tool_discovery import ToolDiscoveryService
from agentflow.core.tool_registry import get_global_tool_registry

registry = get_global_tool_registry()
service = ToolDiscoveryService(registry)

# SkillEngine からスキルを自動発見してツールとして登録
count = await service.discover_skills_from_engine()
print(f"発見されたスキル: {count}")

# スキルをツールとして取得
rag_tool = registry.get("tool://skill/rag")
```

### @agent と AgentRegistry 自動統合

```python
from agentflow import agent
from agentflow.core.agent_registry import get_global_agent_registry
from agentflow.core.capability_spec import CapabilityRequirement

@agent(skills=["rag", "chatbot"])
class MyAgent:
    """RAG と Chatbot スキルを使用する Agent."""
    system_prompt = "あなたは親切なアシスタントです"

# AgentRegistry に自動登録される
registry = get_global_agent_registry()

# タスク要件でマッチング
requirement = CapabilityRequirement(
    description="ドキュメント検索",
    required_tags=["rag"],
)
matches = registry.find_matching(requirement)
# [("MyAgent", 0.85), ...]
```

### SimpleEngine でスキルを自動バインド

```python
from agentflow.engines import SimpleEngine

# スキルを指定して Engine を作成
engine = SimpleEngine(
    agent=MyAgent,
    skills=["rag", "chatbot"],  # スキルをツールとして自動バインド
)

result = await engine.run({"query": "質問"})
```

### Skills ディレクトリ構成

```
skills/                               # ルートレベル Skills ディレクトリ
├── README.md                         # 使用方法ドキュメント
├── builtin/                          # フレームワーク提供スキル
│   ├── rag/SKILL.md
│   ├── chatbot/SKILL.md
│   └── ...
├── user/                             # ユーザー定義スキル
│   └── my-skill/SKILL.md
└── apps/                             # アプリケーション固有スキル
    └── decision_governance_engine/
        ├── dao/SKILL.md
        └── ...
```

---

## 🔮 将来の拡張

### ✅ 実装済み (v1.1.0)

1. **@agent デコレータ統合**: AgentRegistry との自動連携
2. **Engine統合**: SimpleEngine でのツールバインディング自動化
3. **Skills統合**: SkillEngine からのツール自動発見

### 計画中

1. **AgentWizard統合**: 未対応タスクに対する自動Agent生成
2. **DeepAgentCoordinator**: 能力ベースの自動Agent選択
3. **Redis/Consul バックエンド**: 分散環境向けレジストリ

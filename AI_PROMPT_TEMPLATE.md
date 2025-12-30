# AgentFlow 完全開発ガイド（AI向け）

> **目的**: AIアシスタントがAgentFlowフレームワークを正しく・完全に活用してMulti-Agentシステムを構築するための詳細ガイド
> **バージョン**: 0.2.0（2024-12-30更新）

---

## 🆕 v0.2.0 新機能: Decorator API & Provider Layer

### @agent デコレータ（最も簡単）

```python
from agentflow import agent, tool, AgentClient

@agent  # 設定ゼロでAgent定義
class MyAgent:
    """シンプルなAgent"""
    
    system_prompt = "あなたは親切なアシスタントです"
    
    @tool  # ツールを自動登録
    def search(self, query: str) -> list:
        """検索機能"""
        return []

# 呼び出し
result = await AgentClient.get("MyAgent").invoke({"question": "..."})
```

### 統一Provider Layer

```python
from agentflow.providers import LLMProvider, ToolProvider, DataProvider, EventProvider

# LLM（デフォルトOpenAI）
llm = LLMProvider.default()
response = await llm.chat([{"role": "user", "content": "hello"}])

# Tool（@tool自動発見）
tools = ToolProvider.discover()
result = await tools.call("search", query="test")

# Data（SQL/Vector/Cache統一）
db = DataProvider.sql()
vector = DataProvider.vector()
cache = DataProvider.cache()

# Event（SSE/WS統一）
events = EventProvider.create()
await events.emit("progress", {"step": 1})
```

### FastAPI統合（AgentRouter）

```python
from agentflow.integrations import AgentRouter

app = FastAPI()
app.include_router(AgentRouter(agents=["MyAgent"]), prefix="/api")

# 自動生成エンドポイント:
# POST /api/agents/{id}/invoke
# POST /api/agents/{id}/stream
```

---

## 🎯 最重要：統一入口 = Flow

**すべてはFlowから始まる。単一Agentでも複数Agentでも、必ずFlowで包む。**

```
【核心設計原則】
┌────────────────────────────────────────────────────────┐
│  Flow（編排層）                                         │
│    └── Agent（実行層）                                  │
│          ├── Skill（プロンプト）                        │
│          ├── RAG（知識検索）    ← オプション            │
│          └── MCP（外部ツール）  ← オプション            │
└────────────────────────────────────────────────────────┘

【UI/バックエンド連携】
  同期処理 → REST API（デフォルト推奨）
  進捗表示 → SSE ストリーム（リアルタイム必要時）
  双方向   → WebSocket（複雑な対話のみ）
```

---

## 🚀 クイックスタート（3つのパターン）

### Pattern A: 最小構成（単一Agent）
```python
# apps/my_app/main.py
from agentflow.patterns.multi_agent import AgentCoordinator

# 1. Agentを1つ定義
class SimpleQAAgent(AgentBlock):
    async def run(self, input_data: dict) -> dict:
        return {"answer": "..."}

# 2. Coordinatorで包む（単一でも必須）
coordinator = AgentCoordinator(
    agents=[SimpleQAAgent()],
    pattern="sequential"
)

# 3. 実行
result = await coordinator.execute({"question": "..."})
```

### Pattern B: 標準構成（複数Agent順次実行）
```python
# apps/my_app/workflow.py
coordinator = AgentCoordinator(
    agents=[
        GatekeeperAgent(),  # 入口検証
        AnalysisAgent(),    # 分析
        OutputAgent(),      # 出力整形
    ],
    pattern="sequential",
    shared_context=SharedContext(enable_memory=True)
)
result = await coordinator.execute(task)
```

### Pattern C: 高度構成（Supervisor動的選択）
```python
# apps/my_app/workflow.py
from agentflow.patterns.supervisor import SupervisorCoordinator

coordinator = SupervisorCoordinator(
    supervisor=MySupervisorAgent(),
    workers={
        "research": ResearchAgent(),
        "write": WriteAgent(),
        "review": ReviewAgent(),
    },
    max_iterations=10
)
result = await coordinator.execute(task)
```

---

## 📁 推奨プロジェクト構造（シンプル版）

```
apps/my_app/
├── main.py              # FastAPI エントリーポイント（REST/SSE）
├── workflow.py          # AgentCoordinator 定義
├── agents/              # Agent 実装（AgentBlock 継承）
│   └── *.py
├── skills/              # SKILL.md 形式プロンプト
│   └── */SKILL.md
└── schemas/             # Pydantic 入出力定義
    └── *.py
```

---

## 📐 フレームワーク全体像

```
┌─────────────────────────────────────────────────────────────────────┐
│                         AgentFlow Framework                          │
├─────────────────────────────────────────────────────────────────────┤
│  agentflow/                                                          │
│  ├── core/           # 基盤層                                        │
│  │   ├── agent_block.py    # Agent基底クラス ← 全Agent継承必須      │
│  │   ├── engine.py         # AgentFlowEngine ← ワークフロー実行     │
│  │   └── types.py          # WorkflowConfig定義                     │
│  │                                                                   │
│  ├── patterns/       # 協調パターン（5種類）                         │
│  │   ├── multi_agent.py    # AgentCoordinator, SharedContext        │
│  │   ├── supervisor.py     # SupervisorCoordinator                  │
│  │   ├── hierarchical.py   # HierarchicalCoordinator                │
│  │   ├── reflection.py     # ReflectionLoop, ReflectorAgent         │
│  │   └── coordinator.py    # CoordinatorBase                        │
│  │                                                                   │
│  ├── skills/         # 自動進化システム                              │
│  │   ├── engine.py         # SkillEngine ← 越用越厉害               │
│  │   ├── base.py           # Skill, SkillMetadata                   │
│  │   ├── builtin/          # 内蔵Skills（DB/決済/認証/デプロイ）    │
│  │   ├── rag.py            # RAGSkill                               │
│  │   └── chatbot.py        # ChatBotSkill                           │
│  │                                                                   │
│  ├── protocols/      # 4プロトコル統合                               │
│  │   ├── mcp_client.py     # MCP (Model Context Protocol)           │
│  │   ├── a2a_*.py          # A2A (Agent-to-Agent)                   │
│  │   ├── agui_*.py         # AG-UI (Agent UI Events)                │
│  │   └── a2ui/             # A2UI (宣言式UI生成)                     │
│  │                                                                   │
│  ├── memory/         # 3層記憶システム（LightMem準拠）               │
│  │   ├── sensory_memory.py # 感覚記憶（予圧縮）                      │
│  │   ├── short_term_memory.py # 短期記憶（トピックバッファ）        │
│  │   └── long_term_memory.py  # 長期記憶（永続化）                  │
│  │                                                                   │
│  ├── decorators.py   # @auto_adapt デコレーター                      │
│  ├── llm/            # LLMクライアント統合                           │
│  └── studio/         # REST API（FastAPI）                          │
│                                                                      │
│  studio/             # ビジュアルエディタ（React + React Flow）      │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 🔴 必須実装パターン

### 1. Agent基底クラス（継承必須）

```python
# 参照: agentflow/core/agent_block.py
from typing import Any
from agentflow.core.agent_block import AgentBlock

class MyAgent(AgentBlock):
    """エージェント説明.
    
    職責:
    - 単一責任を明記
    
    禁止事項:
    - 禁止事項を明記
    """
    
    # 設定（サブクラスでオーバーライド）
    name: str = "MyAgent"
    max_tokens: int = 1000
    temperature: float = 0.5
    timeout_seconds: int = 30
    
    async def initialize(self) -> None:
        """初期化（オプション）."""
        await super().initialize()
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """メイン処理（必須実装）.
        
        Args:
            input_data: 入力データ（必ずdict型）
            
        Returns:
            出力データ（必ずdict型、構造化）
        """
        # 処理ロジック
        return {"result": "..."}
    
    async def cleanup(self) -> None:
        """クリーンアップ（オプション）."""
        await super().cleanup()
```

### 2. 協調パターン選択ガイド

| パターン | 使用場面 | 参照ファイル |
|---------|---------|-------------|
| **Sequential** | Agent間に依存関係がある場合 | `patterns/multi_agent.py` |
| **Concurrent** | 独立タスクの並列実行 | `patterns/multi_agent.py` |
| **Handoff** | 動的な委譲が必要な場合 | `patterns/multi_agent.py` |
| **Supervisor** | 監督者が動的にワーカー選択 | `patterns/supervisor.py` |
| **Hierarchical** | 階層的タスク分解 | `patterns/hierarchical.py` |
| **Reflection** | 生成→評価→改善ループ | `patterns/reflection.py` |

#### Sequential実装例

```python
# 参照: agentflow/patterns/multi_agent.py
from agentflow.patterns.multi_agent import AgentCoordinator, SharedContext

class MyWorkflow:
    def __init__(self):
        self._context = SharedContext(enable_memory=True)
        self._coordinator = AgentCoordinator(
            agents=[
                GatekeeperAgent(),
                AnalysisAgent(),
                PlanningAgent(),
                ReviewAgent(),
            ],
            pattern="sequential",
            shared_context=self._context,
        )
    
    async def process(self, task: str) -> dict:
        # SharedContextに初期データ設定
        self._context.set("original_task", task)
        
        # 協調実行
        result = await self._coordinator.execute(task)
        
        # 結果構造
        # {
        #   "final_result": 最終Agent出力,
        #   "agent_results": {"AgentName": 結果, ...},
        #   "pattern": "sequential",
        #   "agents_executed": 実行Agent数
        # }
        return result
```

#### Supervisor実装例

```python
# 参照: agentflow/patterns/supervisor.py
from agentflow.patterns.supervisor import SupervisorCoordinator, SupervisorDecision

class MySupervisorAgent(AgentBlock):
    """監督者Agent - 次のワーカーを動的に選択."""
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        task = input_data["task"]
        context = input_data["context"]
        available_workers = input_data["available_workers"]
        
        # LLMで次のアクションを決定
        # action: "DELEGATE" or "FINISH"
        # worker_name: 選択されたワーカー名
        return {
            "action": "DELEGATE",  # or "FINISH"
            "worker_name": "research",
            "worker_input": {"query": "..."},
            "reason": "研究が必要"
        }

# 使用
coordinator = SupervisorCoordinator(
    supervisor=MySupervisorAgent(),
    workers={
        "research": ResearchAgent(),
        "write": WriteAgent(),
        "review": ReviewAgent(),
    },
    max_iterations=10
)
result = await coordinator.execute("レポート作成")
```

#### Reflection実装例

```python
# 参照: agentflow/patterns/reflection.py
from agentflow.patterns.reflection import (
    ReflectionLoop, ReflectorAgent, ImproverAgent, ReflectionResult
)

# 評価Agent
reflector = ReflectorAgent(
    llm_client=llm,
    evaluation_criteria={
        "clarity": "内容が明確か",
        "accuracy": "情報が正確か",
        "completeness": "必要な情報が全て含まれているか"
    },
    acceptance_threshold=70.0  # 70点以上で合格
)

# 改善Agent
improver = ImproverAgent(llm_client=llm)

# ループ実行
loop = ReflectionLoop(
    generator=my_generator_agent,
    reflector=reflector,
    improver=improver,
    max_iterations=3
)
result = await loop.execute("タスク")
# {
#   "final_output": 最終出力,
#   "iterations": 反復回数,
#   "history": [{"iteration": 1, "score": 65, "feedback": "..."}, ...],
#   "final_score": 最終スコア
# }
```

### 3. Skills自動進化システム（必須活用）

```
核心理念:
  用户需求 → 技能匹配 → 存在なら実行
                     → 不在なら自動生成 → 検証 → 固化 → 実行
  = 越用越厉害（使うほど強くなる）
```

#### SKILL.md形式（Claude Code Skills互換）

```markdown
---
name: decision-analysis
description: |
  企業意思決定の本質分析。問題タイプ分類、制約識別、隠れた前提発見を行う。
  意思決定支援、戦略分析、トレードオフ評価に使用。
version: 1.0.0
author: Your Name
triggers:
  - 本質分析
  - 問題分類
  - 制約識別
  - decision
  - essence
  - trade-off
requirements:
  - pydantic>=2.0
tags:
  - decision
  - analysis
  - enterprise
examples:
  - "新規事業の方向性を分析"
  - "A案とB案のトレードオフ評価"
---

# 本質分析指示（DaoAgent用）

あなたはDaoAgent（道）です。問題の本質を見抜く専門家です。

## 唯一の責任
問題の「本質」を一文で表現し、不可変の制約と隠れた前提を明らかにすること。

## 禁止事項
- ❌ 解決策を提示してはいけません
- ❌ 行動を推奨してはいけません
- ❌ 楽観的な予測をしてはいけません
- これらの判断は後続のAgentに委ねてください

## 出力形式
必ず以下のJSON形式で出力してください：
```json
{
    "problem_type": "TRADE_OFF | RESOURCE_ALLOCATION | TIMING_DECISION | RISK_ASSESSMENT | STRATEGY_DIRECTION",
    "essence": "問題の本質を一文で（50字以内）",
    "immutable_constraints": ["変えられない制約（最大5個）"],
    "hidden_assumptions": ["暗黙の前提（最大3個）"]
}
```

## 問題タイプ判定基準
- **TRADE_OFF**: 複数選択肢間のバランス判断
- **RESOURCE_ALLOCATION**: 限られたリソースの配分
- **TIMING_DECISION**: 実行タイミングの判断
- **RISK_ASSESSMENT**: リスクと機会の評価
- **STRATEGY_DIRECTION**: 中長期的な方向性決定
```

#### SkillEngine使用

```python
# 参照: agentflow/skills/engine.py
from pathlib import Path
from agentflow.skills import SkillEngine

class MyDecisionEngine:
    def __init__(self):
        self._skill_engine = SkillEngine(
            skills_dirs=[
                Path("apps/my_app/skills"),  # プロジェクトSkills
                Path.home() / ".agentflow" / "skills",  # グローバルSkills
            ],
            auto_learn=True,   # 自動学習有効
            match_threshold=0.3,
        )
    
    async def get_prompt_for_task(self, task: str) -> str:
        """タスクに対応するSkillプロンプトを取得."""
        result = await self._skill_engine.resolve(task)
        
        if result.generated:
            print(f"🆕 新スキル自動生成: {result.skill.name}")
        else:
            print(f"✅ 既存スキル使用: {result.skill.name}")
        
        # LLMに渡すプロンプト
        return result.instructions
```

### 4. プロトコル統合（@auto_adapt）

```python
# 参照: agentflow/decorators.py
from agentflow.decorators import auto_adapt

@auto_adapt(protocols=["mcp", "a2a", "agui"])
class MyAgent(AgentBlock):
    """プロトコル自動適応Agent."""
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        return {"result": "..."}

# 使用
agent = MyAgent()

# MCPツール取得
mcp_tools = agent.get_mcp_tools()

# A2Aカード取得
a2a_card = agent.get_a2a_card()

# AG-UIエミッター作成
agui_emitter = agent.create_agui_emitter(engine)
```

### 5. A2UI宣言式UI生成

```python
# 参照: agentflow/protocols/a2ui/
from agentflow.protocols.a2ui import (
    A2UIEmitter,
    CardComponent,
    TextComponent,
    ButtonComponent,
    ListComponent,
    FormComponent,
    InputComponent,
)

class MyUIGenerator:
    def __init__(self):
        self._emitter = A2UIEmitter()
    
    def generate_result_ui(self, report: dict) -> CardComponent:
        """結果画面を生成."""
        return CardComponent(
            title="EXECUTIVE SUMMARY",
            children=[
                TextComponent(
                    content=f"💡 結論: {report['conclusion']}",
                    style={"fontSize": "lg", "fontWeight": "bold"}
                ),
                TextComponent(
                    content=f"🎯 最初の一歩: {report['first_step']}"
                ),
                CardComponent(
                    title="⚠️ 主要リスク",
                    children=[
                        ListComponent(items=[
                            TextComponent(content=f"• {risk}")
                            for risk in report['risks']
                        ])
                    ]
                ),
                ButtonComponent(
                    label="📄 PDF出力",
                    action="export_pdf"
                ),
            ]
        )
    
    async def emit(self, component: CardComponent):
        """UIを配信."""
        await self._emitter.emit_component(component)
```

### 6. 記憶システム統合

```python
# 参照: agentflow/memory/
from agentflow.patterns.multi_agent import SharedContext

# 記憶システム有効化
context = SharedContext(
    enable_memory=True,
    enable_vector_search=True,  # ベクトル検索有効
    embedding_dim=384,
)

# 開始
await context.start()

# 記憶（トピック別）
await context.remember("重要な情報", topic="decisions", metadata={"date": "2024-01-01"})

# 検索
memories = await context.recall(
    topic="decisions",
    limit=10,
    min_importance=0.5,
    query="投資判断",  # ベクトル検索
    min_similarity=0.7,
)

# 終了
await context.stop()
```

---

## 📁 推奨プロジェクト構造

```
apps/my_decision_engine/
├── __init__.py
├── main.py              # エントリーポイント
├── workflow.py          # AgentCoordinator使用
├── agent.yaml           # 設定ファイル
│
├── agents/              # Agent実装
│   ├── __init__.py
│   ├── base_agent.py    # プロジェクト固有基底クラス
│   ├── gatekeeper_agent.py
│   ├── analysis_agent.py
│   └── review_agent.py
│
├── skills/              # SKILL.md形式プロンプト
│   ├── gatekeeper/
│   │   └── SKILL.md
│   ├── analysis/
│   │   └── SKILL.md
│   └── review/
│       └── SKILL.md
│
├── schemas/             # Pydantic入出力定義
│   ├── __init__.py
│   ├── input_schemas.py
│   ├── output_schemas.py
│   └── agent_schemas.py
│
├── services/            # ビジネスロジック
│   ├── __init__.py
│   └── llm_gateway.py
│
├── ui/                  # A2UI画面定義
│   ├── __init__.py
│   └── components.py
│
└── prompts/             # （旧式、skills/に移行推奨）
    └── *.txt
```

---

## ⚠️ よくある実装ミス

### ❌ 悪い例

```python
# 1. AgentCoordinatorを使わない手動チェーン
class BadWorkflow:
    async def process(self, task):
        r1 = await agent1.run(task)      # ❌ 手動チェーン
        r2 = await agent2.run(r1)
        r3 = await agent3.run(r2)
        return r3

# 2. Skills未使用のハードコードプロンプト
class BadAgent(AgentBlock):
    PROMPT = """あなたは..."""  # ❌ ハードコード
    
    async def run(self, input_data):
        response = await self._llm.generate(self.PROMPT)  # ❌ Skills未使用
        return {"result": response}

# 3. SharedContext未使用
class BadWorkflow:
    def __init__(self):
        self._results = {}  # ❌ 独自の状態管理

# 4. プロトコル未統合
class BadAgent(AgentBlock):  # ❌ @auto_adapt未使用
    pass
```

### ✅ 良い例

```python
# 1. AgentCoordinatorで協調
class GoodWorkflow:
    def __init__(self):
        self._coordinator = AgentCoordinator(
            agents=[agent1, agent2, agent3],
            pattern="sequential",
            shared_context=SharedContext(enable_memory=True)
        )
    
    async def process(self, task):
        return await self._coordinator.execute(task)

# 2. SkillEngine活用
class GoodAgent(AgentBlock):
    def __init__(self):
        self._skill_engine = SkillEngine(
            skills_dirs=[Path("skills")],
            auto_learn=True
        )
    
    async def run(self, input_data):
        skill_result = await self._skill_engine.resolve(input_data["task"])
        response = await self._llm.generate(skill_result.instructions)
        return {"result": response}

# 3. SharedContext活用
class GoodWorkflow:
    def __init__(self):
        self._context = SharedContext(enable_memory=True)
        await self._context.start()

# 4. プロトコル統合
@auto_adapt(protocols=["mcp", "a2a", "agui"])
class GoodAgent(AgentBlock):
    pass
```

---

## 🔧 agent.yaml設定例

```yaml
# apps/my_app/agent.yaml
meta:
  id: decision-governance-engine
  name: 意思決定支援エンジン
  version: 1.0.0
  description: 企業の意思決定を支援するマルチエージェントシステム
  author: Your Name
  license: MIT
  icon: 🎯
  category: decision-support

# Agent定義
agents:
  gatekeeper:
    name: GatekeeperAgent
    description: 入口検証Agent
    class: apps.my_app.agents.GatekeeperAgent
    config:
      max_tokens: 300
      temperature: 0.1
      use_rag: false
      timeout_seconds: 30
      max_retry: 2

  analysis:
    name: AnalysisAgent
    description: 分析Agent
    class: apps.my_app.agents.AnalysisAgent
    config:
      max_tokens: 800
      temperature: 0.4
      use_rag: true
      rag_sources:
        - industry_practices
        - case_studies

# Workflow定義
workflow:
  type: sequential
  pattern: multi_agent
  stages:
    - name: gate
      agent: gatekeeper
      on_reject: terminate
    - name: analysis
      agent: analysis
    - name: review
      agent: review

# プロトコル設定
protocols:
  mcp: true
  a2a:
    enabled: true
    endpoint: http://localhost:8000
  agui:
    enabled: true
    streaming: true
  a2ui:
    enabled: true
    theme: dark

# Skills設定
skills:
  directories:
    - skills/
    - ~/.agentflow/skills/
  auto_learn: true
  match_threshold: 0.3

# 記憶システム設定
memory:
  enabled: true
  vector_search: true
  embedding_dim: 384
  persistence: postgresql

# RAG設定
rag:
  enabled: true
  sources:
    industry_practices:
      type: vector_store
      path: ./data/industry/
    case_studies:
      type: vector_store
      path: ./data/cases/

# ログ設定
logging:
  level: INFO
  format: "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
```

---

## 📚 参照ファイル一覧

| 機能 | 参照ファイル |
|-----|-------------|
| Agent基底 | `agentflow/core/agent_block.py` |
| 協調パターン | `agentflow/patterns/*.py` |
| Skills | `agentflow/skills/engine.py`, `agentflow/skills/base.py` |
| 内蔵Skills | `agentflow/skills/builtin/*/SKILL.md` |
| プロトコル | `agentflow/protocols/` |
| A2UI | `agentflow/protocols/a2ui/components.py` |
| 記憶 | `agentflow/memory/` |
| デコレーター | `agentflow/decorators.py` |
| Studio API | `agentflow/studio/api.py` |
| 設定Schema | `agentflow/core/schemas.py` |

---

## 🌐 API実装パターン（REST + SSE）

### Pattern 1: REST API（同期・シンプル）

```python
# apps/my_app/main.py
from fastapi import FastAPI
from pydantic import BaseModel

app = FastAPI()

class TaskRequest(BaseModel):
    question: str

class TaskResponse(BaseModel):
    status: str
    result: dict

@app.post("/api/task", response_model=TaskResponse)
async def process_task(req: TaskRequest):
    """同期処理 - 結果を待って返す."""
    coordinator = get_coordinator()
    result = await coordinator.execute({"question": req.question})
    return TaskResponse(status="success", result=result)
```

### Pattern 2: SSE ストリーム（リアルタイム進捗）

```python
# apps/my_app/main.py
from fastapi import FastAPI
from fastapi.responses import StreamingResponse
from agentflow.protocols.agui_emitter import AGUIEmitter

app = FastAPI()

@app.get("/api/task/stream")
async def stream_task(question: str):
    """SSE ストリーム - リアルタイム進捗."""
    async def event_generator():
        emitter = AGUIEmitter()
        coordinator = get_coordinator()

        # フック登録でイベント送信
        coordinator.on_node_start = lambda node: emitter.emit_node_start(node)
        coordinator.on_node_complete = lambda node, r: emitter.emit_node_complete(node, r)

        async for event in emitter.stream():
            yield f"data: {event.json()}\n\n"

        # 実行
        result = await coordinator.execute({"question": question})
        yield f"data: {{'type': 'complete', 'result': {result}}}\n\n"

    return StreamingResponse(
        event_generator(),
        media_type="text/event-stream"
    )
```

### Pattern 3: フロントエンド SSE 受信

```typescript
// frontend/src/hooks/useTaskStream.ts
export function useTaskStream() {
  const [progress, setProgress] = useState(0);
  const [result, setResult] = useState(null);

  const startStream = (question: string) => {
    const eventSource = new EventSource(
      `/api/task/stream?question=${encodeURIComponent(question)}`
    );

    eventSource.onmessage = (e) => {
      const event = JSON.parse(e.data);

      if (event.type === 'node.start') {
        setProgress(event.progress);
      } else if (event.type === 'complete') {
        setResult(event.result);
        eventSource.close();
      }
    };

    return eventSource;
  };

  return { progress, result, startStream };
}
```

---

## 💡 実装順序（推奨）

```
1. schemas/      → Pydantic 入出力定義
2. skills/       → SKILL.md プロンプト
3. agents/       → AgentBlock 継承
4. workflow.py   → AgentCoordinator 構築
5. main.py       → FastAPI エンドポイント（REST + SSE）
6. frontend/     → React + SSE 受信（必要なら）
7. tests/        → 単体 → 統合テスト
```

---

## 🔑 AI助手への最終チェックリスト

実装前に必ず確認：
- [ ] **入口は Flow/Coordinator** - 単一Agentでも包む
- [ ] **Agentは AgentBlock 継承** - run() 実装必須
- [ ] **プロンプトは SKILL.md** - ハードコード禁止
- [ ] **API は REST がデフォルト** - 進捗必要時のみ SSE
- [ ] **SharedContext で状態共有** - 独自dict禁止

---

**このドキュメントを参照して、AgentFlowの全機能を活用したシステムを構築してください。**

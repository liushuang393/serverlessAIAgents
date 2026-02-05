# AgentFlow アーキテクチャ設計書

> **バージョン**: 2.5.0
> **更新日**: 2026-02-05

---

## 📋 概要

AgentFlow は**シンプルさ**と**柔軟性**を両立した多 Agent フレームワークです。

### 設計哲学

| 原則 | 説明 |
|------|------|
| **簡潔** | コアコードは約500行、学習コスト最小 |
| **柔軟** | プラグイン/工場パターンで自由に拡張 |
| **統一** | MCP/A2A/AG-UI/A2UI を統一 API で提供 |
| **型安全** | Python 3.13+ 型ヒント完全活用 |
| **交互統一** | API/CLI/Studio が同一サービス層を使用 |

---

## 🏗️ システム構成

```
┌─────────────────────────────────────────────────────────────────┐
│                        AgentFlow Framework                       │
├─────────────────────────────────────────────────────────────────┤
│  🖥️ 交互層（全て同一サービス層を使用）                           │
│     ├── CLI: Click ベースコマンドライン                         │
│     ├── API: FastAPI REST/SSE                                   │
│     ├── Studio: React + WebSocket                               │
│     └── WebSocket: 双方向リアルタイム通信                       │
├─────────────────────────────────────────────────────────────────┤
│  🔌 サービス層（統一バックエンド）                               │
│     ├── AgentService: Agent実行サービス                         │
│     ├── WorkflowService: Workflow実行サービス                   │
│     └── ServiceEvent: 統一イベントモデル                        │
├─────────────────────────────────────────────────────────────────┤
│  📱 UI 通信層                                                    │
│     ├── AG-UI: イベントストリーム（SSE）                        │
│     ├── A2UI: 宣言式コンポーネント（UI 規範）                   │
│     ├── RichContent: 富文本コンポーネント（共通モジュール）NEW  │
│     ├── WebSocket: 双方向通信（HITL対応）                       │
│     └── RealtimeStateSync: フロントエンド状態同期（NEW）        │
├─────────────────────────────────────────────────────────────────┤
│  🎭 オーケストレーション層（NEW）                                │
│     ├── Orchestrator: 統合オーケストレーター                    │
│     ├── PlannerAgent: タスク分解・計画生成                      │
│     ├── ExecutorAgent: ステップ実行・リトライ                   │
│     └── MonitorAgent: リアルタイム監視・異常検出                │
├─────────────────────────────────────────────────────────────────┤
│  🤖 Agent 層                                                     │
│     ├── AgentBlock: Agent 基底クラス                            │
│     ├── AgentBlueprint: 宣言式Agent定義（YAML/JSON）（NEW）     │
│     ├── Skills: Claude Skills 形式の指示ファイル                │
│     └── Patterns: DeepAgent/Reflection/Pipeline/Reflexion       │
├─────────────────────────────────────────────────────────────────┤
│  🔗 協調層                                                       │
│     ├── A2A: Agent 間通信 (発見・委譲・協調)                    │
│     └── AgentDiscovery: 動的Agent発見・負荷分散（NEW）          │
├─────────────────────────────────────────────────────────────────┤
│  🔧 ツール層                                                     │
│     ├── MCP: 外部ツール接続 (DB/API/ファイル)                   │
│     ├── ToolExecutor: 並行ツール実行（OpenAI互換）              │
│     └── UnifiedToolProvider: 統一ツール抽象層（NEW）            │
├─────────────────────────────────────────────────────────────────┤
│  📊 状態管理層（NEW）                                            │
│     ├── GlobalStateStore: Redux式グローバル状態管理             │
│     ├── Actions: 状態変更アクション定義                         │
│     └── Selectors: 状態クエリセレクター                         │
├─────────────────────────────────────────────────────────────────┤
│  🧠 記憶層                                                       │
│     ├── MemoryManager: 3段階記憶（LightMem）                    │
│     ├── EnhancedMemory: 記憶蒸留・主動遺忘・強化学習            │
│     ├── VectorStore: ベクトル検索（LlamaIndex互換）             │
│     └── Reflexion: 失敗学習システム                             │
├─────────────────────────────────────────────────────────────────┤
│  🛡️ AI安全防護層                                                 │
│     ├── HallucinationDetector: 幻覚検出・可信度評価             │
│     ├── ReasoningMonitor: 多步推理監視・目標逸脱検出            │
│     ├── DataSanitizer: 注入攻撃防護・PII脱敏                    │
│     ├── ConstraintValidator: 入出力・状態遷移検証（NEW）        │
│     ├── DualVerifier: 二重検証・クロスバリデーション（NEW）     │
│     └── AISafetyGuard: 統一安全防護ファサード                   │
├─────────────────────────────────────────────────────────────────┤
│  ⚙️ コア層                                                       │
│     ├── Registry: 統一登録/取得パターン                         │
│     ├── Engine: PocketFlow ワークフローエンジン                 │
│     ├── ErrorResponse: RFC 7807 統一エラー                      │
│     ├── ResilientAgent: 回路遮断・リトライ・検証                │
│     ├── RollbackManager: マルチレベルロールバック（NEW）        │
│     └── Metadata: agent.yaml メタデータ管理                     │
├─────────────────────────────────────────────────────────────────┤
│  🤖 Auto-Agent 層（v1.8.0 NEW）                                  │
│     ├── ToolRegistry: 全ソースのツール統一管理                  │
│     ├── AgentRegistry: Agent能力とファクトリ統一管理            │
│     ├── ToolBinder: ランタイムツールアタッチ                    │
│     └── ToolDiscoveryService: 全ソースツール発見                │
└─────────────────────────────────────────────────────────────────┘
```

---

## 🔑 コアコンセプト

### 1. 統一レジストリパターン

すべてのコンポーネント（Protocol/Skill/Coordinator）を統一 API で管理：

```python
from agentflow.core.registry import Registry

# 基本操作
registry.register("name", item)      # 登録
registry.get("name")                  # 取得（None 許容）
registry.get_or_raise("name")         # 取得（例外付き）
registry.unregister("name")           # 削除
registry.list_names()                 # 名前一覧
registry.list_all()                   # 全アイテム
```

**設計原則**：
- スレッドセーフ（`threading.Lock`）
- 型安全（`Generic[T]`）
- 重複登録警告（上書き可能）

### 2. AgentBlock 基底クラス

すべての Agent の基底：

```python
from agentflow.core.agent_block import AgentBlock

class MyAgent(AgentBlock):
    async def initialize(self) -> None:
        """初期化処理"""
        await super().initialize()

    async def run(self, input_data: dict) -> dict:
        """メイン処理（必須実装）"""
        return {"result": "..."}

    async def cleanup(self) -> None:
        """終了処理"""
        await super().cleanup()

# コンテキストマネージャー使用
async with MyAgent() as agent:
    result = await agent.run({"input": "data"})
```

### 3. 協調パターン

| パターン | クラス | 説明 |
|---------|--------|------|
| **DeepAgent**（推奨） | `DeepAgentCoordinator` | 智能型マルチAgent協調 |
| Reflection | `ReflectionWorkflow` | 自己改善ループ |
| Pipeline | `AgentPipeline` | 順次実行パイプライン |
| **Reflexion** | `ReflectiveEvolver` | 失敗学習パターン |
| **ResilientAgent**（NEW） | `ResilientAgent` | 信頼性強化Agent（回路遮断・リトライ） |

**DeepAgent パターン例**：
```python
from agentflow.patterns import DeepAgentCoordinator

coordinator = DeepAgentCoordinator(
    llm_client=llm,
    max_iterations=10,
    quality_threshold=75.0,
)
result = await coordinator.execute("市場調査レポート作成")
```

### 4. 統一サービス層（NEW）

API/CLI/Studio 全てが使用する統一バックエンド：

```python
from agentflow.services import AgentService, WorkflowService

# API向け: 結果のみ
result = await service.execute(agent_id="MyAgent", input_data={...})

# CLI向け: コールバック
result = await service.execute_with_callback(
    agent_id="MyAgent",
    on_progress=lambda pct, msg: print(f"[{pct}%] {msg}"),
)

# Studio向け: イベントストリーム
async for event in service.execute_stream(agent_id="MyAgent"):
    await websocket.send(event.to_json())
```

### 5. オーケストレーション層（NEW）

タスクの計画・実行・監視を分離したマルチエージェント協調システム：

```python
from agentflow.orchestration import Orchestrator

# オーケストレーターの初期化
orchestrator = Orchestrator(llm_client=llm, tool_provider=tools)
await orchestrator.initialize()

# タスクの実行
result = await orchestrator.execute(
    task="競合他社の価格を分析してレポートを生成",
    context={"market": "EC"},
    available_tools=["web_scraper", "data_analyzer"],
)

# ストリーミング実行（リアルタイム進捗）
async for event in orchestrator.execute_stream(task="..."):
    print(f"[{event['type']}] {event.get('message', '')}")
```

| コンポーネント | クラス | 役割 |
|--------------|--------|------|
| **Planner** | `PlannerAgent` | タスク分解、依存関係グラフ、実行計画生成 |
| **Executor** | `ExecutorAgent` | ステップ実行、リトライ、並行処理 |
| **Monitor** | `MonitorAgent` | リアルタイム監視、異常検出、アラート |
| **統合** | `Orchestrator` | 全体制御、動的再計画、エラー回復 |

### 6. 統一ツールプロバイダー（NEW）

Skills、MCP、組み込みツールを統一インターフェースで提供：

```python
from agentflow.providers import UnifiedToolProvider

provider = UnifiedToolProvider()
await provider.initialize(mcp_config={...})

# URI形式で呼び出し
result = await provider.call("skill://pdf-extractor", {"file": "doc.pdf"})
result = await provider.call("mcp://database/query", {"sql": "SELECT ..."})
result = await provider.call("builtin://calculator", {"expression": "1+1"})

# LLM向けツール定義を取得
tools_for_llm = provider.get_tools_for_llm()
```

| URIスキーム | 説明 | 例 |
|------------|------|-----|
| `skill://` | Skills（SKILL.md） | `skill://pdf-extractor` |
| `mcp://` | MCPツール | `mcp://filesystem/read` |
| `builtin://` | 組み込みツール | `builtin://calculator` |
| (なし) | 自動推論 | `calculator` → builtin |

### 7. グローバル状態管理（NEW）

Redux式の中央集権型状態管理システム：

```python
from agentflow.state import GlobalStateStore, create_action, ActionType, select

# ストアの作成
store = GlobalStateStore(initial_state={"context": {}})

# アクションのディスパッチ
store.dispatch(create_action(ActionType.UPDATE_PROGRESS, {"progress": 0.5}))
store.dispatch(create_action(ActionType.SET_CONTEXT, {"key": "value"}))

# 状態の取得
status = select(store.get_state(), "execution.status")
progress = store.get_state("execution.progress", default=0.0)

# 購読（状態変更時にコールバック）
unsubscribe = store.subscribe(
    callback=lambda state: print(f"Progress: {state['execution']['progress']}"),
    selector="execution.progress",
)

# スナップショット（ロールバック用）
snapshot_id = store.create_snapshot("before_critical_operation")
# ...操作...
store.restore_snapshot(snapshot_id)  # 問題発生時にロールバック
```

### 8. 宣言式Agent定義（NEW）

YAML/JSONでAgentを宣言的に定義：

```yaml
# agent-blueprint.yaml
name: market-analyzer
version: "1.0.0"
description: 市場分析エージェント

system_prompt: |
  あなたは市場分析の専門家です。
  データを収集・分析し、洞察を提供します。

skills:
  - name: web-scraper
    required: true
  - name: data-analyzer

tools:
  - uri: mcp://database/query
  - uri: builtin://calculator

memory:
  type: enhanced
  max_history: 100

safety:
  enable_hallucination_check: true
  enable_pii_detection: true

constraints:
  max_iterations: 10
  timeout_seconds: 300
  allowed_tools:
    - web-scraper
    - data-analyzer
```

```python
from agentflow.core import AgentBlueprint

# YAMLから読み込み
blueprint = AgentBlueprint.from_yaml("agent-blueprint.yaml")

# 検証
result = blueprint.validate()
if not result.is_valid:
    for error in result.errors:
        print(f"Error: {error}")

# Agentインスタンス化
agent = await blueprint.to_agent(llm_client=llm, tool_provider=tools)
result = await agent.run({"task": "EC市場を分析"})
```

### 9. Agent 発見機構（NEW）

大規模マルチエージェント環境での動的 Agent 発見・負荷分散システム：

```python
from agentflow.discovery import InMemoryAgentRegistry, AgentEntry, AgentStatus

# レジストリ初期化
registry = InMemoryAgentRegistry()

# Agent 登録
entry = AgentEntry(
    agent_id="analysis-agent-1",
    name="分析Agent",
    endpoint="http://localhost:8001",
    capabilities=["data_analysis", "report_generation"],
    metadata={"version": "1.0.0"},
)
await registry.register(entry)

# 能力による検索
agents = await registry.discover(capabilities=["data_analysis"])

# 負荷分散選択
selected = await registry.select_agent(
    capability="data_analysis",
    strategy="round_robin",  # または "random", "weighted"
)

# ヘルスチェック
await registry.heartbeat("analysis-agent-1")
```

| コンポーネント | クラス | 役割 |
|--------------|--------|------|
| **AgentEntry** | `AgentEntry` | Agent 登録情報（ID、能力、エンドポイント） |
| **AgentDiscovery** | `AgentDiscovery` | 発見機構基底クラス |
| **Registry** | `InMemoryAgentRegistry` | インメモリ実装 |
| **HealthChecker** | `HealthChecker` | 定期ヘルスチェック |

### 10. 富文本コンポーネント（共通モジュール）（NEW）

Agent レスポンスで使用する富文本構築システム（Studio/CLI/App 共通）：

```python
from agentflow import RichResponse, AlertType, ChartType

# Agent での富文本レスポンス構築
response = RichResponse()
response.add_markdown("# 分析結果")
response.add_table([
    {"name": "A", "value": 100},
    {"name": "B", "value": 200},
])
response.add_chart_from_data(
    data=[{"x": "A", "y": 10}, {"x": "B", "y": 20}],
    x_key="x",
    y_key="y",
    chart_type=ChartType.BAR,
)
response.add_alert("処理完了", AlertType.SUCCESS)

return response.to_dict()  # フロントエンドに送信
```

| コンポーネント | 説明 |
|--------------|------|
| `MarkdownContent` | Markdown テキスト |
| `CodeBlock` | コードブロック（シンタックスハイライト） |
| `DataTable` | テーブル（ソート・フィルタ・ページネーション） |
| `ChartView` | チャート（ECharts 互換） |
| `Alert` | アラート（info/success/warning/error） |
| `Citation` | 引用・ソース表示 |
| `CollapsibleSection` | 折りたたみセクション |
| `Tabs` | タブコンテナ |

### 11. Skills 自動進化システム

Claude Code Skills 完全互換の**自動進化能力システム**：

```
用户需求 → 技能匹配 → 存在なら実行
                   → 不在なら自動生成 → 検証 → 固化 → 実行
= 越用越厉害（使うほど強くなる）
```

**SKILL.md フォーマット**:
```markdown
---
name: pdf-extractor
description: PDFからテキストを抽出。PDF操作時に使用。
triggers: [pdf, extract text]
requirements: [pypdf]
tags: [document]
---
# Instructions
具体的な実行手順...
```

**自動進化エンジン**:
```python
from agentflow.skills import SkillEngine

engine = SkillEngine(auto_learn=True)

# マッチ or 自動生成
result = await engine.resolve("PDFを解析したい")
if result.generated:
    print(f"新スキル生成: {result.skill.name}")
```

| コンポーネント | 役割 |
|--------------|------|
| `SkillMatcher` | triggers/description でマッチング |
| `SkillGenerator` | LLM で新スキル自動生成 |
| `SkillValidator` | フォーマット・安全性検証 |
| `SkillPersister` | learned_skills へ固化 |
| `SkillEngine` | 統合インターフェース |

詳細は [Skills ガイド](guide-skills.md) を参照。

---

## 🛡️ AI安全防護システム（NEW）

LLM の弱点を補完し、信頼性の高い AI アプリケーションを構築するための防護機構。

### 幻覚検出（Hallucination Detection）

```python
from agentflow.security import HallucinationDetector

detector = HallucinationDetector()
result = await detector.check(
    output="専門家によると、GPT-4は2022年にリリースされた",
    context="GPT-4のリリース日に関する情報",
)

if not result.is_reliable:
    print(f"可信度: {result.confidence_score:.2f}")
    for issue in result.issues:
        print(f"- {issue.description}")
```

### 推理監視（Reasoning Monitor）

多步推理の安定性を保証：

```python
from agentflow.security import ReasoningMonitor, ReasoningStep

monitor = ReasoningMonitor(original_goal="売上データ分析")
monitor.add_constraint("外部APIへのアクセス禁止")

for step in reasoning_steps:
    result = monitor.check_step(step)
    if result.needs_correction:
        corrected = await monitor.suggest_correction()
        # 目標逸脱検出、無限ループ検出、制約違反検出
```

### データ脱敏（Data Sanitization）

```python
from agentflow.security import DataSanitizer

sanitizer = DataSanitizer()

# プロンプト注入検出
threats = sanitizer.check_prompt_injection(user_input)

# PII自動脱敏
result = sanitizer.sanitize("メール: test@example.com, 電話: 13812345678")
# → "メール: te***@example.com, 電話: 138****5678"
```

### 統一防護ファサード

```python
from agentflow.security import AISafetyGuard

guard = AISafetyGuard()

# 入力チェック（注入攻撃、脱獄攻撃、PII検出）
input_result = await guard.check_input(user_input)
if not input_result.is_safe:
    return "入力にセキュリティ脅威が検出されました"

# 出力チェック（幻覚検出、PII漏洩検出）
output_result = await guard.check_output(llm_output)
if output_result.needs_review:
    await notify_human_reviewer(output_result)
```

---

## 📦 プロトコルスタック

### AG-UI vs A2UI

| 比較 | AG-UI | A2UI |
|------|-------|------|
| **種別** | 通信プロトコル | UI 規範 |
| **役割** | イベント配信 | コンポーネント記述 |
| **類比** | HTTP | HTML |

### プロトコル選択ガイド

| シナリオ | 推奨プロトコル |
|---------|---------------|
| 外部ツール接続 | MCP |
| Agent 間協調 | A2A |
| UI 更新通知 | AG-UI |
| 動的 UI 生成 | A2UI (AG-UI 経由) |

---

## 📁 ディレクトリ構成

```
agentflow/
├── core/                 # コアモジュール
│   ├── agent_block.py    # Agent 基底クラス
│   ├── blueprint.py      # 宣言式Agent定義
│   ├── registry.py       # 統一レジストリ
│   ├── engine.py         # ワークフローエンジン
│   ├── error_response.py # RFC 7807 統一エラー
│   ├── constraint_validator.py # 制約検証
│   ├── dual_verifier.py  # 二重検証
│   ├── rollback_manager.py # ロールバック管理
│   ├── metadata.py       # メタデータ管理
│   ├── tool_definition.py # 統一ツール定義（v1.8.0 NEW）
│   ├── tool_registry.py  # ツールレジストリ（v1.8.0 NEW）
│   ├── capability_spec.py # Agent能力仕様（v1.8.0 NEW）
│   ├── agent_registry.py # Agentレジストリ（v1.8.0 NEW）
│   ├── tool_binding.py   # ツールバインディング（v1.8.0 NEW）
│   └── tool_discovery.py # ツール発見サービス（v1.8.0 NEW）
├── orchestration/        # オーケストレーション層（NEW）
│   ├── orchestrator.py   # 統合オーケストレーター
│   ├── planner.py        # 計画エージェント
│   ├── executor.py       # 実行エージェント
│   └── monitor.py        # 監視エージェント
├── state/                # 状態管理層（NEW）
│   ├── store.py          # GlobalStateStore
│   ├── actions.py        # アクション定義
│   └── selectors.py      # セレクター
├── services/             # 統一サービス層
│   ├── base.py           # ServiceBase, ServiceEvent
│   ├── agent_service.py  # Agent実行サービス
│   └── workflow_service.py # Workflow実行サービス
├── patterns/             # 協調パターン
│   ├── coordinator.py    # 協調器基底
│   ├── deep_agent.py     # DeepAgentCoordinator（推奨）
│   ├── reflection.py     # ReflectionWorkflow
│   ├── agent_pipeline.py # AgentPipeline
│   ├── task_decomposer.py # タスク分解（NEW）
│   └── reflexion.py      # Reflexion失敗学習
├── protocols/            # プロトコル実装
│   ├── mcp_client.py     # MCP クライアント
│   ├── a2a_server.py     # A2A サーバー
│   ├── agui_emitter.py   # AG-UI エミッター
│   └── a2ui/             # A2UI コンポーネント
│       └── rich_content.py # 富文本コンポーネント（共通）
├── discovery/            # Agent 発見機構（NEW）
│   ├── base.py           # AgentEntry, AgentDiscovery 基底
│   ├── registry.py       # InMemoryAgentRegistry
│   └── health.py         # HealthChecker
├── integrations/         # フレームワーク統合
│   ├── fastapi_integration.py  # FastAPI 統合
│   ├── sse_flow_runner.py      # SSE フロー実行
│   ├── websocket_integration.py # WebSocket
│   └── realtime_sync.py  # リアルタイム状態同期（NEW）
├── providers/            # 統一 Provider Layer
│   ├── llm_provider.py   # LLM Provider
│   ├── tool_provider.py  # Tool Provider
│   ├── unified_tool.py   # 統一ツールプロバイダー（NEW）
│   ├── tool_executor.py  # 並行ツール実行
│   ├── db_provider.py    # DB Provider
│   └── embedding_provider.py # Embedding Provider
├── memory/               # 記憶システム
│   ├── memory_manager.py # 統合マネージャー
│   ├── enhanced_memory.py # 記憶蒸留・主動遺忘
│   ├── sensory_memory.py # Light1: 感覚記憶
│   ├── short_term_memory.py # Light2: 短期記憶
│   ├── long_term_memory.py  # Light3: 長期記憶
│   └── vector_store.py   # ベクトル検索
├── sandbox/              # サンドボックス実行（NEW）
│   └── codeact_executor.py # CodeAct実行器
├── security/             # AI安全防護システム
│   ├── hallucination_detector.py # 幻覚検出
│   ├── reasoning_monitor.py # 推理監視
│   ├── data_sanitizer.py # データ脱敏
│   └── ai_safety_guard.py # 統一防護ファサード
├── engines/              # 簡易 Engine パターン
│   ├── simple_engine.py  # 単一Agent
│   ├── gate_engine.py    # 前置チェック
│   ├── pipeline_engine.py # パイプライン
│   └── rag_engine.py     # RAG増強
└── skills/               # スキル自動進化システム
    ├── base.py           # Skill 基底クラス
    ├── loader.py         # スキルローダー
    └── engine.py         # 統合エンジン

studio/                   # フロントエンド（React）
├── src/
│   ├── components/
│   │   └── rich-content/  # 富文本レンダラー（NEW）
│   │       ├── RichContentRenderer.tsx  # メインレンダラー
│   │       ├── types.ts   # TypeScript 型定義
│   │       └── renderers/ # 個別レンダラー
│   │           ├── MarkdownRenderer.tsx
│   │           ├── CodeBlockRenderer.tsx
│   │           ├── DataTableRenderer.tsx
│   │           ├── AlertRenderer.tsx
│   │           ├── CitationRenderer.tsx
│   │           ├── CollapsibleRenderer.tsx
│   │           └── TabsRenderer.tsx
│   └── ...
└── ...
```

---

## 🔍 VectorDB Provider（黒盒設計）

Agent/サービスは `get_vectordb()` のみを呼び出し、具体的な実装を意識しません。

### 対応プロバイダー

| タイプ | クラス | 特徴 |
|--------|--------|------|
| `faiss` | `FAISSProvider` | ローカル高速、GPU対応 |
| `qdrant` | `QdrantProvider` | 本番推奨、スケーラブル |
| `weaviate` | `WeaviateProvider` | セマンティック検索 |
| `supabase` | `SupabaseVectorProvider` | PostgreSQL pgvector |
| `chromadb` | `ChromaDBProvider` | ローカル開発（デフォルト） |

### 環境変数

```bash
VECTOR_DATABASE_TYPE=qdrant   # "faiss", "qdrant", "weaviate", "supabase", "chromadb"
QDRANT_URL=http://localhost:6333
WEAVIATE_URL=http://localhost:8080
SUPABASE_URL=https://xxx.supabase.co
SUPABASE_KEY=your-key
```

### 使用例

```python
from agentflow import get_vectordb

vdb = get_vectordb()  # 環境変数から自動選択
await vdb.connect()
await vdb.add(documents=["doc1"], embeddings=[[...]])
results = await vdb.search(query="query", query_embedding=[...], top_k=5)
```

---

## 🧩 拡張ポイント

| 拡張対象 | 方法 |
|---------|------|
| カスタム Coordinator | `CoordinatorBase` 継承 |
| カスタム Skill | `Skill.load()` で SKILL.md 読み込み |
| カスタム A2UI | `A2UIComponent` 継承 |
| カスタム Protocol | `ProtocolRegistry` に登録 |
| カスタム VectorDB | `VectorDBProvider` プロトコル実装 |
| カスタム幻覚パターン | `HallucinationDetector.add_pattern()` |
| カスタム脱敏パターン | `DataSanitizer.add_injection_pattern()` |
| カスタム ResilientAgent | `ResilientAgent` 継承、検証ロジック追加 |
| カスタム BuiltinTool | `UnifiedToolProvider.builtin.register()` |
| カスタム StateAction | `ActionType` に追加、`_reduce` に処理追加 |
| カスタム Blueprint | YAML拡張、`AgentBlueprint` カスタマイズ |
| カスタム AgentRegistry | `AgentDiscovery` 継承（Redis/Consul等） |
| カスタム RichComponent | `RichComponent` 継承、フロントエンドレンダラー追加 |

---

## 🤖 Auto-Agent アーキテクチャ（v1.8.0 NEW）

統一ツール・Agentレジストリを通じた、自律的Agent分析と自動Agent生成の基盤システム。

### 設計原則

| 原則 | 説明 |
|------|------|
| **高度抽象化** | ツールソース（MCP/Skills/Builtin）を統一インターフェースで表現 |
| **低結合** | レジストリはインターフェースであり、具体実装に依存しない |
| **高凝集** | 各モジュールは単一責任を持つ |
| **拡張容易** | 新しいツールソースは `ToolDefinition.from_*()` を実装するだけ |

### コアコンポーネント

| コンポーネント | クラス | 役割 |
|--------------|--------|------|
| **ToolDefinition** | `ToolDefinition` | 統一ツール表現（URI、スキーマ、メタデータ） |
| **ToolRegistry** | `ToolRegistry` | ツール登録・検索・フィルタリング |
| **AgentCapabilitySpec** | `AgentCapabilitySpec` | Agent能力宣言（ツール/LLM要件） |
| **AgentRegistry** | `AgentRegistry` | Agent能力登録・マッチング・ファクトリ |
| **ToolBinder** | `ToolBinder` | ランタイムツールバインディング |
| **ToolDiscoveryService** | `ToolDiscoveryService` | 全ソースからツール発見 |

### URI スキーム

| スキーム | 説明 | 例 |
|----------|------|-----|
| `tool://builtin/` | ビルトインツール | `tool://builtin/calculator` |
| `tool://mcp/` | MCPサーバーツール | `tool://mcp/filesystem/read_file` |
| `tool://skill/` | Skillsツール | `tool://skill/summarize` |
| `tool://dynamic/` | 動的生成ツール | `tool://dynamic/custom_tool` |

### 使用例

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
service.register_builtin(
    name="search",
    description="ドキュメント検索",
    input_schema={"type": "object", "properties": {"query": {"type": "string"}}},
)

# Step 2: Agent能力定義・登録
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

# Step 4: ツールをバインド
factory = agent_registry.get_factory(best_agent_id)
agent = factory()
binder = ToolBinder(tool_registry)
bound_agent = await binder.bind_for_capability(agent, capability)

# Step 5: バインドされたツールをLLMに渡す
mcp_tools = bound_agent._tools.to_mcp_format()
```

詳細は [Auto-Agent アーキテクチャ詳細](auto-agent-architecture.md) を参照。

---

## 📚 関連ドキュメント

- [Auto-Agent アーキテクチャ](auto-agent-architecture.md) - 統一ツール・Agentレジストリ詳細（NEW）
- [Skills ガイド](guide-skills.md) - 自動進化システム詳細
- [プロトコル詳細](protocols.md) - MCP/A2A/AG-UI/A2UI の使用方法
- [API リファレンス](api.md) - 全 API 詳細
- [クイックスタート](quickstart.md) - 10分で始める
- [CLI リファレンス](cli.md) - CLI コマンド一覧
- [AI安全防護ガイド](guide-ai-safety.md) - 幻覚検出・推理監視・データ脱敏
- [フレームワークビジョン](design/FRAMEWORK_VISION.md) - 設計思想・ロードマップ
- [パターンガイド](PATTERNS_GUIDE.md) - 協調パターン詳細
- [富文本レンダラー設計](design/RICH_CONTENT_RENDERER_DESIGN.md) - フロントエンド富文本コンポーネント
- [Agent発見機構設計](design/AGENT_DISCOVERY_DESIGN.md) - 動的Agent発見・負荷分散

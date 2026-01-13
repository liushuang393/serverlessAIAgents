# DeepAgentCoordinator 設計ドキュメント

> バージョン: 2.0.0
> 日付: 2026-01-13
> ステータス: **実装完了**

## 1. 概要

### 1.1 目標

LangChain DeepAgents の思想に基づき、AgentFlow フレームワークに**統一パターン**を追加。以下を統合：

- 認知分析（動機理解、意図判定）
- 動的タスク分解（TodoList計画管理）
- 動的Agent分配（事前定義 + 実行時生成）
- ツール/Skills/MCPバインディング
- 進捗/メモリ/通信の標準化管理
- 品質評審とフィードバックループ
- 自己進化（成功学習 + 顧客フィードバック）

### 1.2 設計原則

| 原則 | 説明 |
|------|------|
| **易用性** | シンプルAPI、1行コードで深度Agent起動 |
| **拡張性** | プラグイン化コンポーネント、カスタムAgent/Tools/Skills |
| **堅牢性** | エラー回復、リトライ機構、優雅な降格 |
| **モダン** | async/await、型ヒント、Pydanticモデル |

## 2. アーキテクチャ設計

### 2.1 実行フロー

```
ユーザーリクエスト
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│  Phase 1: 認知分析 (CognitiveAnalyzer)                      │
│  - ユーザー意図を解析、複雑度を判断                          │
│  - 不明確な場合はClarification追加質問                       │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│  Phase 2: タスク分解 (TaskDecomposer)                       │
│  - TodoList生成（タスク、依存関係、優先度）                  │
│  - 並行実行可能なタスクグループを識別                        │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│  Phase 3: リソース割当 (AgentPool + ToolRegistry)           │
│  - Agentをマッチング/生成                                   │
│  - Tools、Skills、MCPをバインド                             │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│  Phase 4: 実行 (Executor)                                   │
│  - 直列/並列でタスク実行                                     │
│  - 進捗追跡、Agent間通信、コンテキスト管理                   │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│  Phase 5: 品質評審 (QualityReviewer)                        │
│  - 結果品質を評価（多次元スコアリング）                      │
│  - 不合格 → Phase 2へフィードバック、再計画                 │
│  - 合格 → Phase 6へ                                         │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│  Phase 6: 進化 (Evolver)                                    │
│  - 成功パターン学習                                          │
│  - 顧客フィードバック処理（提案/指摘/教育）                  │
│  - スキル固定化                                              │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
出力結果
```

### 2.2 コアコンポーネント（実装完了）

| コンポーネント | 責務 | 主要機能 | 状態 |
|----------------|------|----------|------|
| **CognitiveAnalysis** | 認知分析 | 意図識別、複雑度判断、領域識別 | ✅ |
| **TodoItem** | タスク管理 | TodoList生成、依存分析、並行グループ | ✅ |
| **AgentPool** | Agent管理 | 6個の事前定義Agent + 動的生成 | ✅ |
| **ToolProvider** | ツール管理 | @tool登録、自動発見 | ✅ |
| **SkillRegistry** | スキル管理 | SKILL.md互換、動的バインド | ✅ |
| **MCPClient** | MCP統合 | 外部ツール接続 | ✅ |
| **ProgressManager** | 進捗管理 | 追跡、通信、メモリ | ✅ |
| **ContextCompressor** | コンテキスト圧縮 | 選択的/要約/階層的圧縮 | ✅ |
| **QualityReview** | 品質評審 | 多次元スコア、問題識別、リトライ判定 | ✅ |
| **Evolver** | 進化システム | パターン学習、フィードバック処理 | ✅ |

### 2.3 事前定義Agent（6種類）

| Agent | 責務 | 典型ツール |
|-------|------|----------|
| **ResearchAgent** | 調査、情報収集 | 検索、RAG、Web検索 |
| **AnalysisAgent** | データ分析、推論 | 計算、統計 |
| **PlanningAgent** | 戦略計画、方案設計 | - |
| **ExecutionAgent** | タスク実行、操作 | ファイル、API呼び出し |
| **ReviewAgent** | 審査、検証 | ルールチェック |
| **ReportAgent** | レポート、要約 | テンプレート、フォーマット |

## 3. データモデル

### 3.1 TodoItem（タスク項目）

```python
class TodoItem(BaseModel):
    """タスク項目."""
    id: str = Field(default_factory=lambda: str(uuid4()))
    task: str                        # タスク説明
    agent_type: AgentType = AgentType.EXECUTION
    status: TaskStatus = TaskStatus.PENDING
    priority: int = 0                # 優先度（高→低）
    depends_on: list[str] = []       # 依存タスクID
    tools: list[str] = []            # バインドツール
    skills: list[str] = []           # バインドスキル
    result: dict | None = None       # 実行結果
    error: str | None = None         # エラー情報

    def is_ready(self, completed_ids: set[str]) -> bool:
        """依存関係が満たされているか確認."""
        return all(dep in completed_ids for dep in self.depends_on)
```

### 3.2 通信メッセージ（標準化）

```python
class AgentMessage(BaseModel):
    """Agent間メッセージ."""
    id: str = Field(default_factory=lambda: str(uuid4()))
    from_agent: str                  # 送信Agent
    to_agent: str                    # 受信Agent（"*"=ブロードキャスト）
    msg_type: MessageType            # RESULT/REQUEST/NOTIFY/ERROR
    content: dict                    # メッセージ内容
    timestamp: datetime              # タイムスタンプ
    priority: int = 0                # 優先度
```

### 3.3 品質評審（多次元）

```python
class QualityDimension(str, Enum):
    """品質評審次元."""
    COMPLETENESS = "completeness"    # 完全性
    ACCURACY = "accuracy"            # 正確性
    CONSISTENCY = "consistency"      # 一貫性
    EFFICIENCY = "efficiency"        # 効率性
    CLARITY = "clarity"              # 明確性

class QualityReview(BaseModel):
    """品質評審結果."""
    is_acceptable: bool              # 合格判定
    score: float                     # 総合スコア（0-100）
    dimension_scores: dict[str, float]  # 次元別スコア
    verdict: str                     # pass/revise/reject
    issues: list[str]                # 問題点
    suggestions: list[str]           # 改善提案
    confidence: float                # 評価信頼度
```

## 4. API設計

### 4.1 基本使用

```python
from agentflow.patterns import DeepAgentCoordinator

# 最もシンプルな使用法
coordinator = DeepAgentCoordinator(llm_client=my_llm)
result = await coordinator.execute("市場トレンドを分析し、投資提案を行う")
```

### 4.2 カスタムAgent

```python
# カスタムAgentを登録
coordinator.register_agent("financial_expert", my_financial_agent)

# カスタム設定を使用
coordinator = DeepAgentCoordinator(
    llm_client=my_llm,
    predefined_agents={
        "research": MyResearchAgent(),
        "analysis": MyAnalysisAgent(),
    },
)
```

### 4.3 ツールバインディング

```python
# AgentPoolでツールを自動発見
pool = AgentPool(
    llm_client=my_llm,
    tool_provider=ToolProvider.discover(),  # @tool登録を自動発見
)

# MCPツールを接続
await pool.connect_mcp()
mcp_tools = pool.get_mcp_tools()
```

### 4.4 MCP統合

```python
from agentflow.patterns.deep_agent import MCPConfig, AgentPool

# MCP設定
mcp_config = MCPConfig(
    server_command="npx @anthropic/filesystem-mcp",
    server_args=["--root", "/workspace"],
)

# AgentPoolでMCP接続
pool = AgentPool(mcp_config=mcp_config)
await pool.connect_mcp()
```

### 4.5 進捗コールバック

```python
def on_progress(event: dict):
    print(f"[{event['event']}] {event['data']}")

coordinator = DeepAgentCoordinator(
    llm_client=my_llm,
    on_progress=on_progress,  # リアルタイム進捗
)
```

### 4.6 フィードバック学習

```python
# 顧客フィードバックを処理
await coordinator.process_feedback(
    feedback_type="education",  # suggestion/correction/education
    content="投資提案にはリスク要因を考慮する必要がある",
)

# 進化統計を取得
stats = coordinator.get_stats()
print(stats["evolution"]["learned_patterns"])
```

## 5. 既存システムとの統合

### 5.1 Decision Governance Engineとの統合（実装完了）

```python
# apps/decision_governance_engine/engine.py
from agentflow.patterns import DeepAgentCoordinator
from apps.decision_governance_engine.services.deep_agent_adapter import DeepAgentAdapter

class DecisionEngine(PipelineEngine):
    def __init__(self, llm_client=None, enable_deep_agent=True):
        # DeepAgentAdapterを統合
        if enable_deep_agent:
            self._deep_adapter = DeepAgentAdapter(
                llm_client=llm_client,
                enable_evolution=True,
            )

        # 認知分析
        cognitive = await self._deep_adapter.analyze_cognitive(question)

        # 成功パターン学習
        await self._deep_adapter.record_success(task, result)

        # 品質評審
        review = await self._deep_adapter.quality_review(task, results)
```

### 5.2 PipelineEngineとの共存

```python
# 2つの方式が共存可能
# 方式1: シンプルなフローにはPipelineEngine
simple_pipeline = PipelineEngine(stages=[...])

# 方式2: 複雑なフローにはDeepAgentCoordinator
complex_coordinator = DeepAgentCoordinator(...)

# 方式3: PipelineEngine + DeepAgentAdapter（推奨）
engine = DecisionEngine(enable_deep_agent=True)
```

## 6. 実装状況

### 6.1 タスク完了状況

| フェーズ | タスク | 状態 |
|----------|--------|------|
| **P1** | コアフレームワーク | ✅ 完了 |
| 1.1 | データモデル完善（Pydantic） | ✅ |
| 1.2 | CognitiveAnalysis実装 | ✅ |
| 1.3 | TaskDecomposer実装 | ✅ |
| 1.4 | AgentPool実装（並行実行） | ✅ |
| 1.5 | ストレージ層抽象（RuntimeStore/EvolutionStore） | ✅ |
| **P2** | ツール統合 | ✅ 完了 |
| 2.1 | ToolProvider実装 | ✅ |
| 2.2 | Skills統合（SkillRegistry） | ✅ |
| 2.3 | MCPコネクタ（MCPClient） | ✅ |
| **P3** | 進捗管理 | ✅ 完了 |
| 3.1 | ProgressManager完善 | ✅ |
| 3.2 | コンテキスト圧縮（ContextCompressor） | ✅ |
| 3.3 | 標準化通信プロトコル（AgentMessage） | ✅ |
| **P4** | 評審進化 | ✅ 完了 |
| 4.1 | QualityReview完善（多次元評価） | ✅ |
| 4.2 | Evolver完善（永続化統合） | ✅ |
| **P5** | 統合テスト | ✅ 完了 |
| 5.1 | 全コンポーネント統合テスト | ✅ |
| 5.2 | Decision Engine統合（DeepAgentAdapter） | ✅ |

### 6.2 ファイル構造（現在）

```
agentflow/patterns/
├── __init__.py               # 統一エクスポート
├── deep_agent.py             # DeepAgentCoordinator（単一ファイル、2200行）
│   ├── データモデル          # TodoItem, AgentMessage, QualityReview等
│   ├── ストレージ抽象        # RuntimeStore, EvolutionStore
│   ├── ContextCompressor     # コンテキスト圧縮
│   ├── AgentPool             # Agent管理 + Tools/Skills/MCP
│   ├── ProgressManager       # 進捗管理
│   ├── Evolver               # 進化システム
│   └── DeepAgentCoordinator  # メイン協調器
└── ...                       # 他のパターン

apps/decision_governance_engine/services/
└── deep_agent_adapter.py     # Decision Engine統合アダプター
```

## 7. ストレージ階層設計（実装完了）

### 7.1 三層ストレージアーキテクチャ

| 層 | 名称 | ライフサイクル | 保存方式 | 責任 | 状態 |
|----|------|----------------|----------|------|------|
| **L3** | Framework Evolution | 永久 | DB | フレームワーク | ✅ |
| **L2** | App Session | 業務永続 | DB | App | App実装 |
| **L1** | Runtime | 一時 | Memory/Redis | フレームワーク | ✅ |

### 7.2 Layer 3: EvolutionStore（フレームワーク級）

**目的**：フレームワークを使うほど強くなる、App間で学習成果を共有

```python
class EvolutionStore(ABC):
    """進化ストレージ抽象."""

    @abstractmethod
    async def save_pattern(self, pattern_key: str, data: dict) -> None: ...
    @abstractmethod
    async def load_pattern(self, pattern_key: str) -> dict | None: ...
    @abstractmethod
    async def list_patterns(self) -> list[str]: ...
    @abstractmethod
    async def save_feedback(self, feedback: dict) -> None: ...
    @abstractmethod
    async def get_feedbacks(self, limit: int = 100) -> list[dict]: ...

# メモリ実装（デフォルト）
class MemoryEvolutionStore(EvolutionStore): ...
```

### 7.3 Layer 1: RuntimeStore（一時）

**目的**：実行期間中の状態管理

```python
class RuntimeStore(ABC):
    """ランタイムストレージ抽象."""

    @abstractmethod
    async def save_context(self, key: str, data: dict) -> None: ...
    @abstractmethod
    async def load_context(self, key: str) -> dict | None: ...
    @abstractmethod
    async def save_checkpoint(self, checkpoint_id: str, state: dict) -> None: ...
    @abstractmethod
    async def load_checkpoint(self, checkpoint_id: str) -> dict | None: ...
    @abstractmethod
    async def list_checkpoints(self) -> list[str]: ...

# メモリ実装（デフォルト）
class MemoryRuntimeStore(RuntimeStore): ...
```

## 8. リスクと対策

| リスク | 影響 | 対策 | 状態 |
|--------|------|------|------|
| LLM応答不安定 | タスク分解失敗 | デフォルト分解+リトライ | ✅ 実装済み |
| コンテキスト過長 | Token超過 | 自動圧縮（ContextCompressor） | ✅ 実装済み |
| 循環依存 | デッドロック | 依存検出+タイムアウト | ✅ 実装済み |
| 動的Agent品質 | 結果不良 | テンプレート最適化+評審 | ✅ 実装済み |

## 9. 今後の拡張

- [ ] 可視化Dashboard
- [ ] 分散実行サポート
- [ ] A/Bテスト機能
- [ ] 追加MCPアダプター
- [ ] Redis/PostgreSQLストレージ実装

---

## 10. エクスポート一覧

### 10.1 データモデル - 状態・種別

```python
from agentflow.patterns.deep_agent import (
    TaskStatus,          # PENDING/IN_PROGRESS/COMPLETED/FAILED
    AgentType,           # RESEARCH/ANALYSIS/PLANNING/EXECUTION/REVIEW/REPORT
    MessageType,         # RESULT/REQUEST/NOTIFY/ERROR
    CompactionStrategy,  # SELECTIVE/SUMMARIZE/HIERARCHICAL
    QualityDimension,    # COMPLETENESS/ACCURACY/CONSISTENCY/EFFICIENCY/CLARITY
)
```

### 10.2 データモデル - 構造体

```python
from agentflow.patterns.deep_agent import (
    TodoItem,            # タスク項目
    CognitiveAnalysis,   # 認知分析結果
    QualityReview,       # 品質評審結果（多次元）
    EvolutionRecord,     # 進化記録
    AgentMessage,        # Agent間メッセージ
    ParallelGroup,       # 並行実行グループ
    MemoryTier,          # メモリ階層
    CompactionResult,    # 圧縮結果
)
```

### 10.3 ストレージ抽象

```python
from agentflow.patterns.deep_agent import (
    RuntimeStore,        # ランタイムストレージ抽象
    EvolutionStore,      # 進化ストレージ抽象
    MemoryRuntimeStore,  # メモリ実装（L1）
    MemoryEvolutionStore,# メモリ実装（L3）
)
```

### 10.4 コンポーネント

```python
from agentflow.patterns.deep_agent import (
    ContextCompressor,   # コンテキスト圧縮
    AgentPool,           # Agent管理 + Tools/Skills/MCP
    DynamicAgent,        # 動的Agent
    ProgressManager,     # 進捗管理
    Evolver,             # 進化システム
    DeepAgentCoordinator,# メイン協調器
)
```

---

## 11. 使用例

### 11.1 基本使用

```python
import asyncio
from agentflow.patterns.deep_agent import DeepAgentCoordinator

async def main():
    coordinator = DeepAgentCoordinator(llm_client=my_llm)
    result = await coordinator.execute("市場分析を行い、投資提案を作成")
    print(result)

asyncio.run(main())
```

### 11.2 Decision Engine統合

```python
from apps.decision_governance_engine.engine import DecisionEngine

engine = DecisionEngine(enable_deep_agent=True)

# 認知分析
cognitive = await engine.analyze_cognitive("新規事業への投資判断")

# 成功学習
await engine.record_success(task, result)

# 統計取得
stats = engine.get_deep_stats()
```

---

## 12. LangChain DeepAgents との比較

| 機能 | DeepAgents | AgentFlow | 状態 |
|------|------------|-----------|------|
| 認知分析 | ✅ | ✅ CognitiveAnalysis | 実装済み |
| タスク分解 | ✅ | ✅ TodoItem + 依存関係 | 実装済み |
| 動的Agent | ✅ | ✅ AgentPool | 実装済み |
| ツール統合 | ✅ | ✅ ToolProvider + MCP | 実装済み |
| コンテキスト圧縮 | ✅ | ✅ ContextCompressor | 実装済み |
| 品質評審 | ✅ | ✅ QualityReview（多次元） | 実装済み |
| 自己進化 | ✅ | ✅ Evolver | 実装済み |
| Virtual Filesystem | ✅ | ❌ | 今後対応 |
| Checkpoint/復元 | ✅ | △ RuntimeStore設計済み | 今後対応 |

---

## 13. アーキテクチャ図

```
                        ┌─────────────────────────────────────────────────┐
                        │              DeepAgentCoordinator               │
                        │  ┌─────────────────────────────────────────────┐│
                        │  │    認知分析 → タスク分解 → 実行 → 評審 → 進化 ││
                        │  └─────────────────────────────────────────────┘│
                        └─────────────────────────────────────────────────┘
                                              │
           ┌──────────────────────────────────┼──────────────────────────────────┐
           │                                  │                                  │
           ▼                                  ▼                                  ▼
┌─────────────────────┐         ┌─────────────────────┐         ┌─────────────────────┐
│    Storage Layer    │         │   Context Layer     │         │   Agent Layer       │
├─────────────────────┤         ├─────────────────────┤         ├─────────────────────┤
│ RuntimeStore        │         │ ContextCompressor   │         │ AgentPool           │
│ ├─MemoryRuntime     │         │ ├─Selective         │         │ ├─6 PreDefined      │
│ └─(Redis/Postgres)  │         │ ├─Summarize         │         │ ├─Dynamic Gen       │
├─────────────────────┤         │ └─Hierarchical      │         │ └─Tools/Skills/MCP  │
│ EvolutionStore      │         ├─────────────────────┤         └─────────────────────┘
│ ├─MemoryEvolution   │         │ ProgressManager     │
│ └─(Postgres)        │         │ └─AgentMessage      │
└─────────────────────┘         └─────────────────────┘
                                              │
                        ┌─────────────────────┴─────────────────────┐
                        │           Decision Engine Integration      │
                        │  ┌─────────────────────────────────────┐  │
                        │  │ DeepAgentAdapter                    │  │
                        │  │ ├─analyze_cognitive()               │  │
                        │  │ ├─compress_context()                │  │
                        │  │ ├─record_success()                  │  │
                        │  │ └─quality_review()                  │  │
                        │  └─────────────────────────────────────┘  │
                        └───────────────────────────────────────────┘
```

---

*ドキュメント更新日: 2026-01-13*
*バージョン: 2.0.0 - 実装完了*

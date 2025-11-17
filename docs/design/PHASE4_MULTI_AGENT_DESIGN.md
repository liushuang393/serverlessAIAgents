# Phase 4: Multi-Agent Collaboration Pattern 設計書

## 目標

**現状**: 50%  
**目標**: 85%

Multi-Agent Collaboration Pattern を AgentFlow アーキテクチャで実装し、複数の専門 Agent が協調作業できるようにする。

---

## 業界最佳実践の研究成果

### 参考資料
1. **Azure Architecture - AI Agent Orchestration Patterns**
   - Sequential: 順次実行
   - Concurrent: 並行実行
   - Handoff: 動的委譲
   - Group Chat: グループ討論

2. **OpenAI Multi-Agent Collaboration**
   - Handoff vs Agent-as-Tool
   - 専門化による分業
   - 明確な責任分担

### 核心思想（吸収するもの）

✅ **吸収する思想**：
- **専門化**：各 Agent が特定ドメインに特化
- **協調パターン**：Sequential, Concurrent, Handoff
- **動的ルーティング**：タスクに応じて最適な Agent を選択
- **状態共有**：Agent 間でコンテキストを共有

❌ **引入しないもの**：
- AutoGen の GroupChat クラス
- LangChain の MultiAgentExecutor
- 外部フレームワークの依存

---

## AgentFlow 風の設計

### 核心コンポーネント

```
agentflow/patterns/multi_agent.py
├── AgentRouter (AgentBlock)         # Agent ルーティング
├── AgentCoordinator                 # 協調制御
├── SharedContext                    # 共有コンテキスト
└── MultiAgentWorkflow               # WorkflowConfig 工厂
```

### 協調パターン

#### 1. Sequential Collaboration（順次協調）
```
Input → Agent A → Agent B → Agent C → Result
```
**用途**：依存関係がある処理

#### 2. Concurrent Collaboration（並行協調）
```
Input → ┬→ Agent A → Result A ┬→ Aggregated Result
        ├→ Agent B → Result B ┤
        └→ Agent C → Result C ┘
```
**用途**：独立した並行処理

#### 3. Handoff Collaboration（委譲協調）
```
Input → Agent A → (判断) → Agent B → (判断) → Agent C → Result
```
**用途**：動的な専門家選択

### 設計原則

1. **簡単**：AgentBlock ベース、理解しやすい
2. **柔軟**：複数の協調パターンをサポート
3. **健壮**：Agent 障害時の fallback
4. **独立**：外部フレームワーク不要

---

## 実装計画

### Task 4.1: SharedContext 実装

**目的**：Agent 間で状態を共有

**実装内容**：
```python
class SharedContext:
    """Agent 間の共有コンテキスト."""
    
    def __init__(self) -> None:
        """初期化."""
        self._data: dict[str, Any] = {}
        self._history: list[dict[str, Any]] = []
    
    def set(self, key: str, value: Any) -> None:
        """値を設定."""
        self._data[key] = value
        self._history.append({
            "action": "set",
            "key": key,
            "value": value,
            "timestamp": datetime.now(),
        })
    
    def get(self, key: str, default: Any = None) -> Any:
        """値を取得."""
        return self._data.get(key, default)
    
    def get_history(self) -> list[dict[str, Any]]:
        """履歴を取得."""
        return self._history.copy()
```

### Task 4.2: AgentRouter 実装

**目的**：タスクに応じて最適な Agent を選択

**実装内容**：
```python
class AgentRouter(AgentBlock):
    """Agent ルーティング - タスクに応じて Agent を選択."""
    
    def __init__(
        self,
        agents: dict[str, AgentBlock],
        llm_client: Any = None,
        **kwargs: Any,
    ):
        super().__init__(**kwargs)
        self._agents = agents
        self._llm = llm_client
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """最適な Agent を選択して実行.
        
        Returns:
            {
                "selected_agent": str,
                "result": Any,
                "reasoning": str
            }
        """
```

**ルーティング Prompt テンプレート**：
```
以下のタスクを処理するのに最適な Agent を選択してください：

【タスク】
{task}

【利用可能な Agent】
{available_agents}

最適な Agent の名前とその理由を説明してください。
```

### Task 4.3: AgentCoordinator 実装

**目的**：複数 Agent の協調を制御

**実装内容**：
```python
class AgentCoordinator:
    """Agent 協調制御."""
    
    def __init__(
        self,
        agents: list[AgentBlock],
        pattern: str = "sequential",  # sequential, concurrent, handoff
        shared_context: SharedContext | None = None,
    ):
        self._agents = agents
        self._pattern = pattern
        self._context = shared_context or SharedContext()
    
    async def execute(self, task: str) -> dict[str, Any]:
        """協調パターンに従って実行."""
        if self._pattern == "sequential":
            return await self._execute_sequential(task)
        elif self._pattern == "concurrent":
            return await self._execute_concurrent(task)
        elif self._pattern == "handoff":
            return await self._execute_handoff(task)
        else:
            raise ValueError(f"Unknown pattern: {self._pattern}")
    
    async def _execute_sequential(self, task: str) -> dict[str, Any]:
        """順次実行."""
        result = {"task": task}
        for agent in self._agents:
            result = await agent.run(result)
            self._context.set(f"agent_{agent.__class__.__name__}", result)
        return result
    
    async def _execute_concurrent(self, task: str) -> dict[str, Any]:
        """並行実行."""
        tasks = [agent.run({"task": task}) for agent in self._agents]
        results = await asyncio.gather(*tasks)
        return {"results": results}
    
    async def _execute_handoff(self, task: str) -> dict[str, Any]:
        """委譲実行."""
        current_input = {"task": task}
        for agent in self._agents:
            result = await agent.run(current_input)
            if result.get("handoff_to"):
                # 次の Agent に委譲
                continue
            else:
                # 完了
                return result
        return current_input
```

### Task 4.4: MultiAgentWorkflow 実装

**目的**：WorkflowConfig 工厂

**実装内容**：
```python
class MultiAgentWorkflow:
    """Multi-Agent Workflow 工厂."""
    
    @staticmethod
    def create(
        workflow_id: str,
        agents: list[AgentBlock],
        pattern: str = "sequential",
        llm_client: Any = None,
    ) -> WorkflowConfig:
        """Multi-Agent Workflow を作成."""
```

---

## 成功指標

- [ ] SharedContext 実装完了
- [ ] AgentRouter 実装完了
- [ ] AgentCoordinator 実装完了（3パターン）
- [ ] MultiAgentWorkflow 実装完了
- [ ] 15+ 単体テスト、全て合格
- [ ] テストカバレッジ 85%+
- [ ] Sequential/Concurrent/Handoff 全てサポート
- [ ] Agent 障害時の fallback 動作確認

---

## 使用例

```python
from agentflow.patterns import MultiAgentWorkflow, AgentCoordinator
from agentflow.core.engine import AgentFlowEngine

# 専門 Agent を定義
research_agent = ResearchAgent()
analysis_agent = AnalysisAgent()
report_agent = ReportAgent()

# Sequential パターン
workflow = MultiAgentWorkflow.create(
    workflow_id="research-pipeline",
    agents=[research_agent, analysis_agent, report_agent],
    pattern="sequential",
)

# 実行
engine = AgentFlowEngine()
engine.register_workflow(workflow)
result = await engine.execute("research-pipeline", {"task": "AI 市場調査"})
```


# Phase 3: Reflection Pattern 設計書

## 目標

**現状**: 0%  
**目標**: 85%

Reflection Pattern を AgentFlow アーキテクチャで実装し、AI Agent が自己評価・改善を行えるようにする。

---

## 業界最佳実践の研究成果

### 参考資料
1. **Analytics Vidhya - Reflection Pattern**
   - 核心概念：Generate → Reflect → Iterate
   - 3つのステップ：生成、自己批評、改善
   - 適用場景：コンテンツ品質向上、エラー修正

2. **Anthropic - Self-Correction**
   - Chain-of-thought で推論過程を可視化
   - 明確な評価基準を設定
   - 最大反復回数を制限

### 核心思想（吸収するもの）

✅ **吸収する思想**：
- **Generate-Reflect-Iterate ループ**：生成 → 自己評価 → 改善の反復
- **明確な評価基準**：何を「良い」とするかを定義
- **反復制限**：無限ループ防止
- **改善履歴の記録**：各反復の結果を保存

❌ **引入しないもの**：
- LangChain の ReflectionAgent クラス
- AutoGen の ReflectionWorkflow
- 外部フレームワークの依存

---

## AgentFlow 風の設計

### 核心コンポーネント

```
agentflow/patterns/reflection.py
├── ReflectorAgent (AgentBlock)      # 自己評価 Agent
├── ImproverAgent (AgentBlock)       # 改善 Agent  
├── ReflectionLoop                   # 反復制御
└── ReflectionWorkflow               # WorkflowConfig 工厂
```

### アーキテクチャ

```
Input (初期タスク)
    ↓
GeneratorAgent (生成)
    ↓
ReflectorAgent (評価) ←─┐
    ↓                    │
is_acceptable?           │
    ├─ Yes → Result      │
    └─ No → ImproverAgent ┘
            (改善)
```

### 設計原則

1. **簡単**：AgentBlock ベース、理解しやすい
2. **柔軟**：WorkflowConfig で組み合わせ
3. **健壮**：最大反復回数で無限ループ防止
4. **独立**：外部フレームワーク不要

---

## 実装計画

### Task 3.1: ReflectorAgent 実装

**目的**：生成結果を評価する Agent

**実装内容**：
```python
class ReflectorAgent(AgentBlock):
    """自己評価 Agent - 生成結果を評価."""
    
    def __init__(
        self,
        llm_client: Any = None,
        evaluation_criteria: dict[str, Any] | None = None,
        **kwargs: Any,
    ):
        super().__init__(**kwargs)
        self._llm = llm_client
        self._criteria = evaluation_criteria or {}
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """生成結果を評価.
        
        Returns:
            {
                "is_acceptable": bool,
                "score": float,
                "feedback": str,
                "suggestions": list[str]
            }
        """
```

**評価 Prompt テンプレート**：
```
あなたは品質評価の専門家です。以下の結果を評価してください：

【生成結果】
{generated_output}

【評価基準】
{evaluation_criteria}

以下の形式で評価してください：
1. 合格判定：Yes/No
2. スコア：0-100
3. フィードバック：具体的な問題点
4. 改善提案：どう改善すべきか
```

### Task 3.2: ImproverAgent 実装

**目的**：フィードバックに基づいて改善する Agent

**実装内容**：
```python
class ImproverAgent(AgentBlock):
    """改善 Agent - フィードバックに基づいて改善."""
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """結果を改善.
        
        Input:
            - original_output: 元の生成結果
            - feedback: ReflectorAgent からのフィードバック
            - suggestions: 改善提案
        
        Returns:
            - improved_output: 改善後の結果
        """
```

### Task 3.3: ReflectionLoop 実装

**目的**：反復制御ロジック

**実装内容**：
```python
class ReflectionLoop:
    """Reflection ループ制御."""
    
    def __init__(
        self,
        generator: AgentBlock,
        reflector: ReflectorAgent,
        improver: ImproverAgent,
        max_iterations: int = 3,
    ):
        self._generator = generator
        self._reflector = reflector
        self._improver = improver
        self._max_iterations = max_iterations
    
    async def execute(self, task: str) -> dict[str, Any]:
        """Reflection ループを実行."""
        # 1. 初期生成
        # 2. 評価ループ
        # 3. 改善 or 終了
```

### Task 3.4: ReflectionWorkflow 実装

**目的**：WorkflowConfig 工厂

**実装内容**：
```python
class ReflectionWorkflow:
    """Reflection Workflow 工厂."""
    
    @staticmethod
    def create(
        workflow_id: str,
        generator: AgentBlock,
        llm_client: Any,
        evaluation_criteria: dict[str, Any],
        max_iterations: int = 3,
    ) -> WorkflowConfig:
        """Reflection Workflow を作成."""
```

---

## 成功指標

- [ ] ReflectorAgent 実装完了
- [ ] ImproverAgent 実装完了  
- [ ] ReflectionLoop 実装完了
- [ ] ReflectionWorkflow 実装完了
- [ ] 10+ 単体テスト、全て合格
- [ ] テストカバレッジ 85%+
- [ ] 無限ループが発生しない
- [ ] 改善履歴が正しく記録される

---

## 使用例

```python
from agentflow.patterns import ReflectionWorkflow
from agentflow.core.engine import AgentFlowEngine

# Generator Agent を定義
generator = MyContentGenerator()

# Reflection Workflow を作成
workflow = ReflectionWorkflow.create(
    workflow_id="content-reflection",
    generator=generator,
    llm_client=my_llm,
    evaluation_criteria={
        "clarity": "内容が明確か",
        "accuracy": "情報が正確か",
        "completeness": "必要な情報が全て含まれているか"
    },
    max_iterations=3,
)

# 実行
engine = AgentFlowEngine()
engine.register_workflow(workflow)
result = await engine.execute("content-reflection", {"task": "AI の説明を書く"})
```


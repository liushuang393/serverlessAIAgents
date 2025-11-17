# Phase 2: Plan-then-Execute Pattern - 詳細設計書

## 📋 概要

**目標**: 実装 Plan-then-Execute パターン、完成度を 40% → 85% に改善

**期間**: 2-3 週間

**優先度**: ⭐⭐⭐ 高優先級

---

## 🎯 設計原則（基於業界最佳実践）

### 1. Plan-and-Execute の核心優勢

根據 LangChain 和業界実践:

| 指標 | ReAct | Plan-and-Execute |
|------|-------|------------------|
| **タスク完成率** | 85% | **92%** |
| **複雑タスク処理** | 中 | **強** |
| **コスト** | 低 | 中 |
| **レスポンス時間** | 速い | やや遅い |

### 2. 設計原則

1. **明確な計画生成**
   - LLM に全体タスクを"考え抜く"ことを強制
   - 複数ステップの依存関係を明示

2. **変数参照メカニズム**（ReWOO より）
   - `#E1`, `#E2` などの変数で前のステップの結果を参照
   - LLM 呼び出しを減らす

3. **ステップ検証**
   - 各ステップの実行結果を検証
   - 失敗時は再計画

4. **既存エンジンとの統合**
   - AgentFlowEngine との互換性
   - 既存の Hooks システムを活用

---

## 🏗️ 架構設計

### 1. Plan-then-Execute アーキテクチャ

```
┌─────────────────────────────────────────────────────────┐
│              Plan-then-Execute Agent                     │
├─────────────────────────────────────────────────────────┤
│                                                           │
│  ┌─────────────────────────────────────────────────┐    │
│  │              Dynamic Planner                    │    │
│  │  - LLM-driven plan generation                   │    │
│  │  - Task decomposition                           │    │
│  │  - Dependency analysis                          │    │
│  └─────────────────────────────────────────────────┘    │
│                        │                                  │
│                        ▼                                  │
│  ┌─────────────────────────────────────────────────┐    │
│  │              Plan Executor                      │    │
│  │  - Step-by-step execution                       │    │
│  │  - Variable substitution (#E1, #E2)             │    │
│  │  - Result validation                            │    │
│  └─────────────────────────────────────────────────┘    │
│                        │                                  │
│                        ▼                                  │
│  ┌─────────────────────────────────────────────────┐    │
│  │              Step Validator                     │    │
│  │  - Success/failure detection                    │    │
│  │  - Quality assessment                           │    │
│  │  - Trigger replanning if needed                 │    │
│  └─────────────────────────────────────────────────┘    │
│                        │                                  │
│                        ▼                                  │
│  ┌─────────────────────────────────────────────────┐    │
│  │              Replanner                          │    │
│  │  - Analyze failure reason                       │    │
│  │  - Generate new plan                            │    │
│  │  - Preserve completed steps                     │    │
│  └─────────────────────────────────────────────────┘    │
│                                                           │
└─────────────────────────────────────────────────────────┘
```

---

## 📝 詳細実装設計

### 1. Dynamic Planner

#### 1.1 クラス設計

```python
# agentflow/patterns/planner.py

from typing import Any, Optional
from pydantic import BaseModel, Field

class Step(BaseModel):
    """計画ステップ。"""
    
    step_id: str = Field(..., description="ステップ ID（例: E1, E2）")
    description: str = Field(..., description="ステップの説明")
    tool: str = Field(..., description="使用するツール")
    parameters: dict[str, Any] = Field(default_factory=dict, description="ツールパラメータ")
    dependencies: list[str] = Field(default_factory=list, description="依存するステップ ID")

class Plan(BaseModel):
    """実行計画。"""
    
    task: str = Field(..., description="元のタスク")
    steps: list[Step] = Field(..., description="実行ステップリスト")
    created_at: str = Field(..., description="作成時刻")

class DynamicPlanner:
    """
    LLM 駆動の動的プランナー。
    
    業界最佳実践に基づいた計画生成:
    - Chain-of-thought prompting
    - タスク分解
    - 依存関係分析
    """
    
    def __init__(
        self,
        llm: Any,  # LLM インスタンス
        available_tools: list[str],
        max_steps: int = 10,
    ):
        """
        初期化。
        
        Args:
            llm: LLM インスタンス
            available_tools: 利用可能なツールリスト
            max_steps: 最大ステップ数
        """
        self._llm = llm
        self._available_tools = available_tools
        self._max_steps = max_steps
    
    async def create_plan(
        self,
        task: str,
        context: Optional[dict] = None,
    ) -> Plan:
        """
        タスクの実行計画を生成。
        
        Args:
            task: タスク説明
            context: 追加コンテキスト
            
        Returns:
            実行計画
        """
        prompt = self._build_planning_prompt(task, context)
        response = await self._llm.generate(prompt)
        plan = self._parse_plan(response)
        return plan
```

#### 1.2 プロンプト設計

```python
PLANNING_PROMPT = """You are a task planning assistant. Given a task, create a detailed execution plan.

Available tools:
{tools}

Task: {task}

Create a plan with the following format:
Plan: [reasoning about the task]
E1: [tool_name]([parameters]) - [description]
Plan: [reasoning about next step]
E2: [tool_name]([parameters, can reference #E1]) - [description]
...

Rules:
1. Each step should have a unique ID (E1, E2, E3, ...)
2. You can reference previous step results using #E1, #E2, etc.
3. Keep the plan focused and efficient
4. Maximum {max_steps} steps

Example:
Task: What are the stats for the quarterbacks of the super bowl contenders this year?

Plan: I need to know the teams playing in the superbowl this year
E1: Search("Who is competing in the superbowl?") - Find current superbowl teams
Plan: I need to know the quarterbacks for each team
E2: LLM("Quarterback for the first team of #E1") - Extract first team's QB
Plan: I need to know the quarterbacks for each team
E3: LLM("Quarterback for the second team of #E1") - Extract second team's QB
Plan: I need to look up stats for the first quarterback
E4: Search("Stats for #E2") - Get stats for first QB
Plan: I need to look up stats for the second quarterback
E5: Search("Stats for #E3") - Get stats for second QB

Now create a plan for the given task:
"""
```

---

## 🔧 実装タスク

### Task 2.1: Dynamic Planner 実装

**ファイル**: `agentflow/patterns/planner.py`

**実装内容**:
1. `Step` データモデル
2. `Plan` データモデル
3. `DynamicPlanner` クラス
4. プロンプトテンプレート
5. 計画パーサー

### Task 2.2: Plan Executor 実装

**ファイル**: `agentflow/patterns/executor.py`

**実装内容**:
1. `PlanExecutor` クラス
2. 変数置換メカニズム
3. ステップ実行ロジック
4. 結果収集

### Task 2.3: Step Validator 実装

**ファイル**: `agentflow/patterns/validator.py`

**実装内容**:
1. `StepValidator` クラス
2. 成功/失敗検出
3. 品質評価
4. 再計画トリガー

### Task 2.4: Replanner 実装

**ファイル**: `agentflow/patterns/replanner.py`

**実装内容**:
1. `Replanner` クラス
2. 失敗分析
3. 新計画生成
4. 完了ステップの保持

### Task 2.5: AgentFlowEngine 統合

**ファイル**: `agentflow/core/engine.py`

**実装内容**:
1. `plan_and_execute` モード追加
2. Planner/Executor 統合
3. Hooks システム連携
4. 既存ワークフローとの互換性

---

## 📊 成功指標

### 1. 機能完成度

- ✅ Dynamic Planner: 100%
- ✅ Plan Executor: 100%
- ✅ Step Validator: 100%
- ✅ Replanner: 100%
- ✅ Engine 統合: 100%

### 2. テストカバレッジ

- 目標: **90%+**
- 単体テスト: 全コンポーネント
- 統合テスト: エンドツーエンド

### 3. パフォーマンス

- 計画生成時間: < 5s
- ステップ実行成功率: > 90%
- 再計画成功率: > 85%

---

## 🧪 テスト戦略

### 1. 単体テスト

**テストファイル**:
- `tests/unit/test_planner.py`
- `tests/unit/test_executor.py`
- `tests/unit/test_validator.py`
- `tests/unit/test_replanner.py`

### 2. 統合テスト

**テストファイル**:
- `tests/integration/test_plan_execute_flow.py`

**テストシナリオ**:
1. 簡単なタスク（2-3 ステップ）
2. 複雑なタスク（5-8 ステップ）
3. 変数参照を含むタスク
4. 失敗と再計画
5. Engine 統合

---

## 📚 参考資料

1. **LangChain - Plan-and-Execute Agents**
   - https://blog.langchain.com/planning-agents/

2. **ReWOO Paper**
   - Variable reference mechanism

3. **Anthropic - Building Effective Agents**
   - Orchestrator-workers pattern

---

## 🚀 次のステップ

Phase 2 完成後:
1. Phase 3: Reflection パターン実装（オプション）
2. Phase 4: Multi-Agent 協作強化（オプション）
3. パフォーマンス最適化
4. ドキュメント更新


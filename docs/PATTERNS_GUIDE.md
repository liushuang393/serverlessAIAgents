# AgentFlow Patterns ガイド

> バージョン: 2.1.0
> 更新日: 2026-01-13

このドキュメントは、AgentFlow が提供する **3つの主要パターン** の使用方法を説明します。

---

## 📑 目次

1. [パターン概要](#パターン概要)
2. [DeepAgent Pattern（推奨）](#1-deepagent-pattern推奨)
3. [Reflection Pattern](#2-reflection-pattern)
4. [Pipeline Pattern](#3-pipeline-pattern)
5. [Reflexion Pattern（NEW）](#4-reflexion-pattern失敗学習)
6. [パターン選択ガイド](#パターン選択ガイド)
7. [統一サービス層](#統一サービス層)

---

## パターン概要

| パターン | 用途 | 複雑度 | 特徴 |
|---------|------|--------|------|
| **DeepAgent** | 複雑なマルチAgent協調 | ★★★ | 6フェーズ、自己進化、品質評審 |
| **Reflection** | 単一Agentの自己改善 | ★★☆ | Generate→Reflect→Improve |
| **Pipeline** | 順次実行パイプライン | ★☆☆ | シンプル、SSE対応 |
| **Reflexion** | 失敗学習 | ★★☆ | 失敗からの自動学習 |

---

## 1. DeepAgent Pattern（推奨）

### 概要

6フェーズの智能型マルチAgent協調パターン：

```
認知分析 → タスク分解 → Agent選択 → 並行実行 → 品質評審 → 自己進化
```

### 基本使用

```python
from agentflow.patterns import DeepAgentCoordinator

# 最もシンプルな使用
coordinator = DeepAgentCoordinator()
result = await coordinator.execute("市場分析レポートを作成")
```

### LLM付き使用

```python
from agentflow.providers import get_llm
from agentflow.patterns import DeepAgentCoordinator

llm = get_llm()  # 環境変数から自動検出

coordinator = DeepAgentCoordinator(
    llm_client=llm,
    max_iterations=10,
    quality_threshold=75.0,
    enable_evolution=True,  # 自己進化を有効化
)

result = await coordinator.execute("競合分析レポートを作成")
print(result)
```

### カスタムAgent追加

```python
from agentflow.patterns import DeepAgentCoordinator, AgentPool
from agentflow.core.agent_block import AgentBlock

# カスタムAgent定義
class FinanceAnalystAgent(AgentBlock):
    async def run(self, input_data: dict) -> dict:
        task = input_data.get("task", "")
        # 分析ロジック
        return {"agent": "finance_analyst", "output": "分析結果..."}

# AgentPoolに登録
pool = AgentPool(
    llm_client=llm,
    predefined_agents={
        "finance_analyst": FinanceAnalystAgent(),
    },
)

coordinator = DeepAgentCoordinator(
    llm_client=llm,
    agent_pool=pool,
)

result = await coordinator.execute("Q1財務分析")
```

### 進捗表示（CLI向け）

```python
from agentflow.services import WorkflowService

service = WorkflowService()

# コールバックで進捗表示
def on_progress(pct, msg):
    print(f"[{pct:5.1f}%] {msg}")

result = await service.execute_with_callback(
    workflow_type="deep_agent",
    task="市場分析",
    on_progress=on_progress,
)
```

### SSEストリーミング（Studio向け）

```python
from agentflow.services import WorkflowService

service = WorkflowService()

# イベントストリーム
async for event in service.execute_stream(
    workflow_type="deep_agent",
    task="市場分析",
):
    print(event.to_json())
    # WebSocket: await ws.send(event.to_json())
    # SSE: yield event.to_sse()
```

---

## 2. Reflection Pattern

### 概要

単一Agentの自己改善ループ：

```
Generate → Reflect → Improve → (繰り返し)
```

### 基本使用

```python
from agentflow.patterns import ReflectionWorkflow

workflow = ReflectionWorkflow(
    llm_client=llm,
    max_iterations=3,
)

result = await workflow.run({
    "task": "技術ブログ記事を作成",
    "requirements": "Python初心者向け、1000文字程度",
})
```

### カスタム評価関数

```python
from agentflow.patterns import ReflectionWorkflow

def custom_evaluator(output: dict) -> tuple[bool, str]:
    """カスタム評価: 1000文字以上かつ見出しあり"""
    text = output.get("content", "")
    if len(text) < 1000:
        return False, "1000文字以上必要です"
    if "##" not in text:
        return False, "見出しを追加してください"
    return True, "OK"

workflow = ReflectionWorkflow(
    llm_client=llm,
    evaluator=custom_evaluator,
    max_iterations=5,
)

result = await workflow.run({"task": "技術記事作成"})
```

---

## 3. Pipeline Pattern

### 概要

複数Agentの順次実行パイプライン。シンプルで理解しやすい。

### 基本使用

```python
from agentflow.patterns import AgentPipeline, AgentConfig

pipeline = AgentPipeline(
    agents=[
        AgentConfig(name="extractor", agent_class=ExtractorAgent),
        AgentConfig(name="analyzer", agent_class=AnalyzerAgent),
        AgentConfig(name="reporter", agent_class=ReporterAgent),
    ],
)

result = await pipeline.run({"document": "..."})
```

### SSEストリーミング

```python
from agentflow.patterns import AgentPipeline

pipeline = AgentPipeline(agents=[...])

# SSE形式でストリーム
async for event in pipeline.run_stream({"document": "..."}):
    print(f"[{event['type']}] {event.get('node', '')}")
```

### Engines経由での使用

```python
from agentflow.engines import PipelineEngine

engine = PipelineEngine(
    stages=[
        {"name": "gate", "agent": GateAgent, "gate": True},
        {"name": "analysis", "agents": [DaoAgent, FaAgent], "parallel": True},
        {"name": "review", "agent": ReviewAgent, "review": True},
    ],
    max_revisions=2,
)

result = await engine.run({"question": "..."})
```

---

## 4. Reflexion Pattern（失敗学習）

### 概要

Reflexion論文（NeurIPS 2023）に基づいた失敗学習パターン。
失敗から自動的に学習し、将来の実行を改善。

### 基本使用

```python
from agentflow.patterns import ReflectiveEvolver

evolver = ReflectiveEvolver(llm_client=llm)

# 失敗時に学習
try:
    result = await agent.execute(task)
except Exception as e:
    reflection = await evolver.learn_from_failure(
        task="データベース接続",
        error=e,
        context={"retry_count": 3},
    )
    print(f"学習内容: {reflection.how_to_avoid}")

# 成功時も学習
await evolver.learn_from_success(task, result)
```

### 過去の反省をプロンプトに含める

```python
from agentflow.patterns import ReflectiveEvolver

evolver = ReflectiveEvolver(llm_client=llm)

# 関連する反省を取得
reflections = evolver.get_relevant_reflections("データベース操作")

# プロンプトを強化
enhanced_prompt = f"""
{original_task}

## 過去の失敗からの学び
{evolver.get_reflection_prompt("データベース操作")}
"""

# 強化されたプロンプトで実行
result = await agent.execute(enhanced_prompt)
```

### 学習効果のフィードバック

```python
# 反省を適用した結果を記録
evolver.record_outcome(
    reflection_id=reflection.id,
    task="データベース操作",
    success=True,
    feedback="タイムアウトを増やしたことで成功",
)
```

---

## パターン選択ガイド

```
┌─────────────────────────────────────────────────────────────┐
│                    タスクの複雑度                            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  シンプル（1-2ステップ）                                    │
│    → Pipeline                                               │
│                                                             │
│  中程度（3-5ステップ、品質改善が必要）                      │
│    → Reflection                                             │
│                                                             │
│  複雑（多ステップ、依存関係、並行実行）                      │
│    → DeepAgent                                              │
│                                                             │
│  失敗からの学習が必要                                        │
│    → Reflexion（他パターンと組み合わせ）                    │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### 判断フローチャート

```
タスクを分析
    │
    ├─ 単一ステップ? ──────────→ SimpleEngine
    │
    ├─ 品質改善が必要? ────────→ Reflection
    │
    ├─ 複数Agent協調? 
    │   │
    │   ├─ 順序固定? ──────────→ Pipeline
    │   │
    │   └─ 動的選択? ──────────→ DeepAgent
    │
    └─ 失敗パターン学習? ──────→ Reflexion + 上記いずれか
```

---

## 統一サービス層

### 概要

全パターンは **統一サービス層** (`agentflow/services/`) を通じて一貫したインターフェースを提供：

```
┌─────────────┬─────────────┬─────────────┐
│    CLI      │    API      │   Studio    │
└──────┬──────┴──────┬──────┴──────┬──────┘
       │             │             │
       └─────────────┼─────────────┘
                     ▼
       ┌─────────────────────────────┐
       │     Service Layer           │
       │  AgentService               │
       │  WorkflowService            │
       └─────────────────────────────┘
```

### 3つの実行モード

| モード | メソッド | 用途 |
|--------|---------|------|
| 結果のみ | `execute()` | REST API |
| コールバック | `execute_with_callback()` | CLI進捗表示 |
| ストリーム | `execute_stream()` | WebSocket/SSE |

### 使用例

```python
from agentflow.services import AgentService, WorkflowService

# Agent実行
agent_service = AgentService()
result = await agent_service.execute(agent_id="MyAgent", input_data={...})

# Workflow実行（DeepAgent）
workflow_service = WorkflowService()
result = await workflow_service.execute(
    workflow_type="deep_agent",
    task="市場分析",
)

# ストリーム実行
async for event in workflow_service.execute_stream(...):
    # イベント処理
    pass
```

---

## インポートまとめ

```python
# メインパターン
from agentflow.patterns import (
    # DeepAgent
    DeepAgentCoordinator,
    AgentPool,
    DynamicAgent,
    
    # Reflection
    ReflectionWorkflow,
    
    # Pipeline
    AgentPipeline,
    AgentConfig,
    
    # Reflexion（失敗学習）
    ReflectiveEvolver,
    Reflection,
)

# 統一サービス層
from agentflow.services import (
    AgentService,
    WorkflowService,
    ServiceEvent,
    ProgressEvent,
)

# Engines（簡易パターン）
from agentflow.engines import (
    SimpleEngine,
    GateEngine,
    PipelineEngine,
    RAGEngine,
)
```

---

## 関連ドキュメント

- [DeepAgent実装ガイド](design/DEEP_AGENT_IMPLEMENTATION_GUIDE_JA.md)
- [Memory System](memory/MEMORY_SYSTEM_DESIGN.md)
- [プロトコル](protocols.md)
- [API リファレンス](api.md)

---

*最終更新: 2026-01-13*

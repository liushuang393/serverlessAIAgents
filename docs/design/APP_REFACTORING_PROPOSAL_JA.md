# App リファクタリング提案書

> バージョン: 1.0.0  
> 作成日: 2026-01-13

---

## 1. 概要

本ドキュメントでは、改善された `DeepAgentCoordinator` を活用した3つのAppのリファクタリング提案を行います。

### 対象App

| App | 現状 | 複雑度 | 推奨Pattern |
|-----|------|--------|-------------|
| Decision Governance Engine | PipelineEngine + 7 Agents | 高 | DeepAgentCoordinator |
| Code Migration Assistant | Orchestrator + 5 Agents | 高 | DeepAgentCoordinator |
| Market Trend Monitor | 未完成 | 中 | DeepAgentCoordinator |

---

## 2. Decision Governance Engine

### 2.1 現状分析

```
┌─────────────────────────────────────────────────────────────┐
│                    現在のアーキテクチャ                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  User Input                                                 │
│      ↓                                                      │
│  CognitiveGateAgent (質問分析)                              │
│      ↓                                                      │
│  GatekeeperAgent (門番判定)                                 │
│      ↓                                                      │
│  ┌─────────────────────────────────────┐                   │
│  │        PipelineEngine               │                   │
│  │  DaoAgent → FaAgent → ShuAgent → QiAgent              │
│  └─────────────────────────────────────┘                   │
│      ↓                                                      │
│  ReviewAgent (評価)                                         │
│      ↓                                                      │
│  Report Generation                                          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**課題**:
1. 独自の `PipelineEngine` がフレームワークと乖離
2. 品質評審がシンプル（リトライ機構なし）
3. 成功パターンの学習なし
4. チェックポイントなし（長時間タスクで問題）

### 2.2 リファクタリング案

```
┌─────────────────────────────────────────────────────────────┐
│                    新アーキテクチャ                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  User Input                                                 │
│      ↓                                                      │
│  ┌─────────────────────────────────────────────────────┐   │
│  │            DeepAgentCoordinator                      │   │
│  │                                                      │   │
│  │  Phase 1: 認知分析                                   │   │
│  │    └─ CognitiveGateAgent相当の機能を内蔵            │   │
│  │                                                      │   │
│  │  Phase 2: タスク分解                                 │   │
│  │    └─ 質問タイプに応じた実行計画自動生成            │   │
│  │                                                      │   │
│  │  Phase 3: Agent選択                                  │   │
│  │    └─ predefined_agents:                            │   │
│  │         dao: DaoAgent (道の分析)                     │   │
│  │         fa: FaAgent (法の適用)                       │   │
│  │         shu: ShuAgent (術の提案)                     │   │
│  │         qi: QiAgent (器の実装)                       │   │
│  │         review: ReviewAgent (評価)                   │   │
│  │                                                      │   │
│  │  Phase 4: 並行実行                                   │   │
│  │    └─ 依存関係のないAgentを並行実行                 │   │
│  │                                                      │   │
│  │  Phase 5: 品質評審                                   │   │
│  │    └─ 多次元評価 + 自動リトライ                     │   │
│  │                                                      │   │
│  │  Phase 6: 進化                                       │   │
│  │    └─ 成功パターン学習                              │   │
│  └─────────────────────────────────────────────────────┘   │
│      ↓                                                      │
│  Report Generation (既存流用)                               │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### 2.3 実装例

```python
# apps/decision_governance_engine/engine.py (リファクタリング後)

from agentflow.patterns.deep_agent import DeepAgentCoordinator
from apps.decision_governance_engine.agents import (
    DaoAgent, FaAgent, ShuAgent, QiAgent, ReviewAgent
)

class DecisionEngine:
    def __init__(self, llm_client=None):
        self._coordinator = DeepAgentCoordinator(
            llm_client=llm_client,
            predefined_agents={
                "dao": DaoAgent(),
                "fa": FaAgent(),
                "shu": ShuAgent(),
                "qi": QiAgent(),
                "review": ReviewAgent(),
            },
            max_iterations=5,
            max_retries=2,
            quality_threshold=75.0,
            enable_evolution=True,
            enable_memory=True,
        )
    
    async def process_question(self, question: str, context: dict = None):
        """質問を処理."""
        result = await self._coordinator.execute(
            task=question,
            context=context or {},
        )
        
        return {
            "status": result.get("status"),
            "analysis": result.get("result"),
            "quality_score": result.get("quality_score"),
            "execution_time": result.get("execution_time"),
        }
```

### 2.4 メリット

| 項目 | Before | After |
|------|--------|-------|
| コード量 | ~2000行 | ~500行 |
| 品質評審 | 単純 | 多次元 + リトライ |
| 学習機能 | なし | 成功パターン学習 |
| チェックポイント | なし | あり |
| 並行実行 | なし | 依存関係ベース |

---

## 3. Code Migration Assistant

### 3.1 現状分析

```
┌─────────────────────────────────────────────────────────────┐
│                    現在のアーキテクチャ                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  COBOL Source                                               │
│      ↓                                                      │
│  ┌─────────────────────────────────────┐                   │
│  │           Orchestrator              │                   │
│  │                                     │                   │
│  │  CoordinatorAgent                   │                   │
│  │      ↓                              │                   │
│  │  TransformAgent (変換)              │                   │
│  │      ↓                              │                   │
│  │  CheckerAgent (検証)                │                   │
│  │      ↓                              │                   │
│  │  FixerAgent (修正) ←─ エラー時      │                   │
│  │      ↓                              │                   │
│  │  TestGenAgent (テスト生成)          │                   │
│  └─────────────────────────────────────┘                   │
│      ↓                                                      │
│  Java Output                                                │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**課題**:
1. 長時間タスクでの障害復旧が困難
2. アーティファクト（中間ファイル）管理が煩雑
3. 品質評審が限定的
4. 反復改善ループが複雑

### 3.2 リファクタリング案

```python
# apps/code_migration_assistant/engine.py (リファクタリング後)

from agentflow.patterns.deep_agent import DeepAgentCoordinator

class MigrationEngine:
    def __init__(self, llm_client=None):
        self._coordinator = DeepAgentCoordinator(
            llm_client=llm_client,
            predefined_agents={
                "coordinator": CoordinatorAgent(),
                "transform": TransformAgent(),
                "checker": CheckerAgent(),
                "fixer": FixerAgent(),
                "testgen": TestGenAgent(),
            },
            max_iterations=15,  # 移行は複雑
            max_retries=3,
            quality_threshold=85.0,  # 品質重視
            enable_evolution=True,
        )
    
    async def migrate(self, source_code: str, source_lang: str, target_lang: str):
        """コード移行を実行."""
        
        # Virtual Filesystem でソースを保存
        await self._coordinator._runtime_store.write_artifact(
            "/source/input.cob",
            source_code,
        )
        
        result = await self._coordinator.execute(
            task=f"{source_lang}から{target_lang}へのコード移行",
            context={
                "source_lang": source_lang,
                "target_lang": target_lang,
                "source_file": "/source/input.cob",
            },
        )
        
        # 出力ファイルを取得
        output = await self._coordinator._runtime_store.read_artifact(
            "/output/result.java"
        )
        
        return {
            "status": result.get("status"),
            "output_code": output.decode() if output else None,
            "quality_score": result.get("quality_score"),
        }
    
    async def resume_migration(self, checkpoint_id: str):
        """チェックポイントから再開."""
        return await self._coordinator.resume_from_checkpoint(
            checkpoint_id,
            task="移行を再開",
        )
```

### 3.3 Virtual Filesystem 活用

```python
# アーティファクト管理
runtime_store = coordinator._runtime_store

# 中間ファイル保存
await runtime_store.write_artifact("/intermediate/ast.json", ast_json)
await runtime_store.write_artifact("/intermediate/mapping.json", mapping)

# テスト結果保存
await runtime_store.write_artifact(
    "/test_results/junit.xml",
    test_output,
    metadata={"test_count": 10, "passed": 8, "failed": 2},
)

# ファイル一覧取得
files = await runtime_store.list_artifacts("/intermediate")
```

---

## 4. Market Trend Monitor

### 4.1 新規設計

```
┌─────────────────────────────────────────────────────────────┐
│                    提案アーキテクチャ                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Data Sources (API, Database)                               │
│      ↓                                                      │
│  ┌─────────────────────────────────────────────────────┐   │
│  │            DeepAgentCoordinator                      │   │
│  │                                                      │   │
│  │  Phase 1: 認知分析                                   │   │
│  │    └─ 分析目的・対象期間・指標を理解                │   │
│  │                                                      │   │
│  │  Phase 2: タスク分解                                 │   │
│  │    ├─ データ収集タスク                              │   │
│  │    ├─ 分析タスク（統計/トレンド/異常）             │   │
│  │    └─ レポート生成タスク                            │   │
│  │                                                      │   │
│  │  Phase 3: Agent選択                                  │   │
│  │    └─ predefined_agents:                            │   │
│  │         collector: DataCollectorAgent               │   │
│  │         analyzer: AnalyzerAgent + BIAnalyzer        │   │
│  │         reporter: ReporterAgent + ChartGenerator    │   │
│  │                                                      │   │
│  │  Phase 4: 並行実行                                   │   │
│  │    └─ 複数データソースの並行収集                    │   │
│  │                                                      │   │
│  │  Phase 5-6: 品質評審 + 進化                         │   │
│  └─────────────────────────────────────────────────────┘   │
│      ↓                                                      │
│  Dashboard / Report                                         │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### 4.2 実装例

```python
# apps/market_trend_monitor/engine.py

from agentflow.patterns.deep_agent import DeepAgentCoordinator
from agentflow.skills.builtin.bi_analytics import BIAnalyzer, ChartGenerator

class MarketMonitor:
    def __init__(self, llm_client=None):
        self._bi_analyzer = BIAnalyzer(llm_client=llm_client)
        self._chart_generator = ChartGenerator()
        
        self._coordinator = DeepAgentCoordinator(
            llm_client=llm_client,
            predefined_agents={
                "collector": DataCollectorAgent(),
                "analyzer": AnalyzerAgent(
                    bi_analyzer=self._bi_analyzer,
                ),
                "reporter": ReporterAgent(
                    chart_generator=self._chart_generator,
                ),
            },
            max_iterations=10,
            quality_threshold=70.0,
        )
    
    async def analyze_trend(self, query: str):
        """市場トレンドを分析."""
        result = await self._coordinator.execute(query)
        
        # チャートを生成
        if "data" in result.get("result", {}):
            chart = await self._chart_generator.generate(
                result["result"]["data"],
                "line",
                title="Market Trend",
            )
            result["chart"] = chart.to_echarts()
        
        return result
```

---

## 5. Patterns 整理提案

### 5.1 現行 Patterns 分析

| Pattern | ファイル | 行数 | 使用App | 推奨 |
|---------|----------|------|---------|------|
| DeepAgentCoordinator | deep_agent.py | ~2500 | 新規 | **主要** |
| Reflection | reflection.py | ~300 | - | **継続** |
| AgentPipeline | agent_pipeline.py | ~200 | - | **継続** |
| Supervisor | supervisor.py | ~400 | DGE | Deprecated |
| Hierarchical | hierarchical.py | ~350 | - | Deprecated |
| MultiAgent | multi_agent.py | ~450 | - | Deprecated |
| Planner | planner.py | ~400 | - | Deprecated |
| CoordinatorBase | coordinator.py | ~200 | 全て | **継続** |
| ProgressEmitter | progress_emitter.py | ~150 | 全て | **継続** |

### 5.2 統合後の Patterns

```
agentflow/patterns/
├── __init__.py               # 統一エクスポート
├── coordinator.py            # 基底クラス（必須）
├── deep_agent.py             # 主要Pattern
├── reflection.py             # 自己反省（特化用途）
├── agent_pipeline.py         # シンプルパイプライン
├── progress_emitter.py       # SSE配信
│
└── _legacy/                  # 後方互換（警告付き）
    ├── __init__.py           # DeprecationWarning
    ├── supervisor.py
    ├── hierarchical.py
    ├── multi_agent.py
    └── planner.py
```

### 5.3 移行ガイド

```python
# __init__.py での警告付きエクスポート

import warnings
from agentflow.patterns.deep_agent import DeepAgentCoordinator

# 推奨インポート
__all__ = [
    "CoordinatorBase",
    "DeepAgentCoordinator",
    "ReflectionCoordinator",
    "AgentPipeline",
    "ProgressEmitter",
]

# 非推奨インポート（警告付き）
def __getattr__(name):
    legacy_patterns = {
        "SupervisorCoordinator": "supervisor",
        "HierarchicalCoordinator": "hierarchical",
        "MultiAgentCoordinator": "multi_agent",
        "PlannerCoordinator": "planner",
    }
    
    if name in legacy_patterns:
        warnings.warn(
            f"{name} は非推奨です。DeepAgentCoordinator を使用してください。",
            DeprecationWarning,
            stacklevel=2,
        )
        module = __import__(
            f"agentflow.patterns._legacy.{legacy_patterns[name]}",
            fromlist=[name],
        )
        return getattr(module, name)
    
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
```

---

## 6. 実装スケジュール

### Phase 1: 準備（Week 1）

- [ ] 本ドキュメントのレビュー
- [ ] 各Appの現状調査
- [ ] テストカバレッジ確認

### Phase 2: Decision Governance Engine（Week 2-3）

- [ ] `DeepAgentAdapter` 拡張
- [ ] 既存Agentsの `predefined_agents` 登録
- [ ] `PipelineEngine` → `DeepAgentCoordinator` 移行
- [ ] テスト更新
- [ ] E2Eテスト

### Phase 3: Code Migration Assistant（Week 4-5）

- [ ] `Orchestrator` → `DeepAgentCoordinator` 移行
- [ ] Virtual Filesystem 統合
- [ ] チェックポイント実装
- [ ] テスト更新

### Phase 4: Market Trend Monitor（Week 6）

- [ ] 新規実装
- [ ] BI Analytics 統合
- [ ] ダッシュボードUI

### Phase 5: Patterns 整理（Week 7）

- [ ] `_legacy` ディレクトリ作成
- [ ] 警告付きエクスポート実装
- [ ] ドキュメント更新

---

## 7. リスクと対策

| リスク | 影響 | 対策 |
|--------|------|------|
| 後方互換性 | 既存コードが動かない | `_legacy` で警告付き提供 |
| テスト不足 | リグレッション | 移行前にテスト追加 |
| パフォーマンス | 遅延 | ベンチマーク実施 |
| 学習コスト | 開発効率低下 | ドキュメント整備、ペアプロ |

---

*ドキュメント作成日: 2026-01-13*

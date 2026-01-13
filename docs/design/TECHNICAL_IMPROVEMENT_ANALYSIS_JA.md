# AgentFlow 技術改善分析レポート

> 作成日: 2026-01-13

---

## 📋 概要

本ドキュメントでは、AgentFlow の以下 3 つの技術改善項目を分析します：

1. **コード実行サンドボックス** - Docker + Python 解釈器（CodeAct パターン）
2. **信頼性強化フロー** - 強約束 + 多重検証 + ロールバック
3. **監視基盤** - Prometheus + Grafana

---

## 1. コード実行サンドボックス（CodeAct パターン）

### 1.1 背景

Manus の CodeAct パターンは、LLM が「コードとして行動」することで、複雑なタスクを高精度で実行できます。
Agent が動的にコードを生成・実行することで、API コール制限や定型ツール不足を解消します。

### 1.2 オープンソース選択肢

| プロジェクト | 特徴 | 推奨度 |
|-------------|------|--------|
| **[e2b-dev/E2B](https://github.com/e2b-dev/E2B)** | ⭐ クラウド SaaS 提供、SDK 完備、セキュア | ✅ **推奨** |
| **[microsandbox](https://github.com/microsandbox/microsandbox)** | 軽量、セルフホスト可能、OCI 互換 | ✅ **代替案** |
| Docker + exec | シンプル、追加依存なし | ⚠️ 開発用のみ |

### 1.3 推奨アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│                    AgentFlow Engine                         │
├─────────────────────────────────────────────────────────────┤
│  CodeActAgent                                               │
│  ├── _generate_code(task) → LLM でコード生成                │
│  ├── _execute_code(code) → サンドボックス実行               │
│  └── _parse_result(output) → 結果解析                       │
└────────────────┬────────────────────────────────────────────┘
                 │ API 呼び出し
                 ▼
┌─────────────────────────────────────────────────────────────┐
│              Code Execution Sandbox                          │
│  ┌─────────────────┐  ┌─────────────────┐                   │
│  │   e2b Cloud     │  │  microsandbox   │ ← 選択可能        │
│  │   (推奨)        │  │  (セルフホスト) │                   │
│  └─────────────────┘  └─────────────────┘                   │
│                                                              │
│  機能:                                                       │
│  - Python 3.13 インタプリタ                                  │
│  - pip パッケージインストール                                │
│  - ファイル操作（隔離環境内）                                │
│  - タイムアウト・リソース制限                                │
│  - 結果の stdout/stderr 取得                                 │
└─────────────────────────────────────────────────────────────┘
```

### 1.4 実装計画

```python
# agentflow/sandbox/__init__.py
from abc import ABC, abstractmethod
from typing import Any

class SandboxProvider(ABC):
    """サンドボックス実行プロバイダ基底クラス."""

    @abstractmethod
    async def execute(
        self,
        code: str,
        *,
        timeout: float = 30.0,
        packages: list[str] | None = None,
    ) -> dict[str, Any]:
        """コードを実行.

        Returns:
            {"stdout": str, "stderr": str, "exit_code": int, "files": dict}
        """
        ...

    @abstractmethod
    async def close(self) -> None:
        """リソース解放."""
        ...


# 実装例（e2b）
class E2BSandbox(SandboxProvider):
    def __init__(self, api_key: str | None = None):
        self._api_key = api_key or os.getenv("E2B_API_KEY")

    async def execute(self, code: str, **kwargs) -> dict:
        from e2b_code_interpreter import Sandbox
        async with Sandbox() as sandbox:
            execution = sandbox.run_code(code)
            return {
                "stdout": execution.text,
                "stderr": execution.error or "",
                "exit_code": 0 if not execution.error else 1,
            }


# CodeActAgent 使用例
class CodeActAgent(AgentBlock):
    """コード生成・実行 Agent."""

    async def run(self, input_data: dict) -> dict:
        task = input_data["task"]

        # 1. LLM でコード生成
        code = await self._generate_code(task)

        # 2. サンドボックスで実行
        sandbox = get_sandbox()  # 環境変数から自動検出
        result = await sandbox.execute(code, timeout=60.0)

        # 3. 結果解析
        if result["exit_code"] != 0:
            return {"error": result["stderr"], "code": code}

        return {"output": result["stdout"], "code": code}
```

### 1.5 工数見積

| タスク | 工数 |
|--------|------|
| SandboxProvider 抽象化 | 1日 |
| E2BSandbox 実装 | 1日 |
| MicrosandboxProvider 実装 | 1日 |
| CodeActAgent 実装 | 2日 |
| テスト・ドキュメント | 1日 |
| **合計** | **6日** |

---

## 2. 信頼性強化フロー

### 2.1 現状分析

AgentFlow は既に以下のリトライ・チェックポイント機能を持つ：

| 機能 | 実装状況 | 場所 |
|------|---------|------|
| リトライ | ✅ 実装済 | `agentflow/core/retry.py`, `ResilientAgent` |
| タイムアウト | ✅ 実装済 | `ResilientAgent.timeout_seconds` |
| チェックポイント | ✅ 実装済 | `agentflow/hitl/checkpointer.py` |
| HITL 中断・再開 | ✅ 実装済 | `agentflow/hitl/interrupt.py` |
| Review ステージ | ✅ 実装済 | `PipelineEngine` (PASS/REVISE/REJECT) |

### 2.2 不足している機能

```
【現状の課題】
1. エラー伝播制御が弱い（1Agent 失敗 → 全体失敗）
2. ロールバック機構がない（中間成果物の巻き戻し）
3. 検証ステージが任意（強制チェックポイントなし）
4. 品質ゲートが定性的（数値スコア閾値なし）
```

### 2.3 強化提案

```
┌─────────────────────────────────────────────────────────────┐
│               Enhanced Pipeline Engine                       │
├─────────────────────────────────────────────────────────────┤
│  ┌──────────┐    ┌──────────┐    ┌──────────┐              │
│  │   Gate   │───▶│ Stage 1  │───▶│ Stage 2  │───▶ ...     │
│  │ (検証)   │    │ +検証    │    │ +検証    │              │
│  └────┬─────┘    └────┬─────┘    └────┬─────┘              │
│       │               │               │                      │
│       ▼               ▼               ▼                      │
│  ┌──────────────────────────────────────────────┐           │
│  │           Checkpoint Store                    │           │
│  │  - stage_1_result: {...}                     │           │
│  │  - stage_2_result: {...}                     │           │
│  │  - rollback_points: [...]                    │           │
│  └──────────────────────────────────────────────┘           │
│       │                                                      │
│       ▼ 検証失敗時                                           │
│  ┌──────────────────────────────────────────────┐           │
│  │         Rollback Controller                   │           │
│  │  - rollback_to(stage_name)                   │           │
│  │  - retry_with_feedback(error_context)        │           │
│  └──────────────────────────────────────────────┘           │
└─────────────────────────────────────────────────────────────┘
```

### 2.4 具体的な強化項目

#### A. 強制バリデーション

```python
# 各ステージ出力に対する自動検証
class StageValidator:
    """ステージ出力バリデータ."""

    async def validate(self, stage_name: str, output: dict) -> ValidationResult:
        """出力を検証.

        検証項目:
        1. 必須フィールド存在チェック
        2. 型チェック（Pydantic）
        3. 品質スコアチェック（閾値）
        4. 異常値検出
        """
        ...

# Engine 設定
engine = PipelineEngine(
    stages=[...],
    validation_config=ValidationConfig(
        required_fields={"analysis": ["score", "findings"]},
        min_quality_score=70,
        enable_anomaly_detection=True,
    ),
)
```

#### B. 自動ロールバック

```python
# ロールバック対応 Engine
class ResilientPipelineEngine(PipelineEngine):
    """ロールバック対応 Pipeline."""

    async def _execute_with_rollback(self, inputs: dict) -> dict:
        checkpoints: dict[str, dict] = {}

        for stage in self._stages:
            try:
                # チェックポイント保存
                checkpoints[stage.name] = {"inputs": inputs.copy()}

                # ステージ実行
                result = await stage.execute(inputs)

                # 検証
                validation = await self._validator.validate(stage.name, result)
                if not validation.passed:
                    # 前ステージにロールバック
                    return await self._rollback_and_retry(
                        stage.name,
                        checkpoints,
                        validation.feedback,
                    )

                checkpoints[stage.name]["result"] = result
                inputs.update(result)

            except Exception as e:
                # 例外時もロールバック
                return await self._rollback_and_retry(
                    stage.name, checkpoints, str(e)
                )

        return inputs
```

#### C. 多重検証ゲート

```python
# 品質ゲート設定
quality_gates = [
    QualityGate(
        name="completeness",
        check=lambda r: r.get("completeness_score", 0) >= 80,
        on_fail="retry",
    ),
    QualityGate(
        name="accuracy",
        check=lambda r: r.get("accuracy_score", 0) >= 75,
        on_fail="escalate",  # 人間エスカレーション
    ),
    QualityGate(
        name="consistency",
        check=lambda r: r.get("consistency_score", 0) >= 70,
        on_fail="rollback",
    ),
]
```

### 2.5 工数見積

| タスク | 工数 |
|--------|------|
| StageValidator 実装 | 2日 |
| RollbackController 実装 | 2日 |
| ResilientPipelineEngine 拡張 | 2日 |
| QualityGate 機構 | 1日 |
| テスト・ドキュメント | 1日 |
| **合計** | **8日** |


---

## 3. 監視基盤（Prometheus + Grafana）

### 3.1 現状分析

AgentFlow は既に基本的な監視機能を持つ：

| 機能 | 実装状況 | 場所 |
|------|---------|------|
| 構造化ログ | ✅ 実装済 | `agentflow/observability/logging.py` |
| メトリクスコレクタ | ✅ 実装済 | `agentflow/observability/metrics.py` |
| Prometheus 形式出力 | ✅ 実装済 | `MetricsCollector.to_prometheus()` |
| トレーシング | ✅ 実装済 | `agentflow/observability/tracing.py` |
| OTLP エクスポート | ✅ 実装済 | `agentflow/observability/otel_exporter.py` |
| Sentry 統合 | ✅ 実装済 | `agentflow/observability/sentry_integration.py` |

### 3.2 不足している機能

```
【現状の課題】
1. Prometheus エンドポイント未公開（手動実装が必要）
2. Agent 実行メトリクス未収集
3. Grafana ダッシュボード未定義
4. アラート設定なし
```

### 3.3 推奨アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│                   AgentFlow Application                      │
│  ┌─────────────────────────────────────────────────────────┐│
│  │  Engine / Agent                                         ││
│  │  └── @metrics_tracked                                   ││
│  │      ├── agent_execution_duration_seconds               ││
│  │      ├── agent_execution_total{status="success|error"}  ││
│  │      ├── llm_tokens_total{type="input|output"}          ││
│  │      └── llm_latency_seconds                            ││
│  └─────────────────────────────────────────────────────────┘│
│                          │                                   │
│                          ▼                                   │
│  ┌─────────────────────────────────────────────────────────┐│
│  │  GET /metrics  (Prometheus形式)                         ││
│  └─────────────────────────────────────────────────────────┘│
└────────────────────────────┬────────────────────────────────┘
                             │ scrape
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                     Prometheus Server                        │
│  - スクレイプ間隔: 15s                                       │
│  - 保持期間: 15日                                            │
│  - アラートルール定義                                        │
└────────────────────────────┬────────────────────────────────┘
                             │ query
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                     Grafana Dashboard                        │
│  ┌──────────────────┬──────────────────┬──────────────────┐ │
│  │  Agent 成功率    │  平均レイテンシ  │  エラー率        │ │
│  │  [グラフ]        │  [グラフ]        │  [グラフ]        │ │
│  └──────────────────┴──────────────────┴──────────────────┘ │
│  ┌──────────────────┬──────────────────┬──────────────────┐ │
│  │  LLM トークン    │  同時実行数      │  キュー深度      │ │
│  │  [グラフ]        │  [ゲージ]        │  [グラフ]        │ │
│  └──────────────────┴──────────────────┴──────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### 3.4 実装計画

#### A. メトリクスエンドポイント

```python
# agentflow/studio/api.py に追加
from agentflow.observability import get_metrics

@app.get("/metrics")
async def metrics():
    """Prometheus メトリクスエンドポイント."""
    collector = get_metrics()
    return Response(
        content=collector.to_prometheus(),
        media_type="text/plain; version=0.0.4",
    )
```

#### B. Agent メトリクスデコレータ

```python
# agentflow/observability/decorators.py
from functools import wraps
from agentflow.observability import get_metrics
import time

def metrics_tracked(func):
    """Agent 実行メトリクスを自動収集."""
    @wraps(func)
    async def wrapper(self, *args, **kwargs):
        metrics = get_metrics()
        agent_name = getattr(self, "name", self.__class__.__name__)

        start_time = time.time()
        try:
            result = await func(self, *args, **kwargs)
            metrics.counter("agent_execution_total", labels={"agent": agent_name, "status": "success"}).inc()
            return result
        except Exception as e:
            metrics.counter("agent_execution_total", labels={"agent": agent_name, "status": "error"}).inc()
            raise
        finally:
            duration = time.time() - start_time
            metrics.histogram("agent_execution_duration_seconds", labels={"agent": agent_name}).observe(duration)

    return wrapper
```

#### C. 定義すべきメトリクス

| メトリクス名 | 型 | ラベル | 説明 |
|-------------|-----|--------|------|
| `agent_execution_total` | Counter | agent, status | 実行回数 |
| `agent_execution_duration_seconds` | Histogram | agent | 実行時間 |
| `llm_request_total` | Counter | provider, model, status | LLM 呼び出し回数 |
| `llm_latency_seconds` | Histogram | provider, model | LLM レイテンシ |
| `llm_tokens_total` | Counter | provider, model, type | トークン使用量 |
| `pipeline_stage_duration_seconds` | Histogram | pipeline, stage | ステージ実行時間 |
| `hitl_approval_total` | Counter | action, status | HITL 承認回数 |

#### D. Grafana ダッシュボード JSON

```json
{
  "title": "AgentFlow Metrics",
  "panels": [
    {
      "title": "Agent Success Rate",
      "type": "graph",
      "targets": [{
        "expr": "rate(agent_execution_total{status='success'}[5m]) / rate(agent_execution_total[5m])"
      }]
    },
    {
      "title": "P95 Latency",
      "type": "graph",
      "targets": [{
        "expr": "histogram_quantile(0.95, rate(agent_execution_duration_seconds_bucket[5m]))"
      }]
    },
    {
      "title": "Error Rate",
      "type": "singlestat",
      "targets": [{
        "expr": "rate(agent_execution_total{status='error'}[5m])"
      }]
    }
  ]
}
```

### 3.5 アラート設定例

```yaml
# prometheus/alerts.yml
groups:
  - name: agentflow
    rules:
      - alert: HighErrorRate
        expr: rate(agent_execution_total{status="error"}[5m]) > 0.1
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "Agent エラー率が 10% を超えています"

      - alert: HighLatency
        expr: histogram_quantile(0.95, rate(agent_execution_duration_seconds_bucket[5m])) > 30
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "P95 レイテンシが 30 秒を超えています"

      - alert: LLMTokenQuotaNearLimit
        expr: increase(llm_tokens_total[1h]) > 100000
        for: 1m
        labels:
          severity: warning
        annotations:
          summary: "LLM トークン使用量が 1 時間で 10 万を超えました"
```

### 3.6 工数見積

| タスク | 工数 |
|--------|------|
| /metrics エンドポイント追加 | 0.5日 |
| @metrics_tracked デコレータ | 1日 |
| LLM メトリクス統合 | 1日 |
| Grafana ダッシュボード定義 | 1日 |
| アラートルール定義 | 0.5日 |
| Docker Compose 統合 | 0.5日 |
| ドキュメント | 0.5日 |
| **合計** | **5日** |

---

## 📊 総合サマリ

| 項目 | 工数 | 優先度 | 依存関係 |
|------|------|--------|---------|
| コード実行サンドボックス | 6日 | 高 | なし |
| 信頼性強化フロー | 8日 | 高 | なし |
| 監視基盤 | 5日 | 中 | なし |
| **合計** | **19日** |  |  |

### 推奨実装順序

```
1. 監視基盤（5日）
   ↓ 先に計測基盤を整備
2. 信頼性強化フロー（8日）
   ↓ メトリクスで効果を確認しながら
3. コード実行サンドボックス（6日）
   ↓ 新機能として追加
```

### 次のアクション

1. [ ] 各項目の詳細設計書作成
2. [ ] e2b API キー取得（無料枠あり）
3. [ ] Docker Compose に Prometheus + Grafana 追加
4. [ ] StageValidator のインターフェース定義


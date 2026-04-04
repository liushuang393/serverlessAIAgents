# 自律編排アーキテクチャ検証レポート — flight_watch シナリオ

## Context

AutonomousPipeline（Phase 1-5 実装済み）を messaging_hub の機票価格監視タスクで検証。
フレームワーク/App の責務分離を分析し、改善すべきギャップを特定する。

**検証シナリオ:**
- 単発検索: 「NRT→LAX 7月の安い便を探して」
- 監視サブスクリプション: 検索 → 定期監視 → 価格変動検知 → 通知

---

## 問題 1: 二重編排（フレームワーク側 — 最優先）

### 現状

App 側に独自の編排スタックが存在し、harness の AutonomousPipeline と責務が重複。

| 責務 | App 側（現状） | Framework 側（新規） |
|------|---------------|---------------------|
| タスク分解 | `TaskHarnessPlanner` (`task_harness.py:147`) | `PlannerAgent` (`harness/orchestration/planner.py`) |
| 実行計画モデル | `HarnessPlan` (`task_harness.py:147`) | `ExecutionPlan` (`harness/orchestration/models.py`) |
| ルーティング | `CapabilityRouter` (`orchestration_support.py`) | `DynamicFlowGenerator` (`harness/orchestration/dynamic_flow.py`) |
| リスク判定 | `control_plane.py` + `GatePolicy` | `RiskGateMiddleware` (`harness/orchestration/risk_gate.py`) |
| 承認ゲート | `approval_manager.py` (App 独自) | `harness/approval/approval_manager.py` (Framework) |
| 実行記録 | `ExecutionSubstrateService` (`execution_substrate.py`) | `AuditMiddleware` (`harness/orchestration/audit_middleware.py`) |
| 検証 | `TaskHarnessVerifier` (`task_harness.py`) | `StepVerifierMiddleware` (`harness/orchestration/step_verifier.py`) |

### 解決策: Adapter パターン

App の既存 `OrchestrationService` を **AutonomousPipeline の消費者** として接続する。
App は「何を実行するか」（Agent 登録 + ビジネスロジック）のみ担当し、
「どう実行するか」（分解・ゲート・検証・監査）はフレームワークに委譲する。

```
【現状】
OrchestrationService.create_task()
  → TaskHarnessPlanner.build_plan()     ← App 独自の計画
  → _handle_flight_task()               ← App 独自の実行
  → ExecutionSubstrateService            ← App 独自の監査

【改善後】
OrchestrationService.create_task()
  → AutonomousPipeline.execute()         ← フレームワークに委譲
    → PlannerAgent (タスク分解)
    → RiskGateMiddleware (段階的信頼)
    → FlowExecutor (Agent 実行)
    → StepVerifierMiddleware (検証)
    → AuditMiddleware (監査)
```

### 具体的変更

**フレームワーク側 (NEW):** `harness/orchestration/app_adapter.py` (~150行)
- `AppOrchestrationAdapter` — 既存 App の `OrchestrationService` が AutonomousPipeline を利用するためのアダプタ
- App の `HarnessPlan` → `ExecutionPlan` への変換ロジック
- App の `TaskBlueprint` → PlannerAgent のコンテキスト注入

**App 側 (MODIFY):** `apps/messaging_hub/orchestration_service.py`
- `TaskHarnessPlanner` の直接利用を `AutonomousPipeline.execute()` 呼出に置換
- `ExecutionSubstrateService` の記録を `AuditMiddleware` のイベント購読に置換
- App 独自の `approval_manager.py` を `harness/approval/approval_manager.py` に統一

**削除候補（段階的）:**
- `task_harness.py` の `TaskHarnessPlanner` / `TaskHarnessVerifier` → フレームワークで代替
- `execution_substrate.py` の Session/Decision/Checkpoint 記録 → `AuditMiddleware` で代替
- App 独自 `approval_manager.py` → `harness/approval/approval_manager.py` に統一

---

## 問題 2: 巨大サービス分割（App 側 — 高優先）

### 現状

`flight_watch.py` が **2000行超**で 6つの責務を1ファイルに密結合:

```
flight_watch.py (2000行)
├── モデル定義 (DateWindow, FlightOffer, etc.) — 150行
├── NLP パーサー (正規表現で日付/空港/予算を抽出) — 80行
├── 検索プロバイダ (FakeFlightProvider, WebAggregatorFlightProvider) — 200行
├── ランキングエンジン (_rank_offers, _score_offer) — 50行
├── FlightWatchService (検索 + サブスクリプション管理) — 200行
├── 監視ループ (check_due_subscriptions) — 100行
└── 通知サービス (FlightNotificationService: WebSocket + Email) — 150行
```

### 解決策: 単一責務分割

```
apps/messaging_hub/flight/
├── __init__.py
├── models.py              ← モデル定義 (FlightSearchRequest, FlightOffer, etc.)
├── providers/
│   ├── __init__.py
│   ├── base.py            ← FlightSearchProvider Protocol
│   ├── fake.py            ← FakeFlightProvider (テスト用)
│   └── web.py             ← WebAggregatorFlightProvider
├── ranking.py             ← ランキングエンジン
├── search_service.py      ← FlightSearchService (検索のみ)
├── subscription_service.py ← サブスクリプション CRUD + 監視ループ
└── notification_service.py ← 通知サービス (WebSocket + Email)
```

**FlightWatchAgent (65行) は分割後も薄いまま** — これは正しい設計。
Agent は「入力受付 → サービス呼出 → 出力返却」のみ。ビジネスロジックはサービス層に置く。

ただし、AutonomousPipeline 統合後は **Agent を細粒度化** する選択肢がある:

```
【現状】1つの太い Agent
FlightWatchAgent → FlightWatchService.search() or .create_subscription()

【改善後】Pipeline が使う細粒度 Agent
FlightSearchAgent → search_service.search()
FlightRankingAgent → ranking.rank_offers()
FlightSubscriptionAgent → subscription_service.create()
FlightMonitorAgent → subscription_service.check_due()
FlightNotificationAgent → notification_service.notify()
```

これにより AutonomousPipeline が各ステップを個別に **リスク評価・検証・リトライ** できる。

---

## 問題 3: フレームワーク不足（フレームワーク側）

### 3-1. 長時間タスクサポートの欠如

**発見:** 機票監視は「6-24時間間隔で定期ポーリング」する長時間タスク。
現在の AutonomousPipeline は **単発実行** のみで、定期実行・サブスクリプション管理がない。

**必要なフレームワーク機能:**

```python
# harness/orchestration/scheduler.py (NEW)
class ScheduledPipeline:
    """定期実行パイプライン."""
    async def schedule(
        plan: ExecutionPlan,
        interval_hours: int,
        until: datetime | None = None,
    ) -> SubscriptionHandle

    async def check_due() -> list[ExecutionResult]
    async def cancel(subscription_id: str) -> None
```

これにより App の `FlightWatchService.check_due_subscriptions()` がフレームワーク機能に置換可能。

### 3-2. NLP 入力パース共通化の欠如

**発見:** `flight_watch.py` の正規表現パーサー (`_DATE_RANGE_PATTERN`, `_ORIGIN_PATTERN` 等) は
App に閉じているが、他の App でも「ユーザーの自然言語から構造化データを抽出する」パターンは共通。

**必要なフレームワーク機能:**
- `harness/context/input_extractor.py` — LLM ベースの構造化データ抽出
- PlannerAgent の一部として統合可能（計画生成時に入力も構造化）

**優先度:** 低（App 固有の正規表現は十分機能しており、無理にフレームワーク化する必要はない）

### 3-3. Provider Discovery の共通化

**発見:** `task_harness.py` の `TaskProviderDiscoveryService` は「Web 検索で適切なサービスを発見する」
汎用パターン。flight に限らず、他のタスクでも使える。

**必要なフレームワーク機能:**
- `harness/discovery/provider_discovery.py` — 汎用プロバイダ発見サービス
- 現在の `TaskProviderDiscoveryService` をそのまま昇格

**優先度:** 中（messaging_hub 以外のアプリも使いそうなら昇格）

---

## 問題 4: 具体的な統合シナリオ検証

### シナリオ A: 単発検索「NRT→LAX 7月の安い便を探して」

**Pipeline フロー:**
```
AutonomousPipeline.execute("NRT→LAX 7月の安い便を探して")
  │
  ├─ [1] GuardrailPipeline.run_pre_check() → PASS
  │
  ├─ [2] PlannerAgent.process()
  │     → ExecutionPlan:
  │       step-1: FlightSearchAgent (risk=LOW)
  │         input_spec: {origin: "NRT", destination: "LAX", depart_window: ...}
  │       step-2: FlightRankingAgent (risk=LOW, depends: [step-1])
  │         input_spec: {ranking_weights: default}
  │
  ├─ [3] DynamicFlowGenerator.generate_flow(plan)
  │     → FlowBuilder.then(FlightSearchAgent).then(FlightRankingAgent).build()
  │
  ├─ [4] FlowExecutor.execute()
  │     ├─ AuditMiddleware: step-1 start
  │     ├─ RiskGateMiddleware: LOW → ALLOW
  │     ├─ FlightSearchAgent.run() → offers[]
  │     ├─ StepVerifierMiddleware: PASS (offers non-empty)
  │     ├─ AuditMiddleware: step-1 complete
  │     ├─ AuditMiddleware: step-2 start
  │     ├─ RiskGateMiddleware: LOW → ALLOW
  │     ├─ FlightRankingAgent.run() → ranked_offers[]
  │     ├─ StepVerifierMiddleware: PASS
  │     └─ AuditMiddleware: step-2 complete
  │
  └─ [5] GuardrailPipeline.run_post_check() → PASS
```

**検出された問題:**
- PlannerAgent が NL から `{origin: "NRT", destination: "LAX"}` を抽出する必要がある
  → PlannerAgent のプロンプトに「入力パースも行え」と指示するか、別途パースステップを追加
- 現状の `FlightWatchAgent` は1つの Agent だが、Pipeline 統合には細粒度化が必要

### シナリオ B: 監視「NRT→LAX を監視して、5万円以下になったら通知して」

**Pipeline フロー:**
```
AutonomousPipeline.execute("NRT→LAX を監視して、5万円以下になったら通知して")
  │
  ├─ [2] PlannerAgent.process()
  │     → ExecutionPlan:
  │       step-1: FlightSearchAgent (risk=LOW)
  │       step-2: FlightRankingAgent (risk=LOW, depends: [step-1])
  │       step-3: FlightSubscriptionAgent (risk=MEDIUM, depends: [step-2])
  │         input_spec: {target_price: 50000, poll_interval_hours: 6}
  │
  ├─ [4] FlowExecutor.execute()
  │     ├─ ... step-1, step-2 同上 ...
  │     ├─ RiskGateMiddleware: MEDIUM → ALLOW + enhanced_logging
  │     ├─ FlightSubscriptionAgent.run() → subscription_id
  │     └─ StepVerifierMiddleware: PASS
  │
  └─ Result: {subscription_id: "fws_xxx", status: "monitoring"}
```

**フレームワーク不足が露出する箇所:**
- `step-3` でサブスクリプションは作成されるが、**定期監視ループ**はフレームワークにない
- 現状は `FlightWatchService.check_due_subscriptions()` が App 内のバックグラウンドタスクとして実行
- AutonomousPipeline は単発実行のみ → **ScheduledPipeline** が必要

---

## 改善計画（優先順位付き）

### Wave 1: フレームワーク統合 API（最優先）

| # | 作業 | ファイル | 行数 |
|---|------|---------|------|
| 1 | `AppOrchestrationAdapter` | `harness/orchestration/app_adapter.py` (NEW) | ~150 |
| 2 | `HarnessPlan → ExecutionPlan` 変換 | 上記に含む | -- |
| 3 | `OrchestrationService` の Pipeline 統合 | `apps/messaging_hub/orchestration_service.py` (MODIFY) | ~100行変更 |

### Wave 2: App 分割（高優先）

| # | 作業 | ファイル | 行数 |
|---|------|---------|------|
| 4 | `flight_watch.py` → `flight/` パッケージ分割 | `apps/messaging_hub/flight/` (NEW 6ファイル) | ~800合計 |
| 5 | 細粒度 Agent 化 | `apps/messaging_hub/agents/flight_{search,subscription,notification}_agent.py` | ~200合計 |
| 6 | `flight_watch.py` 削除（互換 re-export のみ残す） | `apps/messaging_hub/flight_watch.py` (MODIFY) | ~20 |

### Wave 3: フレームワーク強化（中優先）

| # | 作業 | ファイル | 行数 |
|---|------|---------|------|
| 7 | `ScheduledPipeline` | `harness/orchestration/scheduler.py` (NEW) | ~200 |
| 8 | `ProviderDiscoveryService` 昇格 | `harness/discovery/provider_discovery.py` (NEW) | ~150 |
| 9 | テスト | `tests/harness/orchestration/test_scheduler.py` 等 | ~300 |

### Wave 4: 統一・削除（低優先）

| # | 作業 | ファイル |
|---|------|---------|
| 10 | App 独自 `approval_manager.py` 削除 → Framework 版に統一 | `apps/messaging_hub/approval_manager.py` (DELETE) |
| 11 | `TaskHarnessPlanner` / `TaskHarnessVerifier` 削除 | `apps/messaging_hub/task_harness.py` (SIMPLIFY) |
| 12 | `execution_substrate.py` の記録機能を AuditMiddleware に統一 | `apps/messaging_hub/execution_substrate.py` (SIMPLIFY) |

---

## 検証方法

```bash
# 既存テスト（回帰確認）
conda run -n agentflow pytest tests/harness/orchestration/ -v
conda run -n agentflow pytest apps/messaging_hub/tests/ -v

# 統合テスト（Wave 1 完了後）
conda run -n agentflow pytest tests/harness/orchestration/test_app_adapter.py -v

# E2E テスト（Wave 2 完了後）
conda run -n agentflow pytest apps/messaging_hub/tests/test_flight_pipeline_e2e.py -v
```

---

## 設計原則まとめ

| 原則 | 説明 |
|------|------|
| **フレームワークは HOW を担当** | タスク分解、リスク評価、検証、監査、再計画 |
| **App は WHAT を担当** | Agent 定義、ビジネスロジック、ドメインモデル |
| **App は Pipeline の消費者** | `AutonomousPipeline.execute()` を呼ぶだけ |
| **Agent は薄いラッパー** | 入力受付 → サービス呼出 → 出力返却のみ |
| **サービスに単一責務** | 検索 / ランキング / サブスクリプション / 通知 を分離 |
| **二重実装は段階的に削除** | 新旧共存期間を設け、テスト確認後に旧を削除 |

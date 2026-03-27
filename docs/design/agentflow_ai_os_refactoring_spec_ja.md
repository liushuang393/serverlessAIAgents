# AgentFlow / Harness / AI OS 六層分離リファクタリング要求仕様書

## 0. 文書目的

本書は、現行 AgentFlow フレームワークを、AI 時代に適した **六層構成** へ再設計・再編成するための要求仕様書である。

目的は以下の三点である。

1. **責務分離の明確化**  
   AgentFlow、Harness、AI OS、業務アプリの責務を厳密に分離し、将来の拡張・差し替え・保守性を高める。

2. **多 Agent 協調の弱点を構造的に吸収**  
   共有状態不整合、役割固定化、通信ノイズ、局所最適、安全性不足、評価困難などの弱点を、アーキテクチャ層へ分解して対策する。

3. **実装可能な再配置仕様の提示**  
   package 名 / folder 名 / モジュール責務 / 禁止依存規則を明確化し、AI に実装委譲できる粒度まで整理する。

---

## 1. 基本方針

### 1.1 設計原則

- **Kernel は小さく安定に保つ**
- **Control Plane と Data Plane を分離する**
- **業務アプリは独立デプロイ可能とする**
- **共通機能は Shared Services へ集約する**
- **Governance は横断機能として集中管理する**
- **LLM / Tool / Protocol / App を疎結合にする**
- **契約（Contract）を先に定義し、実装はその後に従属させる**
- **多 Agent 協調の弱点は Prompt ではなく System 設計で吸収する**

### 1.2 六層構成の全体像

本再設計では、システム全体を以下の六層へ分離する。

1. **Layer 1: AI Infrastructure Layer**
2. **Layer 2: AI Gateway & Shared Services Layer**
3. **Layer 3: Agent Runtime / Flow Kernel Layer**
4. **Layer 4: Harness / Governance Layer**
5. **Layer 5: AI OS / Control Plane Layer**
6. **Layer 6: Domain Apps / Studios Layer**

---

# 2. 六層構成と責務定義

## 2.1 Layer 1: AI Infrastructure Layer

### 役割

AI 実行基盤、外部計算資源、永続化基盤、観測基盤などの最下層を提供する。

### 含めるもの

- LLM 推論基盤（vLLM, TGI, Ollama, OpenAI, Claude, Gemini 等）
- Embedding / Rerank 実行基盤
- Vector DB / RDB / Object Storage / Cache
- Queue / Redis / Kafka
- Secret Manager / KMS / IAM
- Sandbox / Browser Automation / Container Execution
- Metrics / Logs / Traces の基盤

### 含めないもの

- Agent の業務ロジック
- Flow 制御
- Policy 判断
- App 管理 UI

### 技術実装概要

- Provider Adapter パターンを採用する。
- 上位層からは抽象 Interface のみ参照する。
- 特定プロバイダへの直接依存を禁止する。
- 実行基盤差し替えは設定と Adapter で完結させる。

### 多 Agent 協調弱点への寄与

- 実行環境差異による挙動ぶれを抑える。
- 外部ツール失敗を再試行・隔離可能にする。
- 並列実行の資源競合を制御しやすくする。

---

## 2.2 Layer 2: AI Gateway & Shared Services Layer

### 役割

モデル・検索・共通リソース・認可・監査・追跡など、全 App / 全 Agent が共有すべき横断サービスを提供する。

### 含めるもの

- LLM Gateway
- Model Registry / Routing Policy
- Embedding Gateway / Rerank Gateway
- Shared RAG Service
- Resource Manager
- Scope Resolver / Access Resolver
- Audit Log Service
- Artifact Store
- Trace Store
- Evaluation Store
- Shared Config Loader

### 含めないもの

- Flow ノードの細かな実行制御
- Agent 間の具体的な会話戦略
- App ごとの画面や業務ロジック

### 技術実装概要

- API / SDK として提供する。
- 全てのモデルアクセスは Gateway 経由に統一する。
- RAG / Resource / Scope / Access の解決は共通サービスへ集約する。
- Trace / Audit / Artifact は共通の ID 系で紐付ける。

### 多 Agent 協調弱点への寄与

- **共有状態の乱れ**に対し、単一の Resource / Trace / Artifact 管理で整合性を高める。
- **古い前提で動く問題**に対し、Context Pack 生成元を統一する。
- **通信ノイズ**に対し、必要情報を構造化して配布する。

---

## 2.3 Layer 3: Agent Runtime / Flow Kernel Layer

### 役割

AgentFlow の中核実行層として、Flow / Agent / Tool の実行、状態遷移、イベント通知、プロトコル吸収を担当する。

### 含めるもの

- Flow Engine
- State Machine
- Agent Pattern 実装
- Planner / Router / Executor / Reviewer / Reporter の実行基盤
- Tool Runtime
- Retry / Timeout / Circuit Breaker
- Event Bus / Event Contract
- Protocol Adapter（MCP / A2A / AG-UI / A2UI）
- Plugin SPI

### 含めないもの

- テナント管理
- 監査ポリシー定義 UI
- 認可管理画面
- App 配布管理
- コスト管理ダッシュボード

### 技術実装概要

- Kernel は最小依存に保つ。
- 外部との接続は Protocol Adapter / Port 経由に限定する。
- Agent Pattern は抽象化し、業務ロジックは Domain Layer 側へ逃がす。
- Event は標準化し、flow.start / node.start / progress / tool.call / tool.result / review / flow.complete を厳密管理する。

### 多 Agent 協調弱点への寄与

- **役割固定化問題**に対し、動的 Router / Replan Hook を提供する。
- **通信過多問題**に対し、イベント契約と Context Pack を構造化する。
- **局所最適問題**に対し、Planner / Reviewer / Validator パターンを標準搭載する。
- **途中で崩れる問題**に対し、Checkpoint / Resume を提供する。

---

## 2.4 Layer 4: Harness / Governance Layer

### 役割

AI 実行の信頼性を担保する運用・制御・評価・安全・品質保証層である。  
Agent 自身ではなく、**Agent が安全かつ安定に動くための実行環境**を定義する。

### 含めるもの

- Context Policy
- Prompt Packaging Policy
- Tool Gating
- Approval Gate
- Risk Classification
- Budget / Loop Breaker
- Execution Recorder
- Validator / Scorer / Evaluator
- Replay / Trace Inspector
- Entropy Cleaner
- Contract Validator
- Schema Validator
- Guardrail Engine

### 含めないもの

- LLM 呼び出し詳細実装
- App 固有業務画面
- App ごとの個別文書テンプレート

### 技術実装概要

- Middleware / Hook / Interceptor として Runtime の上下に差し込む。
- 実行前 / 実行中 / 実行後に Policy 判定点を設ける。
- 全ての Tool 呼び出しを Gating 対象とする。
- 自動再試行は Budget / Risk / Failure Class に従って制御する。
- Trace から Eval を自動実行できるようにする。

### 多 Agent 協調弱点への寄与

- **安全性不足**に対し、権限・承認・監査を集中管理する。
- **誤った合意形成**に対し、Reviewer / Validator / Escalation を導入する。
- **無限ループ・高コスト化**に対し、Budget / Loop Breaker を適用する。
- **評価困難**に対し、Trace + Eval + Replay を標準化する。
- **文脈腐敗**に対し、Compression / Injection / Policy を明示制御する。

---

## 2.5 Layer 5: AI OS / Control Plane Layer

### 役割

システム全体の運用・管理・設定・配布・観測・登録を担う制御平面である。

### 含めるもの

- App Registry
- Agent Registry
- Tool Registry
- Template Registry
- Discovery
- Lifecycle Manager
- Publish / Deploy Manager
- Version Manager
- Tenant / Org Manager
- Config Center
- Governance Admin Console
- Trace Dashboard
- Cost Dashboard
- Operational Dashboard
- Environment Manager

### 含めないもの

- 各 App のビジネスロジック
- Runtime の内部状態管理ロジック
- Tool 実行の直接処理

### 技術実装概要

- Control Plane として独立デプロイする。
- 各 App は独立サービスとして動作し、Control Plane はメタデータ・設定・監視・操作のみ行う。
- App Manifest / Contract を単一真実源とする。
- Runtime を直接実装せず、管理 API を通して制御する。

### 多 Agent 協調弱点への寄与

- **複雑性の爆発**に対し、構成管理と標準化で抑制する。
- **運用不能化**に対し、全 Agent / App / Trace を統合可視化する。
- **設定散逸**に対し、Registry / Manifest 中心へ集約する。

---

## 2.6 Layer 6: Domain Apps / Studios Layer

### 役割

開発、業務自動化、研究開発支援、FAQ、移行工場など、具体的な業務価値を提供する製品群である。

### 含めるもの

- 開発支援 Studio
- Work Automation Studio
- R&D Studio
- Legacy Migration Studio
- FAQ Studio
- Decision Governance Studio
- Computer Assistant Studio

### 含めないもの

- 共通 Runtime
- 共通 Policy エンジン
- 共通 Trace / Audit 永続化

### 技術実装概要

- 各 App は独立デプロイ単位とする。
- App は Layer 2〜5 を利用するが、逆依存は禁止する。
- App ごとの manifest, routes, UI, orchestration template, artifacts を保持する。
- 業務特化ロジックはこの層に閉じ込める。

### 多 Agent 協調弱点への寄与

- **業務ロジックと基盤ロジックの混線**を防ぐ。
- **App 増加時の保守崩壊**を防ぐ。
- **業務ごとの役割設計差**を吸収しやすくする。

---

# 3. 多 Agent 協調の難しさを六層へ分解して吸収する設計

## 3.1 課題一覧

多 Agent 協調の代表的な弱点を以下の七つに整理する。

1. 共有状態の不整合
2. 役割分担の固定化
3. 通信過多とノイズ増幅
4. 局所最適化
5. 実行失敗と外部環境依存
6. 評価困難
7. 安全性・責任分界の曖昧さ

## 3.2 課題と層の対応表

### 課題1: 共有状態の不整合

**対策層**: Layer 2 / Layer 3 / Layer 4  
**対策**:

- Context Pack 契約を定義する。
- Shared Resource / Artifact / Trace を共通 ID 体系で管理する。
- Runtime では Snapshot / Checkpoint を標準化する。
- Harness で Compression / Refresh / Injection Policy を持つ。

### 課題2: 役割分担の固定化

**対策層**: Layer 3 / Layer 4 / Layer 6  
**対策**:

- Agent Role を静的定義ではなく Capability ベースへ変更する。
- Router / Replanner / Escalation Hook を標準実装する。
- Domain App ごとに可変チーム編成テンプレートを持てるようにする。

### 課題3: 通信過多とノイズ増幅

**対策層**: Layer 2 / Layer 3 / Layer 4  
**対策**:

- 自由会話ではなく、構造化メッセージとする。
- Event / Message Schema を定義する。
- 要約・抄録・差分伝達を標準化する。
- Harness で message budget / relay policy を制御する。

### 課題4: 局所最適化

**対策層**: Layer 3 / Layer 4 / Layer 5  
**対策**:

- Planner / Reviewer / Validator / Integrator を役割パターンとして標準化する。
- Exit Criteria / Success Criteria を Flow 定義へ明示する。
- Global Objective を Control Plane から注入可能にする。

### 課題5: 実行失敗と外部環境依存

**対策層**: Layer 1 / Layer 2 / Layer 3 / Layer 4  
**対策**:

- Provider Adapter と Sandbox で隔離する。
- Failure Classifier を導入する。
- Retry / Fallback / Human Escalation を標準化する。
- Tool Gating と Timeout / Circuit Breaker を必須化する。

### 課題6: 評価困難

**対策層**: Layer 2 / Layer 4 / Layer 5  
**対策**:

- Trace を唯一の真実源とする。
- Eval / Replay / Score を Harness に組み込む。
- Pass@1, Recovery Rate, Tool Success Rate, Loop Abort Rate, Cost per Outcome を標準指標化する。

### 課題7: 安全性・責任分界の曖昧さ

**対策層**: Layer 4 / Layer 5  
**対策**:

- 権限委譲を Scope / Action 単位で管理する。
- Approval Gate を実行前後に差し込む。
- Audit / Explainability / Escalation Path を必須化する。
- 高リスク操作は Human-in-the-loop を強制する。

---

# 4. 実装リファクタリング要求

## 4.1 目的

現行リポジトリの package / folder / module を六層へ対応付け、責務混在を解消する。

## 4.2 再編成の基本ルール

- Runtime Core は `kernel` に閉じ込める。
- Governance は `harness` として独立させる。
- Shared Service は `shared` または `gateway` に集約する。
- Control Plane は `platform` に集約する。
- 業務 App は `apps` に集約する。
- Domain 固有処理は `domain` または各 App 配下へ移す。
- 契約は `contracts` に集約し、全層がそれを参照する。
- 逆依存は禁止する。

---

# 5. 推奨ディレクトリ / package 再編成案

## 5.1 目標構造

```text
repo-root: agentflow/
  contracts/  # Layer 0
    app/
    flow/
    tool/
    context/
    trace/
    policy/
    artifact/
    protocol/

  infrastructure/ # Layer 1
    llm/
    embeddings/
    rerank/
    storage/
    vector/
    cache/
    queue/
    sandbox/
    browser/
    secrets/
    observability/

  shared/ # Layer 2
    gateway/
      llm/
      embedding/
      rerank/
    rag/
    resources/
    scope/
    access/
    artifacts/
    trace/
    audit/
    config/
    registry/

  kernel/ # Layer 3
    flow/
    runtime/
    state/
    agents/
    planner/
    router/
    executor/
    reviewer/
    reporter/
    tools/
    plugins/
    events/
    protocols/
      mcp/
      a2a/
      agui/
      a2ui/

  harness/ # Layer 4
    context/
    prompt/
    gating/
    approval/
    risk/
    budget/
    validation/
    scoring/
    evaluation/
    replay/
    guardrails/
    cleanup/
    policies/

  platform/ # Layer 5
    api/
    ui/
    registry/
    discovery/
    lifecycle/
    deploy/
    publish/
    environments/
    tenants/
    config-center/
    governance-console/
    dashboards/
    operations/

  apps/ # Layer 6 実際Aｐｐ機能により、工程名は変更されることがある。下記は例です。
    dev-studio/
    work-automation-studio/
    rd-studio/
    migration-studio/
    faq-studio/
    governance-studio/
    computer-assistant/

  tests/ # Layer test
    contracts/
    integration/
    e2e/
    eval/
```

---

## 5.2 package 命名規約

### Layer 1

- `infrastructure.llm.*`
- `infrastructure.storage.*`
- `infrastructure.vector.*`
- `infrastructure.sandbox.*`

### Layer 2

- `shared.gateway.llm.*`
- `shared.rag.*`
- `shared.resources.*`
- `shared.scope.*`
- `shared.trace.*`
- `shared.audit.*`

### Layer 3

- `kernel.flow.*`
- `kernel.runtime.*`
- `kernel.agents.*`
- `kernel.tools.*`
- `kernel.protocols.*`
- `kernel.events.*`

### Layer 4

- `harness.context.*`
- `harness.gating.*`
- `harness.validation.*`
- `harness.evaluation.*`
- `harness.policies.*`

### Layer 5

- `platform.registry.*`
- `platform.lifecycle.*`
- `platform.publish.*`
- `platform.governance.*`
- `platform.dashboards.*`

### Layer 6

- `apps.dev_studio.*`
- `apps.work_automation_studio.*`
- `apps.migration_studio.*`
- `apps.faq_studio.*`

---

# 6. 依存方向の制約

## 6.1 許可依存

- Layer 6 -> Layer 5, 4, 3, 2, 1
- Layer 5 -> Layer 4, 3, 2, 1
- Layer 4 -> Layer 3, 2, 1
- Layer 3 -> Layer 2, 1
- Layer 2 -> Layer 1
- Layer 1 -> 外部基盤

## 6.2 禁止依存

- Layer 3 -> Layer 5 / 6
- Layer 4 -> Layer 5 / 6
- Layer 2 -> Layer 3 / 4 / 5 / 6
- Layer 1 -> 上位層全般
- App から他 App 内部への直接依存
- Platform から App 内部ロジックへの直接依存

## 6.3 機械的強制

- import linter を導入する。
- package boundary test を追加する。
- forbidden dependency rule を CI に組み込む。
- AI 生成コードも同じ boundary test を通過必須とする。

---

# 7. Contract 先行設計要求

AI 実装の前提として、以下 Contract を先に固定する。

## 7.1 必須 Contract

- `FlowDefinition`
- `FlowExecutionState`
- `AgentRoleSpec`
- `ContextPack`
- `ToolRequest`
- `ToolResult`
- `ArtifactManifest`
- `TraceRecord`
- `PolicyDecision`
- `ApprovalRequest`
- `EvalResult`
- `AppManifest`
- `DeploymentSpec`
- `ProtocolMessage`

## 7.2 要求事項

- JSON Schema / Pydantic / OpenAPI などで厳密化する。
- 破壊的変更には versioning を必須とする。
- すべての層は Contract に依存し、実装詳細には依存しない。

---

# 8. AI 実装タスクへの分解

## Phase 1: Contract 固定

- contracts 配下を新設する。
- Context / Tool / Trace / Policy / AppManifest の schema を定義する。
- 既存イベント型を新 Contract に寄せる。

## Phase 2: Kernel 抽出

- Flow / Runtime / Agent / Tool / Protocol を `kernel/` へ移動する。
- Platform / App 固有ロジックを Kernel から除去する。
- Plugin SPI を整理する。

## Phase 3: Shared Service 抽出

- LLM Gateway, RAG, Resource, Scope, Trace, Audit を `shared/` へ移動する。
- 各 App / Kernel から共通利用させる。

## Phase 4: Harness 独立化

- Context Policy, Tool Gating, Validation, Eval, Budget, Replay を `harness/` へ抽出する。
- Runtime Hook として接続する。

## Phase 5: Platform 再構築

- registry / lifecycle / publish / dashboards / governance-console を `platform/` へ集約する。
- App 直接依存を除去する。

## Phase 6: Apps 再配置

- 各 Studio / App を `apps/` に明確分類する。
- App 固有の orchestration, prompts, templates, UI, artifacts を閉じ込める。

## Phase 7: Boundary Enforcement

- import 制約、CI、contract tests、integration tests、eval tests を追加する。

---

# 9. AI への具体指示テンプレート

以下の方針で AI 実装を進めること。

1. まず `contracts/` を新設し、全主要 Contract を schema 化すること。
2. 既存コードを調査し、Kernel / Shared / Harness / Platform / Apps に分類表を作成すること。
3. 一度に全面書き換えせず、モジュール単位で移設すること。
4. 各移設ごとに import 境界 test を追加すること。
5. Runtime から Platform 依存を除去すること。
6. Governance / Eval / Audit を App から引き剥がして Harness へ集約すること。
7. App は manifest 駆動へ寄せ、Control Plane から発見・操作可能にすること。
8. 多 Agent 協調については、自由会話型ではなく、ContextPack / Event / RoleSpec / ReviewLoop を中心に組み直すこと。
9. すべての主要処理に Trace ID / Artifact ID / Flow ID / Policy Decision ID を付与すること。
10. 既存互換性が必要な箇所には Adapter を置き、段階移行を可能にすること。

---

# 10. 最終設計判断

本再設計において、以下を正式方針とする。

- **AgentFlow は「実行内核」であり、AI OS 全体そのものではない。**
- **Harness は Agent の信頼性を支える制御環境であり、業務 App ではない。**
- **AI OS は Control Plane と Shared Governance の集合であり、Runtime と分離する。**
- **業務価値は Domain Apps / Studios で提供し、基盤へ逆流させない。**
- **多 Agent 協調の難しさは、Prompt 調整ではなく、分層設計・契約・ガードレール・観測性で吸収する。**

以上を、今後の AgentFlow 再設計および AI 実装委譲の基準仕様とする。

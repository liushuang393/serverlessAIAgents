# 六層分離レビューと不足是正計画

## 要約
- 現状判定は「部分着手」で、六層分離は未完了です。新しい箱は作られていますが、実体の大半はまだ旧 `agentflow/` と `apps/platform` に残っています。
- 仕様書 `5.1` との主要不一致は 5 点です。
  - `platform/` が存在せず、代わりに `control_plane/` がある
  - 目標ディレクトリの未作成がある
  - 作成済みディレクトリの多くが placeholder/export のみ
  - 新層が旧 `agentflow/` を直接 import しており、厳密境界になっていない
  - `apps/platform` がまだ Layer 5 本体で、top-level Layer 5 が実運用されていない
- モジュール化も未達です。差し替え口があるのは実質 `LLM`、`TraceExporter`、一部 `Approval/Evaluation/Gating` だけで、仕様で要求した Embedding / Rerank / RAG / Artifact / Discovery / Lifecycle / Protocol には未展開です。

## 層別レビュー
- Layer 0 `contracts`
  - 必須 8 バケットは存在します。
  - 余剰バケット `contracts/runtime` と `contracts/evolution` があり、仕様の目標構造から外れています。
  - `contracts/runtime` は Layer 0 契約ではなく内部プラグイン設定なので、`contracts/base.py` と `shared/registry/*` へ分離すべきです。
  - `contracts/evolution` は以下へ再配分して消すべきです。
    - `app-evolution-config` -> `contracts/app/`
    - `retrieval-decision`, `validation-event` -> `contracts/policy/`
    - `strategy-capsule` -> `contracts/artifact/`
    - `strategy-search.request/response` -> `contracts/protocol/`
- Layer 1 `infrastructure`
  - `embeddings/`, `rerank/` が未作成です。
  - `vector/`, `cache/`, `queue/`, `sandbox/`, `browser/`, `secrets/` は placeholder のみです。
  - `llm/`, `storage/` は旧 `agentflow` 実装を直 import しており、真の抽出ではありません。
- Layer 2 `shared`
  - `rag/`, `artifacts/` が未作成です。
  - `resources/` は placeholder のみです。
  - `access/service.py` が `agentflow.security.policy_engine.AuthContext` に依存しており、Shared 契約化できていません。
- Layer 3 `kernel`
  - `state/`, `planner/`, `executor/`, `reporter/` が未作成です。
  - `flow/`, `runtime/`, `agents/`, `router/`, `reviewer/`, `protocols/` は export/placeholder だけです。
  - `kernel.flow` と `kernel.runtime` は旧 `agentflow.flow` / `agentflow.runtime` の facade で、Kernel 抽出は未達です。
  - `kernel/protocols/mcp|a2a|agui|a2ui` も未分割です。
- Layer 4 `harness`
  - `prompt/` が未作成です。
  - `budget/`, `context/`, `guardrails/`, `policies/`, `replay/`, `risk/`, `scoring/`, `validation/`, `cleanup/` は placeholder のみです。
  - 余剰の `harness/hooks` があり、仕様外です。これは `harness/policies/runtime_pipeline.py` に吸収して消すべきです。
- Layer 5 `platform`
  - 仕様上の `platform/` がなく、`control_plane/` で代替しているため非準拠です。
  - `api/`, `ui/`, `deploy/` が未作成です。
  - `config_center/`, `dashboards/`, `environments/`, `governance_console/`, `operations/`, `publish/`, `tenants/` は placeholder のみです。
  - `control_plane` は tests からしか使われておらず、実運用には未接続です。
  - 実装本体はまだ `apps/platform/{services,routers,schemas,db,frontend,agents}` に残っています。
- Layer 6 `apps`
  - Domain App 群は存在しますが、`apps/platform` と `apps/auth_service` は Layer 6 に置くべきではありません。
  - `apps/platform` は Layer 5 へ再配置、`apps/auth_service` は `shared/access` と `platform/tenants` へ分解すべきです。
- `tests`
  - `tests/contracts/`, `tests/eval/` が未作成です。
  - 余剰の `tests/layers/` があり、ここは `tests/contracts/` と `tests/integration/boundary/` と `tests/eval/` に再配置して消すべきです。

## 先に直す構造
- `control_plane/` を廃止し、仕様どおり top-level `platform/` を作る。
  - `apps/platform/routers` -> `platform/api`
  - `apps/platform/frontend` -> `platform/ui`
  - `apps/platform/services/app_discovery*` -> `platform/discovery`
  - `apps/platform/services/app_lifecycle*`, `runtime_command_resolver`, `port_allocator` -> `platform/lifecycle` / `platform/environments`
  - `apps/platform/services/publish_orchestrator*` -> `platform/publish`
  - `apps/platform/services/tenant_*` -> `platform/tenants` / `platform/dashboards`
  - `apps/platform/services/llm_management*`, `framework_env` -> `platform/environments` / `platform/config_center`
- 目標構造に足りないディレクトリを exact で追加する。
  - `infrastructure/embeddings`, `infrastructure/rerank`
  - `shared/rag`, `shared/artifacts`, `shared/gateway/embedding`, `shared/gateway/rerank`
  - `kernel/state`, `kernel/planner`, `kernel/executor`, `kernel/reporter`, `kernel/protocols/mcp`, `a2a`, `agui`, `a2ui`
  - `harness/prompt`
  - `platform/api`, `platform/ui`, `platform/deploy`
  - `tests/contracts`, `tests/eval`
- 仕様外ディレクトリは消す。
  - `control_plane/`
  - `contracts/runtime/`
  - `contracts/evolution/`
  - `harness/hooks/`
  - `tests/layers/`

## 削除・深度統合・暫定互換の判断
- すぐ削除対象
  - `control_plane/`
  - `contracts/runtime/`
  - `contracts/evolution/`
  - `harness/hooks/`
  - `tests/layers/`
- 深度 merge 対象
  - `agentflow/flow`, `runtime`, `state`, `agents`, `patterns`, `orchestration`, `protocols`, `providers`, `storage`, `sandbox`, `observability`, `knowledge`, `governance`, `hitl`, `discovery`, `deploy`, `security`
  - `apps/platform/{services,schemas,routers,db,agents,frontend,migrations}`
  - `apps/auth_service/{api,core,db,providers,frontend}`
- 暫定互換として一時保持
  - `agentflow/contracts.py`
  - 旧公開 import を新層へ向ける facade
  - `apps/platform/main.py` と `app_config.json` だけの shell
- 最終的に `agentflow/` は「互換 facade 専用」に縮退し、実装本体を置かない形にする。

## 境界とモジュール化の完了条件
- `contracts / infrastructure / shared / kernel / harness / platform / apps / tests` 以外に六層本体ディレクトリを増やさない。
- 新層から `agentflow.*` への import を全面禁止する。許可するのは旧互換側が新層を import する向きだけです。
- `platform` から `apps/*` 内部ロジック直接 import を禁止する。利用は manifest と管理 API のみ。
- 各機能に `default / noop / mock / provider-specific` の差し替え口を持たせる対象は次で固定する。
  - Layer 1: llm, embeddings, rerank, storage, vector, cache, queue, sandbox, browser, secrets, observability
  - Layer 2: rag, artifacts, trace, audit
  - Layer 5: discovery, lifecycle
  - Layer 3: protocol adapters, tool runtime plugins
- boundary test は `agentflow` 旧実装依存も違反として検知するように更新する。

## テスト計画
- `tests/contracts/`
  - 全必須契約の schema snapshot
  - 旧 payload 互換
  - versioning 確認
- `tests/integration/`
  - layer boundary
  - platform discovery/lifecycle
  - kernel + harness 連携
- `tests/eval/`
  - approval, validation, scoring, evaluation, replay の degraded mode と on/off 切替
- 受け入れ条件
  - `platform/` が存在し、`control_plane/` が消えている
  - 仕様外ディレクトリが消えている
  - 新層が `agentflow.*` を import していない
  - `apps/platform` は shell のみ
  - placeholder-only ディレクトリがゼロになっている

## 追加指摘：コード重複と不整合

### 1. `contracts/runtime` と `shared/registry` の完全重複
- `contracts/runtime/{base,components,registry}.py` と `shared/registry/{components,factory_registry}.py` は **同一クラス群** (`ContractModel`, `ComponentToggle`, `ComponentSpec`, `LayerName`, `ToggleableFactoryRegistry`) を二重定義しています。
- 実際の使用先（`infrastructure/llm/registry.py`, `infrastructure/observability/registry.py`, `harness/*`）はすべて `shared.registry` から import しており、`contracts/runtime` 側は **死んだコード** です。
- `contracts/runtime` の `LayerName` は `CONTROL_PLANE = "control_plane"` だが、`shared/registry` の `LayerName` は `PLATFORM = "platform"` で **enum 値が不一致** です。
- 修正方針：
  - `ContractModel` は `contracts/base.py` に既存なのでそちらを正とし、`contracts/runtime/base.py` は削除。
  - `ComponentToggle`, `ComponentSpec`, `LayerName` は Layer 0 契約として `contracts/base.py` または `contracts/policy/contracts.py` へ統合。
  - `ToggleableFactoryRegistry`, `RegisteredComponent` は実装ロジックなので `shared/registry/` を正とし、`contracts/runtime/registry.py` は削除。
  - `contracts/runtime/` パッケージ全体を消す。
  - `LayerName` の enum 値は仕様準拠で `PLATFORM = "platform"` へ統一する。

### 2. `contracts/evolution` の空虚
- `contracts/evolution/` にはファイルが1つも存在しません（`__init__.py` すらなし）。
- 仕様外ディレクトリかつ空なので、即時削除可能です。

### 3. `platform/` の `__init__.py` 欠落
- `platform/` に `__init__.py` が存在しないため、Python パッケージとして認識されません。
- `platform/discovery/service.py` 等は `from shared import load_app_manifest` しており動作しますが、`platform.*` としての import は不可能です。
- 修正：`platform/__init__.py` を追加し、パッケージ化する。

### 4. `agentflow.*` 逆依存の詳細
- 以下のファイルが旧 `agentflow.*` に直接依存しており、六層分離の境界規約に違反しています：

| 層 | ファイル | 依存先 |
|---|---|---|
| L1 | `infrastructure/llm/adapters.py` | `agentflow.providers.llm_provider` |
| L1 | `infrastructure/storage/adapters.py` | `agentflow.storage.memory_backend` |
| L3 | `kernel/tools/executor.py` (TYPE_CHECKING) | `agentflow.providers.tool_provider` |
| L3 | `kernel/runtime/__init__.py` | `agentflow.runtime` (re-export facade) |
| L3 | `kernel/flow/__init__.py` | `agentflow.flow` (re-export facade) |
| L4 | `harness/gating/tool_gate.py` | `agentflow.governance`, `agentflow.providers.tool_provider` |
| L4 | `harness/hooks/runtime.py` (TYPE_CHECKING) | `agentflow.governance`, `agentflow.providers.tool_provider` |

- `kernel/runtime/__init__.py` と `kernel/flow/__init__.py` は旧モジュールの **丸ごと re-export** であり、facade パターンにすぎません。これは「抽出」ではなく「名前空間エイリアス」です。
- 修正方針：
  - `TYPE_CHECKING` 内の型参照 → 先に `contracts/` に Protocol/型を定義し、そちらを参照。
  - `agentflow.providers.llm_provider` → `infrastructure/llm/ports.py` の `LLMBackend` Protocol に置換。
  - `agentflow.governance` → `contracts/policy/` に `GovernanceDecision` 型を移動し、`harness/gating/` から参照。
  - `kernel/flow/__init__.py`, `kernel/runtime/__init__.py` → 旧 facade を廃止し、実ロジックを抽出移動。

### 5. L2→L1 依存の妥当性
- `shared/trace/service.py` → `infrastructure.observability.get_trace_exporter` は **正しい依存方向** です（L2→L1 は許可）。
- `shared/gateway/llm/service.py` → `infrastructure.llm.get_llm_backend` も正方向です。
- これらは問題ありません。

### 6. 実装済み差し替え口の棚卸し
- 仕様で要求した差し替え口（default/noop/mock/provider-specific）の達成状況：

| 対象 | 仕様要求 | ports.py | adapters.py | registry.py | 判定 |
|---|---|---|---|---|---|
| `infrastructure/llm` | ✅ | ✅ | ✅ (noop,mock,agentflow) | ✅ | **済** |
| `infrastructure/observability` | ✅ | ✅ | ✅ (noop,logging,memory) | ✅ | **済** |
| `infrastructure/storage` | ✅ | ✅ | ✅ (noop,memory) | ❌ registry なし | **部分** |
| `infrastructure/embeddings` | ✅ | ❌ | ❌ | ❌ | **未着手** |
| `infrastructure/rerank` | ✅ | ❌ | ❌ | ❌ | **未着手** |
| `infrastructure/vector` | ✅ | ❌ | ❌ | ❌ | **未着手** |
| `infrastructure/cache` | ✅ | ❌ | ❌ | ❌ | **未着手** |
| `infrastructure/queue` | ✅ | ❌ | ❌ | ❌ | **未着手** |
| `infrastructure/sandbox` | ✅ | ❌ | ❌ | ❌ | **未着手** |
| `infrastructure/browser` | ✅ | ❌ | ❌ | ❌ | **未着手** |
| `infrastructure/secrets` | ✅ | ❌ | ❌ | ❌ | **未着手** |
| `shared/rag` | ✅ | ❌ | ❌ | ❌ | **未着手** |
| `shared/artifacts` | ✅ | ❌ | ❌ | ❌ | **未着手** |
| `shared/trace` | ✅ | - | - | - | **済**（exporter 経由） |
| `shared/audit` | ✅ | - | - | - | **済**（ログ出力） |
| `platform/discovery` | ✅ | - | - | - | **済**（service 実装あり） |
| `platform/lifecycle` | ✅ | - | - | - | **済**（service 実装あり） |
| `kernel/protocols` | ✅ | ❌ | ❌ | ❌ | **未着手** |

### 7. Harness 層の実装品質
- `harness/approval/service.py`, `harness/evaluation/service.py` は ComponentToggle 付きのサービスとして実装済みです。
- `harness/gating/tool_gate.py` は `agentflow.governance.GovernanceEngine` に直接依存しているため、境界違反です。
- `harness/hooks/runtime.py` は `HarnessedToolRuntime` として Layer 3 と Layer 4 を接合する唯一のモジュールですが、仕様外の `hooks/` に配置されています。`harness/policies/runtime_pipeline.py` へ移動すべきです。
- `harness/budget`, `context`, `guardrails`, `policies`, `replay`, `risk`, `scoring`, `validation`, `cleanup` はすべて docstring のみの `__init__.py` しかなく、実装ゼロです。

## 優先度付き是正ロードマップ

### P0: 構造整合（即時）
1. `contracts/runtime/` を削除し、`contracts/base.py` と `shared/registry/` に統合。`LayerName` を `PLATFORM` に統一。
2. `contracts/evolution/` を削除。
3. `platform/__init__.py` を追加。
4. `harness/hooks/` を `harness/policies/` に移動して削除。
5. `tests/layers/` の `.py` ファイルを `tests/integration/boundary/` と `tests/eval/` に移動して削除。

### P1: 境界浄化（短期）
6. `kernel/flow/__init__.py` と `kernel/runtime/__init__.py` の re-export facade を廃止し、実ロジックを `agentflow/` から抽出移動。
7. `infrastructure/llm/adapters.py` の `AgentFlowLLMBackend` を `agentflow.providers` 非依存にする（`contracts/` の Protocol 経由に変更）。
8. `infrastructure/storage/adapters.py` の `agentflow.storage` 依存を外す。
9. `harness/gating/tool_gate.py` の `agentflow.governance` 依存を `contracts/policy/` Protocol に置換。
10. `infrastructure/storage/` に `registry.py` を追加し、差し替えパターンを完成させる。

### P2: 未着手モジュールの最低限実装（中期）
11. `infrastructure/{embeddings,rerank,vector,cache,queue,sandbox,browser,secrets}` に ports/adapters/registry の三点セットを追加。
12. `shared/{rag,artifacts}` の基本サービスを実装。
13. `kernel/{state,planner,executor,reporter}` の最低限の Protocol + NoOp 実装。
14. `harness/{budget,context,guardrails,policies,replay,risk,scoring,validation,cleanup}` に最低限のサービスを実装。
15. `kernel/protocols/{mcp,a2a,agui,a2ui}` の分割。

### P3: Platform 層完成（中〜長期）
16. `apps/platform/` の routers/services/schemas/db を `platform/` へ移動。
17. `apps/platform/` を shell（`main.py` + `app_config.json` のみ）に縮退。
18. `apps/auth_service/` を `shared/access` と `platform/tenants` へ分解。
19. `control_plane/` を完全削除。

## 前提と既定値
- 仕様書のディレクトリ名を優先し、`platform/` を採用します。標準ライブラリ `platform` との衝突は repo 内 import 監査で解消します。
- 仕様書中の `config-center` などの表記は、実パッケージ名では `config_center` など Python 互換の underscore 名に固定します。
- `apps/*` の studio 名は例示扱いとし、既存 product 名は維持してよいです。ただし `apps/platform` と `apps/auth_service` は Layer 6 から外します。
- `contracts/runtime` と `shared/registry` の重複は `shared/registry` 側を正とし、`contracts/runtime` は即座に削除対象です。

# 六層分離リファクタリング実行計画

> 履歴注記: この文書は旧リファクタリング計画の記録です。本文中の `apps/platform` および
> top-level `platform/` への言及は当時計画上の名称であり、現行リポジトリの control plane 実装は
> `control_plane/` にあります。

## 概要

- 当時計画では、正規実装パスを top-level の `contracts / infrastructure / shared / kernel / harness / platform / apps` とする想定だった。既存 `agentflow/` と `apps/platform` は第1周では削除せず、互換アダプタ兼 composition root として残す前提だった。
- 実装順は「契約固定（前置）→ Layer 1 → 2 → 3 → 4 → 5 → 6 → Boundary Enforcement → 微調整」。外部 API・CLI・`app_config.json` 契約は初回では壊さない。
- 方針は「再実装最小、移設優先」。既存でテスト済みの機能は流用し、責務混在だけを剥がす。細部最適化は後回しにする。
- フェーズごとに保持する文脈は「対象層 + 参照契約 + 直近テスト + 互換アダプタ」のみとし、無関係な app/UI/prompt は読み込まない。

## 公開契約と互換方針

- `contracts/` に `FlowDefinition / FlowExecutionState / AgentRoleSpec / ContextPack / ToolRequest / ToolResult / ArtifactManifest / TraceRecord / PolicyDecision / ApprovalRequest / EvalResult / AppManifest / DeploymentSpec / ProtocolMessage` を versioned Pydantic + JSON Schema で固定する。
- 流用元は既存の `FlowDefinition`、`ToolResult`、`ApprovalRequest`、`app_config` schema、CMA の artifact models を優先し、重複定義は契約へ一本化して旧 import から re-export する。
- 全主要処理に `flow_id / trace_id / artifact_id / policy_decision_id` を必須付与し、Trace/Audit/Artifact を横断 ID で結ぶ。
- `agentflow.*` の公開 import と、当時想定していた `apps/platform` の既存 HTTP パスは互換維持する。正規実装の import 先だけ新層へ差し替える。

## フェーズ別実装

**前置工程: Contract 固定**

- 設計: 契約候補を `reuse / split / discard / defer` に分類し、`contracts/app|flow|tool|context|trace|policy|artifact|protocol` に配置する。当時の `apps/platform` schema と CMA artifact schema は契約候補として取り込み、app 固有部分だけ app 側へ残す。
- 実装: 新契約を追加し、既存モデルは `model_validate`/adapter で新契約へ寄せる。`ToolResult` や `ApprovalRequest` の重複定義は互換 facade のみに縮退させる。
- テスト: schema snapshot、JSON fixture 互換、旧 import 互換、契約 versioning テストを追加する。
- Context: 契約候補モデル、`app_config.json`、代表契約テストのみ。UI、prompt、個別業務ロジックは除外。

**Layer 1: AI Infrastructure**

- 設計: `agentflow/llm gateway`, provider, storage, vector/cache/queue, sandbox/browser, observability exporter, secret/IAM 接続を `infrastructure/` へ抽出する。ここでは実行基盤 adapter だけを持ち、policy・flow・app 判定は禁止する。
- 実装: direct provider access を infrastructure の adapter に閉じ込める。`agentflow.providers.*` と低レベル client は互換 adapter 化し、上位層は port/interface 経由に切り替える。
- テスト: 既存の provider import guard を infrastructure 以外全面禁止へ拡張し、LLM gateway/router、storage/sandbox、observability の既存 unit を新パスで通す。
- Context: provider/llm/storage/sandbox/observability とそのテストのみ。platform/app/harness の実装詳細は見ない。

**Layer 2: AI Gateway & Shared Services**

- 設計: LLM Gateway、RAG、resource/scope/access、artifact/trace/audit store、shared config/registry を `shared/` へ集約する。Kernel はここを API/SDK として使うだけにする。
- 実装: `LLM contract resolver`、knowledge/RAG、runtime context・tenant/scope・manifest resolver を shared service に再配置する。`apps/auth_service` の本体責務は `shared.access` と `platform.tenants` に分割し、必要なら deploy shell だけ残す。
- テスト: llm contract、RAG/access/scope、manifest runtime、multi-tenant/runtime context の既存テストを shared 向けに再配置する。
- Context: gateway/RAG/access/trace/audit/manifest 周辺のみ。flow engine、app orchestration、frontend は除外。

**Layer 3: Agent Runtime / Flow Kernel**

- 設計: `agentflow.core / flow / engines / orchestration / patterns / protocols / tool runtime` を `kernel/` に集約し、platform/app/governance 依存を除去する。Kernel は `contracts + shared + infrastructure` にのみ依存する。
- 実装: flow/state/agent/router/executor/reviewer/reporter/events/protocols を `kernel/` に移し、`ToolExecutor` から governance/HITL を外して純粋実行器にする。旧 `agentflow/` は kernel の re-export 層へ縮退する。
- テスト: flow/orchestration/protocol/create_flow/compat import を新旧両経路で通し、event contract を `flow.start / node.start / progress / tool.call / tool.result / review / flow.complete` に固定する。
- Context: kernel と contracts/shared/infrastructure の境界だけ保持。platform/app 固有コードは除外。

**Layer 4: Harness / Governance**

- 設計: governance、policy、approval、budget、context compression、validation、evaluation、replay を `harness/` に抽出し、Kernel に hook/middleware として差し込む。CMA の `workflow/control_plane.py` のうち汎用部分は harness へ昇格させる。
- 実装: `GovernanceEngine / PolicyEngine / ApprovalFlow / Context Budget / Validator / Eval / Guardrail` を `harness/` にまとめ、Tool 実行前後と Flow 実行前後に判定点を置く。
- テスト: plugin governance、policy engine、HITL、budget/context、validator/eval/replay の既存テストを harness に寄せ、policy_decision_id と audit 連携を確認する。
- Context: governance/policy/hitl/context-policy/eval のみ。platform dashboard や app UI は除外。

**Layer 5: AI OS / Control Plane**

- 設計: 当時の `apps/platform` が持っていた discovery/lifecycle/publish/dashboard/config/tenant 管理を top-level `platform/` に抽出する想定だった。`apps/platform` は FastAPI/React の起動 shell とし、内部ロジックを `platform.*` 呼び出しへ置換する計画だった。
- 実装: app registry、discovery、lifecycle、publish、config center、dashboards、tenant/org 管理を `platform/` へ移し、`AppManifest` を単一真実源に統一する。`platform` から app 内部ロジックを直接 import しない構造へ改める。
- テスト: app discovery、lifecycle、platform API、schema validation、`/api/studios/*` 互換テストを追加する。
- Context: platform services、manifest、registry、existing platform tests のみ。kernel/harness 内部は参照 API だけに制限する。

**Layer 6: Domain Apps / Studios**

- 設計: `apps/*` 全体を再分類し、Domain App と shared/control-plane 候補を分離する。既定分類は `code_migration_assistant=Migration Studio`, `faq_system=FAQ Studio`, `messaging_hub=Computer Assistant`, `decision_governance_engine / design_skills_engine / market_trend_monitor / orchestration_guardian / legacy_modernization_geo_platform=Layer6 app`, `platform / auth_service=Layer5/2 側へ再分類候補` という当時計画だった。
- 実装: 各 app には manifest、routes、prompts、templates、artifacts、orchestration だけを残し、共通ロジックは下位層へ戻す。CMA は session/backlog/artifact 契約を維持したまま lower layers を参照する構造へ切り替える。
- テスト: 各 app の contract tests、manifest discoverability、主要 1 フローずつの integration、Platform からの発見/起動/状態取得を確認する。CMA backend state API の既存不整合もこの段で解消する。
- Context: 対象 app と参照契約のみ。ほかの app の業務詳細は読まない。

**Boundary Enforcement と微調整**

- 設計: layer import rule、forbidden dependency rule、package boundary test を CI に組み込み、残存 adapter と TODO を明示管理する。
- 実装: `import-linter` 相当の設定、既存 `no direct provider imports` guard の拡張、旧 path 互換 shim、不要 duplicate model の削減、最小の不足修正を行う。
- テスト: boundary tests、contract tests、integration tests、eval tests、最後に `./check.sh all` を通す。Frontend を触った app は個別 build/type-check まで行う。
- Context: 境界設定、CI、互換アダプタ、失敗一覧のみ。業務機能追加は行わない。

## 受け入れ条件

- 下位層から上位層への逆依存が機械的に検出でき、CI で失敗する。
- 既存の主要 HTTP/CLI 契約と `app_config.json` は互換維持される。
- Kernel は Platform/App を import せず、Harness は hook 経由でのみ Kernel に接続する。
- Platform は manifest/registry 経由で app を発見・操作し、app 内部ロジックへ直接依存しない。
- 主要 3 Studio 系 app は lower layers を参照して動作し、app 固有ロジックが下位層へ逆流しない。

## 前提・既定値

- 実装中の追加コメント・docstring・注釈は日本語で統一する。
- `framework` という manifest 表記だけでは lower layer へ落とさず、「共有責務か、独立 deploy する製品か」で最終分類する。
- 既知のベースラインとして、関連テストでは CMA backend state API に `_redis_store` 未定義の実障害がある。これは移行中に必ず潰す。
- ローカル検証は `conda run -n agentflow ...` を前提とし、最終合格は repo 既定の coverage 80% を含む全体チェックで判定する。

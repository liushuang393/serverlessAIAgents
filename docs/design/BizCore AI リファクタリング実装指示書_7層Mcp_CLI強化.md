# BizCore AI リファクタリング実装指示書

## 目的

AgentFlow を**技術負債を増やさない構造**へ再整理する。
特に以下を達成する。

- 層責務を明確化し、README と実装構造を一致させる
- `harness` を独立層として正式に残す
- CLI を一級市民として再定義する
- MCP を「システム間接続」に限定する
- LLM 呼び出しを Gateway に統一する
- コーディング規約・文字コード規約・Git 前チェックを強化する

---

# 1. 現状問題点

## 1.1 層の説明と実装がずれている

現状レビューでは、`contracts`、`infrastructure`、`harness` は比較的明確だが、`shared`、`kernel`、`platform(control_plane)`、`domain` に責務混在がある。特に `shared` は広がりすぎ、`kernel` は抽象と具体が混在し、`platform` は bootstrap / publish / deploy / codegen まで持ってしまっている。`domain/templates/*` も実行可能 Agent を抱えており、領域層に runtime 実装が混入している。

## 1.2 harness の責務は正しいが、境界がまだ甘い

`harness` は approval / evaluation / gating / guardrails の集中先として正しいが、`HarnessedToolRuntime` が kernel や infrastructure の具体を知っており、**横断制御層であるはずなのに実行詳細へ踏み込みすぎている**。

## 1.3 実アーキテクチャで逆依存や越境がある

粗い import スキャンでも、`kernel -> infrastructure`、`shared -> kernel`、`shared -> control_plane`、`control_plane -> apps` など不自然な依存が確認されている。これは README の理想構造と実体が一致していないことを示す。

## 1.4 CLI の位置づけが弱い

過去設計では CLI が入口の一つとして扱われるだけで、**契約・実行・再現・デバッグの主軸**として定義されていない。
一方で外部の流れでは、Gemini CLI 拡張のように、CLI が能力配布と実行の中心入口として扱われている。([Google Codelabs][3])

## 1.5 MCP の責務を広げすぎる危険がある

MCP は接続標準として有用だが、主編排や内部ドメインモデルの中心に据えるべきではない。MCP は公式にも「アプリケーションが LLM にコンテキストを与える標準接続」とされている。([Claude API Docs][1])

---

# 2. 層の再構成

README では**3つを分けて記述**すること。

## 2.1 README で分けるべき 3 つ

### A. 静的アーキテクチャ層

- 何がどの責務を持つか
- ディレクトリと責務の対応
- 依存方向

### B. 実行時フロー

- Request から Plan / Route / Execute / Validate / Return までの流れ
- CLI / MCP / RAG / Gateway / Harness がどこで関与するか

### C. 進化・品質・安全の横断サブシステム

- Replay
- Score
- Risk
- Approval
- Audit
- Trace
- Validation

この 3 つを混ぜて 1 枚図にしないこと。

---

# 3. 正式な 7 層再定義

**harness を正式層として明示する。**

## Layer 1: Experience / Entry

### 役割

ユーザー・外部システムからの入口。

### 含むもの

- Web UI
- REST API
- SDK
- CLI Entry

### ルール

- 業務ロジックを持たない
- 下位へは Contract を通して渡す
- Provider や DB を直接触らない

---

## Layer 2: Contracts

### 役割

唯一の canonical 契約層。

### 含むもの

- request / response schema
- tool contract
- artifact contract
- trace contract
- policy contract
- command contract

### ルール

- 全層が参照してよい
- 二重 schema を作らない
- app 独自モデルで canonical schema を上書きしない

### 改造指示

- manifest / app config / tool result / trace result の二重定義を解消
- README に「契約が唯一の真実源」であることを明記

---

## Layer 3: Kernel

### 役割

AgentFlow の実行中核。

### 含むもの

- planner
- router
- flow runtime
- state
- tool executor abstraction
- agent abstraction
- plugin abstraction
- protocol abstraction

### ルール

- 具体 Provider を知らない
- Harness の具体実装を直接持たない
- platform / apps を import しない

### 改造指示

- Provider 想定コードを infrastructure へ押し戻す
- CLI runtime の抽象口をここに置く
- `kernel` は「何をするか」を持ち、「どう接続するか」は持たない

---

## Layer 4: Harness

### 役割

**横断ガバナンス・品質・安全・検証の専用層。**

### 含むもの

- approval
- gating
- guardrails
- scoring
- evaluation
- replay
- validation
- risk
- budget
- context engineering
- policy enforcement

直近の設計・コミットでも、`ContractAuthGuard`、`ExecutionScorer`、`RiskAssessor`、`ReplayRecorder`、`ContextEngineer` がここに入る方向で整理されている。

### ルール

- Kernel の実行結果を評価・制御する
- Kernel の内部実装詳細に依存しない
- Infrastructure の具体型を知りすぎない
- 高リスク操作はここで止める

### 改造指示

- `HarnessedToolRuntime` から kernel / infrastructure 具体依存を削減
- Harness は wrapper / middleware / policy / evaluation として機能させる
- Trace / Replay / Score を Harness の正式責務に固定

---

## Layer 5: Integration

### 役割

外部・他システムとの接続。

### 含むもの

- MCP
- A2A
- OpenAPI adapters
- DB / SaaS / file / service adapters

### ルール

- 主編排を持たない
- 内部ドメインモデルを支配しない
- 接続・変換・認証・伝達に専念する

### 改造指示

- MCP はこの層に限定
- 「MCP で内部設計を決める」ことを禁止
- CLI 呼び出しを外に見せるなら adapter として提供する

MCP は公式にも接続標準として位置づけられているため、この置き方が自然。([Claude API Docs][1])

---

## Layer 6: Infrastructure / Gateway

### 役割

外部技術基盤とモデルアクセスの統一。

### 含むもの

- LiteLLM Gateway
- model routing
- vector db
- storage
- cache
- queue
- observability
- sandbox
- secrets

### ルール

- LLM 呼び出しは Gateway のみ
- App / Kernel / Harness が provider SDK を直叩きしない
- 認証、予算、fallback、監視を中央集約

LiteLLM の公式 docs でも、Proxy Server は中央ゲートウェイとして、auth、logging、budget、routing を担う。([LiteLLM][2])

### 改造指示

- provider 直 import をゼロ化
- Gateway client 以外の LLM 呼び出しを禁止
- embeddings / rerank / model selection も統一窓口へ寄せる

---

## Layer 7: Platform / Apps

### 役割

アプリ提供・運用・開発支援の面。

### 含むもの

- app registry
- platform API
- studio
- scaffolding
- deploy
- codegen
- app bootstrap

### ルール

- runtime core を持たない
- canonical contracts を再定義しない
- kernel / harness / integration を利用する側に徹する

### 改造指示

- bootstrap / publish / deploy / codegen を platform に集約
- app runtime 装配と platform 管理機能をさらに分ける
- domain/templates の executable 部分は apps か platform tooling へ移す

---

# 4. CLI の正式位置づけ

## 結論

CLI は 1 箇所に置くのではなく、**二面性を持つ**。

### Entry としての CLI

- Layer 1 の入口
- ユーザー/AI が最短で命令を表現する窓口

### Runtime としての CLI

- Layer 3 Kernel の実行手段
- shell / subprocess / stream / artifact の実行器

### 設計ルール

- すべての CLI は `--json` を持つ
- exit code を安定化する
- stdout / stderr / event / artifact を分離する
- dry-run を持つ
- trace_id を返す
- help を LLM に読ませやすい構造にする

### 追加推奨コマンド

- `agentflow run`
- `agentflow plan`
- `agentflow replay`
- `agentflow verify`
- `agentflow tool invoke`
- `agentflow rag search`
- `agentflow doctor`

---

# 5. MCP の正式位置づけ

## 方針

MCP は **Layer 5 Integration 限定** とする。

## MCP がやってよいこと

- 外部ツール公開
- 外部システム接続
- 認証付き通信
- context / tool exposure

## MCP がやってはいけないこと

- 主編排
- 内部契約の中心化
- kernel の代替
- harness の代替

## 実装原則

- MCP Server は adapter
- 内部の tool / cli / rag / artifact は adapter 越しに見せる
- 内部 core logic は MCP に依存しない

---

# 6. README 更新指示（根目录 README）

README を以下の章構成に更新すること。

## 6.1 必須章構成

1. Overview
2. Design Goals
3. Static 7-Layer Architecture
4. Runtime Flow
5. Cross-Cutting Governance (Harness)
6. CLI-First Execution Model
7. MCP-Based Integration Model
8. Gateway-First LLM Access
9. Directory Mapping
10. Dependency Rules
11. Coding / Encoding / Quality Gates
12. Migration Plan

## 6.2 README に必ず書くこと

- Harness は独立層である
- CLI は入口でもあり実行器でもある
- MCP は接続層限定
- LLM は Gateway 経由のみ
- Trace は唯一の真実源
- Replay / Score / Risk / Approval は Harness の正式責務
- Encoding rule を工程ルールへ追加
- Git 前チェックに architecture / encoding / quality gate を追加

---

# 7. 具体的な実装修正方針

## 7.1 依存方向ルール

- Experience -> Contracts -> Kernel -> Integration / Infrastructure
- Harness は Kernel の上で横断制御する
- Platform / Apps は Kernel/Harness/Integration を使う側
- Integration は接続のみ
- Infrastructure は最下層

## 7.2 禁止事項

- provider SDK の直叩き禁止
- platform から apps への知識逆流禁止
- shared に publish / deploy / codegen を置かない
- domain に executable agent を置かない
- harness が kernel 具体実装へ密結合しない
- MCP に主編排を持たせない

## 7.3 Harness の重点改造

- approval, evaluation, replay, scoring, validation, guardrails, budget を正式 package として固定
- `HarnessedToolRuntime` の責務を見直し、kernel 抽象インターフェース越しに接続
- risk / scoring / replay の結果を trace contract に統一
- auth / policy / approval は middleware / gate で提供

---

# 8. 工程ルールへの追加指示

## 8.1 文字コード規約を必ず更新

AI への注意喚起として、工程ルールに以下を必ず追加すること。

### Encoding Rule

- 既定の文字コードは **UTF-8 (BOM なし)**
- Windows PowerShell 5.1 向け `.ps1` は日本語を含む場合、**UTF-8 with BOM** または要求に応じて Shift_JIS を検討する
- 既存ファイル編集前に、**必ず現行エンコーディングを確認**する
- 日本語を含む設定ファイル・スクリプト・README・テンプレートで文字化けを発生させない
- 新規作成時はファイル種別ごとに推奨エンコーディングを明示する

これは特にあなたが重視しているルールなので、**README だけでなく工程規約、PR テンプレート、チェックツールにも反映**すること。

## 8.2 Git 前チェックを更新

`pre-commit` / `scripts/check_rules_compliance.py` / CI に以下を追加する。

### 必須チェック

- layer boundary violation check
- provider direct import check
- forbidden reverse dependency check
- encoding check
- BOM / no-BOM rule check
- CLI JSON contract check
- manifest / contract duplication check
- trace schema validation check

現行でも architecture validation 関連の閾値管理は入っているので、これを拡張する。閾値管理文書では layer boundary violation や provider direct imports などを見ている。

---

# 9. AI への実装制約

- README 先行で更新してからコードを直すこと
- README の 7 層定義と実ディレクトリを一致させること
- 1 回の変更で全部やろうとせず、段階的に PR を分けること
- 各段階で architecture check を通すこと
- provider 直呼びゼロ化を最優先にすること
- harness を削らず、むしろ境界を明確化すること
- CLI を単なる補助ツールとして扱わないこと
- MCP を接続層から越境させないこと

---

# 10. 完了判定チェックリスト

## README

- [ ] 静的 7 層
- [ ] 実行時フロー
- [ ] Harness 横断説明
- [ ] CLI / MCP / Gateway の責務明記
- [ ] Encoding 規約追加

## 契約

- [ ] canonical schema 一本化
- [ ] 二重 manifest 排除
- [ ] trace contract 統一

## Kernel

- [ ] provider 具体依存を削除
- [ ] CLI 実行抽象を整理
- [ ] platform / apps 逆依存なし

## Harness

- [ ] approval / replay / score / risk / validation を正式責務化
- [ ] kernel 具体依存の削減
- [ ] trace 連携完了

## Integration

- [ ] MCP は接続専用
- [ ] adapter として独立
- [ ] 内部主編排を持たない

## Infrastructure

- [ ] Gateway 経由のみで LLM 利用
- [ ] provider 直 import ゼロ
- [ ] budget / routing / auth / logging 集約

## Quality / Encoding

- [ ] pre-commit 更新
- [ ] CI check 更新
- [ ] encoding check 追加
- [ ] Git 前チェックに反映

---

必要なら次に、これをそのまま使える **README 更新草案（章立て＋本文テンプレート）** にして出します。

[1]: https://docs.anthropic.com/en/docs/mcp?utm_source=chatgpt.com "Model Context Protocol (MCP) - Anthropic"
[2]: https://docs.litellm.ai/?utm_source=chatgpt.com "LiteLLM - Getting Started | liteLLM"
[3]: https://codelabs.developers.google.com/getting-started-gemini-cli-extensions?utm_source=chatgpt.com "Getting Started with Gemini CLI Extensions  |  Google Codelabs"

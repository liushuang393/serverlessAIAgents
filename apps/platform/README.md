# AgentFlow Platform


<!-- README_REQUIRED_SECTIONS_START -->
## 機能概要
- 3 Studio 製品線と Framework 管理 API を単一 Control Plane で提供。
- App/Agent/Skill/RAG/MCP の発見・管理・実行・監査を統合。
- Publish/Deploy 導線を標準化し、app ライフサイクル運用を一元化。
- `auth_service` / `design_skills_engine` は**エンドユーザー向け画面ではなく、プラグイン・他 app が利用する共通サービス**として管理する。

## 優位性
- `/api/studios/*` と `/api/studios/framework/*` の正規導線で契約を統一。
- business/developer/operator の surface 分離で利用者体験を最適化。
- framework 監査機能により、manifest と実装の整合性を継続検証。

## 技術アーキテクチャ
- Backend: FastAPI Routers + Discovery/Lifecycle/Audit Services。
- Frontend: React + Zustand による運用コンソール。
- App Manifest と taxonomy（business_base / pattern）で構成情報を標準化。

## アプリケーション階層
- Studio Surface: 顧客向け実行導線。
- Framework Control: App/Agent/Skill/RAG/MCP 管理。
- Governance Layer: 監査・ポリシー・可視性制御。
- Delivery Layer: 発布、起動、停止、運用検証。
<!-- README_REQUIRED_SECTIONS_END -->

Platform は 3 Studio 製品線と Framework 管理面を提供します。

## 1. 役割

- 顧客向け: 3 Studio の実行導線（テンプレート → 設定 → 実行 → 成果物）
- 開発向け: Framework API と Plugin 拡張
- 運用向け: 監査、ポリシー、実行管理

## 2. 正規 API Prefix

- Studio API: `/api/studios/*`
- Framework API: `/api/studios/framework/*`

旧 `/api/agents` などの経路は廃止済みです。

## 共通サービス app（重要）

以下 2 つは「ユーザーが直接操作する app」ではなく、Platform から起動して他 app / plugin で使う共通基盤です。

- `auth_service`
  - 用途: 認証・認可（JWT/OAuth2/SAML/MFA）を共通提供
  - 利用先: FAQ System / contracts.auth / plugin runtime guard
  - 運用: Platform の Apps 画面から起動し、個別 app 詳細への導線は不要
- `design_skills_engine`
  - 用途: デザイン/画像生成の共通実行エンジン（ComfyUI 連携）
  - 利用先: design 系 plugin / 生成パイプライン app
  - 運用: Platform の Apps 画面から起動し、個別 app 詳細への導線は不要

## 3. 主要エンドポイント

### Studio

- `GET /api/studios`
- `GET /api/studios/{studio}/templates`
- `POST /api/studios/{studio}/runs`
- `GET /api/studios/{studio}/runs/{id}/artifacts`

### Framework

- `GET /api/studios/framework/apps`
- `GET /api/studios/framework/agents`
- `GET /api/studios/framework/skills`
- `GET /api/studios/framework/rag/overview`
- `GET /api/studios/framework/mcp/config`
- `GET /api/studios/framework/gallery/featured`
- `GET /api/studios/framework/components`
- `GET /api/studios/framework/dashboard/{tenant_id}`
- `POST /api/studios/framework/publish/deploy`
- `POST /api/studios/framework/tenants/invitations`
- `POST /api/studios/framework/tenants/invitations/{invitation_id}/challenge`
- `POST /api/studios/framework/tenants/invitations/consume`
- `GET /api/studios/framework/tenants/invitations/outbox`（開発時のみ）
- `GET /api/studios/framework/apps/{app_name}/cli/status`
- `POST /api/studios/framework/apps/{app_name}/cli/setup`

## RAG 設定適用モード（hot / restart fallback）

Platform の RAG 設定更新は以下の順で適用されます。

1. `PATCH /api/studios/framework/rag/apps/{app}/config` 保存
2. SSE `rag_config_changed` を発火（`contracts_rag` + legacy `rag_config` 同梱）
3. subscriber が設定されている場合は hot apply
4. hot apply 不成立時のみ app restart を fallback 実行

### 反映確認手順

```bash
# 1) 設定更新
curl -X PATCH http://localhost:8001/api/studios/framework/rag/apps/faq_system/config \\
  -H "Content-Type: application/json" \\
  -d '{"enabled": true, "data_sources": [{"id": "source-docs", "type": "file", "uri": "/data/faq.md"}]}'
# 2) hot_apply / config_version を確認
curl http://localhost:8001/api/studios/framework/rag/apps/faq_system/config
```

レスポンスの `hot_apply.applied` が `false` の場合、UI は restart fallback を実行します。

## FAQ ルーティング 3段戦略（運用標準）

`faq_system` の RAG/Router 運用は、以下 3 段戦略を **標準契約** とします。

1. 企業領域（企業知識・制度・業務 DB・MCP/外部調査）  
   ルート: `faq` / `sql` / `hybrid` / `external`  
   回答: 根拠付き（RAG 文書 / SQL 結果 / 外部ソース）
2. 通用知識（数学・物理・歴史・常識・一般情報 Q&A）  
   ルート: `chat`  
   回答: LLM 直接回答（企業 KB 依存なし）
3. 上記以外（両スコープ外 / 問いとして成立しない要求）  
   ルート: `unclear`  
   回答: 「わかりません（out_of_scope）」＋再質問ガイド

### チーム運用ガードレール

- 例示キーワードのハードコードで分類を固定しない（意図判定優先）。
- `route_hint=faq` の全体固定を禁止（特定ユースケースのみ局所的に付与）。
- RAG は「企業領域の根拠補強」に使い、一般知識質問へ無条件適用しない。
- `unclear` は失敗ではなく仕様上の有効ルートとして扱う。

### 設定レビュー・チェックリスト

1. Platform 保存前に、対象質問群を 3 区分（企業 / 通用 / 範囲外）で分類していること。
2. `faq_system` の `data_sources` は企業データのみを対象にし、一般知識データを混在させないこと。
3. hot apply 後に `query_type` 分布を確認し、`faq` 偏重・`unclear` 欠落・`chat` 欠落がないこと。
4. 反映失敗時は restart fallback 後に同じ質問セットで再検証すること。

### 最低限の回帰確認（運用時）

- 企業制度質問: `faq` または `hybrid` で根拠付き回答になる。
- DB 集計質問: `sql` または `hybrid` で表/集計結果を返す。
- 通用知識質問: `chat` で回答する。
- 範囲外要求: `unclear` + `out_of_scope` で「わかりません」を返す。

## 4. 開発起動

### 4.1 開発環境セットアップ（統一手順）

Platform 単体ではなく、リポジトリ全体の開発環境をセットアップします。

```bash
cd <repo-root>
bash setup_dev.sh
```

手動で行う場合:

```bash
conda activate agentflow
pip install -e ".[dev,apps]"
```

### 4.2 起動（Backend / Frontend）

Backend（FastAPI）:

```bash
conda activate agentflow
python -m apps.platform.main serve
```

Frontend:

```bash
cd apps/platform/frontend
npm install
npm run dev
```

ポートは `apps/platform/app_config.json` が単一定義元です（`ports.api` / `ports.frontend`）。

### 4.3 CLI 自動セットアップと自癒診断

Platform の `Start / Publish / Local Start` は実行前に CLI preflight を行います。

- 対象 CLI: `codex`, `claude`
- preflight: 検出 → （必要時）インストール → 認証確認
- 認証: 既存ログイン状態確認 → API Key ログイン → 交互ログイン案内
- 起動失敗時: CLI 診断を自動実行し、`diagnostic` を action response に返却

`runtime.cli` で app 単位に以下を上書きできます。

- `executable`
- `install_commands`
- `auth.status / auth.api_key_env / auth.api_key_login / auth.interactive_login`
- `diagnostic_mode`
- `diagnostic_command`

### 4.4 ライフサイクル実行ルール（WSL + conda agentflow）

- `local-start`
  - `conda activate agentflow` を前提に backend/frontend を起動
  - PID 生存だけでなく backend health / frontend 待受を確認
  - 片系が直後に停止した場合は失敗としてログ末尾を返す
- `start` / `stop`
  - 先に `execution_mode`（`docker` / `local`）を判定してから実行
  - 判定優先度: docker 稼働中 > local PID 稼働中 > compose-first default
- `publish`
  - docker-first
  - 既存コンテナ/イメージ有り: `docker compose up -d`
  - 既存資産無し: `docker compose up -d --build`
- 失敗時の自動修復
  - `codex` 2 回 → `claude` 2 回
  - action response に `execution_mode` と `repair`（試行履歴）を返す

## 5. テスト/静的チェック（統一スクリプト）

```bash
cd <repo-root>
./check.sh format
./check.sh lint
./check.sh type-check
./check.sh test
```

または:

```bash
cd <repo-root>
make check-all
```

## 6. 本番ビルド/発布（Platform に統一）

### 6.1 Publish（推奨）

Platform CLI から publish/deploy を実行します（生成・検証・発布の導線を統一）。

```bash
conda activate agentflow
python -m apps.platform.main publish ./apps/<app_dir> --target docker
```

### 6.2 API 経由（自動化向け）

- `POST /api/studios/framework/publish/deploy`

### 6.3 Frontend ビルド

```bash
cd apps/platform/frontend
npm install
npm run build
```

## 7. 参照ドキュメント

- 外部向け: `docs/external/README.md`
- 内部向け: `docs/internal/README.md`
- アーキテクチャ: `docs/architecture.md`
- app_config 契約: `apps/platform/docs/app-config-schema.md`

## 6. 共有テスト env 自動生成

```bash
conda run -n agentflow python scripts/bootstrap_test_env.py --env-file .env
```

- 手動でテスト用シークレットを作成しない（空値のみ自動補完）。
- Platform 単体では API キー必須契約はないが、共通 JWT/DB シークレットは同コマンドで補完される。

## 7. 本番 / テナント運用

- 本番は `.env` ではなく Secret Manager 注入を使用する。
- 多租户招待メールは「通知メール」と「ログイン URL メール」を分離送信する。
- 詳細手順: `docs/internal/env-bootstrap-and-tenant-invite-security.md`

## 8. 招待メール API

- `POST /api/studios/framework/tenants/invitations`
- `POST /api/studios/framework/tenants/invitations/{invitation_id}/challenge`
- `POST /api/studios/framework/tenants/invitations/consume`

運用ポイント:
- 1通目は通知のみ（URL/OTP なし）
- 2通目で URL または OTP を別送
- URL はワンタイムトークンのみを含む

## 9. Framework 監査（AST モード）

- プロトコル面（SSE/WS/A2A/MCP）は AST（Python 構文木）解析で判定する。
- 正規表現フォールバックは廃止。
- 構文エラー時は `AST_PARSE_WARNING` を返し、修正提案と影響を併記する。
- 必須プロトコルが AST 解析不能で立証不可の場合は `*_UNVERIFIED` を `error` 扱いにする。

## 10. 認証共通モジュール

- 共通ガード: `agentflow/security/contract_auth_guard.py`
- `app_config.json` の `contracts.auth` を基準に HTTP/WS 認証を統一する。
- 標準ステータス:
  - HTTP: `401`（認証失敗）, `503`（鍵未設定）
  - WS: close code `4401`（認証失敗）, `1011`（サーバー設定不備）

## 11. Plugin 署名運用（P1）

- sidecar 署名: `plugins/<plugin_id>/plugin_manifest.sig`
- trust store: `plugins/trust_store.json`
- env:
  - `AGENTFLOW_PLUGIN_TRUST_STORE`
  - `AGENTFLOW_PLUGIN_SIGNATURE_ENFORCEMENT`（既定 `warn`）
- P1 は warning 運用（署名不整合で deny しない）。

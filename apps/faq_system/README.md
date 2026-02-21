# FAQ System

AgentFlow フレームワーク級 Agent/サービスを使用した FAQ システムです。

## Product Position

- `product_line`: `faq`
- `surface_profile`: `business`
- 顧客導線: `テンプレート選択 → データ/権限設定 → 実行 → 成果物確認`

## クイックスタート

### 前提条件

| 要件 | バージョン | 確認コマンド |
|------|-----------|-------------|
| **Python** | 3.13+ | `python --version` |
| **pip** | 最新 | `pip --version` |
| **Git** | 最新 | `git --version` |

### ローカル環境構築

推奨（統一手順）:

```bash
cd <repo-root>
bash setup_dev.sh
```

### DB マイグレーション（必須）

```bash
# スキーマを最新化（新規DB作成 / 既存DB差分適用 を自動判定）
alembic -c apps/faq_system/alembic.ini upgrade head

# 現在の適用済みリビジョンを確認
alembic -c apps/faq_system/alembic.ini current

# マイグレーション履歴を一覧表示
alembic -c apps/faq_system/alembic.ini history --verbose

# 1つ前のリビジョンにロールバック
alembic -c apps/faq_system/alembic.ini downgrade -1

# 特定リビジョンまでロールバック
alembic -c apps/faq_system/alembic.ini downgrade <revision_id>

# 新しいマイグレーションファイルを自動生成（モデル変更後）
alembic -c apps/faq_system/alembic.ini revision --autogenerate -m "変更内容の説明"

# 空のマイグレーションファイルを手動作成
alembic -c apps/faq_system/alembic.ini revision -m "変更内容の説明"
```

> **補足:** 既存DBに `alembic_version` が無い場合（`create_all()` で作成済み等）、
> `upgrade head` 実行時に初期リビジョンが自動 stamp されるため、手動操作は不要です。

### 起動方法

```bash
# ローカル開発（ホットリロード有効）
# ポートは app_config.json から自動読み込み（8005）
python -m apps.faq_system.main --reload

# 本番起動（リロードなし）
python -m apps.faq_system.main
```

### フロントエンド開発 (React/TypeScript)

```bash
cd apps/faq_system/frontend

# 1. 依存関係のインストール
npm install

# 2. 開発サーバー起動 (HMR 有効)
npm run dev

# 3. 本番ビルド (ビルド後、バックエンドが自動でサーブします)
npm run build
```

### 本番ビルド/発布（Platform に統一）

Platform（Control Plane）に publish/deploy を統一する場合:

```bash
conda activate agentflow
python -m apps.platform.main publish ./apps/faq_system --target docker
```

（この app は `apps/faq_system/app_config.json` の `runtime.commands.publish` に docker compose の発布手順を保持しています）


`FAQ_HOST` / `FAQ_PORT` は一時的な上書き用です。通常は `app_config.json` の `ports.api` を使用します。

起動後、ブラウザで `http://localhost:8005` を開くと WebSocket 対応の富文本チャット UI が表示されます。

**統合済み機能:**
- WebSocket リアルタイム双方向通信（自動再接続付き）
- 富文本レスポンス（Markdown・コードブロック・表・ECharts チャート）
- リアルタイム進捗表示
- 引用/ソース表示
- フィードバック収集（`/api/feedback`）

### Ollama 接続トラブルシュート

`API error: All connection attempts failed` が出る場合は FAQ プロセスから Ollama URL に到達できていません。

```bash
# FAQ 実行環境から接続確認
curl http://localhost:11434/api/tags
```

- WSL で FAQ を動かし、Windows 側 Ollama を動かしている場合:
  - `apps/faq_system/.env` に `OLLAMA_BASE_URL=http://host.docker.internal:11434` を設定
  - つながらない場合は Windows Firewall と Ollama の bind 設定を確認
- OpenAI に切り替える場合:
  - `apps/faq_system/.env` で `LLM_PROVIDER=openai` と `OPENAI_API_KEY` を設定
- `404 model 'xxx' not found` が出る場合:
  - `curl http://localhost:11434/api/tags` で利用可能モデルを確認
  - FAQ 側の `OLLAMA_MODEL` と一致するモデルを pull（例: `ollama pull llama3.2`）

### テスト確認手順

```bash
# 1. 単体テスト
pytest tests/unit/test_nl2sql_services.py -v --no-cov

# 2. FAQ システムテスト
pytest apps/faq_system/tests/ -v --no-cov

# 3. サーバー起動後の API 動作確認（別ターミナル）
curl -X POST http://localhost:8005/api/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "返品ポリシーを教えて"}'

# 4. ヘルスチェック
curl http://localhost:8005/api/health
```

---

## 認証テスト手順

### システム起動方法

> **重要**: バックエンドとフロントエンドを**別々に**起動する必要があります。

**ターミナル 1 — バックエンド**

```bash
cd <repo-root>
conda activate agentflow
python -m apps.faq_system.main --reload
# → http://localhost:8005 でAPIが起動
```

**ターミナル 2 — フロントエンド**

```bash
cd apps/faq_system/frontend
npm install        # 初回のみ
npm run dev
# → http://localhost:3004 でUIが起動
```

ブラウザで **http://localhost:3004** を開いてログイン画面にアクセスしてください。

---

### 1. ユーザー名/パスワード ログイン

初回起動時に以下のデモユーザーが自動作成されます。

| ユーザー名 | パスワード | 表示名 | 役職 | ロール |
|------------|-----------|--------|------|--------|
| `admin` | `admin123` | 管理者 太郎 | 情報システム部 部長 | admin |
| `tanaka` | `tanaka123` | 田中 一郎 | 人事部 課長 | manager |
| `suzuki` | `suzuki123` | 鈴木 花子 | 営業部 主任 | employee |
| `yamamoto` | `yamamoto123` | 山本 健太 | DX推進部 データアナリスト | analyst |
| `sato` | `sato123` | 佐藤 美咲 | 人事部 マネージャー | hr_admin |

**curl でのテスト:**

```bash
# ログイン（成功確認）
curl -X POST http://localhost:8005/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username": "admin", "password": "admin123"}'

# 期待レスポンス:
# {"success": true, "message": "ログイン成功", "user": {...}, "access_token": "eyJ..."}

# 取得したトークンで認証確認
TOKEN="<上記の access_token>"
curl -H "Authorization: Bearer $TOKEN" http://localhost:8005/api/auth/me
```

**デモユーザーのリセット（テスト用）:**

```bash
curl -X POST http://localhost:8005/api/auth/reset-demo \
  -H "Authorization: Bearer $TOKEN"
```

> **トラブルシュート:**
> - `{"success": false, ...}` → ユーザー名/パスワードを上記テーブルと照合
> - `Connection refused` → バックエンドが起動しているか確認 (`ps aux | grep faq`)
> - ログイン画面が表示されない → フロントエンド (port 3004) が起動しているか確認

---

### 2. Google アカウント認証

**事前準備（Google Cloud Console）:**

1. [Google Cloud Console](https://console.cloud.google.com/) にアクセス
2. 「APIs & Services」→「Credentials」→「Create Credentials」→「OAuth 2.0 Client ID」
3. Application type: **Web application**
4. Authorized redirect URIs に追加:
   ```
   http://localhost:8005/api/auth/oauth2/google/callback
   ```
5. クライアントID と クライアントシークレット を取得

**`.env` 設定 (`apps/faq_system/.env`):**

```env
GOOGLE_CLIENT_ID=<取得したクライアントID>.apps.googleusercontent.com
GOOGLE_CLIENT_SECRET=<取得したクライアントシークレット>
```

**テスト手順:**

1. バックエンドを再起動（`.env` 変更を反映）
2. ブラウザで http://localhost:3004/login を開く
3. 「Continue with Google」ボタンをクリック
4. Google ログイン画面が表示されることを確認
5. Google アカウントでログインすると FAQ システムにリダイレクトされる

**curl でのリダイレクト先確認（直接テスト）:**

```bash
curl -v http://localhost:8005/api/auth/oauth2/google/authorize 2>&1 | grep "Location:"
# → Google の認可 URL にリダイレクトされる
```

> **注意:** Google OAuth は `localhost` での動作確認が可能。本番環境では本番ドメインを追加すること。

---

### 3. Microsoft Azure AD 認証

**事前準備（Azure Portal）:**

1. [Azure Portal](https://portal.azure.com/) にアクセス
2. 「Azure Active Directory」→「App registrations」→「New registration」
3. Redirect URI に追加:
   ```
   http://localhost:8005/api/auth/oauth2/azure_ad/callback
   ```
4. 「Certificates & secrets」→「New client secret」でシークレット作成
5. 「Overview」から Application (client) ID と Directory (tenant) ID を取得

**`.env` 設定:**

```env
AZURE_AD_CLIENT_ID=<Application ID>
AZURE_AD_CLIENT_SECRET=<クライアントシークレット値>
AZURE_AD_TENANT_ID=<Directory Tenant ID>
# マルチテナント（任意のMicrosoftアカウント）の場合:
# AZURE_AD_TENANT_ID=common
```

**テスト手順:**

1. バックエンドを再起動
2. ブラウザで http://localhost:3004/login を開く
3. 「Continue with Microsoft」ボタンをクリック
4. Microsoft ログイン画面が表示されることを確認
5. Microsoft アカウントでログインすると FAQ システムにリダイレクトされる

**curl でのテスト:**

```bash
curl -v http://localhost:8005/api/auth/oauth2/azure_ad/authorize 2>&1 | grep "Location:"
# → Microsoft の認可 URL にリダイレクトされる
```

---

### 4. LDAP / Active Directory 認証

**`.env` 設定:**

```env
FAQ_AUTH_PROVIDER=ldap
FAQ_LDAP_SERVER_URI=ldap://your-ad-server:389
FAQ_LDAP_BIND_DN_TEMPLATE=DOMAIN\{username}
# または OpenLDAP の場合:
# FAQ_LDAP_BIND_DN_TEMPLATE=uid={username},ou=users,dc=example,dc=com

FAQ_LDAP_BASE_DN=ou=users,dc=example,dc=com
FAQ_LDAP_USER_FILTER=(sAMAccountName={username})
FAQ_LDAP_DEFAULT_ROLE=employee
# ロールマッピング (AD グループ → FAQ ロール):
FAQ_LDAP_ROLE_MAPPING={"CN=FAQ-Admins,OU=Groups,DC=example,DC=com":"admin","CN=FAQ-Managers,OU=Groups,DC=example,DC=com":"manager"}
```

**ローカルテスト（モックユーザーで確認）:**

実際の LDAP サーバーなしでテストする場合、`FAQ_LDAP_USERS_JSON` でモックユーザーを定義できます:

```env
FAQ_AUTH_PROVIDER=ldap
FAQ_LDAP_USERS_JSON={"testuser": {"password": "testpass", "display_name": "テストユーザー", "department": "IT部", "position": "エンジニア", "role": "employee", "email": "test@example.com"}}
```

テスト:

```bash
curl -X POST http://localhost:8005/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username": "testuser", "password": "testpass"}'
```

**ldap3 のインストール（LDAP を使う場合）:**

```bash
pip install ldap3
```

---

### 5. SAML 企業認証（Okta / Azure AD SAML）

**`.env` 設定:**

```env
FAQ_AUTH_PROVIDER=saml   # 現在未使用。SAML は OAuth2 とは別エンドポイント

# IdP 設定（Okta の例）
FAQ_SAML_IDP_ENTITY_ID=http://www.okta.com/<your-app-id>
FAQ_SAML_IDP_SSO_URL=https://your-org.okta.com/app/<app-name>/sso/saml
FAQ_SAML_IDP_CERT=-----BEGIN CERTIFICATE-----\nMIIC...\n-----END CERTIFICATE-----

# SP 証明書（省略可：本番環境では設定推奨）
# FAQ_SAML_SP_CERT=-----BEGIN CERTIFICATE-----\n...
# FAQ_SAML_SP_KEY=-----BEGIN PRIVATE KEY-----\n...
```

**IdP 側（例: Okta）への SP 設定:**

| 項目 | 値 |
|------|-----|
| Single Sign On URL | `http://localhost:8005/api/auth/saml/acs` |
| Audience URI (SP Entity ID) | `http://localhost:8005/api/auth/saml/metadata` |
| Name ID Format | `EmailAddress` |
| Attribute Statements | `email` → `user.email`, `displayName` → `user.displayName` |

**SP メタデータ確認:**

```bash
curl http://localhost:8005/api/auth/saml/metadata
```

**SAML ログインフロー:**

1. ブラウザで http://localhost:8005/api/auth/saml/login にアクセス
2. IdP (Okta 等) のログイン画面にリダイレクト
3. 認証後、`/api/auth/saml/acs` に POST で戻ってくる
4. JWT と session_token が発行され、フロントエンドにリダイレクト

**pysaml2 のインストール:**

```bash
pip install python3-saml
```

---

### 6. 認証プロキシ連携（nginx / Apache リバースプロキシ）

既存の企業 SSO ゲートウェイ（nginx/Keycloak/Okta など）の後ろに FAQ を置く場合。

**`.env` 設定:**

```env
FAQ_TRUST_PROXY_AUTH=true
FAQ_PROXY_AUTH_SHARED_SECRET=your-very-long-random-secret-here
FAQ_PROXY_AUTH_REQUIRE_SIGNATURE=true
FAQ_PROXY_AUTH_MAX_SKEW_SECONDS=300
```

**プロキシからのリクエストヘッダー形式:**

```
X-Forwarded-User: tanaka
X-Forwarded-Display-Name: 田中 一郎
X-Forwarded-Role: manager
X-Forwarded-Department: 人事部
X-Forwarded-Position: 課長
X-Auth-Timestamp: 1700000000
X-Auth-Nonce: random-uuid-per-request
X-Auth-Signature: sha256=<HMAC-SHA256 hex>
```

**署名生成例（Python）:**

```python
import hmac, hashlib, time, secrets

secret = "your-very-long-random-secret-here"
method = "GET"
path = "/api/chat"
username = "tanaka"
display_name = "田中 一郎"
role = "manager"
department = "人事部"
position = "課長"
timestamp = str(int(time.time()))
nonce = secrets.token_hex(16)

canonical = "\n".join([method, path, username, display_name, role, department, position, timestamp, nonce])
sig = hmac.new(secret.encode(), canonical.encode(), hashlib.sha256).hexdigest()

headers = {
    "X-Forwarded-User": username,
    "X-Forwarded-Display-Name": display_name,
    "X-Forwarded-Role": role,
    "X-Forwarded-Department": department,
    "X-Forwarded-Position": position,
    "X-Auth-Timestamp": timestamp,
    "X-Auth-Nonce": nonce,
    "X-Auth-Signature": f"sha256={sig}",
}
```

**curl テスト:**

```bash
python3 - <<'EOF'
import hmac, hashlib, time, secrets, subprocess

SECRET = "your-very-long-random-secret-here"
METHOD, PATH = "GET", "/api/auth/me"
USERNAME, DISPLAY_NAME, ROLE = "tanaka", "田中 一郎", "manager"
DEPT, POS = "人事部", "課長"
TS = str(int(time.time()))
NONCE = secrets.token_hex(16)

canonical = "\n".join([METHOD, PATH, USERNAME, DISPLAY_NAME, ROLE, DEPT, POS, TS, NONCE])
sig = hmac.new(SECRET.encode(), canonical.encode(), hashlib.sha256).hexdigest()

print(f"curl http://localhost:8005/api/auth/me \\")
print(f'  -H "X-Forwarded-User: {USERNAME}" \\')
print(f'  -H "X-Forwarded-Display-Name: {DISPLAY_NAME}" \\')
print(f'  -H "X-Forwarded-Role: {ROLE}" \\')
print(f'  -H "X-Auth-Timestamp: {TS}" \\')
print(f'  -H "X-Auth-Nonce: {NONCE}" \\')
print(f'  -H "X-Auth-Signature: sha256={sig}"')
EOF
```

---

### 認証フロー早見表

| 方式 | .env 設定 | ユーザー自前管理 | 備考 |
|------|-----------|--------------|------|
| ローカルDB | `FAQ_AUTH_PROVIDER=local_db` | ✅ FAQ DB | デフォルト |
| Google OAuth2 | `GOOGLE_CLIENT_ID/SECRET` 設定 | ❌ Google 管理 | GCP Console 要設定 |
| Microsoft | `AZURE_AD_CLIENT_ID/SECRET/TENANT` 設定 | ❌ Azure 管理 | Azure Portal 要設定 |
| LDAP/AD | `FAQ_AUTH_PROVIDER=ldap` | ❌ AD 管理 | `pip install ldap3` 必要 |
| SAML | IdP で SP 設定 | ❌ IdP 管理 | `pip install python3-saml` 必要 |
| プロキシ | `FAQ_TRUST_PROXY_AUTH=true` | ❌ ゲートウェイ管理 | HMAC署名推奨 |

---

<!-- README_REQUIRED_SECTIONS_START -->
## 機能概要
- FAQ 応答、RAG 検索、SQL 質問応答を単一 API で提供。
- WebSocket/SSE によるリアルタイム対話と進捗表示をサポート。
- KB 設定、フィードバック、履歴永続化を含む運用機能を内蔵。

## 優位性
- 引用付き回答とソース追跡で、回答根拠の説明責任を確保。
- FAQ/RAG/SQL の複合ルーティングで、問い合わせ種別に柔軟対応。
- AgentFlow サービス共通基盤を活用し、拡張時の実装コストを抑制。

## 技術アーキテクチャ
- Backend: FastAPI + AgentFlow Skills + 永続化ストア。
- Frontend: React/TypeScript チャット UI（Markdown/Chart 表示対応）。
- Retrieval: ベクトル検索・再ランキング・設定管理を API 化。

## アプリケーション階層
- Presentation: Web UI / API / Auth。
- Intent Routing: FAQ / RAG / SQL への振り分け。
- Agent Services: 回答生成・検証・引用付与。
- Data Layer: KB、会話履歴、設定、監査ログ。
<!-- README_REQUIRED_SECTIONS_END -->

## 本番環境デプロイ（Docker Compose）

### 起動

```bash
cd apps/faq_system

# .env を準備（API キー等を設定）
cp ../../.env.example .env
# .env を編集: OPENAI_API_KEY=sk-... 等

# 本番（イメージビルド & バックグラウンド起動）
docker compose up --build -d

# 開発（ホットリロード）
docker compose -f docker-compose.yml -f docker-compose.dev.yml up

# ヘルスチェック確認
curl http://localhost:8001/api/health

# ログ確認
docker compose logs -f backend

# 停止
docker compose down
```

### ポート設定

| 変数名 | 説明 | デフォルト |
|--------|------|----------|
| `API_PORT` | ホスト側公開ポート | `8001` |

コンテナ内部は常に `8001` で統一。ホスト側ポートのみ `API_PORT` で変更可能。

### 環境変数一覧

| 変数名 | 説明 | デフォルト | 必須 |
|--------|------|----------|------|
| `OPENAI_API_KEY` | OpenAI API キー | — | ✅ |
| `ANTHROPIC_API_KEY` | Anthropic API キー | — | — |
| `AZURE_OPENAI_API_KEY` | Azure OpenAI API キー | — | — |
| `AZURE_OPENAI_ENDPOINT` | Azure OpenAI エンドポイント | — | — |
| `RAG_COLLECTION` | RAG コレクション名 | `faq_knowledge` | — |
| `DB_SCHEMA` | DB スキーマ JSON | `{}` | — |
| `FAQ_SALES_MATERIAL_DIR` | 営業資料画像の出力先 | `/tmp/faq_sales_material` | — |
| `FAQ_DATABASE_URL` | FAQ 認証/履歴用 DB 接続先 | `postgresql+asyncpg://faq:faq_password@faq-db:5432/faq_system` | — |
| `FAQ_DB_AUTO_CREATE` | モデルから自動テーブル作成（ローカル検証向け） | `false` | — |
| `FAQ_AUTH_PROVIDER` | 認証方式 (`local_db`/`ldap`/`idp`) | `local_db` | — |
| `FAQ_AUTH_MAX_LOGIN_ATTEMPTS` | アカウントロックまでの試行回数 | `5` | — |
| `FAQ_AUTH_LOCKOUT_MINUTES` | アカウントロック時間(分) | `15` | — |
| `JWT_SECRET_KEY` | JWT 署名シークレット | ランダム生成 | — |
| `JWT_EXPIRE_MINUTES` | JWT 有効期限(分) | `60` | — |
| `FAQ_AUTH_DEV_MODE` | パスワード再設定トークンをレスポンスに含める (開発用) | `true` | — |
| `FAQ_LDAP_SERVER_URI` | LDAP サーバー URI | — | LDAP時 |
| `FAQ_LDAP_BASE_DN` | LDAP 検索ベース DN | — | LDAP時 |
| `FAQ_SAML_IDP_ENTITY_ID` | SAML IdP Entity ID | — | SAML時 |
| `FAQ_TRUST_PROXY_AUTH` | 認証プロキシヘッダーを信頼する | `false` | — |
| `FAQ_PROXY_AUTH_SHARED_SECRET` | 認証プロキシ署名検証の共有鍵 | — | 推奨 |
| `FAQ_PROXY_AUTH_REQUIRE_SIGNATURE` | 署名検証を必須化 | `true` | — |
| `FAQ_PROXY_AUTH_MAX_SKEW_SECONDS` | 署名時刻の許容ズレ秒数 | `300` | — |
| `FAQ_KB_INTERNAL_COLLECTION` | 社内KBコレクション | `internal_kb` | — |
| `FAQ_KB_EXTERNAL_COLLECTION` | 対客KBコレクション | `external_kb` | — |
| `FAQ_KB_CONFIDENTIAL_COLLECTION` | 機密KBコレクション | `confidential_kb` | — |
| `FAQ_KB_DEFAULT_TYPE` | 既定KB種別 (`internal/external/confidential`) | `internal` | — |
| `LOG_LEVEL` | ログレベル | `INFO` | — |


---

## ⚠️ 重要：アーキテクチャについて

このアプリは **薄い App 層** として設計されています。
**業務ロジックはすべてフレームワーク側で実装** されています。

## ⚠️ 学習連携の運用方針

- 現在の FAQ app は「実行優先」で運用し、収集/訓練は既定で有効化しません。
- 学習データ収集が必要な案件のみ、期間限定で収集を有効化してください。
- 訓練処理は API 実行経路に混在させず、必ず別ジョブで実行してください。

### Agent/サービスの場所

| コンポーネント | 場所 | 説明 |
|---------------|------|------|
| **FAQAgent** | `agentflow/agents/faq_agent.py` | FAQ 専門 Agent（ResilientAgent 継承） |
| **FAQInput/Output** | `agentflow/agents/faq_agent.py` | 型安全な入出力スキーマ |
| **RAGService** | `agentflow/services/` | RAG 検索サービス |
| **Text2SQLService** | `agentflow/services/` | SQL 生成サービス |

```
apps/faq_system/          ← App層（薄い：APIルーティングのみ）
    └── main.py           ← FAQAgentを呼び出すのみ
        │
        ▼
agentflow/agents/         ← Agent層（新アーキテクチャ）
    └── faq_agent.py      ← FAQAgent（ResilientAgent継承）
        │
        ▼
agentflow/services/       ← サービス層
    ├── rag_service.py
    ├── text2sql_service.py
    ├── chart_service.py
    └── suggestion_service.py
agentflow/skills/builtin/design_skills/ ← 営業資料画像生成
```

### Agent 実装パターン（必読）

新しい Agent を作成する際は、以下のパターンに従ってください：

```python
from agentflow import ResilientAgent
from pydantic import BaseModel

# 1. 入出力スキーマを定義（Pydantic）
class MyInput(BaseModel):
    question: str

class MyOutput(BaseModel):
    answer: str

# 2. ResilientAgent を継承
class MyAgent(ResilientAgent[MyInput, MyOutput]):
    name = "MyAgent"
    temperature = 0.3

    def _parse_input(self, input_data: dict) -> MyInput:
        return MyInput(**input_data)

    async def process(self, input_data: MyInput) -> MyOutput:
        # 業務ロジック
        response = await self._call_llm(f"質問: {input_data.question}")
        return MyOutput(answer=response)
```

## 機能

| 機能 | サービス | 説明 |
|------|----------|------|
| RAG 検索 | `RAGService` | ナレッジベースを検索して回答を生成 |
| Text2SQL | `Text2SQLService` | 自然言語からSQLを生成して実行 |
| チャート生成 | `ChartService` | クエリ結果からチャートを自動生成 |
| 営業資料画像生成 | `design_skills` | 営業向け画像セットを生成し、ダウンロード可能なアセットを返却 |
| 提案生成 | `SuggestionService` | フォローアップ質問を提案 |
| 認証 | `AuthService` | JWT/API Key 認証 |

## API エンドポイント

### 認証

```bash
# ログイン / ログアウト / 自身情報 / トークン更新
POST /api/auth/login
POST /api/auth/logout
GET  /api/auth/me
POST /api/auth/token

# パスワード運用
POST /api/auth/password/change
POST /api/auth/password/forgot
POST /api/auth/password/reset

# 個人属性更新
PATCH /api/auth/profile
```

### 企業認証システム接続

- 既定: `FAQ_AUTH_PROVIDER=local_db`（FAQ 自前 DB 認証）。
- LDAP: `FAQ_AUTH_PROVIDER=ldap`。テスト時は `FAQ_LDAP_USERS_JSON` でモック連携可能。
- IdP: `FAQ_AUTH_PROVIDER=idp`。テスト時は `FAQ_IDP_USERS_JSON`、本番は `FAQ_IDP_TOKEN_URL` / `FAQ_IDP_USERINFO_URL` を使用。
- 認証ゲートウェイ連携: `FAQ_TRUST_PROXY_AUTH=true` で `X-Forwarded-*`（互換で `X-Auth-*`）ヘッダーを受ける。

#### `FAQ_TRUST_PROXY_AUTH` 署名ヘッダー仕様

必要ヘッダー:
- `X-Forwarded-User`（互換: `X-Auth-User`）
- `X-Auth-Timestamp`（UNIX 秒）
- `X-Auth-Nonce`（再送防止）
- `X-Auth-Signature`（`sha256=<hex>` または `<hex>`）

canonical 文字列:

```text
{HTTP_METHOD}
{REQUEST_PATH}
{USERNAME}
{DISPLAY_NAME}
{ROLE}
{DEPARTMENT}
{POSITION}
{TIMESTAMP}
{NONCE}
```

署名:
- `HMAC-SHA256(shared_secret, canonical).hexdigest()`
- nonce は DB 保存し、再利用を拒否

### チャット

```bash
# 同期
POST /api/chat
{
  "message": "返品ポリシーを教えて"
}

# ストリーム（SSE）
POST /api/chat/stream
{
  "message": "今月の売上TOP10は？"
}

# 履歴取得（DB 永続化）
GET /api/chat/history?session_id=sess-xxx&limit=100

# MAQ統合入口（FAQ/SQL/営業資料を自動振り分け）
POST /api/maq/chat
{
  "message": "営業資料図を4枚作成して"
}
```

### RAG

```bash
# クエリ
POST /api/rag/query
{
  "question": "返品ポリシーは？",
  "kb_type": "internal",
  "collection": "internal_kb",
  "top_k": 5
}

# ドキュメント追加
POST /api/rag/add
{
  "kb_type": "internal",
  "content": "返品は30日以内に...",
  "metadata": {"category": "policy"}
}
```

### KB 設定

```bash
# 現在のKB設定を参照（認証必須・DB永続化）
GET /api/kb/settings

# KB設定を更新（admin/manager・即時反映）
PATCH /api/kb/settings
{
  "internal_collection": "internal_kb_v2",
  "external_collection": "external_kb_v2",
  "default_kb": "internal"
}
```

### SQL

```bash
POST /api/sql/query
{
  "question": "今月の売上合計は？"
}
```

### 売上分析

```bash
POST /api/sales/analyze
{
  "question": "今四半期の売上推移を分析して"
}
```

### 営業資料画像のダウンロード

`/api/chat` または `/api/maq/chat` のレスポンスで `artifacts[].download_url` が返る。

```bash
GET /api/assets/{artifact_id}/download
```

### A2A カード

```bash
GET /api/a2a/card
```

### WebSocket（リアルタイム双方向通信）

```
WS /ws/{client_id}?access_token={JWT}

# クライアントから送信:
{ "type": "chat", "message": "質問内容", "sessionId": "...", "options": {} }

# サーバーから受信:
{ "type": "progress", "progress": 50, "message": "処理中..." }
{ "type": "result", "data": { "answer": "..." } }
```

### フィードバック

```bash
POST /api/feedback
{
  "message_id": "msg-xxx",
  "helpful": true,
  "comment": "分かりやすい回答でした"
}
```

### ヘルスチェック

```bash
GET /api/health
→ {"status": "ok", "service": "faq-system", "version": "2.0.0", "timestamp": "..."}
```

## Studio からの利用

このアプリの機能は Studio UI からノーコードで利用できます：

1. **RAGノード**: ナレッジベース検索
2. **Text2SQLノード**: データベースクエリ
3. **チャートノード**: 可視化
4. **提案ノード**: フォローアップ生成

```
GET /api/nodes/service
→ 利用可能なサービスノード一覧を取得
```

## アーキテクチャ

```
┌─────────────────────────────────────────────────┐
│                   App Layer                      │
│  apps/faq_system/main.py                        │
│  - API エンドポイント定義                        │
│  - FAQAgent 呼び出し                            │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│               Agent Layer (NEW)                  │
│  agentflow/agents/faq_agent.py                  │
│  - FAQAgent (ResilientAgent 継承)               │
│  - FAQInput/FAQOutput (Pydantic)                │
│  - 自動リトライ・タイムアウト制御               │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│               Service Layer                      │
│  agentflow/services/                            │
│  ├── rag_service.py      ← RAG 検索            │
│  ├── text2sql_service.py ← SQL 生成・実行      │
│  ├── chart_service.py    ← チャート生成        │
│  └── suggestion_service.py ← 提案生成          │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│                 Core Layer                       │
│  - LLM Provider（松耦合・環境変数から自動取得） │
│  - Vector DB Provider                           │
│  - Database Provider                            │
└─────────────────────────────────────────────────┘
```

## 注意事項（開発者向け）

### ❌ やってはいけないこと

1. **`apps/faq_system/backend/agents/` に独自 Agent を作成しない**
   - Agent はフレームワーク層（`agentflow/agents/`）に配置
   - App 層は API ルーティングのみ

2. **`AgentBlock` を直接継承しない**
   - 必ず `ResilientAgent[Input, Output]` を継承
   - 型パラメータで入出力を明示

3. **`self._llm.chat()` を直接呼び出さない**
   - `self._call_llm(prompt)` を使用（ResilientAgent が提供）
   - 自動リトライ・タイムアウトが適用される

### ✅ やるべきこと

1. **Pydantic で入出力スキーマを定義**
2. **`_parse_input()` と `process()` を実装**
3. **内部メソッドは `_` または `__` でプレフィックス**

---

## 将来構想（backend/ 配下の拡張 Agent）

`apps/faq_system/backend/agents/` に以下の拡張 Agent が設計・実装されています。
現在は `main.py` のルーティングには未結線ですが、将来的に統合予定です。

| Agent | ファイル | 説明 |
|-------|---------|------|
| `InternalKBAgent` | `backend/agents/internal_kb_agent.py` | 社内 KB 検索（保守モード・引用必須） |
| `ExternalKBAgent` | `backend/agents/external_kb_agent.py` | 対客 KB 検索 |
| `MaintenanceAgent` | `backend/agents/maintenance_agent.py` | 仕様差分分析・影響範囲分析 |
| `AnalyticsAgent` | `backend/agents/analytics_agent.py` | NL2SQL 増強版データ分析 |
| `EnhancedFAQAgent` | `backend/agents/enhanced_faq_agent.py` | 統合 FAQ Agent |

詳細な設計は [DESIGN.md](./DESIGN.md) を参照してください。

---

## CLI 参照（運用メモ）

```bash
# workflow YAML を直接実行
agentflow flow run workflow.yaml --json

# 外部 Skill をマウント
agentflow skills mount ./external/faq-policy-check --scope project

# マウント済み Skill を確認
agentflow skills list --project
```

---

## ライセンス

MIT License

## 共有テスト env 自動生成

```bash
conda run -n agentflow python scripts/bootstrap_test_env.py --env-file .env
```

- `JWT_SECRET_KEY` と `FAQ_PROXY_AUTH_SHARED_SECRET` は上記で自動補完されます。
- FAQ は `apps/faq_system/.env` がルート `.env` より優先されるため、同スクリプトで両方を同期してください。

## 本番運用と多租户招待メール

- 本番では署名検証 (`FAQ_PROXY_AUTH_REQUIRE_SIGNATURE=true`) を維持し、共有鍵は Secret Manager から注入します。
- テナント招待メールは最小情報のみ送信し、ログイン URL が必要な場合は別メールで送信します。
- 詳細手順: `docs/internal/env-bootstrap-and-tenant-invite-security.md`

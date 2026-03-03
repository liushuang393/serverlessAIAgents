# Auth Service

スタンドアロン認証サービス for serverlessAIAgents プロジェクト。

JWT・OAuth2（Google/Azure AD）・LDAP・SAML・プロキシ認証・MFA（TOTP）をサポート。

---

## アーキテクチャ概要

```
┌───────────────────────────────────────────────────────────────────┐
│                          auth_service                              │
│                    runs on port 8010                               │
│                                                                   │
│  POST /auth/login        → JWT access + refresh tokens            │
│  POST /auth/refresh      → token rotation                         │
│  GET  /auth/me           → verify & return user info              │
│  GET  /auth/.well-known/jwks.json → JWKS metadata                │
│                                                                   │
│  POST/GET/PUT/DELETE /auth/authorization/* → 認可 API（17 API）   │
│  GET/PUT/DELETE /auth/admin/*              → 管理 API（5 API）    │
└───────────────────────────┬───────────────────────────────────────┘
                            │ shared JWT secret (roles + permissions)
          ┌─────────────────┼──────────────────┐
          ▼                 ▼                  ▼
    faq_system        platform app       your app
   (port 8005)        (port 8000)       (any port)

  from agentflow.security.auth_client import require_auth, require_permission
  @router.get("/protected")
  async def endpoint(user=Depends(require_auth)):
      return {"user": user.username}

  @router.get("/faq-data")
  async def faq_data(user=Depends(require_permission("faq:read"))):
      return {"data": "..."}
```

---

## クイックスタート

### 1. 起動

**開発用スクリプト（推奨）:**

```bash
# .env の自動作成、Docker 依存サービス起動、アプリ起動まで一括で行います
python apps/auth_service/scripts/dev.py
```

**手動起動:**

```bash
# .env を設定
cp apps/auth_service/.env.example apps/auth_service/.env
# JWT_SECRET_KEY を必ず変更してください

# 起動
conda activate agentflow
python -m apps.auth_service.main
```

### 2. 他アプリとの統合（最小設定）

**環境変数 2 つだけ設定する:**

```bash
# .env または環境変数
AUTH_SERVICE_URL=http://localhost:8010
AUTH_SERVICE_JWT_SECRET=<auth_service と同じ JWT_SECRET_KEY の値>
```

**main.py に 1 行追加:**

```python
from agentflow.security.auth_client import AuthClient, AuthMiddleware

# アプリ起動時
auth = AuthClient(
    base_url=os.getenv("AUTH_SERVICE_URL"),
    jwt_secret=os.getenv("AUTH_SERVICE_JWT_SECRET"),
)

# ミドルウェア追加（オプション）
app.add_middleware(AuthMiddleware, auth_client=auth)
```

**ルーターで認証を使う:**

```python
from agentflow.security.auth_client import require_auth, require_role, require_permission, get_current_user

# 認証必須エンドポイント
@router.get("/protected")
async def protected(user=Depends(require_auth)):
    return {"user_id": user.user_id, "username": user.username}

# ロール制限
@router.get("/admin-only")
async def admin(user=Depends(require_role("admin"))):
    return {"admin": True}

# パーミッション制限
@router.get("/faq-data")
async def faq_data(user=Depends(require_permission("faq:read"))):
    return {"data": "..."}

# 認証任意（ゲストも可）
@router.get("/public")
async def public(user=Depends(get_current_user)):
    return {"logged_in": user is not None}
```

### 3. SSO（単一ポイント認証）リダイレクト

クライアントアプリが認証を `auth_service` に委譲し、認証成功後に自動的に元のアプリに戻るフローです。

**フロー:**

1. クライアントアプリがユーザーを `auth_service` のログインページ（または API）へ誘導
2. `POST /auth/sso/login` に `redirect_uri` を含めてリクエスト
3. `auth_service` が認証を行い、成功時に `redirect_uri` にトークンをクエリパラメータとして付与してリダイレクト

**リクエスト例:**

```bash
curl -X POST http://localhost:8010/auth/sso/login \
  -H "Content-Type: application/json" \
  -d '{
    "username": "testuser",
    "password": "TestPass123",
    "redirect_uri": "http://localhost:8000/welcome"
  }'
```

**レスポンス（リダイレクト先）:**
`302 Found` -> `http://localhost:8000/welcome?access_token=...&refresh_token=...&token_type=bearer&expires_in=1800`

---

## Docker デプロイ

本番環境やローカル Docker 環境での実行方法です。

### Platform 運用ターゲット（Start / Local Start / Publish）

`platform` の App 画面から `auth_service` を操作する場合、ターゲットは次で統一します。

1. Backend: `auth-service` (port `8010`)
2. Frontend: `auth-admin` (port `3010`)
3. DB: `auth-db` PostgreSQL (host `localhost`, port `5438`, db `auth_service`)

DB ルール:

- `local_start` と `docker publish/start` は **同一 DB 種別（PostgreSQL）** を使う
- ローカル実行 URL: `postgresql+asyncpg://postgres:postgres@localhost:5438/auth_service`
- Docker 内部 URL: `postgresql+asyncpg://postgres:postgres@auth-db:5432/auth_service`
- compose では `AUTH_DATABASE_URL_DOCKER` を優先し、ローカル `.env` の `AUTH_DATABASE_URL` と衝突させない

健康チェック（漏れ防止）:

- Backend: `GET http://localhost:8010/health`
- Frontend: `GET http://localhost:3010/`
- DB: `localhost:5438` の TCP 待受確認（Platform health component）

### 1. Docker Compose による起動

```bash
cd apps/auth_service
docker compose up --build -d
```

ポート `8010/3010/5438` でサービスが開始され、DB データは `auth-db-data` ボリュームに永続化されます。

### 2. Platform Studio からのパブリッシュ

`app_config.json` に設定されているため、Studio の「Publish」ボタンをクリックするだけで Docker デプロイが実行されます。

### 3. auth_service + FAQ 統合起動

`docker-compose.auth-faq.yml` で auth_service、FAQ（バックエンド + DB + Qdrant）、管理画面を一括起動できます。

```bash
# リポジトリルートで実行
docker compose -f docker-compose.auth-faq.yml up --build -d

# 各サービス URL:
# auth API:    http://localhost:8010/docs
# 管理画面:    http://localhost:3010
# FAQ:         http://localhost:8005
```

サービス構成:

| サービス | ポート | 説明 |
| -------- | ------ | ---- |
| auth-service | 8010 | 認証・認可 API |
| auth-admin | 3010 | 管理画面（React SPA） |
| faq-backend | 8005 | FAQ バックエンド |
| faq-db | 5432 | PostgreSQL（FAQ 用） |
| faq-qdrant | 6333 | Qdrant ベクトル DB |

---

## API エンドポイント

| メソッド | パス                               | 説明                                |
| -------- | ---------------------------------- | ----------------------------------- |
| `POST`   | `/auth/register`                   | ユーザー登録（local_db モードのみ） |
| `POST`   | `/auth/login`                      | ログイン → JWT + refresh token      |
| `POST`   | `/auth/refresh`                    | リフレッシュトークンで再発行        |
| `POST`   | `/auth/logout`                     | ログアウト（トークン失効）          |
| `GET`    | `/auth/me`                         | 現在のユーザー情報                  |
| `POST`   | `/auth/password/change`            | パスワード変更                      |
| `POST`   | `/auth/password/forgot`            | パスワードリセット要求              |
| `POST`   | `/auth/password/reset`             | パスワードリセット実行              |
| `PUT`    | `/auth/profile`                    | プロフィール更新                    |
| `POST`   | `/auth/mfa/setup`                  | MFA 設定開始（QR コード URI 返却）  |
| `POST`   | `/auth/mfa/verify`                 | MFA 設定確認（有効化）              |
| `POST`   | `/auth/mfa/disable`                | MFA 無効化                          |
| `GET`    | `/auth/oauth2/{provider}`          | OAuth2 認可 URL 取得                |
| `GET`    | `/auth/oauth2/{provider}/callback` | OAuth2 コールバック                 |
| `GET`    | `/auth/.well-known/jwks.json`      | JWKS エンドポイント                 |
| `GET`    | `/health`                          | ヘルスチェック                      |

### 認可 API (`/auth/authorization`)

| メソッド | パス | 説明 |
| -------- | ---- | ---- |
| `GET` | `/auth/authorization/roles` | ロール一覧 |
| `GET` | `/auth/authorization/roles/{name}` | ロール詳細 |
| `POST` | `/auth/authorization/roles` | ロール作成 |
| `PUT` | `/auth/authorization/roles/{name}` | ロール更新 |
| `DELETE` | `/auth/authorization/roles/{name}` | ロール削除 |
| `GET` | `/auth/authorization/permissions` | パーミッション一覧 |
| `POST` | `/auth/authorization/roles/{name}/permissions` | ロールにパーミッション割り当て |
| `DELETE` | `/auth/authorization/roles/{name}/permissions/{perm}` | ロールからパーミッション削除 |
| `GET` | `/auth/authorization/users/{user_id}/roles` | ユーザーロール一覧 |
| `POST` | `/auth/authorization/users/{user_id}/roles` | ユーザーにロール割り当て |
| `DELETE` | `/auth/authorization/users/{user_id}/roles/{role}` | ユーザーからロール解除 |
| `GET` | `/auth/authorization/users/{user_id}/permissions` | ユーザー有効パーミッション |
| `POST` | `/auth/authorization/check` | 認可チェック |
| `POST` | `/auth/authorization/check-resource` | リソースアクセスチェック |
| `GET` | `/auth/authorization/resource-permissions` | リソースパーミッション一覧 |
| `POST` | `/auth/authorization/resource-permissions` | リソースパーミッション作成 |
| `DELETE` | `/auth/authorization/resource-permissions/{id}` | リソースパーミッション削除 |

### 管理 API (`/auth/admin`)

| メソッド | パス | 説明 |
| -------- | ---- | ---- |
| `GET` | `/auth/admin/users` | ユーザー一覧（ページネーション付き） |
| `GET` | `/auth/admin/users/{user_id}` | ユーザー詳細 |
| `PUT` | `/auth/admin/users/{user_id}` | ユーザー更新（ロール・有効/無効） |
| `DELETE` | `/auth/admin/users/{user_id}` | ユーザー無効化（論理削除） |
| `POST` | `/auth/admin/users/{user_id}/reset-password` | 管理者パスワードリセット |

### ログイン例

```bash
curl -X POST http://localhost:8010/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username": "admin", "password": "admin123"}'
```

レスポンス:

```json
{
  "success": true,
  "message": "ログイン成功",
  "user": {
    "user_id": "user-admin",
    "username": "admin",
    "display_name": "管理者",
    "role": "admin",
    "roles": ["admin"],
    "permissions": ["*"]
  },
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "refresh_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "token_type": "bearer",
  "expires_in": 1800
}
```

### トークンリフレッシュ

```bash
curl -X POST http://localhost:8010/auth/refresh \
  -H "Content-Type: application/json" \
  -d '{"refresh_token": "<refresh_token>"}'
```

---

## 認証プロバイダー

`AUTH_PROVIDER` 環境変数で切り替え。

| プロバイダー  | 設定値     | 説明                            |
| ------------- | ---------- | ------------------------------- |
| ローカル DB   | `local_db` | デフォルト。PBKDF2 ハッシュ認証 |
| Google OAuth2 | `google`   | Google アカウントでログイン     |
| Azure AD      | `azure_ad` | Microsoft Entra ID でログイン   |
| LDAP          | `ldap`     | Active Directory / OpenLDAP     |
| SAML 2.0      | `saml`     | エンタープライズ IdP            |
| Proxy Auth    | `proxy`    | リバースプロキシ（IAP 等）連携  |

---

## RBAC（ロールベースアクセス制御）

### デフォルトロール

3 つのデフォルトロールが初期データとしてシードされます。

| ロール | パーミッション |
| ------ | -------------- |
| `admin` | `*`（全権限） |
| `manager` | `users:read`, `roles:read`, `faq:read`, `faq:write`, `analytics:read`, `analytics:write`, `system:read` |
| `employee` | `users:read`, `faq:read`, `analytics:read` |

### ワイルドカードマッチング

- `*` は全パーミッションにマッチ（admin 用）
- `faq:*` は `faq:` プレフィックスを持つ全パーミッションにマッチ
- 完全一致: `faq:read` は `faq:read` のみにマッチ

### リソースパーミッション（Default-open）

`resource_permissions` テーブルによるリソース単位のアクセス制御をサポートします。

- **Default-open**: マッピング未設定時はフルアクセス許可
- `resource_type` 例: `vector_db`, `business_db`, `kb`, `app`, `api`
- `permission_level`: `none` / `read` / `write` / `admin`

リソースパーミッションが必要な場合のみ `resource_permissions` テーブルにレコードを追加し、明示的にアクセスレベルを制限します。

### 認可関連の環境変数

`.env.example` で設定可能な認可関連の設定項目です。

| 環境変数 | デフォルト | 説明 |
| -------- | ---------- | ---- |
| `AUTHZ_CACHE_TTL_SECONDS` | `300` | 認可キャッシュ TTL（秒） |
| `AUTHZ_EMBED_PERMISSIONS_IN_JWT` | `true` | JWT にロール・パーミッションを埋め込む |
| `AUTHZ_MAX_JWT_PERMISSIONS` | `50` | JWT に埋め込むパーミッション数の上限 |
| `AUTHZ_DEFAULT_OPEN` | `true` | リソースパーミッション未設定時にフルアクセスを許可 |

---

## セキュリティ設計

### JWT トークン構造

- **アクセストークン**: 30 分有効。`sub`（user_id）、`username`、`role` 等を含む。
- **リフレッシュトークン**: 7 日有効。トークンローテーション付き（family ID 管理）。
- **JTI ブラックリスト**: ログアウト時にアクセストークンの JTI を DB に記録。
- **リプレイ攻撃防止**: リフレッシュトークン再利用検出 → 同一ファミリー全失効。
- **JWT ペイロードに `roles: list[str]` と `permissions: list[str]` を埋め込み**
  - 設定: `AUTHZ_EMBED_PERMISSIONS_IN_JWT=true`（デフォルト有効）
  - パーミッション数上限: `AUTHZ_MAX_JWT_PERMISSIONS=50`（超過時は JWT に埋め込まず、API で都度取得）

### JWKS エンドポイント

`GET /auth/.well-known/jwks.json` で鍵情報を取得できる。
HS256（対称鍵）を使用しているため、クライアントは共有シークレットでローカル検証する。

```python
# ローカル検証（HTTP ラウンドトリップなし）
user = auth_client.verify_token_locally(token)

# リモート検証（フォールバック）
user = await auth_client.verify_token_remote(token)

# 自動選択（ローカル優先）
user = await auth_client.verify_token(token)
```

### パスワードセキュリティ

- PBKDF2-HMAC-SHA256 with 200,000 iterations
- ランダムソルト（32バイト）
- ログイン試行制限（デフォルト 5 回）とロックアウト（15 分）

---

## ディレクトリ構造

```
apps/auth_service/
├── main.py                FastAPI エントリーポイント
├── config.py              pydantic-settings 設定
├── service.py             コアサービスロジック
├── .env.example           設定例
├── requirements.txt       依存関係
├── models/
│   ├── user.py            UserAccount, AuthSession モデル
│   ├── token.py           RefreshToken, TokenBlacklist モデル
│   └── authorization.py   Role, Permission, RolePermission, UserRole, ResourcePermission モデル
├── db/
│   ├── session.py             DB セッション管理
│   └── seed_authorization.py  認可テーブル初期データシード
├── core/
│   ├── jwt.py             JWT 発行/検証
│   ├── password.py        PBKDF2 パスワードユーティリティ
│   ├── mfa.py             TOTP MFA ユーティリティ
│   └── authorization.py   認可コアサービス（RBAC/キャッシュ）
├── providers/             認証プロバイダー（Strategy パターン）
│   ├── base.py            AbstractAuthProvider
│   ├── local.py           ローカル DB プロバイダー（完全実装）
│   ├── google.py          Google OAuth2
│   ├── azure_ad.py        Azure AD OAuth2
│   ├── ldap.py            LDAP/AD
│   ├── saml.py            SAML 2.0（スタブ）
│   └── proxy.py           リバースプロキシ認証
├── api/
│   ├── router.py                  認証エンドポイント
│   ├── router_authorization.py    認可エンドポイント（17 API）
│   ├── router_admin.py            管理エンドポイント（5 API）
│   ├── dependencies.py            require_permission, require_admin 依存関係
│   ├── schemas.py                 Pydantic スキーマ（認証）
│   └── schemas_authorization.py   Pydantic スキーマ（認可・管理）
├── frontend/              管理画面（React + Vite + TypeScript）
│   ├── src/pages/
│   │   ├── LoginPage.tsx      ログイン画面
│   │   ├── UsersPage.tsx      ユーザー管理
│   │   ├── RolesPage.tsx      ロール管理
│   │   └── ResourcesPage.tsx  リソースパーミッション設定
│   └── ...
└── migrations/
    └── versions/
        └── 001_initial.py     初期スキーマ

agentflow/security/auth_client/
├── __init__.py            パブリック API（require_permission 追加）
├── client.py              AuthClient（HTTP + ローカル検証）
├── middleware.py           FastAPI ミドルウェア
├── dependencies.py        require_auth, require_role, require_permission, get_current_user
└── config.py              AuthClientConfig
```

### リソース定義 API

リソースの標準定義を管理するマスタ API。各 App は自分のリソース（vector_db, business_db, kb 等）を登録する。

| エンドポイント | メソッド | 認可 | 説明 |
|---|---|---|---|
| `/auth/authorization/resource-definitions` | GET | 認証済み | リソース定義一覧（app_name, resource_type でフィルタ可） |
| `/auth/authorization/resource-definitions` | POST | `*`（admin） | リソース定義作成 |
| `/auth/authorization/resource-definitions/{id}` | DELETE | `*`（admin） | リソース定義削除 |

### スコープ解決 API

ロールの許可スコープを解決する API。resource_permissions × resource_definitions を結合して返す。

| エンドポイント | メソッド | 認可 | 説明 |
|---|---|---|---|
| `/auth/authorization/resolve-scopes` | GET | 認証済み | `role`, `app_name`, `resource_type` パラメータで許可スコープ一覧を取得 |

**レスポンス例:**

```json
{
  "role": "manager",
  "app_name": "faq_system",
  "resource_type": "vector_db",
  "scopes": [
    {
      "scope": "common",
      "resource_id": "faq__default__common",
      "backend_key": "shared",
      "collection_tpl": "{app}__{tenant}__{scope}",
      "permission_level": "read"
    }
  ]
}
```

### FAQ 用シードデータ

起動時に以下のリソース定義が自動シードされる（`seed_faq_resource_definitions()`）:

| resource_id | scope | backend_key | 説明 |
|---|---|---|---|
| `faq__default__common` | common | shared | 全社員アクセス可 |
| `faq__default__manager` | manager | shared | マネージャー限定 |
| `faq__default__sales` | sales | shared | 営業部限定 |
| `faq__default__employee` | employee | shared | 一般社員限定 |
| `faq__default__confidential` | confidential | confidential | 機密（専用 Qdrant） |

---

## 開発・テスト

```bash
# 起動
uvicorn apps.auth_service.main:app --port 8010 --reload

# API ドキュメント
open http://localhost:8010/docs

# ヘルスチェック
curl http://localhost:8010/health
```

## 自動テスト

```bash
# 1. ローカル起動テスト（最重要 — ImportError が解消されることを確認）
conda activate agentflow
python -m apps.auth_service.main
# → ポート 8010 で起動し "auth_service 起動中..." が表示されること

# 2. ヘルスチェック
curl http://localhost:8010/health
# → {"status":"ok","service":"auth_service","version":"1.0.0","provider":"local_db"}

# 3. ユーザー登録 → ログイン → /auth/me フロー
curl -X POST http://localhost:8010/auth/register \
  -H "Content-Type: application/json" \
  -d '{"username":"testuser","password":"TestPass123","display_name":"テスト"}'

curl -X POST http://localhost:8010/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"testuser","password":"TestPass123"}'

# 4. Docker ビルド＆起動
cd apps/auth_service && docker compose up --build -d
curl http://localhost:8010/health
docker compose down

# 5. 認可テスト
pytest tests/apps/auth_service/test_authorization_models.py -v
pytest tests/apps/auth_service/test_authorization_service.py -v
pytest tests/apps/auth_service/test_authorization_api.py -v
pytest tests/apps/auth_service/test_authorization_e2e.py -v

# 6. 全テスト一括実行
pytest tests/apps/auth_service/ -v --no-cov
```

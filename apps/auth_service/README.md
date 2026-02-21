# Auth Service

スタンドアロン認証サービス for serverlessAIAgents プロジェクト。

JWT・OAuth2（Google/Azure AD）・LDAP・SAML・プロキシ認証・MFA（TOTP）をサポート。

---

## アーキテクチャ概要

```
┌──────────────────────────────────────────────────────────────┐
│                        auth_service                          │
│                  runs on port 8010                           │
│                                                              │
│  POST /auth/login    → JWT access + refresh tokens          │
│  POST /auth/refresh  → token rotation                       │
│  GET  /auth/me       → verify & return user info            │
│  GET  /auth/.well-known/jwks.json → JWKS metadata           │
└──────────────────────────┬───────────────────────────────────┘
                           │ shared JWT secret
         ┌─────────────────┼──────────────────┐
         ▼                 ▼                  ▼
   faq_system        platform app       your app
  (port 8005)        (port 8001)       (any port)

  from agentflow.security.auth_client import require_auth
  @router.get("/protected")
  async def endpoint(user=Depends(require_auth)):
      return {"user": user.username}
```

---

## クイックスタート

### 1. 起動

```bash
# .env を設定
cp apps/auth_service/.env.example apps/auth_service/.env
# JWT_SECRET_KEY を必ず変更してください

# 起動
conda activate agentflow
uvicorn apps.auth_service.main:app --port 8010 --reload
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
from agentflow.security.auth_client import require_auth, require_role, get_current_user

# 認証必須エンドポイント
@router.get("/protected")
async def protected(user=Depends(require_auth)):
    return {"user_id": user.user_id, "username": user.username}

# ロール制限
@router.get("/admin-only")
async def admin(user=Depends(require_role("admin"))):
    return {"admin": True}

# 認証任意（ゲストも可）
@router.get("/public")
async def public(user=Depends(get_current_user)):
    return {"logged_in": user is not None}
```

---

## API エンドポイント

| メソッド | パス | 説明 |
|---------|------|------|
| `POST` | `/auth/register` | ユーザー登録（local_db モードのみ） |
| `POST` | `/auth/login` | ログイン → JWT + refresh token |
| `POST` | `/auth/refresh` | リフレッシュトークンで再発行 |
| `POST` | `/auth/logout` | ログアウト（トークン失効） |
| `GET`  | `/auth/me` | 現在のユーザー情報 |
| `POST` | `/auth/password/change` | パスワード変更 |
| `POST` | `/auth/password/forgot` | パスワードリセット要求 |
| `POST` | `/auth/password/reset` | パスワードリセット実行 |
| `PUT`  | `/auth/profile` | プロフィール更新 |
| `POST` | `/auth/mfa/setup` | MFA 設定開始（QR コード URI 返却） |
| `POST` | `/auth/mfa/verify` | MFA 設定確認（有効化） |
| `POST` | `/auth/mfa/disable` | MFA 無効化 |
| `GET`  | `/auth/oauth2/{provider}` | OAuth2 認可 URL 取得 |
| `GET`  | `/auth/oauth2/{provider}/callback` | OAuth2 コールバック |
| `GET`  | `/auth/.well-known/jwks.json` | JWKS エンドポイント |
| `GET`  | `/health` | ヘルスチェック |

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
    "role": "admin"
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

| プロバイダー | 設定値 | 説明 |
|------------|-------|------|
| ローカル DB | `local_db` | デフォルト。PBKDF2 ハッシュ認証 |
| Google OAuth2 | `google` | Google アカウントでログイン |
| Azure AD | `azure_ad` | Microsoft Entra ID でログイン |
| LDAP | `ldap` | Active Directory / OpenLDAP |
| SAML 2.0 | `saml` | エンタープライズ IdP |
| Proxy Auth | `proxy` | リバースプロキシ（IAP 等）連携 |

---

## セキュリティ設計

### JWT トークン構造

- **アクセストークン**: 30 分有効。`sub`（user_id）、`username`、`role` 等を含む。
- **リフレッシュトークン**: 7 日有効。トークンローテーション付き（family ID 管理）。
- **JTI ブラックリスト**: ログアウト時にアクセストークンの JTI を DB に記録。
- **リプレイ攻撃防止**: リフレッシュトークン再利用検出 → 同一ファミリー全失効。

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
├── main.py              FastAPI エントリーポイント
├── config.py            pydantic-settings 設定
├── service.py           コアサービスロジック
├── .env.example         設定例
├── requirements.txt     依存関係
├── models/
│   ├── user.py          UserAccount, AuthSession モデル
│   └── token.py         RefreshToken, TokenBlacklist モデル
├── db/
│   └── session.py       DB セッション管理
├── core/
│   ├── jwt.py           JWT 発行/検証
│   ├── password.py      PBKDF2 パスワードユーティリティ
│   └── mfa.py           TOTP MFA ユーティリティ
├── providers/           認証プロバイダー（Strategy パターン）
│   ├── base.py          AbstractAuthProvider
│   ├── local.py         ローカル DB プロバイダー（完全実装）
│   ├── google.py        Google OAuth2
│   ├── azure_ad.py      Azure AD OAuth2
│   ├── ldap.py          LDAP/AD
│   ├── saml.py          SAML 2.0（スタブ）
│   └── proxy.py         リバースプロキシ認証
├── api/
│   ├── router.py        全エンドポイント
│   └── schemas.py       Pydantic スキーマ
└── migrations/
    └── versions/
        └── 001_initial.py  初期スキーマ

agentflow/security/auth_client/
├── __init__.py          パブリック API
├── client.py            AuthClient（HTTP + ローカル検証）
├── middleware.py        FastAPI ミドルウェア
├── dependencies.py      require_auth, get_current_user 等
└── config.py            AuthClientConfig
```

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

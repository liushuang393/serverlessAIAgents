# 内蔵 Production-Ready Skills ガイド

> **バージョン**: 1.0.0
> **更新日**: 2025-01-20

---

## 📋 概要

AgentFlow は、すぐに本番環境で使える **企業級スキル** を内蔵しています。これらのスキルにより、Agent システムを迅速に本番化できます。

### 内蔵スキル一覧

| スキル | 説明 | 対応サービス |
|--------|------|------------|
| 🗄️ **database-manager** | データベース統合管理 | Supabase / Turso / PostgreSQL |
| 🔍 **vectordb** | ベクトルデータベース統合 | FAISS / Qdrant / Weaviate / Supabase / ChromaDB |
| 💳 **stripe-payment** | 決済・サブスクリプション | Stripe Checkout / Billing |
| 🚀 **deployment-manager** | デプロイ・環境管理 | Vercel / Cloudflare Pages |
| 🔐 **auth-provider** | 認証・セッション管理 | Supabase Auth / Clerk |

---

## 🔍 VectorDB Provider（黒盒設計）

### 概要

FAISS、Qdrant、Weaviate、Supabase Vector、ChromaDB などのベクトルデータベースを**統一インターフェース**で操作。
Agent/サービスは具体的な実装を意識する必要がありません（黒盒設計）。

### クイックスタート

```python
from agentflow import get_vectordb

# 環境変数から自動検出
vdb = get_vectordb(collection="my_docs")
await vdb.connect()

# ドキュメント追加
await vdb.add(
    documents=["Hello world", "Goodbye world"],
    embeddings=[[0.1, 0.2, ...], [0.3, 0.4, ...]],
    metadatas=[{"type": "greeting"}, {"type": "farewell"}],
)

# 類似検索
results = await vdb.search(
    query="Hello",
    query_embedding=[0.1, 0.2, ...],
    top_k=5,
)
```

### 対応プロバイダー

| プロバイダー | 特徴 | 必要パッケージ |
|-------------|------|---------------|
| **FAISS** | ローカル高速検索、GPU対応、大規模データ | `faiss-cpu` |
| **Qdrant** | クラウド/ローカル、スケーラブル、本番推奨 | `qdrant-client` |
| **Weaviate** | セマンティック検索、GraphQL API | `weaviate-client` |
| **Supabase Vector** | PostgreSQL pgvector、SQL クエリ可能 | `supabase` |
| **ChromaDB** | ローカル開発、シンプル（デフォルト） | `chromadb` |

### 環境変数設定

```bash
# 方法1: 明示的にタイプを指定（推奨）
VECTOR_DATABASE_TYPE=qdrant    # "faiss", "qdrant", "weaviate", "supabase", "chromadb"

# 方法2: サービス URL で自動検出
QDRANT_URL=http://localhost:6333
WEAVIATE_URL=http://localhost:8080
SUPABASE_URL=https://xxx.supabase.co
SUPABASE_KEY=your-api-key
FAISS_INDEX_PATH=/path/to/index.faiss
CHROMA_PERSIST_DIR=/path/to/chroma
```

### 主な機能

- **CRUD 操作**: add / search / delete / clear
- **類似検索**: ベクトル類似度による高速検索
- **メタデータフィルタ**: 属性によるフィルタリング
- **永続化**: ファイル/サーバー保存
- **自動フォールバック**: 依存パッケージ不在時の自動降格

### RAG 統合例

```python
from agentflow.skills.rag import RAGSkill
import os

# Qdrant を使用
os.environ["VECTOR_DATABASE_TYPE"] = "qdrant"
os.environ["QDRANT_URL"] = "http://localhost:6333"

rag = RAGSkill()
await rag.start()
await rag.add_document("AgentFlow documentation...", topic="docs")
result = await rag.query("What is AgentFlow?")
```

詳細は [rag SKILL.md](../agentflow/skills/builtin/rag/SKILL.md) を参照。

---

## 🗄️ Database Manager

### 概要

Supabase、Turso、PostgreSQL などの主流データベースを統一インターフェースで操作。

### クイックスタート

```python
from agentflow.skills.builtin.database_manager import (
    DatabaseManager,
    SupabaseConfig,
)

# 設定
config = SupabaseConfig(
    url="https://xxx.supabase.co",
    anon_key="eyJ...",
    service_role_key="eyJ...",  # RLS バイパス用（オプション）
)

# 接続
db = DatabaseManager(provider="supabase", config=config)
await db.connect()

# CRUD 操作
users = await db.select("users", filters={"status": "active"})
new_user = await db.insert("users", {"email": "test@example.com"})
await db.update("users", {"name": "Updated"}, filters={"id": new_user["id"]})
await db.delete("users", filters={"id": new_user["id"]})
```

### 対応プロバイダー

| プロバイダー | 無料枠 | 特徴 |
|-------------|--------|------|
| **Supabase** | 500MB | RLS、リアルタイム、Edge Functions |
| **Turso** | 9GB | エッジ配置、低レイテンシ |
| **PostgreSQL** | - | 完全制御、セルフホスト |

### 主な機能

- **CRUD 操作**: select / insert / update / delete
- **トランザクション**: 複数操作の原子性保証
- **RLS 管理**: Row Level Security の設定
- **リアルタイム**: テーブル変更の購読（Supabase）
- **マイグレーション**: スキーマ変更の管理

詳細は [database-manager SKILL.md](../agentflow/skills/builtin/database-manager/SKILL.md) を参照。

---

## 💳 Stripe Payment

### 概要

Stripe 決済を統合し、ワンタイム支払い・サブスクリプション・Webhook 処理を提供。

### クイックスタート

```python
from agentflow.skills.builtin.stripe_payment import (
    StripePayment,
    StripeConfig,
)

# 設定
config = StripeConfig(
    secret_key="sk_test_...",
    webhook_secret="whsec_...",
    success_url="https://example.com/success",
    cancel_url="https://example.com/cancel",
)

# 初期化
stripe = StripePayment(config)

# Checkout Session 作成
session = await stripe.create_checkout_session(
    customer_email="customer@example.com",
    line_items=[{"price": "price_xxx", "quantity": 1}],
    mode="subscription",
    metadata={"user_id": "user_123"},
)

print(f"決済URL: {session.url}")
```

### 主な機能

- **Checkout Session**: ホスト型決済ページ
- **サブスクリプション管理**: 作成・更新・キャンセル
- **Customer Portal**: 顧客セルフサービス
- **Webhook 処理**: イベントの安全な処理（冪等性対応）
- **返金**: 全額・一部返金

### Webhook 処理

```python
from fastapi import FastAPI, Request

app = FastAPI()

@app.post("/webhooks/stripe")
async def handle_stripe_webhook(request: Request):
    body = await request.body()
    sig = request.headers.get("stripe-signature")
    
    event = stripe.verify_webhook(body, sig)
    
    await stripe.handle_webhook_event(
        event,
        handlers={
            "checkout.session.completed": handle_checkout_complete,
            "customer.subscription.updated": handle_subscription_update,
        }
    )
    
    return {"received": True}
```

詳細は [stripe-payment SKILL.md](../agentflow/skills/builtin/stripe-payment/SKILL.md) を参照。

---

## 🚀 Deployment Manager

### 概要

Vercel、Cloudflare Pages などへの統一デプロイインターフェース。

### クイックスタート

```python
from agentflow.skills.builtin.deployment_manager import (
    DeploymentManager,
    VercelConfig,
)

# 設定
config = VercelConfig(
    token="your_vercel_token",
    team_id="team_xxx",
)

# 初期化
deployer = DeploymentManager(provider="vercel", config=config)

# デプロイ
deployment = await deployer.deploy(
    project_name="my-agent-app",
    source_path="./dist",
    environment="production",
    env_vars={"API_KEY": "xxx"},
)

print(f"デプロイURL: {deployment.url}")
```

### 対応プラットフォーム

| プラットフォーム | 無料枠 | 特徴 |
|-----------------|--------|------|
| **Vercel** | 100GB/月 | Next.js 最適化、Preview デプロイ |
| **Cloudflare Pages** | 無制限 | グローバル CDN、エッジ |

### 主な機能

- **デプロイ**: 本番・プレビュー環境
- **ロールバック**: 即時復旧
- **環境変数管理**: 暗号化保存
- **ドメイン設定**: カスタムドメイン
- **デプロイ監視**: ステータス・ログ

詳細は [deployment-manager SKILL.md](../agentflow/skills/builtin/deployment-manager/SKILL.md) を参照。

---

## 🔐 Auth Provider

### 概要

Supabase Auth、Clerk などを統一インターフェースで操作。

### クイックスタート

```python
from agentflow.skills.builtin.auth_provider import (
    AuthProvider,
    SupabaseAuthConfig,
)

# 設定
config = SupabaseAuthConfig(
    url="https://xxx.supabase.co",
    anon_key="eyJ...",
    jwt_secret="your-jwt-secret",
)

# 初期化
auth = AuthProvider(provider="supabase", config=config)

# ユーザー登録
user = await auth.sign_up(
    email="user@example.com",
    password="secure_password",
    metadata={"name": "Test User"},
)

# ログイン
session = await auth.sign_in(
    email="user@example.com",
    password="secure_password",
)

print(f"アクセストークン: {session.access_token}")
```

### 対応プロバイダー

| プロバイダー | 無料枠 | 特徴 |
|-------------|--------|------|
| **Supabase Auth** | 50k MAU | RLS 統合、PostgreSQL 一体 |
| **Clerk** | 10k MAU | 最高の DX、プリビルト UI |

### 主な機能

- **認証**: メール/パスワード、Magic Link、OTP
- **OAuth**: Google、Apple、GitHub など
- **セッション管理**: JWT 検証、リフレッシュ
- **MFA**: TOTP 対応
- **パスワードリセット**: メール送信

詳細は [auth-provider SKILL.md](../agentflow/skills/builtin/auth-provider/SKILL.md) を参照。

---

## 🔧 Agent 統合パターン

### 複数スキルの組み合わせ

```python
from agentflow.skills import SkillEngine
from agentflow.skills.builtin.database_manager import DatabaseManager
from agentflow.skills.builtin.stripe_payment import StripePayment
from agentflow.skills.builtin.auth_provider import AuthProvider

# スキル初期化
db = DatabaseManager(provider="supabase", config=db_config)
stripe = StripePayment(stripe_config)
auth = AuthProvider(provider="supabase", config=auth_config)

# Agent Engine
engine = SkillEngine()

@engine.tool("register_user")
async def register_user(email: str, password: str, plan: str) -> dict:
    """ユーザー登録と決済を一括処理"""
    
    # 1. ユーザー作成
    user = await auth.sign_up(email=email, password=password)
    
    # 2. DB にプロファイル作成
    await db.insert("profiles", {
        "user_id": user.id,
        "email": email,
        "plan": plan,
    })
    
    # 3. 決済セッション作成
    if plan != "free":
        session = await stripe.create_checkout_session(
            customer_email=email,
            line_items=[{"price": f"price_{plan}", "quantity": 1}],
            mode="subscription",
            metadata={"user_id": user.id},
        )
        return {"user_id": user.id, "checkout_url": session.url}
    
    return {"user_id": user.id, "status": "registered"}
```

### Webhook ベースの自動化

```python
# Stripe Webhook で DB を自動更新
async def handle_subscription_change(event):
    subscription = event.data.object
    customer_id = subscription.customer
    
    # Stripe 顧客から内部ユーザーを検索
    users = await db.select("profiles", 
        filters={"stripe_customer_id": customer_id}
    )
    
    if users:
        await db.update("profiles",
            {"subscription_status": subscription.status},
            filters={"id": users[0]["id"]}
        )
```

---

## 📊 ベストプラクティス

### 1. 環境変数管理

```python
import os

# 本番環境では環境変数から読み込み
db_config = SupabaseConfig(
    url=os.environ["SUPABASE_URL"],
    anon_key=os.environ["SUPABASE_ANON_KEY"],
)

stripe_config = StripeConfig(
    secret_key=os.environ["STRIPE_SECRET_KEY"],
    webhook_secret=os.environ["STRIPE_WEBHOOK_SECRET"],
)
```

### 2. エラーハンドリング

```python
from agentflow.skills.builtin.database_manager import DatabaseError
from agentflow.skills.builtin.stripe_payment import PaymentError
from agentflow.skills.builtin.auth_provider import AuthError

try:
    await db.insert("users", data)
except DatabaseError as e:
    logger.error(f"DB エラー: {e}")
    # フォールバック処理
```

### 3. 接続プール管理

```python
# アプリケーション起動時に接続
@app.on_event("startup")
async def startup():
    await db.connect()

# シャットダウン時にクリーンアップ
@app.on_event("shutdown")
async def shutdown():
    await db.disconnect()
```

---

## 📚 関連ドキュメント

- [Skills ガイド](guide-skills.md) - 自動進化システム
- [LLM ルーター](guide-llm-router.md) - マルチモデル切替
- [クイックスタート](quickstart.md) - 入門ガイド


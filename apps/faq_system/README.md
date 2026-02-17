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

```bash
# 1. リポジトリをクローン（初回のみ）
git clone https://github.com/liushuang393/serverlessAIAgents.git
cd serverlessAIAgents

# 2. Python 仮想環境を作成
python -m venv .venv
source .venv/bin/activate        # Linux / macOS
# .venv\Scripts\activate         # Windows

# 3. 依存関係をインストール（apps オプション含む）
pip install -e ".[dev,apps]"

# 4. 環境変数を設定
cp .env.example .env
# .env を編集して以下を設定:
#   OPENAI_API_KEY=sk-...        （または ANTHROPIC_API_KEY）
#   RAG_COLLECTION=faq_knowledge （任意）
#   DB_SCHEMA={}                 （任意: JSON 形式の DB スキーマ）
#   FAQ_SALES_MATERIAL_DIR=/tmp/faq_sales_material （任意）
#   FAQ_DATABASE_URL=postgresql+asyncpg://faq:faq_password@localhost:5433/faq_system
#   FAQ_AUTH_PROVIDER=local_db   （local_db / ldap / idp）

# 5. FAQ アプリ専用の上書き設定（任意）
cp apps/faq_system/.env.example apps/faq_system/.env
# apps/faq_system/.env は FAQ 起動時に自動ロードされ、
# ルート .env より優先して適用されます。
# LLM は apps/faq_system/.env の LLM_PROVIDER で明示選択:
#   LLM_PROVIDER=ollama  または  LLM_PROVIDER=openai
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

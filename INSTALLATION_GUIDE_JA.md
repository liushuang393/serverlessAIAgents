# BizCore AI インストール・セットアップガイド

> **対象**: 初心者向けの詳細な手順書
> **最終更新**: 2026-03-20

---

## 📋 目次

1. [前提条件](#前提条件)
2. [BizCore AI のインストール](#bizcore-ai-のインストール)
3. [環境変数の設定](#環境変数の設定)
4. [Docker デプロイメント（推奨）](#docker-デプロイメント推奨)
   - [auth_service](#1-auth_service-認証サービス)
   - [decision_governance_engine](#2-decision_governance_engine-意思決定ガバナンスエンジン)
   - [market_trend_monitor](#3-market_trend_monitor-市場トレンドモニター)
   - [dev_studio](#4-dev_studio-開発者スタジオ)
5. [手動起動（Docker なし）](#手動起動docker-なし)
6. [動作確認](#動作確認)
7. [トラブルシューティング](#トラブルシューティング)

---

## 前提条件

### 必要なソフトウェア

| ソフトウェア | バージョン | 説明 |
|------------|----------|------|
| **Docker Desktop** | 最新版 | コンテナ実行環境（推奨デプロイ方法） |
| **Python** | 3.13+ | Python プログラミング言語（手動起動時） |
| **Node.js** | 22+ | フロントエンド開発用（手動起動時） |
| **npm** または **pnpm** | 最新版 | Node.js パッケージマネージャー |
| **Git** | 最新版 | コードの取得用 |

### インストール確認

```bash
# Docker のバージョン確認（Docker Desktop 起動後）
docker --version
# 出力例: Docker version 27.x.x

# Python のバージョン確認
python --version
# 出力例: Python 3.13.0

# Node.js のバージョン確認
node --version
# 出力例: v22.x.x
```

**注意**:
- Docker Desktop は [公式サイト](https://www.docker.com/products/docker-desktop/) からインストールし、起動してから作業を行ってください。
- WSL2 環境の場合は、Docker Desktop の「Settings → Resources → WSL Integration」で対象ディストリビューションを有効化してください。

---

## BizCore AI のインストール

### ステップ 1: プロジェクトの取得

```bash
# Git でクローンする場合
git clone https://github.com/liushuang393/serverlessAIAgents.git
cd serverlessAIAgents

# または ZIP でダウンロードした場合
# 解凍してフォルダに移動
cd serverlessAIAgents
```

### ステップ 2: Python 仮想環境の作成（推奨）

**Windows:**
```bash
# 仮想環境を作成
python -m venv venv

# 仮想環境を有効化
venv\Scripts\activate
```

**macOS / Linux:**
```bash
# 仮想環境を作成
python3 -m venv venv

# 仮想環境を有効化
source venv/bin/activate
```

**確認**: ターミナルの先頭に `(venv)` が表示されれば成功です。

### ステップ 3: BizCore AI のインストール

```bash
# プロジェクトルートで実行
pip install -e ".[apps]"

# または、開発用依存関係も含める場合
pip install -e ".[apps,dev]"
```

**インストール確認:**
```bash
# BizCore 配布パッケージがインストールされたか確認
pip show bizcore

# CLI コマンドが使えるか確認
bizcore --help

# レイヤーパッケージが import できるか確認
python -c "import kernel, shared; print('imports ok')"
```

---

## 環境変数の設定

### ステップ 1: .env ファイルの作成

プロジェクトルート（`serverlessAIAgents/`）に `.env` ファイルを作成：

```bash
# プロジェクトルートで実行
touch .env
# Windows の場合: type nul > .env
```

### ステップ 2: LLM API キーの設定

`.env` ファイルに以下を記述：

```env
# ============================================
# LLM Provider 設定（必須）
# ============================================

# OpenAI を使用する場合
OPENAI_API_KEY=

# または Anthropic Claude を使用する場合
ANTHROPIC_API_KEY=

# または Google Gemini を使用する場合
GOOGLE_API_KEY=

# ============================================
# モデル選択（オプション）
# ============================================
# LLM_MODEL=gpt-4o-mini          # OpenAI
# LLM_MODEL=claude-3-5-sonnet-20241022  # Anthropic
# LLM_MODEL=gemini-1.5-pro       # Google

# ============================================
# デバッグモード（オプション）
# ============================================
# DEBUG=true

# ============================================
# 知識ストア設定（オプション - 長期記憶機能）
# ============================================
# バックエンド選択:
#   auto   - 自動検出（memvid-sdkがあれば使用、なければmemoryにフォールバック）
#   memvid - Rust製高性能RAG（.mv2ファイル、セマンティック+BM25検索）
#   memory - インメモリ実装（BM25検索、JSON永続化可能、依存なし）
# KNOWLEDGE_BACKEND=auto

# 知識データの保存先パス
# KNOWLEDGE_STORAGE_PATH=memory/knowledge

# 自動永続化（終了時に保存）
# KNOWLEDGE_AUTO_PERSIST=true
```

**重要**:
- API キーは必ず `.env` ファイルに設定してください
- `.env` ファイルは Git にコミットしないでください（既に `.gitignore` に含まれています）

### ステップ 3: 環境変数の取得方法

#### OpenAI API キー
1. [OpenAI Platform](https://platform.openai.com/) にアクセス
2. アカウント作成・ログイン
3. 「API Keys」→「Create new secret key」
4. キーをコピーして `.env` に貼り付け

#### Anthropic API キー
1. [Anthropic Console](https://console.anthropic.com/) にアクセス
2. アカウント作成・ログイン
3. 「API Keys」→「Create Key」
4. キーをコピーして `.env` に貼り付け

---

## Docker デプロイメント（推奨）

> **前提**: Docker Desktop が起動していること、リポジトリルート（`serverlessAIAgents/`）で作業すること。

### ポート一覧

| サービス | コンポーネント | ホストポート | 説明 |
|---------|-------------|------------|------|
| **auth_service** | API | 8010 | 認証 API (JWT/OAuth2/LDAP) |
| **auth_service** | Admin UI | 3010 | 管理画面 (Nginx) |
| **auth_service** | PostgreSQL | 5438 | DB (コンテナ内 5432) |
| **decision_governance_engine** | API | 8001 | 意思決定 API |
| **decision_governance_engine** | Frontend | 5174 | React Dev Server |
| **decision_governance_engine** | PostgreSQL | 5432 | DB |
| **decision_governance_engine** | Redis | 6379 | キャッシュ |
| **market_trend_monitor** | API | 8002 | 市場トレンド API |
| **market_trend_monitor** | Frontend | 3002 | React (Nginx) |
| **market_trend_monitor** | SQLite | — | ボリューム内永続化 |

---

### 1. auth_service（認証サービス）

**場所**: `shared/auth_service/`
**ビルドコンテキスト**: リポジトリルート（自動設定済み）

```bash
# リポジトリルートから実行
cd shared/auth_service

# ビルド＆バックグラウンド起動
docker compose up --build -d

# ログ確認
docker compose logs -f auth-service

# 停止
docker compose down
```

**アクセス先:**
- API: `http://localhost:8010/health`
- API Docs: `http://localhost:8010/docs`
- Admin UI: `http://localhost:3010`

**主な環境変数**（`.env` または環境変数で上書き可能）:

```env
JWT_SECRET_KEY=your-secret-key-here      # 必須: JWTシークレット（本番では強力なキーを設定）
AUTH_PROVIDER=local_db                   # local_db / google / azure / ldap / saml
AUTH_DB_NAME=auth_service
AUTH_DB_USER=postgres
AUTH_DB_PASSWORD=postgres
AUTH_DB_PORT=5438
AUTH_SERVICE_PORT=8010
AUTH_ADMIN_PORT=3010
DEV_MODE=true                            # 開発時は true
```

> **nginx DNS 解決について（本番サーバー含む）:**
> `auth-admin`（nginx）は起動時に `auth-service` の DNS 解決を行いません。
> `/etc/resolv.conf` から DNS サーバーを動的取得し、リクエスト時に解決するため、
> Docker Compose・Kubernetes・本番サーバーのいずれでも追加設定なしに動作します。

---

### 2. decision_governance_engine（意思決定ガバナンスエンジン）

**場所**: `apps/decision_governance_engine/`
**ビルドコンテキスト**: リポジトリルート（自動設定済み）

```bash
# リポジトリルートから実行
cd apps/decision_governance_engine

# 開発モード（ホットリロード有効、フロントエンド port 5174）
docker compose -f docker-compose.yml -f docker-compose.dev.yml up --build -d

# ログ確認
docker compose -f docker-compose.yml -f docker-compose.dev.yml logs -f

# DB のみ先に起動（バックエンド開発時）
docker compose up -d postgres-main redis

# 停止（ボリューム保持）
docker compose -f docker-compose.yml -f docker-compose.dev.yml down

# 停止（ボリュームも削除）
docker compose -f docker-compose.yml -f docker-compose.dev.yml down -v
```

**アクセス先:**
- API: `http://localhost:8001/api/health`
- API Docs: `http://localhost:8001/docs`
- Frontend: `http://localhost:5174`

**主な環境変数**（`.env` に設定）:

```env
OPENAI_API_KEY=sk-...           # または ANTHROPIC_API_KEY / GOOGLE_API_KEY
POSTGRES_USER=dge
POSTGRES_PASSWORD=dge_password
POSTGRES_DB=decision_governance
DB_PORT=5432
REDIS_PORT=6379
API_PORT=8001
FRONTEND_DEV_PORT=5174
ENABLE_RAG=true
LOG_LEVEL=DEBUG
```

---

### 3. market_trend_monitor（市場トレンドモニター）

**場所**: `apps/market_trend_monitor/`
**ビルドコンテキスト**: リポジトリルート（自動設定済み）

```bash
# リポジトリルートから実行
cd apps/market_trend_monitor

# ビルド＆バックグラウンド起動
docker compose up --build -d

# ログ確認
docker compose logs -f

# 停止
docker compose down
```

**アクセス先:**
- API: `http://localhost:8002/health`
- API Docs: `http://localhost:8002/docs`
- Frontend: `http://localhost:3002`

**主な環境変数**（`.env` に設定）:

```env
OPENAI_API_KEY=sk-...           # または ANTHROPIC_API_KEY
DATABASE_URL=sqlite:////app/apps/market_trend_monitor/data/market_trend.db
API_PORT=8002
FRONTEND_PORT=3002
LOG_LEVEL=INFO
```

---

### 4. dev_studio（開発者スタジオ）

`dev_studio` は **Python ライブラリ**として設計されており、スタンドアロンの Docker サービスではありません。

提供機能:
- `apps/dev_studio/code_intelligence/` — AST 解析・コード品質・CI/CD インテグレーション
- `apps/dev_studio/codegen/` — コード生成エンジン
- `apps/dev_studio/wizard/` — AI エージェントウィザード（SkillForge, Synthesizer 等）

```bash
# BizCore パッケージとして他サービスが使用（追加インストール不要）
python -c "from apps.dev_studio.wizard.agent_wizard import AgentWizard; print('dev_studio OK')"

# コード生成の例
python -c "
from apps.dev_studio.codegen.generator import CodeGenerator
# ジェネレーターを使用
"
```

---

## 手動起動（Docker なし）

### decision_governance_engine

**ターミナル 1（バックエンド）:**

```bash
# プロジェクトルートで実行
uvicorn apps.decision_governance_engine.api:app --reload --host 0.0.0.0 --port 8001
```

**ターミナル 2（フロントエンド）:**

```bash
cd apps/decision_governance_engine/frontend
npm install && npm run dev
```

**CLI モード:**

```bash
python -m apps.decision_governance_engine.main --interactive
```

### market_trend_monitor

**ターミナル 1（バックエンド）:**

```bash
python -m apps.market_trend_monitor.backend.api.main
```

**ターミナル 2（フロントエンド）:**

```bash
cd apps/market_trend_monitor/frontend
npm install && npm run dev
```

### auth_service

```bash
python -m shared.auth_service.main
```

---

## 動作確認

### Docker コンテナ一覧の確認

```bash
# 稼働中コンテナの確認
docker ps

# 期待されるコンテナ（全サービス起動時）:
# auth-db, auth-service, auth-admin
# dge-postgres-main, dge-redis, dge-backend, dge-frontend
# market-trend-monitor-backend, market-trend-monitor-frontend
```

### 各サービスのヘルスチェック

```bash
# auth_service
curl http://localhost:8010/health

# decision_governance_engine
curl http://localhost:8001/api/health

# market_trend_monitor
curl http://localhost:8002/health
```

### フロントエンドへのアクセス

| サービス | URL |
|---------|-----|
| auth_service 管理画面 | http://localhost:3010 |
| DGE フロントエンド | http://localhost:5174 |
| market_trend_monitor | http://localhost:3002 |

### API ドキュメント（Swagger UI）

| サービス | URL |
|---------|-----|
| auth_service | http://localhost:8010/docs |
| DGE | http://localhost:8001/docs |
| market_trend_monitor | http://localhost:8002/docs |

### CLI モードの確認（DGE）

```bash
python -m apps.decision_governance_engine.main --interactive
質問> 新規事業への投資判断をしたい
```

---

## トラブルシューティング

### 問題 0: Docker が見つからない / 接続できない（WSL2）

**エラー:**
```
The command 'docker' could not be found in this WSL 2 distro.
```

**解決方法:**
1. Docker Desktop を起動する（Windows タスクバーのクジラアイコンが「Running」になるまで待つ）
2. Docker Desktop → Settings → Resources → WSL Integration で対象ディストリビューションを有効化
3. WSL2 ターミナルを再起動して確認:
   ```bash
   docker --version
   ```

---

### 問題 0b: nginx が起動しない（upstream host not found）

**エラー:**
```
[emerg] host not found in upstream "auth-service" in /etc/nginx/conf.d/default.conf:9
nginx: [emerg] host not found in upstream "auth-service"
```

**原因:**
nginx はデフォルトで upstream ホスト名を**起動時に静的解決**します。
`proxy_pass http://auth-service:8010;` と書くと、nginx が起動した瞬間に `auth-service` を解決しようとします。依存サービスがまだ起動中だったり、DNS が準備できていない場合、nginx がクラッシュします。

**本プロジェクトでの対策（実装済み）:**
`shared/auth_service/frontend/Dockerfile` で以下の 2 段階の仕組みを採用しています：

1. **起動時 DNS 動的取得** (`01-set-resolver.envsh`)
   コンテナ起動時に `/etc/resolv.conf` から実際の DNS サーバー IP を読み取り、`RESOLVER` 変数にセットします。

2. **nginx テンプレート展開** (`/etc/nginx/templates/default.conf.template`)
   `${RESOLVER}` プレースホルダーが起動時に実際の IP に置換されます。
   `set $backend` で変数経由の `proxy_pass` にすることで、DNS 解決をリクエスト時まで遅延させます。

```nginx
# /etc/resolv.conf から動的取得（Docker/K8s/Podman 全対応）
resolver ${RESOLVER} valid=30s ipv6=off;

location /auth/ {
    set $backend http://auth-service:8010;  # 変数経由→リクエスト時解決
    proxy_pass $backend;
}
```

**対応環境:**

| 実行環境 | DNS | 動作 |
|---------|-----|------|
| Docker Compose（ローカル） | `127.0.0.11` | ✅ 自動取得 |
| Docker Compose（本番サーバー） | `127.0.0.11` | ✅ 自動取得 |
| Kubernetes | `10.96.0.10`（CoreDNS 等） | ✅ 自動取得 |
| Podman / その他 | `/etc/resolv.conf` に準拠 | ✅ 自動取得 |

> **注意**: 本実装は `nginx:alpine` の公式テンプレート機能（`/etc/nginx/templates/`）と `.envsh` sourcing 機能を利用しています。他のサービスで nginx を使う場合も同じパターンで実装してください。

---

### 問題 0c: Docker ビルドが失敗する（build context エラー）

**エラー:**
```
ERROR: unable to prepare context: path not found
```

**解決方法:**
- 各サービスのディレクトリから実行する（リポジトリルートではなく）:
  ```bash
  # auth_service の場合
  cd shared/auth_service && docker compose up --build -d

  # DGE の場合
  cd apps/decision_governance_engine
  docker compose -f docker-compose.yml -f docker-compose.dev.yml up --build -d

  # market_trend_monitor の場合
  cd apps/market_trend_monitor && docker compose up --build -d
  ```

---

### 問題 1: Python のバージョンエラー

**エラー:**
```
Python 3.13+ required, but you have Python 3.10
```

**解決方法:**
1. [Python 公式サイト](https://www.python.org/downloads/)から Python 3.13+ をインストール
2. 仮想環境を再作成:
   ```bash
   rm -rf venv
   python3.13 -m venv venv
   source venv/bin/activate  # Windows: venv\Scripts\activate
   pip install -e ".[apps]"
   ```

### 問題 2: API キーが見つからない

**エラー:**
```
AgentFlowError: LLM API key not found
```

**解決方法:**
1. `.env` ファイルがプロジェクトルートにあるか確認
2. `.env` ファイルに正しい API キーが設定されているか確認
3. 環境変数が読み込まれているか確認:
   ```bash
   python -c "import os; print(os.getenv('OPENAI_API_KEY'))"
   ```

### 問題 3: ポートが既に使用されている

**エラー:**
```
Address already in use: 8000
```

**解決方法:**
1. 別のポートを指定:
   ```bash
   python -m uvicorn api:app --reload --port 8001
   ```
2. または、使用中のプロセスを終了:
   ```bash
   # Windows
   netstat -ano | findstr :8000
   taskkill /PID <PID番号> /F
   
   # macOS / Linux
   lsof -ti:8000 | xargs kill -9
   ```

### 問題 4: npm install が失敗する

**エラー:**
```
npm ERR! code ERESOLVE
```

**解決方法:**
1. `package-lock.json` を削除して再インストール:
   ```bash
   rm package-lock.json
   npm install
   ```
2. または、`--legacy-peer-deps` を使用:
   ```bash
   npm install --legacy-peer-deps
   ```

### 問題 5: フロントエンドがバックエンドに接続できない

**エラー:**
```
Failed to fetch: http://localhost:8000/api/...
```

**解決方法:**
1. バックエンドが起動しているか確認
2. CORS 設定を確認（`api.py` の `allow_origins`）
3. フロントエンドの API クライアント設定を確認:
   ```typescript
   // apps/decision_governance_engine/frontend/src/api/client.ts
   // baseUrl が正しいか確認
   ```

### 問題 6: モジュールが見つからない

**エラー:**
```
ModuleNotFoundError: No module named 'kernel'
```

**解決方法:**
1. 仮想環境が有効化されているか確認
2. BizCore 配布パッケージがインストールされているか確認:
   ```bash
   pip show bizcore
   ```
3. 再インストール:
   ```bash
   pip install -e ".[apps]"
   ```

---

## よくある質問（FAQ）

### Q1: どの LLM プロバイダーを使えばいいですか？

**A:** プロジェクトの要件に応じて選択してください：

| プロバイダー | 特徴 | 推奨用途 |
|------------|------|---------|
| **OpenAI** | 最も一般的、安定 | 汎用的な用途 |
| **Anthropic** | 高品質、長文対応 | 複雑な分析が必要な場合 |
| **Google** | コスト効率 | 大量処理が必要な場合 |

`.env` ファイルで API キーを設定すれば、自動的に検出されます。

### Q2: フロントエンドなしで使えますか？

**A:** はい、CLI モードで使用できます：

```bash
python -m apps.decision_governance_engine.main --interactive
```

### Q3: 本番環境でのデプロイ方法は？

**A:** 
1. **バックエンド**: Docker コンテナまたはサーバーレス（AWS Lambda、Vercel など）
2. **フロントエンド**: `npm run build` でビルド後、静的ホスティング（Vercel、Netlify など）

詳細は `control_plane/deploy/` モジュールを参照してください。

### Q4: 他のアプリ（market_trend_monitor など）も同じ手順ですか？

**A:** 基本的に同じです。各アプリの `README.md` を確認してください。

### Q5: 高性能な長期記憶（Memvid）を使うには？

**A:** Memvid は Rust 製の高性能 RAG ライブラリです。以下の手順でインストール：

```bash
# Memvid オプション依存をインストール
pip install "bizcore[memvid]"
```

インストール後、自動的に Memvid が使用されます。インストールされていない場合は、メモリ内実装（BM25 検索）にフォールバックします。

**使用例：**

```python
from shared.memory.knowledge import get_knowledge_manager

# 知識マネージャーを取得（環境に応じて自動選択）
manager = get_knowledge_manager()
await manager.start()

# 知識を追加
await manager.add(
    content="FAQ: よくある質問の回答",
    title="FAQ項目",
    tags=["faq", "support"]
)

# 知識を検索
results = await manager.query("質問内容")

# プロンプト用コンテキスト生成
context = manager.format_context(results)

await manager.stop()
```

---

## 次のステップ

インストールが完了したら：

1. **チュートリアル**: `docs/quickstart.md` を参照
2. **API ドキュメント**: `http://localhost:8000/docs` で確認
3. **サンプルコード**: `examples/` ディレクトリを参照
4. **開発ガイド**: `CONTRIBUTING.md` を参照

---

## サポート

問題が解決しない場合：

1. **GitHub Issues**: [Issues](https://github.com/liushuang393/serverlessAIAgents/issues) で報告
2. **ドキュメント**: `docs/` ディレクトリを確認
3. **ログ確認**: `DEBUG=true` を設定して詳細ログを確認

---

**最終更新**: 2026-03-20
**バージョン**: 1.1.0

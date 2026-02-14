# AgentFlow Platform (`apps/platform`)

## 概要

`apps/platform` は、AgentFlow の「統合プラットフォーム層」を提供するアプリです。
**バックエンド（Python / FastAPI）** と **フロントエンド（React / Vite）** の 2 層で構成されます。

主要機能:

| # | 機能 | 説明 |
|---|------|------|
| 1 | **Apps** | `apps/` 配下の全 App を自動検出・管理・ヘルスチェック |
| 2 | **Agents** | 全 App 横断の Agent 一覧・能力検索・統計 |
| 3 | **Skills** | スキルカタログの検索・タグ管理・統計 |
| 4 | **RAG** | RAG 機能概要・チャンキング戦略・リランカー一覧 |
| 5 | **Gallery** | ローカルコンポーネント + Marketplace の検索/発見 |
| 6 | **Components** | 共通コンポーネントの CRUD・依存可視化 |
| 7 | **Publish** | Validate → CodeGen → Deploy → Register の一括発布 |
| 8 | **Dashboard** | テナント単位の利用統計/人気コンポーネント/アクティビティ |

主要エントリポイント:

- バックエンド API: `apps/platform/main.py`
- Engine 統合層: `apps/platform/engine.py`
- Router 層: `apps/platform/routers/`
- Service 層: `apps/platform/services/`
- フロントエンド: `apps/platform/frontend/`

---

## 前提条件

| 項目 | 要件 | 確認済みバージョン |
|------|------|-------------------|
| **conda 環境** | `conda activate agentflow` | miniconda3 |
| Python | 3.13+ | 3.13.11 |
| Node.js | 22+ LTS | 22.21.0 |
| npm | 11+ | 11.8.0 |
| Docker | 29+ | 29.2.0 |
| Docker Compose | v5+ | v5.0.2 |
| FastAPI | 0.123+ | 0.123.10 |
| Pydantic | 2.12+ | 2.12.5 |
| Uvicorn | 0.40+ | 0.40.0 |
| httpx | 0.28+ | 0.28.1 |

---

## クイックスタート（ローカル起動）

### 手順 1: リポジトリルートへ移動 & conda 環境有効化

```bash
cd /path/to/serverlessAIAgents
conda activate agentflow
```

### 手順 2: 依存インストール（初回のみ）

```bash
pip install --upgrade pip
pip install -e ".[apps,dev]"
```

### 手順 3: 環境変数の設定（任意）

```bash
cp .env.example .env
# .env を編集して OPENAI_API_KEY 等を設定
# 未設定でも LLM は mock にフォールバック可能
```

### 手順 4: バックエンド API サーバーを起動

```bash
# リポジトリルートで実行（conda agentflow 環境内）
# ローカル開発: ポート 8001
python -m apps.platform.main serve --port 8001

# 本番: ポート 8000
python -m apps.platform.main serve --host 0.0.0.0 --port 8000
```

起動後の確認 URL（ローカル開発時）:

| URL | 内容 |
|-----|------|
| http://localhost:8001/health | ヘルスチェック |
| http://localhost:8001/docs | Swagger UI |
| http://localhost:8001/redoc | ReDoc |

### 手順 5: フロントエンド開発サーバーを起動

**別のターミナル**を開いて以下を実行:

```bash
cd apps/platform/frontend
npm install      # 初回のみ
npm run dev
```

起動後の確認 URL:

| URL | 内容 |
|-----|------|
| http://localhost:3000 | フロントエンド画面 |

> **補足**: フロントエンドの Vite 開発サーバーは `/api` へのリクエストを
> バックエンドにプロキシします（`vite.config.ts` で設定済み）。
> そのため、**バックエンドを先に起動**してからフロントエンドを起動してください。

---

## フロントエンド詳細

### 技術スタック

| 項目 | 技術 |
|------|------|
| フレームワーク | React 18 |
| ビルドツール | Vite 6 |
| 言語 | TypeScript 5 |
| 状態管理 | Zustand |
| HTTP クライアント | Axios |
| ルーティング | React Router v6 |
| CSS | Tailwind CSS 3 |

### 利用可能なコマンド

```bash
cd apps/platform/frontend

npm run dev          # 開発サーバー起動（http://localhost:3000）
npm run build        # 本番ビルド（dist/ に出力）
npm run preview      # ビルド結果のプレビュー
npm run test         # テスト実行（Vitest）
npm run lint         # ESLint チェック
npm run type-check   # TypeScript 型チェック
```

### 画面構成

| 画面 | コンポーネント | 説明 |
|------|---------------|------|
| App 一覧 | `AppList.tsx` | 検出された全 App の一覧表示 |
| App 詳細 | `AppDetail.tsx` | 個別 App の設定・契約・ヘルス |
| Agent ブラウザ | `AgentBrowser.tsx` | 全 Agent の横断検索 |
| Skill カタログ | `SkillCatalog.tsx` | スキル一覧・タグ検索 |
| RAG 概要 | `RAGOverview.tsx` | RAG 機能の統計・戦略一覧 |
| ダッシュボード | `Dashboard.tsx` | テナント統計・アクティビティ |
| 設定 | `Settings.tsx` | プラットフォーム設定 |

---

## API 一覧

### 1. Apps（App 管理）

| メソッド | パス | 説明 |
|---------|------|------|
| `GET` | `/api/apps` | 全 App 一覧 |
| `GET` | `/api/apps/summary` | App 概要統計 |
| `POST` | `/api/apps/refresh` | App 一覧再スキャン |
| `GET` | `/api/apps/{app_name}` | App 詳細 |
| `GET` | `/api/apps/{app_name}/config` | app_config.json 取得 |
| `PATCH` | `/api/apps/{app_name}/config` | app_config.json 部分更新 |
| `GET` | `/api/apps/{app_name}/contracts` | 契約設定取得 |
| `PATCH` | `/api/apps/{app_name}/contracts` | 契約設定部分更新 |
| `GET` | `/api/apps/{app_name}/health` | ヘルスチェック |
| `POST` | `/api/apps/{app_name}/publish` | App 発布（`docker compose up -d --build`） |
| `POST` | `/api/apps/{app_name}/start` | App 起動（`docker compose up -d`） |
| `POST` | `/api/apps/{app_name}/stop` | App 停止（`docker compose down`） |

### 2. Agents（Agent 管理）

| メソッド | パス | 説明 |
|---------|------|------|
| `GET` | `/api/agents` | 全 App 横断の Agent 一覧 |
| `GET` | `/api/agents/stats` | Agent 統計 |
| `GET` | `/api/agents/capabilities` | 全能力タグ一覧 |
| `GET` | `/api/agents/by-app` | App 別グルーピング |
| `GET` | `/api/agents/search` | 能力ベース検索 |

### 3. Skills（スキルカタログ）

| メソッド | パス | 説明 |
|---------|------|------|
| `GET` | `/api/skills` | 全スキル一覧 |
| `GET` | `/api/skills/stats` | スキル統計 |
| `GET` | `/api/skills/tags` | 全タグ一覧 |
| `GET` | `/api/skills/search` | タグベース検索 |
| `GET` | `/api/skills/{skill_name}` | スキル詳細 |

### 4. RAG（検索拡張生成）

| メソッド | パス | 説明 |
|---------|------|------|
| `GET` | `/api/rag/overview` | RAG 機能概要 |
| `GET` | `/api/rag/strategies` | チャンキング戦略一覧 |
| `GET` | `/api/rag/rerankers` | リランカー一覧 |
| `GET` | `/api/rag/retrieval-methods` | 取得方式一覧（vector / hybrid 等） |
| `GET` | `/api/rag/patterns` | 推奨 RAG パターン一覧 |
| `GET` | `/api/rag/apps` | RAG 使用 App 一覧 |
| `GET` | `/api/rag/apps/configs` | 全 App の RAG 設定一覧 |
| `GET` | `/api/rag/apps/{app_name}/config` | App 単位 RAG 設定取得 |
| `PATCH` | `/api/rag/apps/{app_name}/config` | App 単位 RAG 設定更新 |
| `GET` | `/api/rag/stats` | RAG 統計 |

`PATCH /api/rag/apps/{app_name}/config` では、以下を一括管理できます。

- `vector_provider` / `vector_url` / `vector_collection`
- `chunk_strategy` / `chunk_size` / `chunk_overlap`
- `retrieval_method` / `reranker` / `top_k` / `score_threshold`
- `data_sources`（取得元 URL・パス等）
- `pattern`（推奨設定テンプレート）

### 5. Gallery（コンポーネント検索）

| メソッド | パス | 説明 |
|---------|------|------|
| `GET` | `/api/gallery/search` | 検索 |
| `GET` | `/api/gallery/featured` | 推薦リスト |
| `GET` | `/api/gallery/{item_id}` | アイテム詳細 |
| `POST` | `/api/gallery/install` | Marketplace からインストール |

### 6. Components（コンポーネント CRUD）

| メソッド | パス | 説明 |
|---------|------|------|
| `GET` | `/api/components` | 一覧 |
| `POST` | `/api/components` | 作成 |
| `GET` | `/api/components/{component_id}` | 詳細 |
| `PUT` | `/api/components/{component_id}` | 更新 |
| `DELETE` | `/api/components/{component_id}` | 削除 |
| `GET` | `/api/components/{component_id}/dependencies` | 依存関係取得 |

### 7. Publish（一括発布）

| メソッド | パス | 説明 |
|---------|------|------|
| `POST` | `/api/publish/deploy` | 非同期発布開始 |
| `POST` | `/api/publish/deploy/sync` | 同期発布実行 |
| `GET` | `/api/publish/{publish_id}` | 発布状態取得 |
| `POST` | `/api/publish/{publish_id}/cancel` | 発布キャンセル |
| `GET` | `/api/publish/stream/{publish_id}` | SSE ストリーム |

発布ターゲット: `docker` / `vercel` / `aws_lambda` / `github_actions` / `local` / `gallery`

### 8. Dashboard（テナント統計）

| メソッド | パス | 説明 |
|---------|------|------|
| `GET` | `/api/dashboard/{tenant_id}` | サマリー |
| `GET` | `/api/dashboard/{tenant_id}/stats` | 統計 |
| `GET` | `/api/dashboard/{tenant_id}/top-components` | 人気コンポーネント |
| `GET` | `/api/dashboard/{tenant_id}/activities` | 最近のアクティビティ |
| `GET` | `/api/dashboard/{tenant_id}/trends` | 使用傾向 |

---

## CLI 利用

```bash
# Gallery 検索
python3 -m apps.platform.main search "PDF"

# Components 一覧
python3 -m apps.platform.main components list --limit 20

# 発布（例: local ターゲット）
python3 -m apps.platform.main publish ./my-agent --target local

# ダッシュボード表示
python3 -m apps.platform.main dashboard tenant-001

# 詳細ログ付きで起動
python3 -m apps.platform.main -v serve --port 8000
```

---

## テスト手順

### バックエンド

```bash
PYTHONDONTWRITEBYTECODE=1 \
python3 -m pytest -q \
  tests/apps/platform/test_gallery_agent.py \
  tests/unit/test_platform.py \
  -p no:cacheprovider
```

### フロントエンド

```bash
cd apps/platform/frontend
npm run test
npm run lint
npm run type-check
```

> **補足**: `ModuleNotFoundError: pydantic` / `No module named pytest` が出る場合、
> 先に `pip install -e ".[apps,dev]"` を実行してください。

---

## Claude Code CLI 連携（日本語運用手順）

`apps/platform` の前後端を最終品質確認するために、Claude Code CLI を使ったレビュー手順を用意します。

### 1. インストール / 認証

```bash
# Claude Code CLI のインストール（未導入時）
claude install stable

# 認証
claude auth login

# 確認
claude --version
```

### 2. プロジェクトでの設定

```bash
cd /path/to/serverlessAIAgents

# 必要に応じて許可ディレクトリを追加
claude --add-dir apps/platform
```

### 3. 非対話レビュー（推奨）

```bash
# バックエンド + フロントエンドのコードレビュー
claude -p --tools Read "apps/platform の実装レビューを行ってください。対象: routers/apps.py, services/app_lifecycle.py, frontend/src/components/AppList.tsx, frontend/src/components/AppDetail.tsx。重大度順に問題点を列挙。"

# App 追加後の起動・導線確認レビュー
claude -p --tools Read "AppCreateModal と App 詳細画面の導線をレビューし、追加直後に実行可能な運用手順を提案してください。"
```

### 4. 環境構築 / 起動 / テストの最終チェック

```bash
# バックエンド起動
python3 -m apps.platform.main serve --host 0.0.0.0 --port 8000

# フロントエンド起動
cd apps/platform/frontend && npm run dev

# テスト実行（例）
cd /path/to/serverlessAIAgents
python3 -m pytest -q tests/apps/platform -p no:cacheprovider
```

### 5. 注意点

- TTY がない CI/自動実行環境では、`claude -p` の非対話モードを使用してください。
- 端末環境によっては raw mode 制約で対話コマンドが失敗するため、その場合は `-p` を優先してください。

---

## 技術アーキテクチャ

```
┌─────────────────────────────────────────────────────┐
│  Frontend (React / Vite)  — http://localhost:3000    │
│    /api/* → proxy → Backend                         │
└──────────────────────┬──────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────┐
│  Backend (FastAPI / Uvicorn) — http://localhost:8000  │
│                                                      │
│  Routers:                                            │
│    apps / agents / skills / rag                      │
│    gallery / components / publish / dashboard         │
│                                                      │
│  Services:                                           │
│    AppDiscoveryService / AppLifecycleManager          │
│    AgentAggregatorService / SkillCatalogService       │
│    RAGOverviewService / GalleryService                │
│    ComponentLibrary / PublishOrchestrator              │
│    TenantDashboard                                   │
│                                                      │
│  Engine:                                             │
│    PlatformEngine (統合層)                             │
└──────────────────────────────────────────────────────┘
```

---

## フォルダ構造

```
apps/platform/
├── __init__.py                        # 外部公開 API エクスポート
├── main.py                            # CLI + FastAPI エントリ
├── engine.py                          # Gallery/Publish/Dashboard 統合エンジン
├── app_config.json                    # Platform 自身の設定
├── agents/
│   ├── gallery_agent.py               # 自然言語検索 Agent
│   ├── publish_agent.py               # 発布支援 Agent
│   └── analytics_agent.py             # 分析/提案 Agent
├── routers/
│   ├── apps.py                        # App 管理 API
│   ├── agents.py                      # Agent 管理 API
│   ├── skills.py                      # Skill カタログ API
│   ├── rag.py                         # RAG 概要 API
│   ├── gallery.py                     # Gallery API
│   ├── components.py                  # Components API
│   ├── publish.py                     # Publish API
│   └── dashboard.py                   # Dashboard API
├── services/
│   ├── app_discovery.py               # App 自動検出
│   ├── app_lifecycle.py               # App ライフサイクル管理
│   ├── agent_aggregator.py            # Agent 横断集約
│   ├── skill_catalog.py               # スキルカタログ
│   ├── rag_overview.py                # RAG 概要集約
│   ├── component_library.py           # コンポーネント管理
│   ├── gallery_service.py             # 検索/推薦/インストール
│   ├── publish_orchestrator.py        # 発布オーケストレーション
│   └── tenant_dashboard.py            # テナント統計
├── schemas/
│   ├── app_config_schemas.py          # App Config Pydantic モデル
│   ├── gallery_schemas.py             # Gallery Pydantic モデル
│   ├── component_schemas.py           # Component Pydantic モデル
│   └── publish_schemas.py             # Publish Pydantic モデル
├── docs/                              # 設計ドキュメント
│   ├── api-design.md
│   ├── app-config-schema.md
│   ├── architecture.md
│   ├── development-workflow.md
│   └── renovation-plan.md
└── frontend/                          # React フロントエンド
    ├── package.json
    ├── tsconfig.json
    ├── vite.config.ts                 # Vite 設定（プロキシ含む）
    ├── tailwind.config.js
    ├── postcss.config.js
    ├── index.html                     # SPA エントリ
    ├── public/                        # 静的アセット
    └── src/
        ├── main.tsx                   # React エントリ
        ├── App.tsx                    # ルーティング定義
        ├── index.css                  # グローバル CSS
        ├── api/
        │   └── client.ts             # API クライアント（Axios）
        ├── components/
        │   ├── Layout.tsx             # 共通レイアウト
        │   ├── AppList.tsx            # App 一覧
        │   ├── AppDetail.tsx          # App 詳細
        │   ├── AppHealthBadge.tsx     # ヘルスバッジ
        │   ├── AgentBrowser.tsx       # Agent ブラウザ
        │   ├── SkillCatalog.tsx       # Skill カタログ
        │   ├── RAGOverview.tsx        # RAG 概要
        │   ├── Dashboard.tsx          # ダッシュボード
        │   └── Settings.tsx           # 設定画面
        ├── store/
        │   └── useAppStore.ts         # Zustand ストア
        └── types/
            └── index.ts              # 型定義
```

---

## 本番環境構築

### 1. 本番依存インストール

```bash
cd /path/to/serverlessAIAgents
python3 -m venv .venv
source .venv/bin/activate
pip install --upgrade pip
pip install -e ".[apps]"
```

### 2. フロントエンドビルド

```bash
cd apps/platform/frontend
npm install
npm run build
# dist/ に静的ファイルが生成される
```

### 3. 本番用環境変数

最低限:

- `LOG_LEVEL=INFO`
- 必要に応じて `OPENAI_API_KEY` 等

発布ターゲット使用時の追加例:

- Docker: レジストリ認証情報
- Vercel: Token
- AWS Lambda: Access Key / Secret Key / Role ARN

### 4. 運用前チェック

```bash
python3 -m pytest -q tests/apps/platform/test_gallery_agent.py tests/unit/test_platform.py
```

---

## 本番環境起動手順

### 推奨: Uvicorn マルチワーカー

```bash
uvicorn apps.platform.main:create_app \
  --factory \
  --host 0.0.0.0 \
  --port 8000 \
  --workers 4
```

### systemd 化（任意）

`ExecStart` には上記 `uvicorn` コマンドを設定し、`Restart=always` を推奨します。

フロントエンドの `dist/` は Nginx 等のリバースプロキシで配信し、`/api` を Uvicorn にプロキシしてください。

---

## 注意事項

1. 現在の保存層はデフォルトでインメモリのため、再起動時にデータが消えます。
2. マルチテナント可視性判定は `TenantContext` 前提です。API 層でのテナント設定ミドルウェアは未実装です。
3. `publish --target docker` では、`source_path` 側に `Dockerfile` が必要です。
4. フロントエンド開発サーバー（Vite）は開発専用です。本番では `npm run build` で生成した `dist/` を配信してください。
5. Marketplace クライアントは現状モックデータ実装です。

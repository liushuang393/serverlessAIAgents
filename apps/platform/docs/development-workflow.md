# 新規 App 開発ワークフロー設計

> **最終更新**: 2026-02-14
> **対象**: Platform から新規 App を追加する際の統一手順

---

## 1. 概要

新規 App 作成時、以下の 7 ステップを統一フローとして実行する。
Platform UI（Phase 2）完成後はウィザード形式で自動化される。

```mermaid
flowchart LR
    S1["① 計画"] --> S2["② テンプレート生成"]
    S2 --> S3["③ ポート割り当て"]
    S3 --> S4["④ app_config.json 作成"]
    S4 --> S5["⑤ 開発・実装"]
    S5 --> S6["⑥ Platform 登録確認"]
    S6 --> S7["⑦ デプロイ"]
```

---

## 2. 各ステップ詳細

### ① 計画

App の基本情報を決定する。

| 項目 | 例 | 備考 |
|------|-----|------|
| App 名 | `inventory_manager` | snake_case 必須 |
| 表示名 | `在庫管理システム` | UI 表示用 |
| Agent 数 | 3 | 初期見込み |
| DB 種別 | `postgresql` | postgresql / sqlite / none |
| Redis | `true` | キャッシュ利用有無 |
| Frontend | `true` | React UI 有無 |

### ② テンプレート生成

既存ツール `TemplateManager` を使用:

```bash
# 対話モードで基盤生成
python -m agentflow.cli template generate fullstack-app \
  apps/inventory_manager -i

# パラメータ指定モード
python -m agentflow.cli template generate fullstack-app \
  apps/inventory_manager \
  -p app_name=inventory_manager \
  -p app_title="在庫管理システム" \
  -p db_name=inventory_db \
  -p redis_enabled=true \
  -p frontend_enabled=true
```

**生成物**: `docker-compose.yml`, `api.py`, `config/`, `db/`, `routers/`, `services/`, `schemas/`, `tests/`, `frontend/`

### ③ ポート割り当て

既存ツール `PortManager` を使用:

```bash
python -m agentflow.tools.port_manager inventory_manager \
  apps/inventory_manager
```

**自動検出対象**:
- システム使用中ポート（socket テスト）
- Docker コンテナ使用中ポート
- 既存 `apps/*/.env` の設定ポート
- `apps/*/docker-compose.yml` のポート

**生成物**: `.env`, `.env.example`（DB/Redis/API/Frontend のポート設定）

### ④ app_config.json 作成

Platform が認識するためのマニフェストファイルを配置:

```bash
# 自動生成（将来の Platform UI 機能）
# 現時点では手動作成
cat > apps/inventory_manager/app_config.json << 'EOF'
{
  "name": "inventory_manager",
  "display_name": "在庫管理システム",
  "version": "1.0.0",
  "icon": "📦",
  "ports": { "api": 8006, "frontend": 3006, "db": 5438 },
  "entry_points": {
    "api_module": "apps.inventory_manager.api:app",
    "health": "/health"
  },
  "agents": [],
  "dependencies": { "database": "postgresql", "redis": true },
  "tags": ["inventory", "warehouse"]
}
EOF
```

### ⑤ 開発・実装

Agent / Service / Router を実装する。

```mermaid
flowchart TB
    subgraph Dev["開発フェーズ"]
        A["Agent 実装"] --> B["Service 層"]
        B --> C["Router 定義"]
        C --> D["Schema 定義"]
        D --> E["テスト作成"]
    end
    E --> F["docker-compose up -d"]
    F --> G["pytest 実行"]
```

### ⑥ Platform 登録確認

Platform API で App が正しく認識されることを確認:

```bash
# App 一覧を再スキャン
curl -X POST http://localhost:8000/api/apps/refresh

# 新規 App が表示されることを確認
curl http://localhost:8000/api/apps/inventory_manager

# ヘルスチェック
curl http://localhost:8000/api/apps/inventory_manager/health
```

### ⑦ デプロイ

既存の `PublishOrchestrator` または手動デプロイ:

```bash
# Docker デプロイ
cd apps/inventory_manager && docker-compose up -d

# Vercel (フロントエンドのみ)
cd apps/inventory_manager/frontend && vercel deploy
```

---

## 3. ツール対応表

```mermaid
flowchart LR
    subgraph Existing["既存ツール（青）"]
        TM["TemplateManager"]
        PM["PortManager"]
        PO["PublishOrchestrator"]
        CLI["agentflow CLI"]
    end
    subgraph New["新規開発（赤）"]
        AC["app_config.json 自動生成"]
        AW["App Wizard UI"]
        AR["Platform 自動登録"]
    end
    TM --> AC
    PM --> AC
    AC --> AR
    AW --> TM
    AW --> PM
    AW --> AC
```

| ステップ | 使用ツール | 状態 |
|----------|-----------|------|
| ② テンプレート生成 | `TemplateManager` + CLI | ✅ 既存 |
| ③ ポート割り当て | `PortManager` | ✅ 既存 |
| ④ app_config.json | 手動 → 将来自動生成 | 🔴 新規 |
| ⑥ Platform 確認 | `AppDiscoveryService` | 🔴 新規 |
| ⑦ デプロイ | `PublishOrchestrator` | ✅ 既存 |

---

## 4. 将来の自動化（Phase 2 以降）

Platform UI に「New App」ウィザードを実装し、①〜④ を GUI 操作で完結させる:

1. フォーム入力 → App 名、Agent 構成、DB/Redis 設定
2. TemplateManager で基盤生成
3. PortManager でポート自動割り当て
4. `app_config.json` 自動生成
5. Platform に即座に反映


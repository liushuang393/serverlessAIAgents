# ポート管理ガイド

AgentFlow の Apps は Docker コンテナで DB/Redis/API を起動するため、複数の App を同時に開発する際にポート衝突が発生します。
このガイドでは、ポート管理ツールを使って自動的に衝突を回避する方法を説明します。

## 概要

### 問題

```
App A: PostgreSQL 5432, API 8000
App B: PostgreSQL 5432 ← 衝突！
```

### 解決策

ポート管理ツールが自動的に空きポートを検出し、`.env` ファイルを生成します。

```
App A: PostgreSQL 5432, API 8000
App B: PostgreSQL 5433, API 8001 ← 自動割り当て
```

## クイックスタート

```bash
# 新規 App のポートを自動設定
python -m agentflow.tools.port_manager my_app apps/my_app

# 出力:
# ✓ ポート割り当て完了: my_app
#   DB Main:    5435
#   DB History: 5436
#   Redis:      6381
#   API:        8001
#   Frontend:   3001
#   .env:       apps/my_app/.env
```

## ポート管理ツール

### 機能

| 機能 | 説明 |
|------|------|
| システムポート検出 | `socket` でローカル使用中ポートを検出 |
| Docker ポート検出 | `docker ps` でコンテナ使用ポートを検出 |
| Apps 設定検出 | `apps/*/` の `.env` / `docker-compose.yml` をパース |
| 空きポート割り当て | 開始ポートから順番に空きを探索 |
| .env 生成 | 自動的に `.env` と `.env.example` を作成 |

### ポート範囲

| 種類 | 開始 | 終了 | 説明 |
|------|------|------|------|
| DB | 5432 | 5500 | PostgreSQL |
| Redis | 6379 | 6400 | キャッシュ |
| API | 8000 | 8100 | FastAPI |
| Frontend | 3000 | 3100 | React/Next.js |

### 検出ロジック

```
1. システム使用中ポート（socket.bind テスト）
2. Docker コンテナ使用中ポート（docker ps）
3. apps/*/.env の設定ポート
4. apps/*/docker-compose.yml の設定ポート
   ↓
5. 上記を除外して空きポートを採番
```

## 生成される .env ファイル

```bash
# 環境識別
APP_ENV=development

# ホスト設定
HOST=localhost

# ポート設定
DB_MAIN_PORT=5435
DB_HISTORY_PORT=5436
REDIS_PORT=6381
API_PORT=8001
FRONTEND_PORT=3001

# 接続 URL
DATABASE_URL=postgresql+asyncpg://user:pass@${HOST}:${DB_MAIN_PORT}/dbname
REDIS_URL=redis://${HOST}:${REDIS_PORT}/0
VITE_API_URL=http://${HOST}:${API_PORT}
```

## docker-compose.yml での使用

```yaml
services:
  postgres-main:
    ports:
      - "${DB_MAIN_PORT:-5432}:5432"  # .env から読み込み

  api:
    ports:
      - "${API_PORT:-8000}:8000"
    environment:
      HOST: ${HOST:-localhost}
```

## ホスト設定（環境統一）

| 環境 | HOST 設定 | 備考 |
|------|-----------|------|
| 開発（ローカル） | `localhost` | 127.0.0.1 も可 |
| Docker 内から | `host.docker.internal` | ホスト側サービスへ接続 |
| 本番 | 実際の IP/ドメイン | 外部公開 |

### フロントエンドからの API 呼び出し

```javascript
// .env から読み込み（Vite）
const API_URL = import.meta.env.VITE_API_URL;

// .env から読み込み（Next.js）
const API_URL = process.env.NEXT_PUBLIC_API_URL;

// fetch 例
const response = await fetch(`${API_URL}/api/users`);
```

## Python API

```python
from agentflow.tools import PortManager, setup_app_ports

# 簡易 API
allocation = setup_app_ports("my_app", "apps/my_app")
print(f"API Port: {allocation.api_port}")

# 詳細 API
manager = PortManager()

# 使用中ポート確認
system_ports = manager.get_system_used_ports()
docker_ports = manager.get_docker_used_ports()
apps_ports = manager.get_apps_used_ports()

# 空きポート検索
allocation = manager.allocate_ports_for_app("new_app")

# .env 生成
manager.generate_env_file(
    app_dir=Path("apps/new_app"),
    app_name="new_app",
    allocation=allocation,
    host="localhost",
    db_user="app_user",
    db_password="secure_password",
)
```

## トラブルシューティング

### ポートが既に使用中

```bash
# 使用中ポートを確認
lsof -i :5432
netstat -tulpn | grep 5432

# Docker コンテナを確認
docker ps --format "{{.Names}}: {{.Ports}}"
```

### .env が読み込まれない

```bash
# docker-compose で .env を明示的に指定
docker-compose --env-file .env up -d
```

## 関連ドキュメント

- [app-builder Skill](../agentflow/skills/builtin/app-builder/SKILL.md) - App 構築手順
- [db-migration Skill](../agentflow/skills/builtin/db-migration/SKILL.md) - Alembic マイグレーション
- [app-development-flow Skill](../agentflow/skills/builtin/app-development-flow/SKILL.md) - 開発フロー


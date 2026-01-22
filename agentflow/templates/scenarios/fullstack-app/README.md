# {{ app_title }}

{{ app_description }}

## クイックスタート

### 1. コンテナ起動

```bash
# 全サービス起動
docker-compose up -d

# DB のみ起動（開発時）
docker-compose up -d postgres-main{% if redis_enabled %} redis{% endif %}
```

### 2. DB マイグレーション

```bash
# 初回マイグレーション適用
alembic upgrade head

# マイグレーション履歴確認
alembic history
```

### 3. アプリケーション起動

```bash
# 開発モード（ホットリロード）
uvicorn api:app --reload --port {{ api_port }}

# 本番モード
uvicorn api:app --host 0.0.0.0 --port {{ api_port }}
```

### 4. 動作確認

```bash
# ヘルスチェック
curl http://localhost:{{ api_port }}/health

# API 情報
curl http://localhost:{{ api_port }}/api/info
```

## プロジェクト構成

```
{{ app_name }}/
├── docker-compose.yml      # コンテナ定義
├── Dockerfile              # API コンテナ
├── alembic.ini             # DB マイグレーション設定
├── requirements.txt        # Python 依存パッケージ
├── api.py                  # FastAPI エントリポイント
├── config/                 # 設定管理
│   ├── __init__.py         # 設定読み込み
│   └── settings.yaml       # アプリ設定
├── db/                     # DB 関連
│   └── init/               # 初期 SQL
├── migrations/             # Alembic マイグレーション
│   ├── env.py
│   └── versions/
├── repositories/           # データアクセス層
│   ├── __init__.py
│   ├── database.py         # DB 接続
│   └── models.py           # SQLAlchemy モデル
├── routers/                # API ルーター
│   └── __init__.py
├── schemas/                # Pydantic スキーマ
│   └── __init__.py
├── services/               # ビジネスロジック
│   └── __init__.py
└── tests/                  # テスト
    └── unit/
```

## 開発フロー

### 1. 基盤做成 → 2. 式様読取 → 3. 設計 → 4. 実装 → 5. テスト → ループ

詳細は [app-development-flow Skill](../../agentflow/skills/builtin/app-development-flow/SKILL.md) を参照。

## DB マイグレーション

```bash
# 新規マイグレーション作成
alembic revision --autogenerate -m "説明"

# マイグレーション適用
alembic upgrade head

# ロールバック
alembic downgrade -1
```

詳細は [db-migration Skill](../../agentflow/skills/builtin/db-migration/SKILL.md) を参照。

## 環境変数

| 変数名 | 説明 | デフォルト |
|--------|------|-----------|
| `DATABASE_URL` | PostgreSQL 接続 URL | localhost:{{ db_port }} |
{% if redis_enabled %}| `REDIS_URL` | Redis 接続 URL | localhost:6379 |{% endif %}
| `APP_ENV` | 環境 (development/staging/production) | development |

## テスト実行

```bash
# 単体テスト
pytest tests/unit/ -v

# カバレッジ付き
pytest --cov=. --cov-report=html
```

## ライセンス

MIT License


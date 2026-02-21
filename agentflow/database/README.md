# agentflow.database — 統一データベース管理モジュール

## 1. 概要

`agentflow.database` は、AgentFlow フレームワークの **統一データベース管理層** です。
各アプリで重複していたセッション管理・Alembic マイグレーション環境構築を一元化し、
アプリ側は **設定のみ** で DB 機能を利用できます。

### 解決する課題

| 課題 | Before | After |
|------|--------|-------|
| セッション管理の重複 | 各アプリ 150-220 行 | フレームワークに委譲、アプリ側 30 行 |
| migrations/env.py の重複 | 各アプリ 100-180 行 | `MigrationEnv` 呼び出し 30 行 |
| sync/async ドライバ切替 | 各アプリで個別実装 | `url_utils` で自動変換 |
| 既存 DB の Alembic 統合 | 手動 stamp 必要 | auto-stamp で自動対応 |

### 対応データベース

| DB | 同期ドライバ | 非同期ドライバ | テスト済み |
|----|-------------|---------------|-----------|
| SQLite | `sqlite` | `sqlite+aiosqlite` | ✅ |
| PostgreSQL | `postgresql+psycopg2` | `postgresql+asyncpg` | ✅ |
| MySQL | `mysql+pymysql` | `mysql+aiomysql` | — |

---

## 2. アーキテクチャ

```
agentflow/database/
├── __init__.py      # 公開 API エクスポート
├── config.py        # DatabaseConfig (Pydantic 設定モデル)
├── url_utils.py     # URL 変換ユーティリティ
├── session.py       # DatabaseManager (エンジン/セッション管理)
├── migration.py     # MigrationEnv (Alembic 統合)
└── README.md        # 本ドキュメント
```

### 依存関係

```
DatabaseConfig ──→ DatabaseManager ──→ アプリ session.py
     │                                      │
url_utils ────→ MigrationEnv ──────→ アプリ migrations/env.py
```

---

## 3. 使用方法

### 3.1 セッション管理 (DatabaseManager)

```python
from agentflow.database import DatabaseConfig, DatabaseManager
from my_app.models import Base

# 設定 + マネージャ作成
_db = DatabaseManager(
    config=DatabaseConfig(
        url="sqlite+aiosqlite:///./app.db",
        url_env_key="MY_APP_DATABASE_URL",  # 環境変数で上書き可能
        echo_env_key="MY_APP_DB_ECHO",
    ),
    metadata=Base.metadata,
)

# 初期化
async def init_db() -> None:
    await _db.init()

# セッション取得
async def get_session():
    async with _db.session() as session:
        yield session

# テーブル作成 (開発用)
async def create_tables() -> None:
    await _db.create_all_tables()

# クローズ
async def close_db() -> None:
    await _db.close()
```

### 3.2 Alembic マイグレーション (MigrationEnv)

アプリの `migrations/env.py` を以下に置き換えます:

```python
"""Alembic マイグレーション環境."""
from logging.config import fileConfig
from alembic import context
from agentflow.database import MigrationEnv
from my_app.models import Base

# Alembic ロギング設定
if context.config.config_file_name:
    fileConfig(context.config.config_file_name)

MigrationEnv(
    target_metadata=Base.metadata,
    db_url_env="MY_APP_DATABASE_URL",
    initial_revision="0001",
    initial_tables=frozenset({"users", "posts", "comments"}),
).run()
```

### 3.3 SQLite 同期モード

SQLite で同期モードが必要な場合:

```python
_db = DatabaseManager(
    config=DatabaseConfig(url="sqlite:///./app.db"),
    metadata=Base.metadata,
    force_sync=True,  # 同期モード強制
)

# 同期セッション取得
await _db.init()
session = _db.session_sync()
```

---

## 4. API リファレンス

### DatabaseConfig

| フィールド | 型 | デフォルト | 説明 |
|-----------|-----|----------|------|
| `url` | `str` | `sqlite+aiosqlite:///./app.db` | 接続 URL |
| `url_env_key` | `str` | `DATABASE_URL` | URL 上書き環境変数名 |
| `echo` | `bool` | `False` | SQL ログ出力 |
| `echo_env_key` | `str` | `""` | echo 上書き環境変数名 |
| `pool_size` | `int` | `5` | コネクションプールサイズ |
| `max_overflow` | `int` | `10` | プール最大オーバーフロー数 |
| `pool_pre_ping` | `bool` | `True` | 接続前ヘルスチェック |
| `connect_args` | `dict` | `{}` | ドライバ固有の接続引数 |

### DatabaseManager

| メソッド | 説明 |
|---------|------|
| `async init()` | エンジン/セッションファクトリを初期化 |
| `async session()` | 非同期セッション (コンテキストマネージャ) |
| `session_sync()` | 同期セッション (force_sync 時のみ) |
| `async create_all_tables()` | テーブル直接作成 (開発用) |
| `async close()` | 全接続をクローズ |
| `is_sync_mode` | 同期モードかどうか (プロパティ) |
| `is_initialized` | 初期化済みかどうか (プロパティ) |
| `resolved_url` | 解決済み URL (プロパティ) |

### MigrationEnv

| パラメータ | 型 | 必須 | 説明 |
|-----------|-----|------|------|
| `target_metadata` | `MetaData` | ✅ | SQLAlchemy Base.metadata |
| `db_url_env` | `str` | — | DB URL 環境変数名 (default: `DATABASE_URL`) |
| `db_url_default` | `str\|None` | — | デフォルト URL |
| `initial_revision` | `str\|None` | — | 初期リビジョン ID (auto-stamp 用) |
| `initial_tables` | `frozenset` | — | 初期マイグレーションのテーブル名集合 |
| `compare_type` | `bool` | — | autogenerate 時のカラム型比較 (default: `True`) |

### URL ユーティリティ

| 関数 | 説明 |
|------|------|
| `to_async_url(url)` | 同期 URL → 非同期ドライバ URL |
| `to_sync_url(url)` | 非同期 URL → 同期ドライバ URL |
| `is_sqlite(url)` | SQLite URL かどうか |
| `is_async_url(url)` | 非同期ドライバ URL かどうか |
| `get_dialect(url)` | DB ダイアレクト名を取得 |

---

## 5. Auto-Stamp 機構

`MigrationEnv` は既存 DB を自動検知し、Alembic 履歴がない場合に初期リビジョンを自動 stamp します。

### 動作フロー

| ケース | 動作 |
|--------|------|
| **新規 DB** (テーブルなし) | 全マイグレーションを実行 |
| **既存 DB** (`alembic_version` なし) | 初期リビジョンを auto-stamp → 差分のみ実行 |
| **通常 DB** (`alembic_version` あり) | 未適用マイグレーションのみ実行 |

### 設定要件

auto-stamp を有効にするには、`initial_revision` と `initial_tables` の両方を指定します:

```python
MigrationEnv(
    target_metadata=Base.metadata,
    initial_revision="20260213_0001",     # 初期マイグレーションのリビジョン ID
    initial_tables=frozenset({            # 初期マイグレーションで作成されるテーブル
        "users", "posts",
    }),
)
```

---

## 6. 新規アプリへの適用手順

1. **モデル定義**: `models.py` で `Base = declarative_base()` とモデルを定義
2. **session.py**: `DatabaseManager` を使用してセッション管理を実装 (3.1 参照)
3. **alembic init**: `alembic init migrations` で Alembic を初期化
4. **env.py**: `MigrationEnv` を使用して `migrations/env.py` を実装 (3.2 参照)
5. **初期マイグレーション**: `alembic revision --autogenerate -m "初期スキーマ"` で生成
6. **適用**: `alembic upgrade head` で適用

---

## 7. トラブルシューティング

| 症状 | 原因 | 対処 |
|------|------|------|
| `table already exists` | `create_all()` 済みの DB に初回マイグレーション実行 | `initial_revision` と `initial_tables` を設定して auto-stamp 有効化 |
| `Target database is not up to date` | 適用済みリビジョンが最新でない | `alembic upgrade head` を実行 |
| `Can't locate revision` | リビジョン ID の不一致 | `alembic history` で ID を確認 |
| 非同期セッションエラー | `force_sync=True` で非同期セッション取得 | `session_sync()` を使用するか `force_sync=False` に変更 |

---

## 8. 開発/テスト/ビルド（統一手順）

開発環境のセットアップと品質チェックはリポジトリルートの統一スクリプトを使用します。

```bash
cd <repo-root>
bash setup_dev.sh
./check.sh test
./check.sh type-check
```

本番ビルド（Python パッケージ）:

```bash
cd <repo-root>
make build
```

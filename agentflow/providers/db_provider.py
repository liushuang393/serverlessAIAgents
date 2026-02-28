"""DBProvider - 松耦合データベースアクセス.

Agent/サービスは具体的なDB実装を知る必要がありません。
環境変数から自動検出してDBインスタンスを提供します。

使用例:
    >>> from agentflow import get_db
    >>> db = get_db()  # 環境変数から自動検出
    >>> await db.connect()
    >>> users = await db.select("users", filters={"status": "active"})

環境変数優先順位:
    1. FAQ_DATABASE_URL / DATABASE_URL → SQLAlchemy (PostgreSQL/MySQL/SQLite/MSSQL)
    2. SUPABASE_URL + SUPABASE_KEY → Supabase
    3. TURSO_URL + TURSO_AUTH_TOKEN → Turso (未実装)
    4. なし → Mock
"""

import logging
import re
from typing import TYPE_CHECKING, Any, Protocol, cast, runtime_checkable
from urllib.parse import urlparse


if TYPE_CHECKING:
    from agentflow.runtime import RuntimeContext

logger = logging.getLogger(__name__)

# グローバルシングルトン
_db_instance: "DBProvider | None" = None


@runtime_checkable
class DBProvider(Protocol):
    """データベースプロバイダーの統一インターフェース.

    全てのDB実装はこのプロトコルに準拠する必要があります。
    """

    async def connect(self) -> None:
        """データベースに接続."""
        ...

    async def disconnect(self) -> None:
        """データベースから切断."""
        ...

    async def select(
        self,
        table: str,
        columns: list[str] | None = None,
        filters: dict[str, Any] | None = None,
        limit: int | None = None,
    ) -> list[dict[str, Any]]:
        """データを取得."""
        ...

    async def insert(
        self,
        table: str,
        data: dict[str, Any],
    ) -> dict[str, Any]:
        """データを挿入."""
        ...

    async def update(
        self,
        table: str,
        data: dict[str, Any],
        filters: dict[str, Any],
    ) -> list[dict[str, Any]]:
        """データを更新."""
        ...

    async def delete(
        self,
        table: str,
        filters: dict[str, Any],
    ) -> int:
        """データを削除."""
        ...

    async def execute(
        self,
        sql: str,
        params: list[Any] | None = None,
    ) -> Any:
        """生SQLを実行."""
        ...

    async def execute_raw(
        self,
        sql: str,
        params: list[Any] | None = None,
    ) -> list[dict[str, Any]]:
        """生SQLを実行し、辞書配列を返す."""
        ...

    def get_provider_name(self) -> str:
        """プロバイダー名を取得."""
        ...


class MockDBProvider:
    """Mock DB Provider（開発・テスト用）.

    API キーがない場合のフォールバック実装。
    """

    def __init__(self) -> None:
        """初期化."""
        self._connected = False
        self._data: dict[str, list[dict[str, Any]]] = {}

    async def connect(self) -> None:
        """接続."""
        self._connected = True

    async def disconnect(self) -> None:
        """切断."""
        self._connected = False

    async def select(
        self,
        table: str,
        columns: list[str] | None = None,
        filters: dict[str, Any] | None = None,
        limit: int | None = None,
    ) -> list[dict[str, Any]]:
        """Mock select."""
        rows = self._data.get(table, [])
        if filters:
            rows = [r for r in rows if all(r.get(k) == v for k, v in filters.items())]
        if limit:
            rows = rows[:limit]
        if columns:
            rows = [{k: r.get(k) for k in columns} for r in rows]
        return rows

    async def insert(self, table: str, data: dict[str, Any]) -> dict[str, Any]:
        """Mock insert."""
        if table not in self._data:
            self._data[table] = []
        data["id"] = len(self._data[table]) + 1
        self._data[table].append(data)
        return data

    async def update(self, table: str, data: dict[str, Any], filters: dict[str, Any]) -> list[dict[str, Any]]:
        """Mock update."""
        updated = []
        for row in self._data.get(table, []):
            if all(row.get(k) == v for k, v in filters.items()):
                row.update(data)
                updated.append(row)
        return updated

    async def delete(self, table: str, filters: dict[str, Any]) -> int:
        """Mock delete."""
        if table not in self._data:
            return 0
        original = len(self._data[table])
        self._data[table] = [r for r in self._data[table] if not all(r.get(k) == v for k, v in filters.items())]
        return original - len(self._data[table])

    async def execute(self, sql: str, params: list[Any] | None = None) -> Any:
        """Mock execute."""
        return None

    async def execute_raw(
        self,
        sql: str,
        params: list[Any] | None = None,
    ) -> list[dict[str, Any]]:
        """Mock execute_raw."""
        _ = (sql, params)
        return []

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        return "mock"


class SupabaseDBProvider:
    """Supabase DB Provider.

    Supabase の REST API を使用したデータベースアクセス。
    """

    def __init__(self, url: str, key: str) -> None:
        """初期化."""
        self._url = url
        self._key = key
        self._client: Any = None

    async def connect(self) -> None:
        """Supabase に接続."""
        try:
            from supabase import create_client

            self._client = create_client(self._url, self._key)
            logger.info(f"Connected to Supabase: {self._url[:30]}...")
        except ImportError:
            msg = "supabase package required: pip install supabase"
            raise ImportError(msg)

    async def disconnect(self) -> None:
        """切断."""
        self._client = None

    async def select(
        self,
        table: str,
        columns: list[str] | None = None,
        filters: dict[str, Any] | None = None,
        limit: int | None = None,
    ) -> list[dict[str, Any]]:
        """Select."""
        query = self._client.table(table).select(",".join(columns) if columns else "*")
        if filters:
            for k, v in filters.items():
                query = query.eq(k, v)
        if limit:
            query = query.limit(limit)
        return cast("list[dict[str, Any]]", query.execute().data)

    async def insert(self, table: str, data: dict[str, Any]) -> dict[str, Any]:
        """Insert."""
        result = self._client.table(table).insert(data).execute()
        return result.data[0] if result.data else {}

    async def update(self, table: str, data: dict[str, Any], filters: dict[str, Any]) -> list[dict[str, Any]]:
        """Update."""
        query = self._client.table(table).update(data)
        for k, v in filters.items():
            query = query.eq(k, v)
        return cast("list[dict[str, Any]]", query.execute().data)

    async def delete(self, table: str, filters: dict[str, Any]) -> int:
        """Delete."""
        query = self._client.table(table).delete()
        for k, v in filters.items():
            query = query.eq(k, v)
        result = query.execute()
        return len(result.data) if result.data else 0

    async def execute(self, sql: str, params: list[Any] | None = None) -> Any:
        """Execute raw SQL via RPC."""
        payload: dict[str, Any] = {"query": sql}
        if params is not None:
            payload["params"] = params
        return self._client.rpc("exec_sql", payload).execute()

    async def execute_raw(
        self,
        sql: str,
        params: list[Any] | None = None,
    ) -> list[dict[str, Any]]:
        """Execute raw SQL and return rows."""
        result = await self.execute(sql, params)
        data = getattr(result, "data", None)
        if isinstance(data, list):
            return [row for row in data if isinstance(row, dict)]
        return []

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        return "supabase"


_IDENTIFIER_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")


class SQLAlchemyDBProvider:
    """SQLAlchemy DB Provider.

    DATABASE_URL / FAQ_DATABASE_URL で指定された RDB に接続し、
    Text2SQL などの execute_raw 呼び出しに対応する。
    """

    def __init__(self, url: str) -> None:
        self._url = self._normalize_async_url(url)
        self._engine: Any = None
        self._dialect = self._detect_dialect(self._url)

    @staticmethod
    def _normalize_async_url(url: str) -> str:
        lowered = url.lower()
        if lowered.startswith("postgres://"):
            return "postgresql+asyncpg://" + url.split("://", 1)[1]
        if lowered.startswith("postgresql://") and "+" not in lowered.split("://", 1)[0]:
            return "postgresql+asyncpg://" + url.split("://", 1)[1]
        if lowered.startswith("mysql://") and "+" not in lowered.split("://", 1)[0]:
            return "mysql+aiomysql://" + url.split("://", 1)[1]
        if lowered.startswith("sqlite://") and "+" not in lowered.split("://", 1)[0]:
            return "sqlite+aiosqlite://" + url.split("://", 1)[1]
        if lowered.startswith("mssql://") and "+" not in lowered.split("://", 1)[0]:
            return "mssql+aioodbc://" + url.split("://", 1)[1]
        return url

    @staticmethod
    def _detect_dialect(url: str) -> str:
        lowered = url.lower()
        if lowered.startswith("postgres"):
            return "postgresql"
        if lowered.startswith("mysql"):
            return "mysql"
        if lowered.startswith("sqlite"):
            return "sqlite"
        if lowered.startswith("mssql") or lowered.startswith("sqlserver"):
            return "mssql"
        parsed = urlparse(url)
        return (parsed.scheme or "").split("+", 1)[0].lower()

    @staticmethod
    def _validate_identifier(name: str) -> str:
        if _IDENTIFIER_RE.match(name):
            return name
        msg = f"Invalid identifier: {name}"
        raise ValueError(msg)

    async def connect(self) -> None:
        """データベースに接続."""
        if self._engine is not None:
            return
        try:
            from sqlalchemy.ext.asyncio import create_async_engine
        except ImportError as exc:
            msg = "sqlalchemy is required: pip install sqlalchemy[asyncio]"
            raise ImportError(msg) from exc

        self._engine = create_async_engine(
            self._url,
            echo=False,
            pool_pre_ping=True,
            future=True,
        )
        # 接続検証
        await self.execute("SELECT 1")
        logger.info("Connected to SQLAlchemy DB: %s", self._dialect)

    async def disconnect(self) -> None:
        """データベースから切断."""
        if self._engine is None:
            return
        await self._engine.dispose()
        self._engine = None

    async def select(
        self,
        table: str,
        columns: list[str] | None = None,
        filters: dict[str, Any] | None = None,
        limit: int | None = None,
    ) -> list[dict[str, Any]]:
        """テーブルからデータを取得."""
        from sqlalchemy import text

        table_name = self._validate_identifier(table)
        safe_columns = "*"
        if columns:
            safe_columns = ", ".join(self._validate_identifier(col) for col in columns)

        where_clause = ""
        params: dict[str, Any] = {}
        if filters:
            conditions: list[str] = []
            for idx, (key, value) in enumerate(filters.items()):
                col = self._validate_identifier(str(key))
                param_name = f"p_{idx}"
                conditions.append(f"{col} = :{param_name}")
                params[param_name] = value
            if conditions:
                where_clause = f" WHERE {' AND '.join(conditions)}"

        limit_clause = ""
        if isinstance(limit, int) and limit > 0:
            if self._dialect == "mssql":
                safe_columns = f"TOP {limit} {safe_columns}"
            else:
                limit_clause = f" LIMIT {limit}"

        sql = f"SELECT {safe_columns} FROM {table_name}{where_clause}{limit_clause}"
        rows = await self._execute_text_rows(sql, params)
        return rows

    async def insert(self, table: str, data: dict[str, Any]) -> dict[str, Any]:
        """テーブルへ1行挿入."""
        from sqlalchemy import text

        table_name = self._validate_identifier(table)
        if not data:
            msg = "insert data must not be empty"
            raise ValueError(msg)

        columns = [self._validate_identifier(str(col)) for col in data]
        placeholders = [f":v_{idx}" for idx in range(len(columns))]
        params = {f"v_{idx}": data[key] for idx, key in enumerate(data)}

        sql = f"INSERT INTO {table_name} ({', '.join(columns)}) VALUES ({', '.join(placeholders)})"
        if self._engine is None:
            await self.connect()

        async with self._engine.begin() as conn:
            await conn.execute(text(sql), params)
        return data

    async def update(self, table: str, data: dict[str, Any], filters: dict[str, Any]) -> list[dict[str, Any]]:
        """テーブルを更新."""
        from sqlalchemy import text

        table_name = self._validate_identifier(table)
        if not data:
            msg = "update data must not be empty"
            raise ValueError(msg)
        if not filters:
            msg = "update filters must not be empty"
            raise ValueError(msg)

        set_parts: list[str] = []
        where_parts: list[str] = []
        params: dict[str, Any] = {}

        for idx, (key, value) in enumerate(data.items()):
            col = self._validate_identifier(str(key))
            param_name = f"s_{idx}"
            set_parts.append(f"{col} = :{param_name}")
            params[param_name] = value

        for idx, (key, value) in enumerate(filters.items()):
            col = self._validate_identifier(str(key))
            param_name = f"w_{idx}"
            where_parts.append(f"{col} = :{param_name}")
            params[param_name] = value

        sql = f"UPDATE {table_name} SET {', '.join(set_parts)} WHERE {' AND '.join(where_parts)}"
        if self._engine is None:
            await self.connect()

        async with self._engine.begin() as conn:
            await conn.execute(text(sql), params)
        return []

    async def delete(self, table: str, filters: dict[str, Any]) -> int:
        """テーブルから削除."""
        from sqlalchemy import text

        table_name = self._validate_identifier(table)
        if not filters:
            msg = "delete filters must not be empty"
            raise ValueError(msg)

        where_parts: list[str] = []
        params: dict[str, Any] = {}
        for idx, (key, value) in enumerate(filters.items()):
            col = self._validate_identifier(str(key))
            param_name = f"w_{idx}"
            where_parts.append(f"{col} = :{param_name}")
            params[param_name] = value

        sql = f"DELETE FROM {table_name} WHERE {' AND '.join(where_parts)}"
        if self._engine is None:
            await self.connect()

        async with self._engine.begin() as conn:
            result = await conn.execute(text(sql), params)
            return int(result.rowcount or 0)

    async def execute(
        self,
        sql: str,
        params: list[Any] | None = None,
    ) -> Any:
        """生SQLを実行."""
        if self._engine is None:
            await self.connect()

        async with self._engine.begin() as conn:
            return await conn.exec_driver_sql(sql, tuple(params or []))

    async def execute_raw(
        self,
        sql: str,
        params: list[Any] | None = None,
    ) -> list[dict[str, Any]]:
        """生SQLを実行し辞書配列で返す."""
        if self._engine is None:
            await self.connect()

        async with self._engine.connect() as conn:
            result = await conn.exec_driver_sql(sql, tuple(params or []))
            if not result.returns_rows:
                return []
            mappings = result.mappings().all()
            return [dict(row) for row in mappings]

    async def _execute_text_rows(
        self,
        sql: str,
        params: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        from sqlalchemy import text

        if self._engine is None:
            await self.connect()
        async with self._engine.connect() as conn:
            result = await conn.execute(text(sql), params or {})
            if not result.returns_rows:
                return []
            rows = result.mappings().all()
            return [dict(row) for row in rows]

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        return "sqlalchemy"


def _is_sqlalchemy_supported_url(db_url: str) -> bool:
    lowered = db_url.strip().lower()
    prefixes = (
        "postgresql",
        "postgres",
        "mysql",
        "sqlite",
        "mssql",
        "sqlserver",
    )
    return lowered.startswith(prefixes)


def get_db(
    *,
    context: "RuntimeContext | None" = None,
    _new_instance: bool = False,
) -> DBProvider:
    """データベースプロバイダーを取得（松耦合）.

    環境変数から自動検出して最適なDBプロバイダーを返します。
    Agent/サービスは具体的な実装を知る必要がありません。

    Args:
        context: RuntimeContext（テナント/設定の分離用）
        _new_instance: 新しいインスタンスを強制作成（テスト用）

    Returns:
        DBProvider インスタンス

    環境変数優先順位:
        1. FAQ_DATABASE_URL / DATABASE_URL → SQLAlchemy
        2. SUPABASE_URL + SUPABASE_KEY → Supabase
        3. TURSO_URL + TURSO_AUTH_TOKEN → Turso
        4. なし → MockDBProvider
    """
    global _db_instance

    if _db_instance is not None and not _new_instance and context is None:
        return _db_instance

    from agentflow.runtime import get_env, resolve_settings

    settings = resolve_settings(context) if context is not None else None
    provider: DBProvider

    explicit_db_url = (
        get_env("FAQ_DATABASE_URL", context=context)
        or (settings.database_url if settings else None)
        or get_env("DATABASE_URL", context=context)
    )
    if explicit_db_url:
        if _is_sqlalchemy_supported_url(explicit_db_url):
            logger.info("Using SQLAlchemy provider: %s", explicit_db_url.split("://", 1)[0])
            provider = SQLAlchemyDBProvider(explicit_db_url)
            if context is None and not _new_instance:
                _db_instance = provider
            return provider
        logger.warning("Unsupported DATABASE_URL scheme, fallback provider used: %s", explicit_db_url[:24])

    # Supabase
    supabase_url = settings.supabase_url if settings else get_env("SUPABASE_URL", context=context)
    supabase_key = (
        (settings.supabase_key if settings else None)
        or get_env("SUPABASE_KEY", context=context)
        or get_env("SUPABASE_ANON_KEY", context=context)
    )
    if supabase_url and supabase_key:
        logger.info("Using Supabase provider (from env)")
        provider = SupabaseDBProvider(supabase_url, supabase_key)
        if context is None and not _new_instance:
            _db_instance = provider
        return provider

    # Turso (TODO: 実装)
    turso_url = settings.turso_url if settings else get_env("TURSO_URL", context=context)
    turso_token = get_env("TURSO_AUTH_TOKEN", context=context)
    if turso_url and turso_token:
        logger.warning("Turso provider not yet implemented, using mock")

    # フォールバック: Mock
    logger.info("No DB config found in environment. Using mock provider.")
    provider = MockDBProvider()
    if context is None and not _new_instance:
        _db_instance = provider
    return provider


def reset_db() -> None:
    """DBインスタンスをリセット（テスト用）."""
    global _db_instance
    _db_instance = None

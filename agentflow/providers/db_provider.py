"""DBProvider - 松耦合データベースアクセス.

Agent/サービスは具体的なDB実装を知る必要がありません。
環境変数から自動検出してDBインスタンスを提供します。

使用例:
    >>> from agentflow import get_db
    >>> db = get_db()  # 環境変数から自動検出
    >>> await db.connect()
    >>> users = await db.select("users", filters={"status": "active"})

環境変数優先順位:
    1. SUPABASE_URL + SUPABASE_KEY → Supabase
    2. TURSO_URL + TURSO_AUTH_TOKEN → Turso
    3. DATABASE_URL (postgres://) → PostgreSQL
    4. DATABASE_URL (sqlite://) → SQLite（デフォルト）
"""

import logging
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable


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

    async def update(
        self, table: str, data: dict[str, Any], filters: dict[str, Any]
    ) -> list[dict[str, Any]]:
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
        self._data[table] = [
            r for r in self._data[table]
            if not all(r.get(k) == v for k, v in filters.items())
        ]
        return original - len(self._data[table])

    async def execute(self, sql: str, params: list[Any] | None = None) -> Any:
        """Mock execute."""
        return None

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
        query = self._client.table(table).select(
            ",".join(columns) if columns else "*"
        )
        if filters:
            for k, v in filters.items():
                query = query.eq(k, v)
        if limit:
            query = query.limit(limit)
        return query.execute().data

    async def insert(self, table: str, data: dict[str, Any]) -> dict[str, Any]:
        """Insert."""
        result = self._client.table(table).insert(data).execute()
        return result.data[0] if result.data else {}

    async def update(
        self, table: str, data: dict[str, Any], filters: dict[str, Any]
    ) -> list[dict[str, Any]]:
        """Update."""
        query = self._client.table(table).update(data)
        for k, v in filters.items():
            query = query.eq(k, v)
        return query.execute().data

    async def delete(self, table: str, filters: dict[str, Any]) -> int:
        """Delete."""
        query = self._client.table(table).delete()
        for k, v in filters.items():
            query = query.eq(k, v)
        result = query.execute()
        return len(result.data) if result.data else 0

    async def execute(self, sql: str, params: list[Any] | None = None) -> Any:
        """Execute raw SQL via RPC."""
        return self._client.rpc("exec_sql", {"query": sql}).execute()

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        return "supabase"


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
        1. SUPABASE_URL + SUPABASE_KEY → Supabase
        2. TURSO_URL + TURSO_AUTH_TOKEN → Turso
        3. DATABASE_URL → PostgreSQL/SQLite
        4. なし → MockDBProvider
    """
    global _db_instance

    if _db_instance is not None and not _new_instance and context is None:
        return _db_instance

    from agentflow.runtime import get_env, resolve_settings

    settings = resolve_settings(context) if context is not None else None

    # Supabase
    supabase_url = (
        settings.supabase_url if settings else get_env("SUPABASE_URL", context=context)
    )
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
    turso_url = (
        settings.turso_url if settings else get_env("TURSO_URL", context=context)
    )
    turso_token = get_env("TURSO_AUTH_TOKEN", context=context)
    if turso_url and turso_token:
        logger.warning("Turso provider not yet implemented, using mock")

    # DATABASE_URL (PostgreSQL/SQLite) - TODO: 実装
    db_url = (
        settings.database_url if settings else get_env("DATABASE_URL", context=context)
    )
    if db_url:
        logger.warning(f"DATABASE_URL detected but not implemented: {db_url[:20]}...")

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

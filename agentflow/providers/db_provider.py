# -*- coding: utf-8 -*-
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
import os
from typing import Any, Protocol, runtime_checkable

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
            raise ImportError("supabase package required: pip install supabase")

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


def get_db() -> DBProvider:
    """データベースプロバイダーを取得（松耦合）.

    環境変数から自動検出して最適なDBプロバイダーを返します。
    Agent/サービスは具体的な実装を知る必要がありません。

    Returns:
        DBProvider インスタンス

    環境変数優先順位:
        1. SUPABASE_URL + SUPABASE_KEY → Supabase
        2. TURSO_URL + TURSO_AUTH_TOKEN → Turso
        3. DATABASE_URL → PostgreSQL/SQLite
        4. なし → MockDBProvider
    """
    global _db_instance

    if _db_instance is not None:
        return _db_instance

    # Supabase
    supabase_url = os.getenv("SUPABASE_URL")
    supabase_key = os.getenv("SUPABASE_KEY") or os.getenv("SUPABASE_ANON_KEY")
    if supabase_url and supabase_key:
        logger.info("Using Supabase provider (from env)")
        _db_instance = SupabaseDBProvider(supabase_url, supabase_key)
        return _db_instance

    # Turso (TODO: 実装)
    turso_url = os.getenv("TURSO_URL")
    turso_token = os.getenv("TURSO_AUTH_TOKEN")
    if turso_url and turso_token:
        logger.warning("Turso provider not yet implemented, using mock")

    # DATABASE_URL (PostgreSQL/SQLite) - TODO: 実装
    db_url = os.getenv("DATABASE_URL")
    if db_url:
        logger.warning(f"DATABASE_URL detected but not implemented: {db_url[:20]}...")

    # フォールバック: Mock
    logger.info("No DB config found in environment. Using mock provider.")
    _db_instance = MockDBProvider()
    return _db_instance


def reset_db() -> None:
    """DBインスタンスをリセット（テスト用）."""
    global _db_instance
    _db_instance = None

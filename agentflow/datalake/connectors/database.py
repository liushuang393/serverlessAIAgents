# -*- coding: utf-8 -*-
"""DatabaseConnector - データベースコネクタ.

PostgreSQL / MySQL / SQLite への統一アクセスを提供。
SQLAlchemy を使用した非同期データベースアクセス。

使用例:
    >>> connector = DatabaseConnector(DatabaseConfig(
    ...     connection_string="postgresql+asyncpg://user:pass@localhost/db"
    ... ))
    >>> result = await connector.query("users", "age > 18")
    >>> await connector.execute("INSERT INTO users (name) VALUES ('John')")

環境変数:
    DATABASE_URL: データベース接続文字列
"""

import json
import logging
from datetime import datetime, timezone
from typing import Any, AsyncIterator, Dict, List, Union

from pydantic import Field

from agentflow.datalake.connector import ConnectorConfig, DataConnector
from agentflow.datalake.core import DataItem, ReadResult

logger = logging.getLogger(__name__)


class DatabaseConfig(ConnectorConfig):
    """データベースコネクタ設定.

    Attributes:
        connection_string: 接続文字列
        pool_size: コネクションプールサイズ
        max_overflow: 最大オーバーフロー
        echo: SQLログ出力
    """

    connection_string: str | None = Field(default=None, description="接続文字列")
    pool_size: int = Field(default=5, description="コネクションプールサイズ")
    max_overflow: int = Field(default=10, description="最大オーバーフロー")
    echo: bool = Field(default=False, description="SQLログ出力")


class DatabaseConnector(DataConnector):
    """データベースコネクタ.

    db:// スキームでデータベースにアクセス。
    パス形式: db://table_name または db://schema.table_name

    使用例:
        >>> # PostgreSQL
        >>> connector = DatabaseConnector(DatabaseConfig(
        ...     connection_string="postgresql+asyncpg://user:pass@localhost/db"
        ... ))
        >>>
        >>> # MySQL
        >>> connector = DatabaseConnector(DatabaseConfig(
        ...     connection_string="mysql+aiomysql://user:pass@localhost/db"
        ... ))
        >>>
        >>> # SQLite
        >>> connector = DatabaseConnector(DatabaseConfig(
        ...     connection_string="sqlite+aiosqlite:///path/to/db.sqlite"
        ... ))
    """

    def __init__(self, config: DatabaseConfig | None = None) -> None:
        """初期化.

        Args:
            config: コネクタ設定
        """
        self._config = config or DatabaseConfig()
        self._engine = None
        self._metadata = None

    @property
    def scheme(self) -> str:
        """URIスキーム."""
        return "db"

    async def _get_engine(self):
        """SQLAlchemyエンジンを取得."""
        if self._engine is None:
            try:
                from sqlalchemy.ext.asyncio import create_async_engine
            except ImportError as e:
                raise ImportError(
                    "SQLAlchemy is required for database support. "
                    "Install with: pip install sqlalchemy[asyncio]"
                ) from e

            import os
            conn_str = self._config.connection_string or os.getenv("DATABASE_URL")
            if not conn_str:
                raise ValueError(
                    "Database connection string required. "
                    "Set DATABASE_URL environment variable."
                )

            self._engine = create_async_engine(
                conn_str,
                pool_size=self._config.pool_size,
                max_overflow=self._config.max_overflow,
                echo=self._config.echo,
            )

        return self._engine

    def _parse_path(self, path: str) -> tuple[str | None, str]:
        """パスをスキーマとテーブル名に分割.

        Args:
            path: パス（schema.table または table）

        Returns:
            (schema, table) のタプル
        """
        path = path.strip("/")
        if "." in path:
            schema, table = path.split(".", 1)
            return schema, table
        return None, path

    async def list(
        self,
        path: str,
        recursive: bool = False,
        pattern: str | None = None,
        limit: int | None = None,
    ) -> list[DataItem]:
        """テーブル一覧を取得.

        Args:
            path: スキーマ名（空の場合は全テーブル）
            recursive: 未使用
            pattern: テーブル名フィルタ
            limit: 最大件数

        Returns:
            DataItemのリスト（各テーブル）
        """
        import fnmatch
        from sqlalchemy import inspect

        engine = await self._get_engine()
        items: list[DataItem] = []

        async with engine.connect() as conn:
            # 同期inspectorを使用
            def get_tables(connection):
                inspector = inspect(connection)
                return inspector.get_table_names()

            tables = await conn.run_sync(get_tables)

            for table in tables:
                if pattern and not fnmatch.fnmatch(table, pattern):
                    continue

                items.append(
                    DataItem(
                        uri=f"db://{table}",
                        name=table,
                        is_directory=False,
                        metadata={"type": "table"},
                    )
                )

                if limit and len(items) >= limit:
                    break

        return items

    async def read(self, path: str) -> ReadResult:
        """テーブルデータを読み取り（全行）.

        Args:
            path: テーブル名

        Returns:
            ReadResult（JSON形式）
        """
        from sqlalchemy import text

        engine = await self._get_engine()
        schema, table = self._parse_path(path)

        # テーブル名のサニタイズ（SQLインジェクション対策）
        if not table.isidentifier():
            raise ValueError(f"Invalid table name: {table}")

        query = f"SELECT * FROM {table}"
        if schema:
            query = f"SELECT * FROM {schema}.{table}"

        async with engine.connect() as conn:
            result = await conn.execute(text(query))
            rows = [dict(row._mapping) for row in result]

        # JSON形式で返す
        content = json.dumps(rows, default=str, ensure_ascii=False)

        return ReadResult(
            uri=f"db://{path}",
            content=content.encode("utf-8"),
            content_type="application/json",
            size=len(content),
            metadata={"row_count": len(rows)},
        )

    async def write(
        self,
        path: str,
        content: bytes | str | list[dict],
        content_type: str | None = None,
        metadata: dict[str, str] | None = None,
    ) -> DataItem:
        """テーブルにデータを挿入.

        Args:
            path: テーブル名
            content: 挿入データ（JSON配列 or dict list）
            content_type: 未使用
            metadata: 未使用

        Returns:
            DataItem
        """
        from sqlalchemy import text

        engine = await self._get_engine()
        schema, table = self._parse_path(path)

        # データ準備
        if isinstance(content, bytes):
            content = content.decode("utf-8")
        if isinstance(content, str):
            rows = json.loads(content)
        else:
            rows = content

        if not rows:
            raise ValueError("No data to insert")

        # テーブル名のサニタイズ
        if not table.isidentifier():
            raise ValueError(f"Invalid table name: {table}")

        # INSERT文生成
        columns = list(rows[0].keys())
        col_names = ", ".join(columns)
        placeholders = ", ".join([f":{col}" for col in columns])

        full_table = f"{schema}.{table}" if schema else table
        query = f"INSERT INTO {full_table} ({col_names}) VALUES ({placeholders})"

        async with engine.begin() as conn:
            for row in rows:
                await conn.execute(text(query), row)

        logger.debug(f"Inserted {len(rows)} rows into {full_table}")

        return DataItem(
            uri=f"db://{path}",
            name=table,
            metadata={"inserted_rows": len(rows)},
        )

    async def exists(self, path: str) -> bool:
        """テーブル存在確認."""
        from sqlalchemy import inspect

        engine = await self._get_engine()
        schema, table = self._parse_path(path)

        async with engine.connect() as conn:
            def check_table(connection):
                inspector = inspect(connection)
                return table in inspector.get_table_names(schema=schema)

            return await conn.run_sync(check_table)

    async def delete(self, path: str) -> bool:
        """テーブルを削除（DROP TABLE）.

        注意: この操作は取り消せません。

        Args:
            path: テーブル名

        Returns:
            成功の場合True
        """
        from sqlalchemy import text

        engine = await self._get_engine()
        schema, table = self._parse_path(path)

        if not table.isidentifier():
            raise ValueError(f"Invalid table name: {table}")

        full_table = f"{schema}.{table}" if schema else table

        try:
            async with engine.begin() as conn:
                await conn.execute(text(f"DROP TABLE IF EXISTS {full_table}"))
            logger.warning(f"Dropped table {full_table}")
            return True
        except Exception as e:
            logger.error(f"Failed to drop table {full_table}: {e}")
            return False

    async def query(
        self,
        path: str,
        query: str,
        **kwargs: Any,
    ) -> list[dict[str, Any]]:
        """SQLクエリを実行.

        Args:
            path: テーブル名（WHERE句のベース）
            query: WHERE句条件 または 完全なSQL
            **kwargs: クエリパラメータ

        Returns:
            結果行のリスト
        """
        from sqlalchemy import text

        engine = await self._get_engine()

        # 完全なSQLかWHERE句かを判定
        if query.strip().upper().startswith(("SELECT", "WITH")):
            sql = query
        else:
            schema, table = self._parse_path(path)
            full_table = f"{schema}.{table}" if schema else table
            sql = f"SELECT * FROM {full_table} WHERE {query}"

        async with engine.connect() as conn:
            result = await conn.execute(text(sql), kwargs)
            return [dict(row._mapping) for row in result]

    async def execute(
        self,
        sql: str,
        params: dict[str, Any] | None = None,
    ) -> int:
        """SQL文を実行.

        Args:
            sql: SQL文
            params: パラメータ

        Returns:
            影響を受けた行数
        """
        from sqlalchemy import text

        engine = await self._get_engine()

        async with engine.begin() as conn:
            result = await conn.execute(text(sql), params or {})
            return result.rowcount

    async def close(self) -> None:
        """エンジンをクローズ."""
        if self._engine:
            await self._engine.dispose()
            self._engine = None


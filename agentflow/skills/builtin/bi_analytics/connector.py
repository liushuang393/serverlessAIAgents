"""データコネクタ.

各種データソースへの接続を管理。
"""

from __future__ import annotations

import csv
import io
import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)


class ConnectorType(str, Enum):
    """コネクタ種別."""

    POSTGRES = "postgres"
    MYSQL = "mysql"
    BIGQUERY = "bigquery"
    CSV = "csv"
    EXCEL = "excel"
    MEMORY = "memory"  # テスト用


@dataclass
class ConnectionConfig:
    """接続設定."""

    connector_type: ConnectorType
    host: str = ""
    port: int = 0
    database: str = ""
    username: str = ""
    password: str = ""
    extra: dict[str, Any] = field(default_factory=dict)


class DataFrame:
    """シンプルなデータフレーム実装.

    pandas依存を避けるための軽量実装。
    """

    def __init__(
        self,
        data: list[dict[str, Any]] | None = None,
        columns: list[str] | None = None,
    ) -> None:
        """初期化."""
        self._data = data or []
        self._columns = columns or (list(data[0].keys()) if data else [])

    @property
    def columns(self) -> list[str]:
        """カラム名."""
        return self._columns

    @property
    def rows(self) -> list[dict[str, Any]]:
        """行データ."""
        return self._data

    def __len__(self) -> int:
        """行数."""
        return len(self._data)

    def __iter__(self):
        """イテレート."""
        return iter(self._data)

    def head(self, n: int = 5) -> DataFrame:
        """先頭N行."""
        return DataFrame(self._data[:n], self._columns)

    def describe(self) -> dict[str, Any]:
        """基本統計."""
        stats: dict[str, Any] = {
            "count": len(self._data),
            "columns": self._columns,
        }

        # 数値列の統計
        for col in self._columns:
            values = [row.get(col) for row in self._data if isinstance(row.get(col), (int, float))]
            if values:
                stats[col] = {
                    "min": min(values),
                    "max": max(values),
                    "mean": sum(values) / len(values),
                }

        return stats

    def to_dict(self) -> list[dict[str, Any]]:
        """辞書リストに変換."""
        return self._data.copy()

    def to_csv(self) -> str:
        """CSV文字列に変換."""
        output = io.StringIO()
        writer = csv.DictWriter(output, fieldnames=self._columns)
        writer.writeheader()
        writer.writerows(self._data)
        return output.getvalue()


class DataConnector(ABC):
    """データコネクタ抽象基底クラス."""

    def __init__(self, config: ConnectionConfig) -> None:
        """初期化."""
        self._config = config
        self._connected = False
        self._logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")

    @property
    def connector_type(self) -> ConnectorType:
        """コネクタ種別."""
        return self._config.connector_type

    @property
    def is_connected(self) -> bool:
        """接続状態."""
        return self._connected

    @abstractmethod
    async def connect(self) -> None:
        """接続."""
        ...

    @abstractmethod
    async def disconnect(self) -> None:
        """切断."""
        ...

    @abstractmethod
    async def query(self, query: str, params: dict[str, Any] | None = None) -> DataFrame:
        """クエリ実行."""
        ...

    @abstractmethod
    async def list_tables(self) -> list[str]:
        """テーブル一覧."""
        ...

    @classmethod
    def from_url(cls, url: str) -> DataConnector:
        """URLから作成.

        Args:
            url: 接続URL（例: postgres://user:pass@host:5432/db）

        Returns:
            DataConnector インスタンス
        """
        if url.startswith(("postgres://", "postgresql://")):
            return MemoryConnector(ConnectionConfig(connector_type=ConnectorType.POSTGRES))
        if url.startswith("mysql://"):
            return MemoryConnector(ConnectionConfig(connector_type=ConnectorType.MYSQL))
        if url.startswith("memory://"):
            return MemoryConnector(ConnectionConfig(connector_type=ConnectorType.MEMORY))
        msg = f"不明な接続URL: {url}"
        raise ValueError(msg)


class MemoryConnector(DataConnector):
    """メモリベースコネクタ（テスト/開発用）."""

    def __init__(self, config: ConnectionConfig | None = None) -> None:
        """初期化."""
        super().__init__(config or ConnectionConfig(connector_type=ConnectorType.MEMORY))
        self._tables: dict[str, DataFrame] = {}

    async def connect(self) -> None:
        """接続."""
        self._connected = True

    async def disconnect(self) -> None:
        """切断."""
        self._connected = False

    async def query(self, query: str, params: dict[str, Any] | None = None) -> DataFrame:
        """クエリ実行.

        シンプルな SELECT 文のみサポート。
        """
        query = query.strip().lower()

        # SELECT * FROM table
        if query.startswith("select"):
            # テーブル名を抽出
            parts = query.split("from")
            if len(parts) > 1:
                table_name = parts[1].strip().split()[0]
                if table_name in self._tables:
                    return self._tables[table_name]

        return DataFrame()

    async def list_tables(self) -> list[str]:
        """テーブル一覧."""
        return list(self._tables.keys())

    def add_table(self, name: str, data: list[dict[str, Any]]) -> None:
        """テーブルを追加."""
        self._tables[name] = DataFrame(data)

    def add_sample_data(self) -> None:
        """サンプルデータを追加."""
        # 売上データ
        self.add_table("sales", [
            {"date": "2024-01-01", "product": "A", "amount": 1000, "quantity": 10},
            {"date": "2024-01-02", "product": "B", "amount": 1500, "quantity": 15},
            {"date": "2024-01-03", "product": "A", "amount": 1200, "quantity": 12},
            {"date": "2024-01-04", "product": "C", "amount": 800, "quantity": 8},
            {"date": "2024-01-05", "product": "B", "amount": 2000, "quantity": 20},
        ])

        # ユーザーデータ
        self.add_table("users", [
            {"id": 1, "name": "Alice", "age": 28, "department": "sales"},
            {"id": 2, "name": "Bob", "age": 35, "department": "engineering"},
            {"id": 3, "name": "Carol", "age": 42, "department": "sales"},
        ])

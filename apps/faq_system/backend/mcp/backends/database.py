"""DatabaseBackend - Text2SQL 連携データベース検索バックエンド.

自然言語クエリを Text2SQLService で SQL に変換・実行し、
結果を RetrievedDocument に変換して返す。

使用例:
    >>> backend = DatabaseBackend(dialect="postgresql", schema={"users": ["id", "name"]})
    >>> result = await backend.retrieve(RetrievalQuery(query="売上トップ10"))
"""

from __future__ import annotations

import json
import logging
import uuid
from typing import Any

from apps.faq_system.backend.mcp.backends.base import (
    BackendType,
    RetrievalBackend,
    RetrievalQuery,
    RetrievalResult,
    RetrievedDocument,
)


logger = logging.getLogger(__name__)


class DatabaseBackend(RetrievalBackend):
    """データベース検索バックエンド（Text2SQL 連携）.

    自然言語 → SQL → 実行 → 結果を RetrievedDocument に変換。
    内部で shared/services/text2sql_service.py を利用。
    """

    def __init__(
        self,
        dialect: str = "postgresql",
        schema: dict[str, list[str]] | None = None,
        max_rows: int = 100,
        timeout_seconds: int = 30,
    ) -> None:
        """初期化.

        Args:
            dialect: SQL 方言 ("postgresql", "mysql", "sqlite")
            schema: テーブル名→カラム名のマッピング
            max_rows: 最大取得行数
            timeout_seconds: SQL 実行タイムアウト（秒）
        """
        super().__init__(backend_type=BackendType.DATABASE, name="database:text2sql")
        self._dialect = dialect
        self._schema = schema or {}
        self._max_rows = max_rows
        self._timeout_seconds = timeout_seconds
        self._text2sql: Any = None  # 理由: 循環 import 回避のため遅延型
        self._started = False

    async def initialize(self) -> None:
        """Text2SQLService を初期化."""
        if self._started:
            return
        try:
            from shared.services.text2sql_service import SQLDialect, Text2SQLConfig, Text2SQLService

            dialect_enum = SQLDialect(self._dialect)
            config = Text2SQLConfig(
                dialect=dialect_enum,
                max_rows=self._max_rows,
                timeout_seconds=self._timeout_seconds,
                schema=self._schema,
            )
            self._text2sql = Text2SQLService(config)
            await self._text2sql.start()
            self._started = True
            self._logger.info("DatabaseBackend 初期化完了: dialect=%s", self._dialect)
        except Exception:
            self._logger.exception("DatabaseBackend 初期化失敗")
            raise

    async def retrieve(self, query: RetrievalQuery) -> RetrievalResult:
        """自然言語 → SQL → 実行 → RetrievedDocument に変換.

        Args:
            query: 統一検索クエリ

        Returns:
            統一検索結果（SQL 結果を RetrievedDocument に変換）
        """
        if not self._started:
            await self.initialize()

        if self._text2sql is None:
            return RetrievalResult(
                query=query.query,
                backend_type=BackendType.DATABASE,
                metadata={"error": "Text2SQLService 未初期化"},
            )

        try:
            # Text2SQLService.execute() → ServiceResult
            action = query.options.get("action", "query")
            result = await self._text2sql.execute(
                action=action,
                question=query.query,
            )

            if not result.success:
                error_msg = result.error_message or "SQL 実行失敗"
                self._logger.warning("DB検索失敗: %s", error_msg)
                return RetrievalResult(
                    query=query.query,
                    backend_type=BackendType.DATABASE,
                    metadata={"error": error_msg},
                )

            # ServiceResult.data を RetrievedDocument に変換
            documents = self._convert_sql_result(result.data, query.query)

            # top_k で制限
            documents = documents[: query.top_k]

            return RetrievalResult(
                documents=documents,
                query=query.query,
                total_found=len(documents),
                backend_type=BackendType.DATABASE,
                metadata={
                    "sql": result.data.get("sql", ""),
                    "row_count": result.data.get("row_count", 0),
                    "columns": result.data.get("columns", []),
                },
            )
        except Exception as e:
            self._logger.exception("DB検索エラー")
            return RetrievalResult(
                query=query.query,
                backend_type=BackendType.DATABASE,
                metadata={"error": str(e)},
            )

    async def health_check(self) -> bool:
        """ヘルスチェック."""
        return self._started and self._text2sql is not None

    async def cleanup(self) -> None:
        """Text2SQLService を停止."""
        if self._text2sql is not None and self._started:
            await self._text2sql.stop()
            self._started = False
            self._logger.info("DatabaseBackend クリーンアップ完了")

    def _convert_sql_result(
        self,
        data: dict[str, Any],
        query: str,
    ) -> list[RetrievedDocument]:
        """SQL 実行結果を RetrievedDocument に変換.

        各行を1ドキュメントとして扱い、カラム情報をメタデータに含める。

        Args:
            data: ServiceResult.data（rows, columns, sql 等を含む）
            query: 元のクエリ

        Returns:
            変換済みドキュメントリスト
        """
        rows = data.get("rows", [])
        columns = data.get("columns", [])
        sql = data.get("sql", "")
        answer = data.get("answer", "")

        documents: list[RetrievedDocument] = []

        # 回答テキストがあればトップに追加
        if answer:
            documents.append(
                RetrievedDocument(
                    doc_id=f"sql_answer_{uuid.uuid4().hex[:8]}",
                    content=answer,
                    score=1.0,
                    source="text2sql:answer",
                    metadata={"type": "answer", "sql": sql},
                )
            )

        # 各行をドキュメント化
        for i, row in enumerate(rows):
            if not isinstance(row, dict):
                continue
            # 行の内容をテキスト化
            content_parts = [f"{col}: {row.get(col, '')}" for col in columns if col in row]
            content = " | ".join(content_parts) if content_parts else json.dumps(row, ensure_ascii=False)

            documents.append(
                RetrievedDocument(
                    doc_id=f"sql_row_{i}_{uuid.uuid4().hex[:6]}",
                    content=content,
                    score=max(0.0, 0.9 - (i * 0.01)),  # 順序に基づくスコア
                    source="text2sql:row",
                    metadata={"row_index": i, "row_data": row, "columns": columns},
                )
            )

        return documents


# -*- coding: utf-8 -*-
"""Text2SQL Service - フレームワーク級 Text2SQL サービス.

自然言語からSQLを生成し、実行結果を可視化する再利用可能なサービス。

機能:
- 自然言語 → SQL変換
- SQL実行とエラーハンドリング
- 結果の分析と要約
- チャート自動生成

使用例:
    >>> from agentflow.services import Text2SQLService
    >>> 
    >>> service = Text2SQLService(
    ...     dialect="postgresql",
    ...     schema={"sales": ["id", "amount", "date"]},
    ... )
    >>> result = await service.execute(
    ...     action="query",
    ...     question="今月の売上TOP10は？",
    ... )
"""

from __future__ import annotations

import logging
import re
import time
from collections.abc import AsyncIterator
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

from agentflow.services.base import (
    ServiceBase,
    ServiceEvent,
    ServiceEventType,
    ProgressEvent,
    ResultEvent,
)

logger = logging.getLogger(__name__)


# =============================================================================
# 設定・型定義
# =============================================================================


class SQLDialect(str, Enum):
    """SQLダイアレクト."""
    POSTGRESQL = "postgresql"
    MYSQL = "mysql"
    SQLITE = "sqlite"
    MSSQL = "mssql"


class ChartType(str, Enum):
    """チャートタイプ."""
    BAR = "bar"
    LINE = "line"
    PIE = "pie"
    SCATTER = "scatter"
    TABLE = "table"


@dataclass
class Text2SQLConfig:
    """Text2SQL 設定."""
    
    dialect: SQLDialect = SQLDialect.POSTGRESQL
    max_rows: int = 1000
    timeout_seconds: int = 30
    auto_chart: bool = True
    schema: dict[str, list[str]] = field(default_factory=dict)
    
    @classmethod
    def get_config_fields(cls) -> list[dict[str, Any]]:
        """Studio 設定フィールド定義."""
        return [
            {
                "name": "dialect",
                "type": "select",
                "label": "SQLダイアレクト",
                "options": [e.value for e in SQLDialect],
                "default": "postgresql",
            },
            {
                "name": "max_rows",
                "type": "number",
                "label": "最大取得行数",
                "default": 1000,
                "min": 1,
                "max": 10000,
            },
            {
                "name": "auto_chart",
                "type": "boolean",
                "label": "チャート自動生成",
                "default": True,
            },
            {
                "name": "schema",
                "type": "json",
                "label": "DBスキーマ",
                "description": "テーブル名とカラムのマッピング",
            },
        ]


@dataclass
class SQLResult:
    """SQL実行結果."""
    sql: str
    data: list[dict[str, Any]]
    columns: list[str]
    row_count: int
    success: bool
    error: str | None = None
    execution_time_ms: float = 0


@dataclass
class ChartData:
    """チャートデータ."""
    chart_type: ChartType
    title: str
    data: dict[str, Any]
    options: dict[str, Any] = field(default_factory=dict)


# =============================================================================
# Text2SQL Service 実装
# =============================================================================


class Text2SQLService(ServiceBase):
    """Text2SQL Service - フレームワーク級サービス.
    
    Studio/CLI/SDK/API 全てで同一インターフェース。
    
    Actions:
    - query: 自然言語 → SQL → 実行 → 回答
    - generate_sql: SQLのみ生成
    - execute_sql: SQL直接実行
    """

    def __init__(self, config: Text2SQLConfig | None = None) -> None:
        """初期化."""
        super().__init__()
        self._config = config or Text2SQLConfig()
        self._llm = None
        self._db = None
        self._started = False

    async def start(self) -> None:
        """サービス開始."""
        if self._started:
            return
        
        from agentflow.providers import get_llm, get_db
        
        self._llm = get_llm(temperature=0)
        try:
            self._db = get_db()
            await self._db.connect()
        except Exception as e:
            self._logger.warning(f"DB接続失敗: {e}")
            self._db = None
        
        self._started = True

    async def stop(self) -> None:
        """サービス停止."""
        if self._db:
            await self._db.disconnect()
        self._started = False

    async def _execute_internal(
        self,
        execution_id: str,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """内部実行ロジック."""
        action = kwargs.get("action", "query")
        
        if not self._started:
            await self.start()
        
        if action == "query":
            async for event in self._do_query(execution_id, **kwargs):
                yield event
        elif action == "generate_sql":
            async for event in self._do_generate_sql(execution_id, **kwargs):
                yield event
        elif action == "execute_sql":
            async for event in self._do_execute_sql(execution_id, **kwargs):
                yield event
        else:
            yield self._emit_error(execution_id, "invalid_action", f"不明なアクション: {action}")

    async def _do_query(
        self,
        execution_id: str,
        question: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """自然言語 → SQL → 実行 → 回答."""
        start_time = time.time()
        
        yield self._emit_progress(execution_id, 10, "SQLを生成中...", phase="generate")
        
        sql = await self._generate_sql_internal(question)
        
        yield self._emit_progress(execution_id, 30, "SQLを実行中...", phase="execute")
        yield ServiceEvent(
            type=ServiceEventType.LOG,
            execution_id=execution_id,
            message=f"生成SQL: {sql}",
            data={"sql": sql},
        )
        
        sql_result = await self._execute_sql_internal(sql)
        
        if not sql_result.success:
            yield self._emit_error(
                execution_id, 
                "sql_error", 
                f"SQL実行エラー: {sql_result.error}"
            )
            return
        
        yield self._emit_progress(execution_id, 60, "結果を分析中...", phase="analyze")
        
        answer = await self._summarize_result(question, sql_result)
        
        chart = None
        if self._config.auto_chart and sql_result.data:
            yield self._emit_progress(execution_id, 80, "チャートを生成中...", phase="chart")
            chart = self._generate_chart(question, sql_result)
        
        yield self._emit_progress(execution_id, 100, "完了", phase="complete")
        
        result_data = {
            "answer": answer,
            "sql": sql,
            "columns": sql_result.columns,
            "data": sql_result.data[:100],
            "row_count": sql_result.row_count,
        }
        if chart:
            result_data["chart"] = {
                "type": chart.chart_type.value,
                "title": chart.title,
                "data": chart.data,
            }
        
        yield self._emit_result(execution_id, result_data, (time.time() - start_time) * 1000)

    async def _do_generate_sql(
        self,
        execution_id: str,
        question: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """SQLのみ生成."""
        start_time = time.time()
        
        yield self._emit_progress(execution_id, 50, "SQLを生成中...", phase="generate")
        
        sql = await self._generate_sql_internal(question)
        
        yield self._emit_result(execution_id, {
            "sql": sql,
            "question": question,
        }, (time.time() - start_time) * 1000)

    async def _do_execute_sql(
        self,
        execution_id: str,
        sql: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """SQL直接実行."""
        start_time = time.time()
        
        yield self._emit_progress(execution_id, 30, "SQLを実行中...", phase="execute")
        
        sql_result = await self._execute_sql_internal(sql)
        
        if not sql_result.success:
            yield self._emit_error(
                execution_id,
                "sql_error",
                f"SQL実行エラー: {sql_result.error}"
            )
            return
        
        chart = None
        if self._config.auto_chart and sql_result.data:
            chart = self._generate_chart("", sql_result)
        
        result_data = {
            "sql": sql,
            "columns": sql_result.columns,
            "data": sql_result.data[:100],
            "row_count": sql_result.row_count,
            "execution_time_ms": sql_result.execution_time_ms,
        }
        if chart:
            result_data["chart"] = {
                "type": chart.chart_type.value,
                "title": chart.title,
                "data": chart.data,
            }
        
        yield self._emit_result(execution_id, result_data, (time.time() - start_time) * 1000)

    async def _generate_sql_internal(self, question: str) -> str:
        """SQL生成ロジック."""
        schema_info = self._format_schema()
        
        prompt = f"""あなたはSQLエキスパートです。以下のデータベーススキーマに基づいて、
ユーザーの質問に答えるSQLクエリを生成してください。

## データベーススキーマ
{schema_info}

## SQLダイアレクト
{self._config.dialect.value}

## ルール
- SELECT文のみ生成（INSERT/UPDATE/DELETEは禁止）
- 必ず LIMIT {self._config.max_rows} を付ける
- 集計クエリには適切なGROUP BYを使用
- 日付フィルタは明示的に指定
- テーブル名・カラム名は正確に

## ユーザーの質問
{question}

## 出力
SQLクエリのみを出力してください（説明不要）:
```sql
"""
        
        response = await self._llm.chat([{"role": "user", "content": prompt}])
        sql = self._extract_sql(response["content"])
        sql = self._sanitize_sql(sql)
        
        return sql

    async def _execute_sql_internal(self, sql: str) -> SQLResult:
        """SQL実行ロジック."""
        if not self._db:
            return SQLResult(
                sql=sql,
                data=[],
                columns=[],
                row_count=0,
                success=False,
                error="データベース未接続",
            )
        
        start_time = time.time()
        
        try:
            rows = await self._db.execute_raw(sql)
            exec_time = (time.time() - start_time) * 1000
            
            if rows:
                columns = list(rows[0].keys()) if hasattr(rows[0], 'keys') else []
                data = [dict(r) if hasattr(r, 'keys') else r for r in rows]
            else:
                columns, data = [], []
            
            return SQLResult(
                sql=sql,
                data=data,
                columns=columns,
                row_count=len(data),
                success=True,
                execution_time_ms=exec_time,
            )
        except Exception as e:
            return SQLResult(
                sql=sql,
                data=[],
                columns=[],
                row_count=0,
                success=False,
                error=str(e),
                execution_time_ms=(time.time() - start_time) * 1000,
            )

    async def _summarize_result(self, question: str, result: SQLResult) -> str:
        """結果を要約."""
        if not result.data:
            return "該当するデータが見つかりませんでした。"
        
        sample = result.data[:10]
        
        prompt = f"""以下のSQLクエリ結果に基づいて、ユーザーの質問に自然な日本語で回答してください。

## ユーザーの質問
{question}

## 結果概要
- 取得行数: {result.row_count}行
- カラム: {', '.join(result.columns)}

## データサンプル（最大10行）
{sample}

## 回答ルール
- 数値は適切にフォーマット（カンマ区切り、単位など）
- 主要な洞察を簡潔に述べる
- 必要に応じてデータの傾向を説明

回答:"""
        
        response = await self._llm.chat([{"role": "user", "content": prompt}])
        return response["content"].strip()

    def _generate_chart(self, question: str, result: SQLResult) -> ChartData | None:
        """チャート生成."""
        if not result.data or not result.columns:
            return None
        
        chart_type = self._determine_chart_type(result)
        
        data = result.data[:50]
        labels = [str(r.get(result.columns[0], "")) for r in data]
        values = [r.get(result.columns[1], 0) if len(result.columns) > 1 else 0 for r in data]
        
        return ChartData(
            chart_type=chart_type,
            title=question[:50] if question else "クエリ結果",
            data={
                "labels": labels,
                "datasets": [{"data": values}],
                "xAxis": {"type": "category", "data": labels},
                "yAxis": {"type": "value"},
                "series": [{
                    "type": chart_type.value if chart_type != ChartType.PIE else "pie",
                    "data": [{"name": l, "value": v} for l, v in zip(labels, values)] if chart_type == ChartType.PIE else values,
                }],
            },
        )

    def _determine_chart_type(self, result: SQLResult) -> ChartType:
        """データに適したチャートタイプを決定."""
        if len(result.data) <= 5:
            return ChartType.PIE
        
        if len(result.columns) >= 2:
            first_col_values = [r.get(result.columns[0]) for r in result.data[:5]]
            if all(self._is_date_like(v) for v in first_col_values if v):
                return ChartType.LINE
        
        return ChartType.BAR

    def _format_schema(self) -> str:
        """スキーマ情報をフォーマット."""
        if not self._config.schema:
            return "（スキーマ情報なし）"
        
        lines = []
        for table, columns in self._config.schema.items():
            lines.append(f"テーブル: {table}")
            for col in columns:
                lines.append(f"  - {col}")
        return "\n".join(lines)

    def _extract_sql(self, text: str) -> str:
        """レスポンスからSQLを抽出."""
        match = re.search(r'```sql\s*(.*?)\s*```', text, re.DOTALL | re.IGNORECASE)
        if match:
            return match.group(1).strip()
        
        match = re.search(r'SELECT.*', text, re.DOTALL | re.IGNORECASE)
        if match:
            return match.group(0).strip()
        
        return text.strip()

    def _sanitize_sql(self, sql: str) -> str:
        """SQLを安全化."""
        sql = sql.strip()
        
        dangerous = ['INSERT', 'UPDATE', 'DELETE', 'DROP', 'TRUNCATE', 'ALTER', 'CREATE']
        upper_sql = sql.upper()
        
        for keyword in dangerous:
            if keyword in upper_sql:
                raise ValueError(f"危険なSQLキーワードを検出: {keyword}")
        
        if 'LIMIT' not in upper_sql:
            sql = f"{sql.rstrip(';')} LIMIT {self._config.max_rows}"
        
        return sql

    def _is_date_like(self, value: Any) -> bool:
        """値が日付っぽいか判定."""
        if value is None:
            return False
        s = str(value)
        date_patterns = [r'\d{4}-\d{2}-\d{2}', r'\d{2}/\d{2}/\d{4}', r'\d{4}/\d{2}/\d{2}']
        return any(re.match(p, s) for p in date_patterns)

    # =========================================================================
    # Studio 統合用メソッド
    # =========================================================================

    @classmethod
    def get_node_definition(cls) -> dict[str, Any]:
        """Studio ノード定義."""
        return {
            "type": "text2sql",
            "label": "Text2SQL",
            "category": "data",
            "icon": "database",
            "description": "自然言語からSQLを生成して実行",
            "inputs": [
                {"name": "question", "type": "string", "label": "質問"},
            ],
            "outputs": [
                {"name": "answer", "type": "string", "label": "回答"},
                {"name": "sql", "type": "string", "label": "生成SQL"},
                {"name": "data", "type": "array", "label": "データ"},
                {"name": "chart", "type": "object", "label": "チャート"},
            ],
            "config": Text2SQLConfig.get_config_fields(),
        }


__all__ = [
    "Text2SQLService",
    "Text2SQLConfig",
    "SQLResult",
    "ChartData",
    "SQLDialect",
    "ChartType",
]

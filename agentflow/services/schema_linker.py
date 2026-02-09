"""Schema Linker サービス - NL2SQL の核心コンポーネント.

自然言語クエリから関連テーブル・カラムを特定する。
学術研究に基づく設計:
- String Matching: 高速な初期フィルタリング
- LLM-based Linking: 意味理解による精緻な選択
- Confidence Scoring: 信頼度スコアリング

使用例:
    >>> from agentflow.services.schema_linker import SchemaLinker
    >>>
    >>> linker = SchemaLinker(schema={
    ...     "sales": ["id", "amount", "date", "customer_id"],
    ...     "customers": ["id", "name", "region"],
    ... })
    >>> await linker.start()
    >>> result = await linker.link("今月の売上TOP10")
    >>> print(result.relevant_tables)  # ["sales"]
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field


logger = logging.getLogger(__name__)


@dataclass
class ColumnInfo:
    """カラム情報."""

    name: str
    table: str
    data_type: str = "unknown"
    description: str = ""
    is_primary_key: bool = False
    is_foreign_key: bool = False
    sample_values: list[str] = field(default_factory=list)


@dataclass
class TableInfo:
    """テーブル情報."""

    name: str
    description: str = ""
    columns: list[ColumnInfo] = field(default_factory=list)
    row_count: int | None = None


@dataclass
class SchemaLinkResult:
    """Schema Linking 結果."""

    query: str
    relevant_tables: list[str] = field(default_factory=list)
    relevant_columns: list[str] = field(default_factory=list)
    table_scores: dict[str, float] = field(default_factory=dict)
    column_scores: dict[str, float] = field(default_factory=dict)
    linked_schema: str = ""
    confidence: float = 0.0


@dataclass
class SchemaLinkerConfig:
    """Schema Linker 設定."""

    use_llm: bool = True
    string_match_weight: float = 0.3
    llm_weight: float = 0.7
    min_confidence: float = 0.1
    max_tables: int = 5
    max_columns_per_table: int = 20


class SchemaLinker:
    """Schema Linker - NL2SQL の核心コンポーネント.

    学術研究（RAT-SQL, RESDSQL等）に基づく設計:
    1. 文字列マッチング: 高速な初期候補選択
    2. LLM-based: 意味的関連性の判定
    3. スコアリング: 複合スコアで最終選択
    """

    def __init__(
        self,
        schema: dict[str, list[str]] | None = None,
        config: SchemaLinkerConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            schema: テーブル→カラムのマッピング
            config: 設定
        """
        self._config = config or SchemaLinkerConfig()
        self._tables: dict[str, TableInfo] = {}
        self._llm = None
        self._started = False
        self._logger = logging.getLogger(__name__)

        if schema:
            self._load_schema(schema)

    def _load_schema(self, schema: dict[str, list[str]]) -> None:
        """スキーマをロード.

        Args:
            schema: テーブル→カラムのマッピング
        """
        for table_name, columns in schema.items():
            col_infos = [
                ColumnInfo(name=col, table=table_name)
                for col in columns
            ]
            self._tables[table_name] = TableInfo(
                name=table_name,
                columns=col_infos,
            )

    def add_table(self, table: TableInfo) -> None:
        """テーブルを追加.

        Args:
            table: テーブル情報
        """
        self._tables[table.name] = table

    async def start(self) -> None:
        """サービス開始."""
        if self._started:
            return

        if self._config.use_llm:
            from agentflow.providers import get_llm
            self._llm = get_llm(temperature=0)

        self._started = True

    async def stop(self) -> None:
        """サービス停止."""
        self._started = False

    async def link(self, query: str) -> SchemaLinkResult:
        """自然言語クエリから関連スキーマを特定.

        Args:
            query: 自然言語クエリ

        Returns:
            Schema Linking 結果
        """
        result = SchemaLinkResult(query=query)

        # 1. 文字列マッチングによる初期スコアリング
        string_scores = self._string_match_score(query)

        # 2. LLM による意味的スコアリング（オプション）
        llm_scores: dict[str, float] = {}
        if self._config.use_llm and self._llm:
            llm_scores = await self._llm_score(query)

        # 3. スコア統合
        combined_scores = self._combine_scores(string_scores, llm_scores)

        # 4. 関連テーブル・カラム選択
        result.table_scores = combined_scores["tables"]
        result.column_scores = combined_scores["columns"]

        # 閾値以上のテーブルを選択
        result.relevant_tables = [
            t for t, score in sorted(
                result.table_scores.items(),
                key=lambda x: x[1],
                reverse=True,
            )[:self._config.max_tables]
            if score >= self._config.min_confidence
        ]

        # 関連カラムを選択
        for table in result.relevant_tables:
            table_info = self._tables.get(table)
            if not table_info:
                continue
            for col in table_info.columns[:self._config.max_columns_per_table]:
                col_key = f"{table}.{col.name}"
                if result.column_scores.get(col_key, 0) >= self._config.min_confidence:
                    result.relevant_columns.append(col_key)

        # 5. 簡約スキーマ生成
        result.linked_schema = self._format_linked_schema(result.relevant_tables)
        result.confidence = self._calculate_confidence(result)

        return result

    def _string_match_score(self, query: str) -> dict[str, dict[str, float]]:
        """文字列マッチングによるスコアリング.

        Args:
            query: クエリ

        Returns:
            {"tables": {name: score}, "columns": {name: score}}
        """
        query_lower = query.lower()
        query_tokens = set(re.findall(r"\w+", query_lower))

        table_scores: dict[str, float] = {}
        column_scores: dict[str, float] = {}

        for table_name, table_info in self._tables.items():
            table_lower = table_name.lower()
            table_tokens = set(re.findall(r"\w+", table_lower))

            # テーブル名マッチング
            overlap = len(query_tokens & table_tokens)
            exact_match = 1.0 if table_lower in query_lower else 0.0
            partial_match = overlap / max(len(table_tokens), 1)

            table_scores[table_name] = max(exact_match, partial_match * 0.8)

            # カラム名マッチング
            for col in table_info.columns:
                col_lower = col.name.lower()
                col_tokens = set(re.findall(r"\w+", col_lower))
                col_key = f"{table_name}.{col.name}"

                col_overlap = len(query_tokens & col_tokens)
                col_exact = 1.0 if col_lower in query_lower else 0.0
                col_partial = col_overlap / max(len(col_tokens), 1)

                # カラムがマッチすればテーブルスコアも上昇
                col_score = max(col_exact, col_partial * 0.7)
                column_scores[col_key] = col_score

                if col_score > 0.5:
                    table_scores[table_name] = max(
                        table_scores[table_name],
                        col_score * 0.9,
                    )

        return {"tables": table_scores, "columns": column_scores}

    async def _llm_score(self, query: str) -> dict[str, float]:
        """LLM による意味的スコアリング.

        Args:
            query: クエリ

        Returns:
            テーブル→スコアのマップ
        """
        if not self._llm:
            return {}

        # スキーマ概要を作成
        schema_summary = self._get_schema_summary()

        prompt = f"""あなたはデータベースエキスパートです。
以下のユーザークエリに関連するテーブルを特定してください。

## データベーススキーマ
{schema_summary}

## ユーザークエリ
{query}

## 出力形式（JSON）
各テーブルの関連度を0.0〜1.0で評価してください:
{{"table_name": 0.8, "another_table": 0.3, ...}}

関連度の基準:
- 1.0: クエリに直接必要
- 0.7-0.9: 関連性が高い
- 0.4-0.6: 関連性がある可能性
- 0.1-0.3: 低い関連性
- 0.0: 無関係

JSON出力:"""

        try:
            response = await self._llm.chat([{"role": "user", "content": prompt}])
            content = response["content"]

            # JSON抽出
            import json
            json_match = re.search(r"\{[^}]+\}", content, re.DOTALL)
            if json_match:
                return json.loads(json_match.group())
        except Exception as e:
            self._logger.warning(f"LLM schema linking failed: {e}")

        return {}

    def _combine_scores(
        self,
        string_scores: dict[str, dict[str, float]],
        llm_scores: dict[str, float],
    ) -> dict[str, dict[str, float]]:
        """スコアを統合.

        Args:
            string_scores: 文字列マッチスコア
            llm_scores: LLMスコア

        Returns:
            統合スコア
        """
        combined_tables: dict[str, float] = {}

        for table in self._tables:
            str_score = string_scores["tables"].get(table, 0.0)
            llm_score = llm_scores.get(table, str_score)

            if llm_scores:
                combined_tables[table] = (
                    str_score * self._config.string_match_weight +
                    llm_score * self._config.llm_weight
                )
            else:
                combined_tables[table] = str_score

        return {
            "tables": combined_tables,
            "columns": string_scores["columns"],
        }

    def _format_linked_schema(self, tables: list[str]) -> str:
        """リンクされたスキーマをフォーマット.

        Args:
            tables: テーブルリスト

        Returns:
            フォーマット済みスキーマ
        """
        lines = []
        for table_name in tables:
            table_info = self._tables.get(table_name)
            if not table_info:
                continue

            columns_str = ", ".join(
                col.name for col in table_info.columns[:self._config.max_columns_per_table]
            )
            lines.append(f"Table: {table_name}")
            lines.append(f"  Columns: {columns_str}")
            if table_info.description:
                lines.append(f"  Description: {table_info.description}")

        return "\n".join(lines)

    def _get_schema_summary(self) -> str:
        """スキーマ概要を取得.

        Returns:
            スキーマ概要
        """
        lines = []
        for table_name, table_info in self._tables.items():
            columns = [col.name for col in table_info.columns[:10]]
            lines.append(f"- {table_name}: {', '.join(columns)}")
        return "\n".join(lines)

    def _calculate_confidence(self, result: SchemaLinkResult) -> float:
        """信頼度を計算.

        Args:
            result: リンク結果

        Returns:
            信頼度
        """
        if not result.relevant_tables:
            return 0.0

        avg_score = sum(result.table_scores.get(t, 0) for t in result.relevant_tables)
        avg_score /= len(result.relevant_tables)

        return min(1.0, avg_score)


__all__ = [
    "ColumnInfo",
    "SchemaLinkResult",
    "SchemaLinker",
    "SchemaLinkerConfig",
    "TableInfo",
]


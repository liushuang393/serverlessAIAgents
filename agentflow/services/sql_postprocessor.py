"""SQL Post-Processor - NL2SQL 後処理モジュール.

学術研究に基づく後処理パイプライン:
- 構文検証: SQL 構文エラーの検出
- 実行フィードバック: エラー時の自動修正
- 自己一貫性投票: 複数候補からの最適解選択

使用例:
    >>> from agentflow.services.sql_postprocessor import SQLPostProcessor
    >>>
    >>> processor = SQLPostProcessor()
    >>> await processor.start()
    >>> result = await processor.process(
    ...     sql="SELECT * FROM sales",
    ...     query="売上データを見せて",
    ... )
    >>> print(result.final_sql)
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import Callable


logger = logging.getLogger(__name__)


class ValidationLevel(Enum):
    """検証レベル."""

    SYNTAX = "syntax"
    SEMANTIC = "semantic"
    EXECUTION = "execution"


class SQLErrorType(Enum):
    """SQL エラータイプ."""

    SYNTAX = "syntax"
    TABLE_NOT_FOUND = "table_not_found"
    COLUMN_NOT_FOUND = "column_not_found"
    AMBIGUOUS_COLUMN = "ambiguous_column"
    TYPE_MISMATCH = "type_mismatch"
    PERMISSION = "permission"
    TIMEOUT = "timeout"
    UNKNOWN = "unknown"


@dataclass
class ValidationResult:
    """検証結果."""

    is_valid: bool
    level: ValidationLevel
    errors: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    suggestions: list[str] = field(default_factory=list)


@dataclass
class CorrectionResult:
    """修正結果."""

    original_sql: str
    corrected_sql: str
    was_corrected: bool
    correction_reason: str = ""
    error_type: SQLErrorType | None = None
    confidence: float = 1.0


@dataclass
class PostProcessResult:
    """後処理結果."""

    original_sql: str
    final_sql: str
    validation: ValidationResult
    corrections: list[CorrectionResult] = field(default_factory=list)
    execution_tested: bool = False
    execution_success: bool = False
    execution_error: str = ""
    total_corrections: int = 0


@dataclass
class PostProcessorConfig:
    """Post-Processor 設定."""

    max_correction_attempts: int = 3
    enable_syntax_validation: bool = True
    enable_execution_test: bool = False
    enable_llm_correction: bool = True
    timeout_seconds: float = 30.0
    allowed_tables: list[str] | None = None
    blocked_keywords: list[str] = field(default_factory=lambda: [
        "DROP", "DELETE", "TRUNCATE", "ALTER", "CREATE", "INSERT", "UPDATE",
    ])


# SQL 構文パターン
SQL_PATTERNS = {
    "select": re.compile(r"\bSELECT\b", re.IGNORECASE),
    "from": re.compile(r"\bFROM\b", re.IGNORECASE),
    "where": re.compile(r"\bWHERE\b", re.IGNORECASE),
    "group_by": re.compile(r"\bGROUP\s+BY\b", re.IGNORECASE),
    "order_by": re.compile(r"\bORDER\s+BY\b", re.IGNORECASE),
    "limit": re.compile(r"\bLIMIT\b", re.IGNORECASE),
    "join": re.compile(r"\b(INNER|LEFT|RIGHT|FULL|CROSS)?\s*JOIN\b", re.IGNORECASE),
    "semicolon": re.compile(r";"),
}

# 危険キーワード
DANGEROUS_PATTERNS = [
    re.compile(r"\bDROP\s+(TABLE|DATABASE|INDEX)\b", re.IGNORECASE),
    re.compile(r"\bDELETE\s+FROM\b", re.IGNORECASE),
    re.compile(r"\bTRUNCATE\s+TABLE\b", re.IGNORECASE),
    re.compile(r"\bALTER\s+TABLE\b", re.IGNORECASE),
    re.compile(r"\bINSERT\s+INTO\b", re.IGNORECASE),
    re.compile(r"\bUPDATE\s+\w+\s+SET\b", re.IGNORECASE),
    re.compile(r"\bCREATE\s+(TABLE|DATABASE|INDEX)\b", re.IGNORECASE),
    re.compile(r"--", re.IGNORECASE),  # SQL injection
    re.compile(r"/\*.*\*/", re.IGNORECASE | re.DOTALL),  # SQL injection
]


class SQLPostProcessor:
    """SQL Post-Processor - 後処理パイプライン.

    学術研究（PICARD, DIN-SQL等）に基づく設計:
    1. 構文検証: 基本的な SQL 構文チェック
    2. セキュリティ検証: 危険なクエリのブロック
    3. 実行テスト: 実際の DB での検証（オプション）
    4. LLM 修正: エラー時の自動修正
    """

    def __init__(
        self,
        config: PostProcessorConfig | None = None,
        execute_func: Callable[[str], Any] | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
            execute_func: SQL 実行関数（テスト用）
        """
        self._config = config or PostProcessorConfig()
        self._execute_func = execute_func
        self._llm = None
        self._started = False
        self._logger = logging.getLogger(__name__)

    async def start(self) -> None:
        """サービス開始."""
        if self._started:
            return

        if self._config.enable_llm_correction:
            from agentflow.providers import get_llm
            self._llm = get_llm(temperature=0)

        self._started = True

    async def stop(self) -> None:
        """サービス停止."""
        self._started = False

    async def process(
        self,
        sql: str,
        query: str,
        schema_context: str = "",
    ) -> PostProcessResult:
        """SQL を後処理.

        Args:
            sql: 生成された SQL
            query: 元のクエリ
            schema_context: スキーマ情報

        Returns:
            後処理結果
        """
        result = PostProcessResult(
            original_sql=sql,
            final_sql=sql,
            validation=ValidationResult(is_valid=True, level=ValidationLevel.SYNTAX),
        )

        # 1. SQL クリーンアップ
        cleaned_sql = self._cleanup_sql(sql)
        result.final_sql = cleaned_sql

        # 2. 構文検証
        if self._config.enable_syntax_validation:
            validation = self._validate_syntax(cleaned_sql)
            result.validation = validation

            if not validation.is_valid:
                # LLM 修正を試行
                if self._config.enable_llm_correction and self._llm:
                    correction = await self._llm_correct(
                        cleaned_sql, query, validation.errors, schema_context
                    )
                    if correction.was_corrected:
                        result.corrections.append(correction)
                        result.final_sql = correction.corrected_sql
                        result.total_corrections += 1

        # 3. セキュリティ検証
        security_result = self._validate_security(result.final_sql)
        if not security_result.is_valid:
            result.validation = security_result
            return result

        # 4. 実行テスト（オプション）
        if self._config.enable_execution_test and self._execute_func:
            result = await self._test_execution(result, query, schema_context)

        return result

    def _cleanup_sql(self, sql: str) -> str:
        """SQL をクリーンアップ.

        Args:
            sql: 元の SQL

        Returns:
            クリーンアップ済み SQL
        """
        # Markdown コードブロックを除去
        sql = re.sub(r"```sql\s*", "", sql)
        sql = re.sub(r"```\s*", "", sql)

        # 前後の空白を除去
        sql = sql.strip()

        # 複数のセミコロンを1つに
        sql = re.sub(r";+", ";", sql)

        # 末尾のセミコロンを除去（一般的に不要）
        return sql.rstrip(";")


    def _validate_syntax(self, sql: str) -> ValidationResult:
        """構文を検証.

        Args:
            sql: SQL

        Returns:
            検証結果
        """
        errors = []
        warnings = []

        # SELECT 文かチェック
        if not SQL_PATTERNS["select"].search(sql):
            errors.append("SELECT 文ではありません")

        # FROM 句があるかチェック（サブクエリ以外）
        if SQL_PATTERNS["select"].search(sql) and not SQL_PATTERNS["from"].search(sql):
            # SELECT 1 + 1 のような単純な式は OK
            if not re.match(r"^\s*SELECT\s+[\d\+\-\*/\s]+$", sql, re.IGNORECASE):
                warnings.append("FROM 句がありません")

        # 括弧のバランスをチェック
        open_parens = sql.count("(")
        close_parens = sql.count(")")
        if open_parens != close_parens:
            errors.append(f"括弧のバランスが不正: 開き={open_parens}, 閉じ={close_parens}")

        # 引用符のバランスをチェック
        single_quotes = sql.count("'") - sql.count("\\'")
        if single_quotes % 2 != 0:
            errors.append("シングルクォートのバランスが不正")

        return ValidationResult(
            is_valid=len(errors) == 0,
            level=ValidationLevel.SYNTAX,
            errors=errors,
            warnings=warnings,
        )

    def _validate_security(self, sql: str) -> ValidationResult:
        """セキュリティを検証.

        Args:
            sql: SQL

        Returns:
            検証結果
        """
        errors = []

        # 危険パターンをチェック
        for pattern in DANGEROUS_PATTERNS:
            if pattern.search(sql):
                errors.append(f"危険なパターンを検出: {pattern.pattern}")

        # 許可テーブルをチェック
        if self._config.allowed_tables:
            tables_in_sql = self._extract_tables(sql)
            for table in tables_in_sql:
                if table.lower() not in [t.lower() for t in self._config.allowed_tables]:
                    errors.append(f"許可されていないテーブル: {table}")

        return ValidationResult(
            is_valid=len(errors) == 0,
            level=ValidationLevel.SEMANTIC,
            errors=errors,
        )

    def _extract_tables(self, sql: str) -> list[str]:
        """SQL からテーブル名を抽出.

        Args:
            sql: SQL

        Returns:
            テーブル名リスト
        """
        # FROM / JOIN 後のテーブル名を抽出
        pattern = r"\b(?:FROM|JOIN)\s+([a-zA-Z_][a-zA-Z0-9_]*)"
        matches = re.findall(pattern, sql, re.IGNORECASE)
        return list(set(matches))

    async def _llm_correct(
        self,
        sql: str,
        query: str,
        errors: list[str],
        schema_context: str,
    ) -> CorrectionResult:
        """LLM による修正.

        Args:
            sql: 問題のある SQL
            query: 元のクエリ
            errors: エラーリスト
            schema_context: スキーマ情報

        Returns:
            修正結果
        """
        if not self._llm:
            return CorrectionResult(
                original_sql=sql,
                corrected_sql=sql,
                was_corrected=False,
            )

        prompt = f"""あなたは SQL エキスパートです。
以下の SQL にエラーがあります。修正してください。

## 元のクエリ
{query}

## 問題のある SQL
```sql
{sql}
```

## 検出されたエラー
{chr(10).join(f"- {e}" for e in errors)}

{f"## スキーマ情報{chr(10)}{schema_context}" if schema_context else ""}

## 出力
修正した SQL のみを出力してください（コードブロック不要）:"""

        try:
            response = await self._llm.chat([{"role": "user", "content": prompt}])
            corrected_sql = self._cleanup_sql(response["content"])

            return CorrectionResult(
                original_sql=sql,
                corrected_sql=corrected_sql,
                was_corrected=True,
                correction_reason="LLM による自動修正",
                confidence=0.8,
            )
        except Exception as e:
            self._logger.warning(f"LLM 修正に失敗: {e}")
            return CorrectionResult(
                original_sql=sql,
                corrected_sql=sql,
                was_corrected=False,
            )

    async def _test_execution(
        self,
        result: PostProcessResult,
        query: str,
        schema_context: str,
    ) -> PostProcessResult:
        """実行テスト.

        Args:
            result: 現在の結果
            query: 元のクエリ
            schema_context: スキーマ情報

        Returns:
            更新された結果
        """
        if not self._execute_func:
            return result

        result.execution_tested = True
        attempts = 0

        while attempts < self._config.max_correction_attempts:
            try:
                # LIMIT 1 で実行テスト
                test_sql = self._add_limit_for_test(result.final_sql)
                await self._execute_func(test_sql)
                result.execution_success = True
                return result

            except Exception as e:
                error_msg = str(e)
                result.execution_error = error_msg
                attempts += 1

                # エラータイプを判定
                error_type = self._classify_error(error_msg)

                # LLM 修正を試行
                if self._config.enable_llm_correction and self._llm:
                    correction = await self._llm_correct(
                        result.final_sql,
                        query,
                        [error_msg],
                        schema_context,
                    )
                    correction.error_type = error_type

                    if correction.was_corrected:
                        result.corrections.append(correction)
                        result.final_sql = correction.corrected_sql
                        result.total_corrections += 1
                    else:
                        break
                else:
                    break

        return result

    def _add_limit_for_test(self, sql: str) -> str:
        """テスト用に LIMIT を追加.

        Args:
            sql: SQL

        Returns:
            LIMIT 付き SQL
        """
        if SQL_PATTERNS["limit"].search(sql):
            return sql
        return f"{sql} LIMIT 1"

    def _classify_error(self, error_msg: str) -> SQLErrorType:
        """エラータイプを分類.

        Args:
            error_msg: エラーメッセージ

        Returns:
            エラータイプ
        """
        error_lower = error_msg.lower()

        if "syntax" in error_lower or "parse" in error_lower:
            return SQLErrorType.SYNTAX
        if "table" in error_lower and ("not found" in error_lower or "exist" in error_lower):
            return SQLErrorType.TABLE_NOT_FOUND
        if "column" in error_lower and ("not found" in error_lower or "exist" in error_lower):
            return SQLErrorType.COLUMN_NOT_FOUND
        if "ambiguous" in error_lower:
            return SQLErrorType.AMBIGUOUS_COLUMN
        if "type" in error_lower or "cast" in error_lower:
            return SQLErrorType.TYPE_MISMATCH
        if "permission" in error_lower or "denied" in error_lower:
            return SQLErrorType.PERMISSION
        if "timeout" in error_lower:
            return SQLErrorType.TIMEOUT

        return SQLErrorType.UNKNOWN


__all__ = [
    "CorrectionResult",
    "PostProcessResult",
    "PostProcessorConfig",
    "SQLErrorType",
    "SQLPostProcessor",
    "ValidationLevel",
    "ValidationResult",
]


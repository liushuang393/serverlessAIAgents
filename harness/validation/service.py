"""Layer 4 Validation - 入出力バリデーションサービス.

Flow/Agent の入出力を検証し、スキーマ違反や不正値を検出する。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Protocol

_logger = logging.getLogger(__name__)


class ValidationSeverity(str, Enum):
    """バリデーション重要度."""

    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


@dataclass
class ValidationIssue:
    """バリデーション問題.

    Attributes:
        field: 問題のあるフィールド名
        message: 問題の説明
        severity: 重要度
    """

    field: str
    message: str
    severity: ValidationSeverity = ValidationSeverity.ERROR


@dataclass
class ValidationResult:
    """バリデーション結果.

    Attributes:
        valid: バリデーション合格かどうか
        issues: 検出された問題リスト
    """

    valid: bool = True
    issues: list[ValidationIssue] = field(default_factory=list)

    def add_issue(self, issue: ValidationIssue) -> None:
        """問題を追加."""
        self.issues.append(issue)
        if issue.severity == ValidationSeverity.ERROR:
            self.valid = False


class Validator(Protocol):
    """バリデータプロトコル."""

    def validate(self, data: dict[str, Any]) -> ValidationResult:
        """データを検証."""
        ...


class SchemaValidator:
    """スキーマベースバリデータ.

    必須フィールドの存在と型チェックを行う。
    """

    def __init__(self, required_fields: dict[str, type] | None = None) -> None:
        """初期化.

        Args:
            required_fields: フィールド名→期待型のマッピング
        """
        self._required_fields = required_fields or {}

    def validate(self, data: dict[str, Any]) -> ValidationResult:
        """データを検証.

        Args:
            data: 検証対象データ

        Returns:
            バリデーション結果
        """
        result = ValidationResult()
        for field_name, expected_type in self._required_fields.items():
            if field_name not in data:
                result.add_issue(
                    ValidationIssue(
                        field=field_name,
                        message=f"必須フィールド '{field_name}' が存在しません",
                    )
                )
            elif not isinstance(data[field_name], expected_type):
                result.add_issue(
                    ValidationIssue(
                        field=field_name,
                        message=f"型不一致: 期待={expected_type.__name__}, 実際={type(data[field_name]).__name__}",
                    )
                )
        return result


__all__ = [
    "SchemaValidator",
    "ValidationIssue",
    "ValidationResult",
    "ValidationSeverity",
    "Validator",
]


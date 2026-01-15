# -*- coding: utf-8 -*-
"""強約束バリデータ - Agent自由度制限システム.

Manus分析に基づく高信頼性設計：
- Agentの行動を制限し、安全な操作のみを許可
- 入出力スキーマの強制検証
- ツール呼び出しのホワイトリスト制御
- 危険操作の事前ブロック

使用例:
    >>> from agentflow.core.constraint_validator import ConstraintValidator
    >>>
    >>> validator = ConstraintValidator(
    ...     allowed_tools=["search", "analyze"],
    ...     input_schema=MyInputSchema,
    ...     output_schema=MyOutputSchema,
    ... )
    >>> result = validator.validate_tool_call(tool_call)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable

from pydantic import BaseModel, Field, ValidationError

logger = logging.getLogger(__name__)


class ValidationSeverity(str, Enum):
    """検証結果の深刻度."""
    
    INFO = "info"          # 情報のみ
    WARNING = "warning"    # 警告（続行可能）
    ERROR = "error"        # エラー（続行不可）
    CRITICAL = "critical"  # 重大（即座に停止）


class ConstraintType(str, Enum):
    """制約の種類."""
    
    SCHEMA = "schema"          # スキーマ検証
    TOOL_WHITELIST = "tool_whitelist"  # ツールホワイトリスト
    VALUE_RANGE = "value_range"    # 値の範囲
    PATTERN = "pattern"        # パターンマッチ
    CUSTOM = "custom"          # カスタム検証


@dataclass
class ValidationResult:
    """検証結果.
    
    Attributes:
        is_valid: 検証成功かどうか
        severity: 深刻度
        constraint_type: 制約の種類
        message: メッセージ
        details: 詳細情報
        timestamp: 検証時刻
    """
    
    is_valid: bool
    severity: ValidationSeverity = ValidationSeverity.INFO
    constraint_type: ConstraintType = ConstraintType.SCHEMA
    message: str = ""
    details: dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)
    
    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "is_valid": self.is_valid,
            "severity": self.severity.value,
            "constraint_type": self.constraint_type.value,
            "message": self.message,
            "details": self.details,
            "timestamp": self.timestamp.isoformat(),
        }


class ToolCallConstraint(BaseModel):
    """ツール呼び出し制約.
    
    Attributes:
        tool_name: ツール名
        allowed_args: 許可された引数名
        forbidden_args: 禁止された引数名
        arg_validators: 引数バリデータ
    """
    
    tool_name: str
    allowed_args: list[str] = Field(default_factory=list)
    forbidden_args: list[str] = Field(default_factory=list)
    max_calls_per_session: int = Field(default=100, ge=1)
    require_confirmation: bool = Field(default=False)


class DangerousOperationConfig(BaseModel):
    """危険操作の設定.
    
    Attributes:
        blocked_keywords: ブロックするキーワード
        require_approval: 承認が必要な操作
        max_impact_level: 最大許可影響レベル
    """
    
    blocked_keywords: list[str] = Field(
        default_factory=lambda: [
            "DROP", "DELETE", "TRUNCATE",  # SQL危険操作
            "rm -rf", "format",  # システム危険操作
            "transfer", "withdraw",  # 金融危険操作
        ]
    )
    require_approval_keywords: list[str] = Field(
        default_factory=lambda: [
            "UPDATE", "INSERT",  # SQL変更操作
            "send", "publish",  # 外部送信
        ]
    )
    max_impact_level: int = Field(default=3, ge=1, le=5)


class ConstraintValidator:
    """強約束バリデータ.
    
    Agentの行動を制限し、安全な操作のみを許可する。
    Manus分析に基づく「強約束」設計を実装。
    
    主な機能:
    - 入出力スキーマの強制検証
    - ツール呼び出しのホワイトリスト制御
    - 危険操作の事前ブロック
    - カスタムバリデータの登録
    
    Example:
        >>> validator = ConstraintValidator(
        ...     allowed_tools=["search", "analyze", "report"],
        ...     input_schema=MyInputSchema,
        ... )
        >>> 
        >>> # ツール呼び出しの検証
        >>> result = validator.validate_tool_call(tool_call)
        >>> if not result.is_valid:
        ...     raise ValueError(result.message)
    """
    
    def __init__(
        self,
        *,
        allowed_tools: list[str] | None = None,
        input_schema: type[BaseModel] | None = None,
        output_schema: type[BaseModel] | None = None,
        tool_constraints: dict[str, ToolCallConstraint] | None = None,
        dangerous_ops: DangerousOperationConfig | None = None,
        custom_validators: list[Callable[[Any], ValidationResult]] | None = None,
    ) -> None:
        """初期化."""
        self._allowed_tools = set(allowed_tools) if allowed_tools else None
        self._input_schema = input_schema
        self._output_schema = output_schema
        self._tool_constraints = tool_constraints or {}
        self._dangerous_ops = dangerous_ops or DangerousOperationConfig()
        self._custom_validators = custom_validators or []
        self._tool_call_counts: dict[str, int] = {}
        self._logger = logging.getLogger(__name__)

    def validate_input(
        self,
        data: dict[str, Any],
        schema: type[BaseModel] | None = None,
    ) -> ValidationResult:
        """入力データを検証.

        Args:
            data: 入力データ
            schema: スキーマ（Noneの場合はデフォルトスキーマを使用）

        Returns:
            ValidationResult
        """
        target_schema = schema or self._input_schema
        if not target_schema:
            return ValidationResult(
                is_valid=True,
                message="スキーマ未設定のため検証スキップ",
            )

        try:
            target_schema.model_validate(data)
            return ValidationResult(
                is_valid=True,
                constraint_type=ConstraintType.SCHEMA,
                message="入力検証成功",
            )
        except ValidationError as e:
            return ValidationResult(
                is_valid=False,
                severity=ValidationSeverity.ERROR,
                constraint_type=ConstraintType.SCHEMA,
                message=f"入力検証失敗: {e.error_count()}件のエラー",
                details={"errors": e.errors()},
            )

    def validate_output(
        self,
        data: dict[str, Any],
        schema: type[BaseModel] | None = None,
    ) -> ValidationResult:
        """出力データを検証.

        Args:
            data: 出力データ
            schema: スキーマ（Noneの場合はデフォルトスキーマを使用）

        Returns:
            ValidationResult
        """
        target_schema = schema or self._output_schema
        if not target_schema:
            return ValidationResult(
                is_valid=True,
                message="スキーマ未設定のため検証スキップ",
            )

        try:
            target_schema.model_validate(data)
            return ValidationResult(
                is_valid=True,
                constraint_type=ConstraintType.SCHEMA,
                message="出力検証成功",
            )
        except ValidationError as e:
            return ValidationResult(
                is_valid=False,
                severity=ValidationSeverity.ERROR,
                constraint_type=ConstraintType.SCHEMA,
                message=f"出力検証失敗: {e.error_count()}件のエラー",
                details={"errors": e.errors()},
            )

    def validate_tool_call(
        self,
        tool_name: str,
        arguments: dict[str, Any] | None = None,
    ) -> ValidationResult:
        """ツール呼び出しを検証.

        ホワイトリスト、引数制約、危険操作を検査。

        Args:
            tool_name: ツール名
            arguments: ツール引数

        Returns:
            ValidationResult
        """
        arguments = arguments or {}

        # 1. ホワイトリストチェック
        if self._allowed_tools and tool_name not in self._allowed_tools:
            return ValidationResult(
                is_valid=False,
                severity=ValidationSeverity.CRITICAL,
                constraint_type=ConstraintType.TOOL_WHITELIST,
                message=f"ツール '{tool_name}' は許可リストにありません",
                details={
                    "tool_name": tool_name,
                    "allowed_tools": list(self._allowed_tools),
                },
            )

        # 2. 呼び出し回数チェック
        if tool_name in self._tool_constraints:
            constraint = self._tool_constraints[tool_name]
            current_count = self._tool_call_counts.get(tool_name, 0)
            if current_count >= constraint.max_calls_per_session:
                return ValidationResult(
                    is_valid=False,
                    severity=ValidationSeverity.ERROR,
                    constraint_type=ConstraintType.VALUE_RANGE,
                    message=f"ツール '{tool_name}' の呼び出し上限に達しました",
                    details={
                        "current_count": current_count,
                        "max_calls": constraint.max_calls_per_session,
                    },
                )

        # 3. 危険操作チェック
        danger_result = self._check_dangerous_operation(tool_name, arguments)
        if not danger_result.is_valid:
            return danger_result

        # 4. 呼び出しカウント更新
        self._tool_call_counts[tool_name] = self._tool_call_counts.get(tool_name, 0) + 1

        return ValidationResult(
            is_valid=True,
            constraint_type=ConstraintType.TOOL_WHITELIST,
            message=f"ツール '{tool_name}' の呼び出しを許可",
        )

    def _check_dangerous_operation(
        self,
        tool_name: str,
        arguments: dict[str, Any],
    ) -> ValidationResult:
        """危険操作をチェック.

        Args:
            tool_name: ツール名
            arguments: 引数

        Returns:
            ValidationResult
        """
        # 引数を文字列化して検査
        args_str = str(arguments).upper()

        # ブロックキーワードチェック
        for keyword in self._dangerous_ops.blocked_keywords:
            if keyword.upper() in args_str:
                return ValidationResult(
                    is_valid=False,
                    severity=ValidationSeverity.CRITICAL,
                    constraint_type=ConstraintType.PATTERN,
                    message=f"危険な操作が検出されました: '{keyword}'",
                    details={
                        "tool_name": tool_name,
                        "blocked_keyword": keyword,
                    },
                )

        # 承認必要キーワードチェック
        for keyword in self._dangerous_ops.require_approval_keywords:
            if keyword.upper() in args_str:
                return ValidationResult(
                    is_valid=False,
                    severity=ValidationSeverity.WARNING,
                    constraint_type=ConstraintType.PATTERN,
                    message=f"操作 '{keyword}' には承認が必要です",
                    details={
                        "tool_name": tool_name,
                        "require_approval": True,
                        "keyword": keyword,
                    },
                )

        return ValidationResult(is_valid=True)

    def add_custom_validator(
        self,
        validator: Callable[[Any], ValidationResult],
    ) -> None:
        """カスタムバリデータを追加.

        Args:
            validator: バリデータ関数
        """
        self._custom_validators.append(validator)

    def validate_all(
        self,
        data: Any,
    ) -> list[ValidationResult]:
        """全てのカスタムバリデータを実行.

        Args:
            data: 検証対象データ

        Returns:
            全てのValidationResult
        """
        results = []
        for validator in self._custom_validators:
            try:
                result = validator(data)
                results.append(result)
            except Exception as e:
                results.append(ValidationResult(
                    is_valid=False,
                    severity=ValidationSeverity.ERROR,
                    constraint_type=ConstraintType.CUSTOM,
                    message=f"カスタムバリデータでエラー: {e}",
                ))
        return results

    def reset_counts(self) -> None:
        """ツール呼び出しカウントをリセット."""
        self._tool_call_counts.clear()

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        return {
            "allowed_tools": list(self._allowed_tools) if self._allowed_tools else [],
            "tool_call_counts": dict(self._tool_call_counts),
            "has_input_schema": self._input_schema is not None,
            "has_output_schema": self._output_schema is not None,
            "custom_validator_count": len(self._custom_validators),
        }


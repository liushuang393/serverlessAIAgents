# -*- coding: utf-8 -*-
"""構造化出力規約（Output Contracts）.

L5層：AI出力の品質と安全性を担保するための構造化出力規約を定義。
将来のアプリ変更時の影響を最小化し、安定したAPIを提供。

設計原則:
- 型安全: 出力スキーマを厳密に定義
- 根拠追跡: 全ての主張に根拠（Evidence）を要求可能
- フォールバック: 品質基準を満たさない場合の代替処理
- 監査可能: 出力検証結果を記録

使用例:
    >>> from agentflow.security.output_contracts import (
    ...     OutputContract, EvidenceRule, ContractValidator
    ... )
    >>>
    >>> # 見積書出力の規約を定義
    >>> contract = OutputContract(
    ...     name="quote_output",
    ...     schema={"amount": "number", "reason": "string"},
    ...     evidence_rules=[
    ...         EvidenceRule(field="amount", require_source=True),
    ...     ],
    ...     fallback_action="request_human_review",
    ... )
    >>>
    >>> # 出力を検証
    >>> validator = ContractValidator()
    >>> result = validator.validate(output_data, contract)
"""

from __future__ import annotations

import logging
import re
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class EvidenceType(str, Enum):
    """根拠タイプ."""

    TOOL_RESULT = "tool_result"     # ツール実行結果
    RAG_CITATION = "rag_citation"   # RAG検索からの引用
    USER_INPUT = "user_input"       # ユーザー入力
    CALCULATION = "calculation"     # 計算結果
    SYSTEM_DATA = "system_data"     # システムデータ
    EXTERNAL_API = "external_api"   # 外部API


class FallbackAction(str, Enum):
    """フォールバックアクション."""

    REJECT = "reject"                       # 出力を拒否
    REQUEST_HUMAN_REVIEW = "request_human_review"  # 人間レビュー要求
    USE_DEFAULT = "use_default"             # デフォルト値を使用
    RETRY = "retry"                         # 再試行
    PARTIAL_ACCEPT = "partial_accept"       # 検証通過部分のみ受入


class Evidence(BaseModel):
    """根拠情報.

    Attributes:
        evidence_type: 根拠タイプ
        source: ソース名（ツール名、ドキュメント名等）
        content: 根拠の内容
        confidence: 信頼度（0.0-1.0）
        timestamp: 取得日時
        metadata: メタデータ
    """

    evidence_type: EvidenceType = Field(..., description="根拠タイプ")
    source: str = Field(..., description="ソース名")
    content: str | dict[str, Any] = Field(..., description="根拠の内容")
    confidence: float = Field(default=1.0, ge=0.0, le=1.0, description="信頼度")
    timestamp: datetime = Field(default_factory=datetime.now, description="取得日時")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


class EvidenceRule(BaseModel):
    """根拠要求ルール.

    特定フィールドに対する根拠要件を定義。

    Attributes:
        field: 対象フィールド名（ドット記法でネスト対応）
        require_source: ソース必須か
        allowed_types: 許可される根拠タイプ
        min_confidence: 最低信頼度
        required: この根拠が必須か
    """

    field: str = Field(..., description="対象フィールド名")
    require_source: bool = Field(default=True, description="ソース必須か")
    allowed_types: list[EvidenceType] = Field(
        default_factory=lambda: list(EvidenceType),
        description="許可される根拠タイプ",
    )
    min_confidence: float = Field(default=0.7, ge=0.0, le=1.0, description="最低信頼度")
    required: bool = Field(default=True, description="必須か")


class FieldSchema(BaseModel):
    """フィールドスキーマ定義.

    Attributes:
        name: フィールド名
        field_type: データ型
        required: 必須か
        description: 説明
        validation_rules: バリデーションルール
    """

    name: str = Field(..., description="フィールド名")
    field_type: str = Field(..., description="データ型 (string/number/boolean/array/object)")
    required: bool = Field(default=True, description="必須か")
    description: str = Field(default="", description="説明")
    validation_rules: dict[str, Any] = Field(
        default_factory=dict,
        description="バリデーションルール（min, max, pattern等）",
    )


class OutputContract(BaseModel):
    """出力規約.

    AI出力の構造・品質要件を定義。

    Attributes:
        name: 規約名
        version: バージョン
        description: 説明
        fields: フィールドスキーマリスト
        evidence_rules: 根拠要求ルールリスト
        fallback_action: 検証失敗時のフォールバック
        quality_threshold: 品質閾値（0.0-1.0）
        metadata: メタデータ
    """

    name: str = Field(..., description="規約名")
    version: str = Field(default="1.0.0", description="バージョン")
    description: str = Field(default="", description="説明")
    fields: list[FieldSchema] = Field(default_factory=list, description="フィールドスキーマ")
    evidence_rules: list[EvidenceRule] = Field(
        default_factory=list,
        description="根拠要求ルール",
    )
    fallback_action: FallbackAction = Field(
        default=FallbackAction.REQUEST_HUMAN_REVIEW,
        description="検証失敗時のフォールバック",
    )
    quality_threshold: float = Field(
        default=0.8,
        ge=0.0,
        le=1.0,
        description="品質閾値",
    )
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


class ValidationResult(BaseModel):
    """検証結果.

    Attributes:
        is_valid: 検証成功か
        quality_score: 品質スコア（0.0-1.0）
        missing_fields: 欠落フィールドリスト
        invalid_fields: 無効フィールド詳細リスト
        missing_evidence: 欠落根拠リスト
        insufficient_evidence: 信頼度不足の根拠リスト
        fallback_triggered: フォールバックが発生したか
        fallback_action: 実行されたフォールバック
        errors: エラーリスト
        warnings: 警告リスト
        validated_at: 検証日時
    """

    is_valid: bool = Field(default=True, description="検証成功か")
    quality_score: float = Field(default=1.0, ge=0.0, le=1.0, description="品質スコア")
    missing_fields: list[str] = Field(default_factory=list, description="欠落フィールド")
    invalid_fields: list[dict[str, Any]] = Field(default_factory=list, description="無効フィールド詳細")
    missing_evidence: list[str] = Field(default_factory=list, description="欠落根拠")
    insufficient_evidence: list[dict[str, Any]] = Field(
        default_factory=list,
        description="信頼度不足の根拠",
    )
    fallback_triggered: bool = Field(default=False, description="フォールバック発生")
    fallback_action: FallbackAction | None = Field(default=None, description="実行フォールバック")
    errors: list[str] = Field(default_factory=list, description="エラー")
    warnings: list[str] = Field(default_factory=list, description="警告")
    validated_at: datetime = Field(default_factory=datetime.now, description="検証日時")

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "is_valid": self.is_valid,
            "quality_score": self.quality_score,
            "missing_fields": self.missing_fields,
            "invalid_fields": self.invalid_fields,
            "missing_evidence": self.missing_evidence,
            "insufficient_evidence": self.insufficient_evidence,
            "fallback_triggered": self.fallback_triggered,
            "fallback_action": self.fallback_action.value if self.fallback_action else None,
            "errors": self.errors,
            "warnings": self.warnings,
            "validated_at": self.validated_at.isoformat(),
        }


class ContractValidator:
    """出力規約バリデータ.

    AI出力を規約に従って検証し、品質・根拠要件を確認。

    Example:
        >>> validator = ContractValidator()
        >>> contract = OutputContract(
        ...     name="quote_output",
        ...     fields=[
        ...         FieldSchema(name="amount", field_type="number", required=True),
        ...         FieldSchema(name="reason", field_type="string", required=True),
        ...     ],
        ...     evidence_rules=[
        ...         EvidenceRule(field="amount", min_confidence=0.8),
        ...     ],
        ... )
        >>> result = validator.validate(
        ...     output={"amount": 1000, "reason": "計算結果"},
        ...     contract=contract,
        ...     evidence={"amount": [Evidence(
        ...         evidence_type=EvidenceType.CALCULATION,
        ...         source="price_calculator",
        ...         content={"calculated": 1000},
        ...     )]},
        ... )
    """

    # 型チェック用マッピング
    TYPE_CHECKS: dict[str, type | tuple[type, ...]] = {
        "string": str,
        "number": (int, float),
        "integer": int,
        "boolean": bool,
        "array": list,
        "object": dict,
    }

    def __init__(self, strict_mode: bool = False) -> None:
        """初期化.

        Args:
            strict_mode: 厳格モード（警告もエラーとして扱う）
        """
        self._strict_mode = strict_mode
        self._logger = logging.getLogger(__name__)

    def validate(
        self,
        output: dict[str, Any],
        contract: OutputContract,
        evidence: dict[str, list[Evidence]] | None = None,
    ) -> ValidationResult:
        """出力を規約に従って検証.

        Args:
            output: 検証対象の出力データ
            contract: 適用する出力規約
            evidence: フィールド別の根拠リスト（キー: フィールド名）

        Returns:
            ValidationResult
        """
        result = ValidationResult()
        evidence = evidence or {}

        # 1. フィールド検証
        self._validate_fields(output, contract.fields, result)

        # 2. 根拠要件検証
        self._validate_evidence(output, contract.evidence_rules, evidence, result)

        # 3. 品質スコア計算
        result.quality_score = self._calculate_quality_score(result, contract)

        # 4. 閾値チェックとフォールバック判定
        if result.quality_score < contract.quality_threshold:
            result.is_valid = False
            result.fallback_triggered = True
            result.fallback_action = contract.fallback_action
            result.errors.append(
                f"品質スコア({result.quality_score:.2f})が閾値"
                f"({contract.quality_threshold:.2f})を下回りました"
            )

        # 5. 厳格モード時は警告もエラーとして扱う
        if self._strict_mode and result.warnings:
            result.is_valid = False
            result.errors.extend(result.warnings)

        # 最終判定
        if result.missing_fields or result.errors:
            result.is_valid = False

        self._logger.debug(
            "出力検証完了: contract=%s, valid=%s, score=%.2f",
            contract.name,
            result.is_valid,
            result.quality_score,
        )

        return result

    def _validate_fields(
        self,
        output: dict[str, Any],
        fields: list[FieldSchema],
        result: ValidationResult,
    ) -> None:
        """フィールド検証."""
        for field in fields:
            value = self._get_nested_value(output, field.name)

            # 必須チェック
            if value is None:
                if field.required:
                    result.missing_fields.append(field.name)
                    result.errors.append(f"必須フィールド '{field.name}' が見つかりません")
                continue

            # 型チェック
            expected_type = self.TYPE_CHECKS.get(field.field_type)
            if expected_type and not isinstance(value, expected_type):
                result.invalid_fields.append({
                    "field": field.name,
                    "expected_type": field.field_type,
                    "actual_type": type(value).__name__,
                    "value": str(value)[:100],
                })
                result.errors.append(
                    f"フィールド '{field.name}' の型が不正です"
                    f"（期待: {field.field_type}, 実際: {type(value).__name__}）"
                )
                continue

            # バリデーションルール適用
            self._apply_validation_rules(field, value, result)

    def _apply_validation_rules(
        self,
        field: FieldSchema,
        value: Any,
        result: ValidationResult,
    ) -> None:
        """バリデーションルールを適用."""
        rules = field.validation_rules

        # 最小値
        if "min" in rules and isinstance(value, (int, float)):
            if value < rules["min"]:
                result.invalid_fields.append({
                    "field": field.name,
                    "rule": "min",
                    "expected": rules["min"],
                    "actual": value,
                })
                result.errors.append(
                    f"フィールド '{field.name}' が最小値({rules['min']})を下回っています"
                )

        # 最大値
        if "max" in rules and isinstance(value, (int, float)):
            if value > rules["max"]:
                result.invalid_fields.append({
                    "field": field.name,
                    "rule": "max",
                    "expected": rules["max"],
                    "actual": value,
                })
                result.errors.append(
                    f"フィールド '{field.name}' が最大値({rules['max']})を超えています"
                )

        # 正規表現パターン
        if "pattern" in rules and isinstance(value, str):
            if not re.match(rules["pattern"], value):
                result.invalid_fields.append({
                    "field": field.name,
                    "rule": "pattern",
                    "pattern": rules["pattern"],
                    "value": value[:100],
                })
                result.errors.append(
                    f"フィールド '{field.name}' がパターンに一致しません"
                )

        # 列挙値
        if "enum" in rules:
            if value not in rules["enum"]:
                result.invalid_fields.append({
                    "field": field.name,
                    "rule": "enum",
                    "allowed": rules["enum"],
                    "actual": value,
                })
                result.errors.append(
                    f"フィールド '{field.name}' が許可された値ではありません"
                )

        # 最小長
        if "min_length" in rules and isinstance(value, (str, list)):
            if len(value) < rules["min_length"]:
                result.warnings.append(
                    f"フィールド '{field.name}' が最小長({rules['min_length']})を下回っています"
                )

        # 最大長
        if "max_length" in rules and isinstance(value, (str, list)):
            if len(value) > rules["max_length"]:
                result.warnings.append(
                    f"フィールド '{field.name}' が最大長({rules['max_length']})を超えています"
                )


    def _validate_evidence(
        self,
        output: dict[str, Any],
        rules: list[EvidenceRule],
        evidence: dict[str, list[Evidence]],
        result: ValidationResult,
    ) -> None:
        """根拠要件を検証."""
        for rule in rules:
            field_value = self._get_nested_value(output, rule.field)

            # フィールドが存在しない場合はスキップ
            if field_value is None and not rule.required:
                continue

            # 根拠を取得
            field_evidence = evidence.get(rule.field, [])

            # 必須なのに根拠がない
            if rule.required and not field_evidence:
                result.missing_evidence.append(rule.field)
                result.errors.append(
                    f"フィールド '{rule.field}' に根拠が必要ですが提供されていません"
                )
                continue

            # 根拠がない場合は警告
            if not field_evidence:
                result.warnings.append(
                    f"フィールド '{rule.field}' に根拠がありません"
                )
                continue

            # 各根拠を検証
            for ev in field_evidence:
                # タイプチェック
                if ev.evidence_type not in rule.allowed_types:
                    result.warnings.append(
                        f"フィールド '{rule.field}' の根拠タイプ({ev.evidence_type.value})は"
                        f"許可されていません"
                    )

                # 信頼度チェック
                if ev.confidence < rule.min_confidence:
                    result.insufficient_evidence.append({
                        "field": rule.field,
                        "evidence_type": ev.evidence_type.value,
                        "source": ev.source,
                        "confidence": ev.confidence,
                        "required_confidence": rule.min_confidence,
                    })
                    result.warnings.append(
                        f"フィールド '{rule.field}' の根拠信頼度({ev.confidence:.2f})が"
                        f"要求値({rule.min_confidence:.2f})を下回っています"
                    )

                # ソース必須チェック
                if rule.require_source and not ev.source:
                    result.warnings.append(
                        f"フィールド '{rule.field}' の根拠にソースが指定されていません"
                    )

    def _calculate_quality_score(
        self,
        result: ValidationResult,
        contract: OutputContract,
    ) -> float:
        """品質スコアを計算."""
        score = 1.0

        # 欠落フィールドによる減点
        total_fields = len(contract.fields) if contract.fields else 1
        missing_penalty = len(result.missing_fields) * (0.3 / total_fields)
        score -= missing_penalty

        # 無効フィールドによる減点
        invalid_penalty = len(result.invalid_fields) * 0.15
        score -= invalid_penalty

        # 欠落根拠による減点
        total_rules = len(contract.evidence_rules) if contract.evidence_rules else 1
        evidence_penalty = len(result.missing_evidence) * (0.2 / total_rules)
        score -= evidence_penalty

        # 信頼度不足による減点
        insufficient_penalty = len(result.insufficient_evidence) * 0.05
        score -= insufficient_penalty

        # 警告による減点
        warning_penalty = len(result.warnings) * 0.02
        score -= warning_penalty

        return max(0.0, min(1.0, score))

    def _get_nested_value(self, data: dict[str, Any], path: str) -> Any:
        """ドット記法でネストした値を取得."""
        keys = path.split(".")
        current = data

        for key in keys:
            if isinstance(current, dict):
                current = current.get(key)
            elif isinstance(current, list) and key.isdigit():
                idx = int(key)
                current = current[idx] if idx < len(current) else None
            else:
                return None

            if current is None:
                return None

        return current


# ==========================================================================
# 規約レジストリ（複数規約の管理）
# ==========================================================================
class ContractRegistry:
    """出力規約レジストリ.

    複数の出力規約を管理し、名前やバージョンで検索可能。

    Example:
        >>> registry = ContractRegistry()
        >>> registry.register(quote_contract)
        >>> registry.register(report_contract)
        >>> contract = registry.get("quote_output")
    """

    def __init__(self) -> None:
        """初期化."""
        self._contracts: dict[str, OutputContract] = {}
        self._versions: dict[str, dict[str, OutputContract]] = {}
        self._logger = logging.getLogger(__name__)

    def register(self, contract: OutputContract) -> None:
        """規約を登録."""
        self._contracts[contract.name] = contract

        if contract.name not in self._versions:
            self._versions[contract.name] = {}
        self._versions[contract.name][contract.version] = contract

        self._logger.info(
            "規約登録: name=%s, version=%s",
            contract.name,
            contract.version,
        )

    def get(self, name: str, version: str | None = None) -> OutputContract | None:
        """規約を取得."""
        if version:
            return self._versions.get(name, {}).get(version)
        return self._contracts.get(name)

    def list_all(self) -> list[OutputContract]:
        """全規約を取得."""
        return list(self._contracts.values())

    def list_versions(self, name: str) -> list[str]:
        """規約の全バージョンを取得."""
        return list(self._versions.get(name, {}).keys())


# ==========================================================================
# エクスポート
# ==========================================================================
__all__ = [
    "EvidenceType",
    "FallbackAction",
    "Evidence",
    "EvidenceRule",
    "FieldSchema",
    "OutputContract",
    "ValidationResult",
    "ContractValidator",
    "ContractRegistry",
]

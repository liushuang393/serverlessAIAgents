# -*- coding: utf-8 -*-
"""Agentテンプレート基底クラス.

業界別Agentテンプレートの基底クラスと共通インフラを提供。

【設計原則】
- テンプレートはAgent生成のための設計図
- パラメータ化により柔軟なカスタマイズが可能
- バリデーションルールで安全性を確保
- レジストリパターンでテンプレート管理
"""

from __future__ import annotations

import logging
from abc import ABC, abstractmethod
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

if TYPE_CHECKING:
    from agentflow import ResilientAgent

logger = logging.getLogger(__name__)


class IndustryType(str, Enum):
    """業界タイプ."""

    FINANCE = "finance"
    MANUFACTURING = "manufacturing"
    HEALTHCARE = "healthcare"
    GENERAL = "general"


class TemplateCategory(str, Enum):
    """テンプレートカテゴリ."""

    # 金融
    RISK_ASSESSMENT = "risk_assessment"
    COMPLIANCE = "compliance"
    TRADING = "trading"
    FINANCIAL_ANALYSIS = "financial_analysis"
    # 製造
    QUALITY_CONTROL = "quality_control"
    SUPPLY_CHAIN = "supply_chain"
    PRODUCTION_PLANNING = "production_planning"
    # 医療
    DIAGNOSTIC_SUPPORT = "diagnostic_support"
    TREATMENT_PLANNING = "treatment_planning"
    PATIENT_ANALYSIS = "patient_analysis"
    # 汎用
    GENERAL = "general"


class ValidationRuleType(str, Enum):
    """バリデーションルールタイプ."""

    REQUIRED = "required"
    RANGE = "range"
    REGEX = "regex"
    ENUM = "enum"
    CUSTOM = "custom"


class TemplateValidationRule(BaseModel):
    """テンプレートバリデーションルール."""

    rule_type: ValidationRuleType
    field: str
    params: dict[str, Any] = Field(default_factory=dict)
    error_message: str = ""


class TemplateParameter(BaseModel):
    """テンプレートパラメータ定義."""

    name: str
    description: str
    param_type: str = "string"  # string, int, float, bool, list, dict
    default: Any = None
    required: bool = False
    validation_rules: list[TemplateValidationRule] = Field(default_factory=list)


class TemplateMetadata(BaseModel):
    """テンプレートメタデータ."""

    author: str = "AgentFlow Team"
    version: str = "1.0.0"
    created_at: str = ""
    updated_at: str = ""
    tags: list[str] = Field(default_factory=list)
    dependencies: list[str] = Field(default_factory=list)
    documentation_url: str = ""


class TemplateConfig(BaseModel):
    """テンプレート設定."""

    # LLM設定
    model: str = "gpt-4o"
    temperature: float = 0.4
    max_tokens: int = 4096
    # Agent設定
    timeout_seconds: int = 120
    max_retries: int = 3
    # 追加設定
    extra: dict[str, Any] = Field(default_factory=dict)


class AgentTemplate(ABC, BaseModel):
    """Agentテンプレート基底クラス.

    業界別Agentテンプレートの抽象基底クラス。
    サブクラスは `create_agent` メソッドを実装する必要がある。
    """

    template_id: str
    name: str
    description: str
    industry: IndustryType
    category: TemplateCategory
    parameters: list[TemplateParameter] = Field(default_factory=list)
    validation_rules: list[TemplateValidationRule] = Field(default_factory=list)
    default_config: TemplateConfig = Field(default_factory=TemplateConfig)
    metadata: TemplateMetadata = Field(default_factory=TemplateMetadata)

    class Config:
        """Pydantic設定."""

        arbitrary_types_allowed = True

    @abstractmethod
    def create_agent(
        self,
        config: dict[str, Any] | None = None,
        llm_client: Any = None,
    ) -> ResilientAgent[Any, Any]:
        """テンプレートからAgentを生成.

        Args:
            config: カスタム設定（オプション）
            llm_client: LLMクライアント（オプション）

        Returns:
            生成されたAgent
        """
        ...

    def validate_config(self, config: dict[str, Any]) -> list[str]:
        """設定をバリデート.

        Args:
            config: 検証する設定

        Returns:
            エラーメッセージのリスト（空なら有効）
        """
        errors: list[str] = []
        for rule in self.validation_rules:
            error = self._apply_validation_rule(rule, config)
            if error:
                errors.append(error)
        return errors

    def _apply_validation_rule(
        self, rule: TemplateValidationRule, config: dict[str, Any]
    ) -> str | None:
        """バリデーションルールを適用.

        Args:
            rule: バリデーションルール
            config: 検証する設定

        Returns:
            エラーメッセージ（有効ならNone）
        """
        value = config.get(rule.field)

        if rule.rule_type == ValidationRuleType.REQUIRED:
            if value is None:
                return rule.error_message or f"{rule.field} は必須です"
        elif rule.rule_type == ValidationRuleType.RANGE:
            min_val = rule.params.get("min")
            max_val = rule.params.get("max")
            if value is not None:
                if min_val is not None and value < min_val:
                    return rule.error_message or f"{rule.field} は {min_val} 以上である必要があります"
                if max_val is not None and value > max_val:
                    return rule.error_message or f"{rule.field} は {max_val} 以下である必要があります"
        elif rule.rule_type == ValidationRuleType.ENUM:
            allowed = rule.params.get("values", [])
            if value is not None and value not in allowed:
                return rule.error_message or f"{rule.field} は {allowed} のいずれかである必要があります"
        elif rule.rule_type == ValidationRuleType.REGEX:
            import re

            pattern = rule.params.get("pattern", "")
            if value is not None and not re.match(pattern, str(value)):
                return rule.error_message or f"{rule.field} の形式が不正です"

        return None

    def merge_config(self, custom_config: dict[str, Any] | None) -> TemplateConfig:
        """デフォルト設定とカスタム設定をマージ.

        Args:
            custom_config: カスタム設定

        Returns:
            マージされた設定
        """
        if not custom_config:
            return self.default_config

        merged = self.default_config.model_dump()
        for key, value in custom_config.items():
            if key == "extra":
                merged["extra"].update(value)
            else:
                merged[key] = value

        return TemplateConfig(**merged)

    def get_full_id(self) -> str:
        """フルテンプレートIDを取得.

        Returns:
            フルID（industry.category.template_id形式）
        """
        return f"{self.industry.value}.{self.category.value}.{self.template_id}"


# ========================================
# テンプレートレジストリ
# ========================================


class TemplateRegistry:
    """テンプレートレジストリ.

    シングルトンパターンでテンプレートを管理。
    """

    _instance: TemplateRegistry | None = None
    _templates: dict[str, AgentTemplate]

    def __new__(cls) -> TemplateRegistry:
        """シングルトンインスタンスを返す."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._templates = {}
        return cls._instance

    def register(self, template: AgentTemplate) -> None:
        """テンプレートを登録.

        Args:
            template: 登録するテンプレート
        """
        full_id = template.get_full_id()
        self._templates[full_id] = template
        # 短縮IDでも登録
        self._templates[template.template_id] = template
        logger.info(f"テンプレート登録: {full_id}")

    def get(self, template_id: str) -> AgentTemplate | None:
        """テンプレートを取得.

        Args:
            template_id: テンプレートID

        Returns:
            テンプレート（存在しない場合はNone）
        """
        return self._templates.get(template_id)

    def list_all(
        self,
        industry: IndustryType | None = None,
        category: TemplateCategory | None = None,
    ) -> list[AgentTemplate]:
        """テンプレート一覧を取得.

        Args:
            industry: フィルタする業界
            category: フィルタするカテゴリ

        Returns:
            テンプレートリスト
        """
        # 重複を排除（フルIDと短縮IDの両方で登録されているため）
        unique: dict[str, AgentTemplate] = {}
        for template in self._templates.values():
            if industry and template.industry != industry:
                continue
            if category and template.category != category:
                continue
            unique[template.get_full_id()] = template
        return list(unique.values())

    def clear(self) -> None:
        """全テンプレートをクリア（テスト用）."""
        self._templates.clear()


# ========================================
# ユーティリティ関数
# ========================================


def get_template(template_id: str) -> AgentTemplate | None:
    """テンプレートを取得.

    Args:
        template_id: テンプレートID

    Returns:
        テンプレート（存在しない場合はNone）
    """
    return TemplateRegistry().get(template_id)


def list_templates(
    industry: str | IndustryType | None = None,
    category: str | TemplateCategory | None = None,
) -> list[AgentTemplate]:
    """テンプレート一覧を取得.

    Args:
        industry: フィルタする業界
        category: フィルタするカテゴリ

    Returns:
        テンプレートリスト
    """
    ind = IndustryType(industry) if isinstance(industry, str) else industry
    cat = TemplateCategory(category) if isinstance(category, str) else category
    return TemplateRegistry().list_all(industry=ind, category=cat)


def register_template(template: AgentTemplate) -> None:
    """テンプレートを登録.

    Args:
        template: 登録するテンプレート
    """
    TemplateRegistry().register(template)


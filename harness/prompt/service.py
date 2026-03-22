"""Layer 4 Prompt - プロンプトテンプレート管理サービス.

プロンプトの構築・変数展開・バージョン管理を行う。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any


_logger = logging.getLogger(__name__)


@dataclass
class PromptVariable:
    """プロンプト変数.

    Attributes:
        name: 変数名
        description: 説明
        required: 必須かどうか
        default: デフォルト値
    """

    name: str
    description: str = ""
    required: bool = True
    default: str = ""


@dataclass
class PromptTemplate:
    """プロンプトテンプレート.

    Attributes:
        name: テンプレート名
        template: テンプレート文字列（{variable}形式）
        variables: 変数定義リスト
        version: バージョン
    """

    name: str
    template: str
    variables: list[PromptVariable] = field(default_factory=list)
    version: str = "1.0.0"

    def render(self, values: dict[str, Any]) -> str:
        """テンプレートを展開.

        Args:
            values: 変数値マッピング

        Returns:
            展開済みプロンプト

        Raises:
            ValueError: 必須変数が不足している場合
        """
        for var in self.variables:
            if var.required and var.name not in values and not var.default:
                msg = f"必須変数 '{var.name}' が指定されていません"
                raise ValueError(msg)

        result = self.template
        for var in self.variables:
            value = values.get(var.name, var.default)
            result = result.replace(f"{{{var.name}}}", str(value))
        return result


class PromptBuilder:
    """プロンプトビルダー."""

    def __init__(self) -> None:
        """初期化."""
        self._templates: dict[str, PromptTemplate] = {}

    def register_template(self, template: PromptTemplate) -> None:
        """テンプレートを登録.

        Args:
            template: プロンプトテンプレート
        """
        self._templates[template.name] = template

    def build(self, template_name: str, values: dict[str, Any]) -> str:
        """テンプレートからプロンプトを構築.

        Args:
            template_name: テンプレート名
            values: 変数値

        Returns:
            構築済みプロンプト

        Raises:
            KeyError: テンプレートが見つからない場合
        """
        if template_name not in self._templates:
            msg = f"テンプレート '{template_name}' が登録されていません"
            raise KeyError(msg)
        return self._templates[template_name].render(values)

    def list_templates(self) -> list[str]:
        """登録済みテンプレート名一覧."""
        return list(self._templates.keys())


__all__ = [
    "PromptBuilder",
    "PromptTemplate",
    "PromptVariable",
]

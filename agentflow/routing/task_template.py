# -*- coding: utf-8 -*-
"""Task Template - 再利用可能なタスク定義.

「メール整理」「レポート作成」等の再利用可能なタスクテンプレート。
Skill.md の上位層として、パラメータ化されたタスク定義を提供。

設計原則:
- 宣言的定義: YAML/コードで簡単に定義
- パラメータ抽出: 自然言語からパラメータを抽出
- スキル連携: 必要なスキルを自動連携
- 松耦合: 具体的なスキル実装を知らない

使用例:
    >>> template = TaskTemplate(
    ...     name="email_organize",
    ...     triggers=["メール整理", "受信箱整理"],
    ...     description="メールを重要度別に整理",
    ...     required_skills=["email", "summarizer"],
    ...     parameters=[
    ...         TaskParameter(name="folder", default="inbox"),
    ...         TaskParameter(name="days", pattern=r"(\d+)日", default=7),
    ...     ],
    ... )
    >>> score, params = template.match("過去3日のメールを整理")
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from typing import Any

_logger = logging.getLogger(__name__)


@dataclass
class TaskParameter:
    """タスクパラメータ定義.

    Attributes:
        name: パラメータ名
        description: 説明
        pattern: 抽出用正規表現
        default: デフォルト値
        required: 必須かどうか
        type: 型（str, int, float, bool, list）
    """

    name: str
    description: str = ""
    pattern: str | None = None
    default: Any = None
    required: bool = False
    type: str = "str"


@dataclass
class TaskTemplate:
    """再利用可能なタスクテンプレート.

    Attributes:
        name: テンプレート名（一意識別子）
        triggers: トリガーキーワード/フレーズ
        description: タスクの説明
        required_skills: 必要なスキル名リスト
        parameters: パラメータ定義リスト
        output_format: 出力フォーマット指定
        tags: 分類タグ
    """

    name: str
    triggers: list[str] = field(default_factory=list)
    description: str = ""
    required_skills: list[str] = field(default_factory=list)
    parameters: list[TaskParameter] = field(default_factory=list)
    output_format: str = "text"  # text, markdown, json, summary
    tags: list[str] = field(default_factory=list)
    priority: int = 0  # 高いほど優先

    def match(self, text: str) -> tuple[float, dict[str, Any]]:
        """テキストにマッチングしスコアとパラメータを返す.

        Args:
            text: 入力テキスト（小文字化済み推奨）

        Returns:
            (マッチスコア 0-1, 抽出されたパラメータ)
        """
        text_lower = text.lower()
        score = 0.0
        params: dict[str, Any] = {}

        # トリガーマッチング
        for trigger in self.triggers:
            trigger_lower = trigger.lower()
            if trigger_lower in text_lower:
                # 完全一致ボーナス
                if trigger_lower == text_lower.strip():
                    score = max(score, 1.0)
                else:
                    # 部分一致
                    score = max(score, 0.7 + len(trigger) / 100)
            else:
                # トリガーを分解してキーワードマッチング
                # 例: "メール整理" -> ["メール", "整理"] が両方テキストに含まれるか
                trigger_keywords = self._split_keywords(trigger_lower)
                if len(trigger_keywords) >= 2:
                    matches = sum(1 for kw in trigger_keywords if kw in text_lower)
                    if matches == len(trigger_keywords):
                        # 全キーワードマッチ
                        score = max(score, 0.6 + len(trigger) / 100)
                    elif matches >= len(trigger_keywords) * 0.7:
                        # 部分マッチ
                        score = max(score, 0.4 + matches * 0.1)

        # スコアが低い場合は説明文でも確認
        if score < 0.5 and self.description:
            desc_words = set(self.description.lower().split())
            text_words = set(text_lower.split())
            overlap = len(desc_words & text_words)
            if overlap >= 2:
                score = max(score, 0.3 + overlap * 0.1)

        # パラメータ抽出
        if score > 0:
            for param in self.parameters:
                if param.pattern:
                    match = re.search(param.pattern, text, re.IGNORECASE)
                    if match:
                        value = match.group(1) if match.groups() else match.group()
                        # 型変換
                        params[param.name] = self._convert_type(value, param.type)
                elif param.default is not None:
                    params[param.name] = param.default

        return min(score, 1.0), params

    def _convert_type(self, value: str, target_type: str) -> Any:
        """型変換."""
        try:
            if target_type == "int":
                return int(value)
            elif target_type == "float":
                return float(value)
            elif target_type == "bool":
                return value.lower() in ("true", "1", "yes", "はい", "是")
            elif target_type == "list":
                return [v.strip() for v in value.split(",")]
        except (ValueError, AttributeError):
            pass
        return value

    def _split_keywords(self, text: str) -> list[str]:
        """テキストをキーワードに分割（日本語/中国語対応）.

        例: "メール整理" -> ["メール", "整理"]
            "file organize" -> ["file", "organize"]
        """
        # 英語の場合はスペースで分割
        if text.isascii():
            return [w for w in text.split() if len(w) >= 2]

        # 日本語/中国語: 2文字以上の単位で分割を試みる
        keywords: list[str] = []
        # カタカナ・漢字・英数字の塊で分割
        import re

        parts = re.findall(r"[ァ-ヶー]+|[一-龥々]+|[a-zA-Z0-9]+", text)
        for part in parts:
            if len(part) >= 2:
                keywords.append(part)

        return keywords if keywords else [text]

    def validate_params(self, params: dict[str, Any]) -> list[str]:
        """パラメータを検証しエラーリストを返す."""
        errors: list[str] = []
        for param in self.parameters:
            if param.required and param.name not in params:
                errors.append(f"必須パラメータ '{param.name}' がありません")
        return errors


class TemplateRegistry:
    """テンプレートレジストリ - シングルトン管理.

    使用例:
        >>> registry = TemplateRegistry()
        >>> registry.register(my_template)
        >>> template = registry.get("email_organize")
    """

    _instance: "TemplateRegistry | None" = None

    def __new__(cls) -> "TemplateRegistry":
        """シングルトン."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._templates = {}
        return cls._instance

    def __init__(self) -> None:
        """初期化（シングルトンのため空）."""
        if not hasattr(self, "_templates"):
            self._templates: dict[str, TaskTemplate] = {}

    def register(self, template: TaskTemplate) -> None:
        """テンプレート登録."""
        self._templates[template.name] = template
        _logger.debug("テンプレート登録: %s", template.name)

    def get(self, name: str) -> TaskTemplate | None:
        """テンプレート取得."""
        return self._templates.get(name)

    def list_all(self) -> list[TaskTemplate]:
        """全テンプレート取得."""
        return list(self._templates.values())

    def find_by_tag(self, tag: str) -> list[TaskTemplate]:
        """タグで検索."""
        return [t for t in self._templates.values() if tag in t.tags]

    def clear(self) -> None:
        """全クリア（テスト用）."""
        self._templates.clear()


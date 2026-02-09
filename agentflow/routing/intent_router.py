"""Intent Router - 意図解析とルーティング.

自然言語から意図を解析し、適切なSkill/Agent/テンプレートにルーティングする。
SkillMatcher の上位層として、より高度な意図理解を提供。

設計原則:
- 多言語対応: 日本語・中国語・英語
- 軽量判定: まずルールベース、必要時のみLLM
- コンテキスト考慮: 会話履歴を活用
- 松耦合: LLMプロバイダーを意識しない

使用例:
    >>> router = IntentRouter()
    >>> intent = await router.route("今日のメールを整理して")
    >>> print(intent.category)  # IntentCategory.TASK_EXECUTION
    >>> print(intent.template_name)  # "email_organize"
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from agentflow.routing.task_template import TaskTemplate

_logger = logging.getLogger(__name__)


class IntentCategory(str, Enum):
    """意図カテゴリ."""

    TASK_EXECUTION = "task_execution"  # タスク実行要求
    INFORMATION_QUERY = "information_query"  # 情報照会
    STATUS_CHECK = "status_check"  # 状態確認
    CONFIGURATION = "configuration"  # 設定変更
    CASUAL_CHAT = "casual_chat"  # 雑談
    CLARIFICATION = "clarification"  # 明確化要求
    UNKNOWN = "unknown"  # 不明


@dataclass
class Intent:
    """解析された意図.

    Attributes:
        category: 意図カテゴリ
        template_name: マッチしたテンプレート名（あれば）
        confidence: 信頼度（0-1）
        parameters: 抽出されたパラメータ
        original_text: 元のテキスト
        rewritten_query: リライト後のクエリ
    """

    category: IntentCategory
    template_name: str | None = None
    confidence: float = 0.0
    parameters: dict[str, Any] = field(default_factory=dict)
    original_text: str = ""
    rewritten_query: str = ""
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class RouterConfig:
    """ルーター設定."""

    confidence_threshold: float = 0.5
    use_llm_fallback: bool = True
    max_templates_to_check: int = 10
    enable_parameter_extraction: bool = True


class IntentRouter:
    """意図解析ルーター.

    自然言語から意図を解析し、適切なテンプレート/スキルにルーティング。
    """

    # タスク実行を示唆するキーワード（多言語）
    _TASK_KEYWORDS = {
        # 日本語
        "整理", "作成", "生成", "調査", "分析", "最適化", "検索",
        "削除", "移動", "コピー", "送信", "確認", "報告", "まとめ",
        # 中国語（日本語と重複しないもの）
        "创建", "调查", "优化", "搜索",
        "删除", "移动", "复制", "发送", "确认", "汇总",
        # 英語
        "organize", "create", "generate", "investigate", "analyze",
        "optimize", "search", "delete", "move", "copy", "send",
        "check", "report", "summarize", "clean", "sort",
    }

    # 情報照会を示唆するキーワード
    _QUERY_KEYWORDS = {
        # 日本語
        "教えて", "どう", "何", "いつ", "どこ", "誰", "なぜ",
        # 中国語
        "告诉", "怎么", "什么", "什么时候", "哪里", "谁", "为什么",
        # 英語
        "tell me", "what", "when", "where", "who", "why", "how",
    }

    def __init__(
        self,
        config: RouterConfig | None = None,
        templates: list[TaskTemplate] | None = None,
    ) -> None:
        """初期化."""
        self._config = config or RouterConfig()
        self._templates: dict[str, TaskTemplate] = {}
        self._logger = logging.getLogger(__name__)

        # テンプレート登録
        if templates:
            for t in templates:
                self.register_template(t)

    def register_template(self, template: TaskTemplate) -> None:
        """テンプレートを登録."""
        self._templates[template.name] = template
        self._logger.debug("テンプレート登録: %s", template.name)

    async def route(
        self,
        text: str,
        context: dict[str, Any] | None = None,
    ) -> Intent:
        """意図を解析してルーティング."""
        context = context or {}
        text_lower = text.lower()

        # 1. カテゴリ判定
        category = self._classify_category(text_lower)

        # 2. テンプレートマッチング
        template_name, confidence, params = self._match_template(text, text_lower)

        # 3. パラメータ抽出
        if self._config.enable_parameter_extraction and template_name:
            params = self._extract_parameters(text, template_name, params)

        # 4. Intent構築
        return Intent(
            category=category,
            template_name=template_name,
            confidence=confidence,
            parameters=params,
            original_text=text,
            rewritten_query=self._rewrite_query(text),
        )

    def _classify_category(self, text: str) -> IntentCategory:
        """カテゴリを判定."""
        # タスク実行キーワードチェック
        task_score = sum(1 for kw in self._TASK_KEYWORDS if kw in text)
        if task_score >= 1:
            return IntentCategory.TASK_EXECUTION

        # 情報照会キーワードチェック
        query_score = sum(1 for kw in self._QUERY_KEYWORDS if kw in text)
        if query_score >= 1:
            return IntentCategory.INFORMATION_QUERY

        # 疑問形チェック
        if re.search(r"[?？]$", text.strip()):
            return IntentCategory.INFORMATION_QUERY

        return IntentCategory.UNKNOWN

    def _match_template(
        self,
        text: str,
        text_lower: str,
    ) -> tuple[str | None, float, dict[str, Any]]:
        """テンプレートにマッチング."""
        best_match: str | None = None
        best_score = 0.0
        best_params: dict[str, Any] = {}

        for name, template in self._templates.items():
            score, params = template.match(text_lower)
            if score > best_score:
                best_score = score
                best_match = name
                best_params = params

        if best_score >= self._config.confidence_threshold:
            return best_match, best_score, best_params

        return None, 0.0, {}

    def _extract_parameters(
        self,
        text: str,
        template_name: str,
        existing_params: dict[str, Any],
    ) -> dict[str, Any]:
        """パラメータを抽出."""
        params = existing_params.copy()
        template = self._templates.get(template_name)

        if not template:
            return params

        # テンプレートのパラメータ定義に基づいて抽出
        for param in template.parameters:
            if param.name in params:
                continue
            # パターンマッチング
            if param.pattern:
                match = re.search(param.pattern, text, re.IGNORECASE)
                if match:
                    params[param.name] = match.group(1) if match.groups() else match.group()
            # デフォルト値
            elif param.default is not None:
                params[param.name] = param.default

        return params

    def _rewrite_query(self, text: str) -> str:
        """クエリをリライト（検索用）."""
        # 敬語・フィラー除去
        removals = [
            r"(してください|お願いします|教えてください|ください)",
            r"(请|请问|麻烦)",
            r"(please|could you|can you)",
        ]
        result = text
        for pattern in removals:
            result = re.sub(pattern, "", result, flags=re.IGNORECASE)
        return re.sub(r"\s+", " ", result).strip()

    def list_templates(self) -> list[str]:
        """登録済みテンプレート一覧."""
        return list(self._templates.keys())

    def get_template(self, name: str) -> TaskTemplate | None:
        """テンプレート取得."""
        return self._templates.get(name)


# -*- coding: utf-8 -*-
"""Intent Routing モジュール - 意図解析とルーティング.

自然言語から意図を解析し、適切なSkill/Agent/テンプレートにルーティングする。

主な機能:
- IntentRouter: 意図解析とルーティング
- Intent: 解析された意図データ
- TaskTemplate: 再利用可能なタスク定義

使用例:
    >>> from agentflow.routing import IntentRouter, TaskTemplate
    >>> 
    >>> router = IntentRouter()
    >>> router.register_template(TaskTemplate(
    ...     name="email_organize",
    ...     triggers=["メール整理", "受信箱整理", "メールを片付け"],
    ...     description="メールを重要度別に整理",
    ...     required_skills=["email", "summarizer"],
    ... ))
    >>> 
    >>> intent = await router.route("今日のメールを整理して")
    >>> print(intent.template_name)  # "email_organize"
"""

from agentflow.routing.intent_router import (
    IntentRouter,
    Intent,
    IntentCategory,
    RouterConfig,
)
from agentflow.routing.task_template import (
    TaskTemplate,
    TaskParameter,
    TemplateRegistry,
)
from agentflow.routing.executive_summary import (
    ExecutiveSummaryBuilder,
    SummaryConfig,
    ExecutiveSummary,
)

__all__ = [
    # Intent Router
    "IntentRouter",
    "Intent",
    "IntentCategory",
    "RouterConfig",
    # Task Template
    "TaskTemplate",
    "TaskParameter",
    "TemplateRegistry",
    # Executive Summary
    "ExecutiveSummaryBuilder",
    "SummaryConfig",
    "ExecutiveSummary",
]


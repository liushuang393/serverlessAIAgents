# -*- coding: utf-8 -*-
"""Code Migration Agents.

Multi-Agent 架构：各 Agent 各司其职

- TransformAgent: 代码变换（LLM 全权负责翻译）
- TestGenAgent: 测试生成（LLM 全权负责生成测试用例）
- CheckerAgent: 验证（确定性执行 + LLM 分析差异）
- FixerAgent: 自动修复（LLM 全权负责修复代码）
- CoordinatorAgent: 编排（协调各 Agent）
"""

from apps.code_migration_assistant.agents.transform_agent import TransformAgent
from apps.code_migration_assistant.agents.testgen_agent import TestGenAgent
from apps.code_migration_assistant.agents.checker_agent import CheckerAgent
from apps.code_migration_assistant.agents.fixer_agent import FixerAgent
from apps.code_migration_assistant.agents.coordinator_agent import CoordinatorAgent

__all__ = [
    "TransformAgent",
    "TestGenAgent",
    "CheckerAgent",
    "FixerAgent",
    "CoordinatorAgent",
]


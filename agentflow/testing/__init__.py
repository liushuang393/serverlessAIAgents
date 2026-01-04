# -*- coding: utf-8 -*-
"""AgentFlow テストツールモジュール.

Agent のテスト、モック、E2E テストのためのツールを提供します。

機能:
- Agent テストフレームワーク
- Mock LLM Provider
- E2E テストテンプレート
- テストフィクスチャ

使用例:
    >>> from agentflow.testing import AgentTestCase, MockLLM
    >>>
    >>> class TestMyAgent(AgentTestCase):
    ...     async def test_basic_query(self):
    ...         agent = await self.create_agent(MyAgent)
    ...         result = await agent.invoke({"question": "Hello"})
    ...         self.assertIn("response", result)
"""

from agentflow.testing.mock_llm import (
    MockLLMProvider,
    MockResponse,
    MockStreamResponse,
    create_mock_llm,
)
from agentflow.testing.agent_test_framework import (
    AgentTestCase,
    AgentTestRunner,
    TestContext,
)
from agentflow.testing.fixtures import (
    agent_fixture,
    mock_llm_fixture,
    clean_env_fixture,
)

__all__ = [
    # Mock LLM
    "MockLLMProvider",
    "MockResponse",
    "MockStreamResponse",
    "create_mock_llm",
    # Test Framework
    "AgentTestCase",
    "AgentTestRunner",
    "TestContext",
    # Fixtures
    "agent_fixture",
    "mock_llm_fixture",
    "clean_env_fixture",
]


"""Agent テストフレームワークモジュール.

Agent のテストを簡単に行うためのフレームワークを提供します。

特徴:
- 非同期テストサポート
- Agent ライフサイクル管理
- モック注入
- アサーションヘルパー
"""

from __future__ import annotations

import asyncio
import logging
import os
import unittest
from contextlib import asynccontextmanager
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, cast

from agentflow.testing.mock_llm import MockLLMProvider, create_mock_llm


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)


@dataclass
class TestContext:
    """テストコンテキスト.

    Attributes:
        mock_llm: Mock LLM Provider
        env_vars: 設定された環境変数
        agents: 作成された Agent
        cleanup_tasks: クリーンアップタスク
    """

    mock_llm: MockLLMProvider = field(default_factory=MockLLMProvider)
    env_vars: dict[str, str] = field(default_factory=dict)
    agents: list[Any] = field(default_factory=list)
    cleanup_tasks: list[Any] = field(default_factory=list)


class AgentTestCase(unittest.TestCase):
    """Agent テストケース基底クラス.

    Agent のテストを簡単に行うための基底クラス。

    Example:
        >>> class TestMyAgent(AgentTestCase):
        ...     async def test_basic_query(self):
        ...         agent = await self.create_agent(MyAgent)
        ...         result = await agent.invoke({"question": "Hello"})
        ...         self.assertIn("response", result)
    """

    def setUp(self) -> None:
        """テストセットアップ."""
        self.context = TestContext()
        self.mock_llm = create_mock_llm()
        self.context.mock_llm = self.mock_llm

        # 環境変数を保存
        self._original_env = os.environ.copy()

        # Mock LLM をグローバルに設定
        self._setup_mock_llm()

    def tearDown(self) -> None:
        """テストクリーンアップ."""
        # 環境変数を復元
        os.environ.clear()
        os.environ.update(self._original_env)

        # Agent をクリーンアップ
        loop = asyncio.get_event_loop()
        for agent in self.context.agents:
            if hasattr(agent, "cleanup"):
                try:
                    loop.run_until_complete(agent.cleanup())
                except Exception as e:
                    logger.warning(f"Agent cleanup failed: {e}")

        # Mock をリセット
        self._reset_mock_llm()

    def _setup_mock_llm(self) -> None:
        """Mock LLM をセットアップ."""
        # グローバル LLM をモックに置き換え
        from agentflow.providers import llm_provider as llm_provider_module

        llm_provider = cast("Any", llm_provider_module)
        self._original_llm_instance = getattr(llm_provider, "_llm_instance", None)
        llm_provider._llm_instance = self.mock_llm

    def _reset_mock_llm(self) -> None:
        """Mock LLM をリセット."""
        from agentflow.providers import llm_provider as llm_provider_module

        llm_provider = cast("Any", llm_provider_module)
        llm_provider._llm_instance = self._original_llm_instance

    def set_env(self, key: str, value: str) -> None:
        """環境変数を設定.

        Args:
            key: キー
            value: 値
        """
        os.environ[key] = value
        self.context.env_vars[key] = value

    async def create_agent(
        self,
        agent_class: type[Any],
        **kwargs: Any,
    ) -> Any:
        """Agent を作成.

        Args:
            agent_class: Agent クラス
            **kwargs: Agent への引数

        Returns:
            Agent インスタンス
        """
        agent = agent_class(**kwargs)

        if hasattr(agent, "initialize"):
            await agent.initialize()

        self.context.agents.append(agent)
        return agent

    def set_mock_response(self, content: str, **kwargs: Any) -> None:
        """Mock LLM のレスポンスを設定.

        Args:
            content: レスポンス内容
            **kwargs: 追加引数
        """
        self.mock_llm.set_response(content, **kwargs)

    def add_mock_pattern(self, pattern: str, content: str, **kwargs: Any) -> None:
        """Mock LLM のパターンレスポンスを追加.

        Args:
            pattern: 正規表現パターン
            content: レスポンス内容
            **kwargs: 追加引数
        """
        self.mock_llm.add_pattern_response(pattern, content, **kwargs)

    def assert_llm_called(self) -> None:
        """LLM が呼び出されたことを検証."""
        self.mock_llm.assert_called()

    def assert_llm_called_with(self, content: str) -> None:
        """LLM が特定の内容で呼び出されたことを検証.

        Args:
            content: 期待するメッセージ内容
        """
        self.mock_llm.assert_called_with(content)

    def get_llm_calls(self) -> list[Any]:
        """LLM の呼び出し記録を取得.

        Returns:
            呼び出し記録のリスト
        """
        return self.mock_llm.get_calls()

    def assertResponseContains(self, response: dict[str, Any], key: str, substring: str | None = None) -> None:
        """レスポンスに特定のキーと値が含まれることを検証.

        Args:
            response: レスポンス辞書
            key: キー
            substring: 期待するサブ文字列（オプション）
        """
        assert key in response, f"Response should contain '{key}'"
        if substring:
            assert substring in str(response[key]), (
                f"Response['{key}'] should contain '{substring}'"
            )

    def assertResponseSuccess(self, response: dict[str, Any]) -> None:
        """レスポンスが成功を示すことを検証.

        Args:
            response: レスポンス辞書
        """
        if "error" in response:
            self.fail(f"Response contains error: {response['error']}")
        if "success" in response:
            assert response["success"], "Response should indicate success"


class AgentTestRunner:
    """Agent テストランナー.

    複数のテストケースを実行します。

    Example:
        >>> runner = AgentTestRunner()
        >>> results = runner.run_tests(TestMyAgent)
    """

    def __init__(self, verbosity: int = 2) -> None:
        """初期化.

        Args:
            verbosity: 出力の詳細度
        """
        self._verbosity = verbosity
        self._logger = logging.getLogger(__name__)

    def run_tests(
        self,
        test_class: type[AgentTestCase],
        test_names: list[str] | None = None,
    ) -> unittest.TestResult:
        """テストを実行.

        Args:
            test_class: テストクラス
            test_names: 実行するテスト名（None で全て）

        Returns:
            TestResult
        """
        loader = unittest.TestLoader()

        if test_names:
            suite = unittest.TestSuite()
            for name in test_names:
                suite.addTest(test_class(name))
        else:
            suite = loader.loadTestsFromTestCase(test_class)

        runner = unittest.TextTestRunner(verbosity=self._verbosity)
        return runner.run(suite)

    def run_all(self, test_module: Any) -> unittest.TestResult:
        """モジュール内の全テストを実行.

        Args:
            test_module: テストモジュール

        Returns:
            TestResult
        """
        loader = unittest.TestLoader()
        suite = loader.loadTestsFromModule(test_module)
        runner = unittest.TextTestRunner(verbosity=self._verbosity)
        return runner.run(suite)


@asynccontextmanager
async def agent_test_context(
    mock_response: str = "Mock response",
) -> AsyncIterator[TestContext]:
    """Agent テストコンテキストを作成.

    Example:
        >>> async with agent_test_context() as ctx:
        ...     agent = MyAgent()
        ...     result = await agent.invoke({"question": "Hello"})
        ...     ctx.mock_llm.assert_called()

    Args:
        mock_response: デフォルトモックレスポンス

    Yields:
        TestContext
    """
    context = TestContext()
    context.mock_llm = create_mock_llm(mock_response)

    # 環境を保存
    original_env = os.environ.copy()

    # Mock LLM を設定
    from agentflow.providers import llm_provider as llm_provider_module

    llm_provider = cast("Any", llm_provider_module)
    original_llm = getattr(llm_provider, "_llm_instance", None)
    llm_provider._llm_instance = context.mock_llm

    try:
        yield context
    finally:
        # 環境を復元
        os.environ.clear()
        os.environ.update(original_env)
        llm_provider._llm_instance = original_llm

        # Agent をクリーンアップ
        for agent in context.agents:
            if hasattr(agent, "cleanup"):
                try:
                    await agent.cleanup()
                except Exception as e:
                    logger.warning(f"Agent cleanup failed: {e}")

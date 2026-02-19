"""Pytest フィクスチャモジュール.

Pytest で使用するフィクスチャを提供します。

使用例:
    >>> # conftest.py で
    >>> from agentflow.testing.fixtures import mock_llm_fixture, agent_fixture
    >>>
    >>> @pytest.fixture
    >>> def mock_llm(mock_llm_fixture):
    ...     return mock_llm_fixture
"""

from __future__ import annotations

import os
from contextlib import contextmanager
from typing import TYPE_CHECKING, Any

from agentflow.testing.mock_llm import MockLLMProvider, create_mock_llm


if TYPE_CHECKING:
    from collections.abc import Generator


def mock_llm_fixture(
    default_response: str = "Mock response",
) -> Generator[MockLLMProvider]:
    """Mock LLM フィクスチャ.

    Args:
        default_response: デフォルトレスポンス

    Yields:
        MockLLMProvider インスタンス
    """
    mock = create_mock_llm(default_response)

    # グローバル LLM を置き換え
    from agentflow.providers import llm_provider

    original_instance = llm_provider._llm_instance
    llm_provider._llm_instance = mock

    try:
        yield mock
    finally:
        llm_provider._llm_instance = original_instance


@contextmanager
def agent_fixture(
    agent_class: type,
    **kwargs: Any,
) -> Generator[Any]:
    """Agent フィクスチャ.

    Args:
        agent_class: Agent クラス
        **kwargs: Agent への引数

    Yields:
        Agent インスタンス
    """
    import asyncio

    agent = agent_class(**kwargs)
    loop = asyncio.get_event_loop()

    # 初期化
    if hasattr(agent, "initialize"):
        loop.run_until_complete(agent.initialize())

    try:
        yield agent
    finally:
        # クリーンアップ
        if hasattr(agent, "cleanup"):
            loop.run_until_complete(agent.cleanup())


@contextmanager
def clean_env_fixture(
    env_vars: dict[str, str] | None = None,
) -> Generator[dict[str, str]]:
    """クリーンな環境変数フィクスチャ.

    Args:
        env_vars: 設定する環境変数

    Yields:
        設定された環境変数
    """
    # 元の環境を保存
    original_env = os.environ.copy()

    # 環境をクリア（オプション：必要に応じて）
    if env_vars:
        for key, value in env_vars.items():
            os.environ[key] = value

    try:
        yield env_vars or {}
    finally:
        # 環境を復元
        os.environ.clear()
        os.environ.update(original_env)


# Pytest フィクスチャとして使用する場合のヘルパー
def pytest_fixtures() -> dict[str, Any]:
    """Pytest フィクスチャを生成.

    conftest.py で使用:
        >>> from agentflow.testing.fixtures import pytest_fixtures
        >>> fixtures = pytest_fixtures()
        >>> mock_llm = fixtures["mock_llm"]

    Returns:
        フィクスチャ辞書
    """
    try:
        import pytest
    except ImportError:
        return {}

    fixtures: dict[str, Any] = {}

    @pytest.fixture
    def mock_llm() -> Generator[MockLLMProvider]:
        """Mock LLM フィクスチャ."""
        yield from mock_llm_fixture()

    @pytest.fixture
    def clean_env() -> Generator[dict[str, str]]:
        """クリーン環境フィクスチャ."""
        with clean_env_fixture() as env:
            yield env

    fixtures["mock_llm"] = mock_llm
    fixtures["clean_env"] = clean_env

    return fixtures


# conftest.py 用テンプレート
CONFTEST_TEMPLATE = '''# -*- coding: utf-8 -*-
"""Pytest configuration and shared fixtures."""

import pytest
from agentflow.testing import MockLLMProvider, create_mock_llm


@pytest.fixture
def mock_llm():
    """Mock LLM Provider fixture."""
    mock = create_mock_llm("Default mock response")

    # グローバル LLM を置き換え
    from agentflow.providers import llm_provider
    original_instance = llm_provider._llm_instance
    llm_provider._llm_instance = mock

    yield mock

    llm_provider._llm_instance = original_instance


@pytest.fixture
def clean_env():
    """Clean environment fixture."""
    import os
    original_env = os.environ.copy()

    yield {}

    os.environ.clear()
    os.environ.update(original_env)


@pytest.fixture
async def agent_context():
    """Agent test context fixture."""
    from agentflow.testing.agent_test_framework import agent_test_context

    async with agent_test_context() as ctx:
        yield ctx
'''


def generate_conftest(output_path: str = "tests/conftest.py") -> str:
    """conftest.py を生成.

    Args:
        output_path: 出力パス

    Returns:
        生成されたコンテンツ
    """
    from pathlib import Path

    path = Path(output_path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(CONFTEST_TEMPLATE, encoding="utf-8")

    return CONFTEST_TEMPLATE

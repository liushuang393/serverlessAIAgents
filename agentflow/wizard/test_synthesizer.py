"""TestSynthesizer - テストケース自動合成.

Agent 仕様からテストケースを自動生成します。

使用例:
    >>> synthesizer = TestSynthesizer()
    >>> test_cases = await synthesizer.synthesize(agent_spec)
    >>> for test in test_cases:
    ...     print(f"{test.name}: {test.description}")
"""

from __future__ import annotations

import logging
from typing import Any

from agentflow.providers import get_llm
from agentflow.wizard.models import AgentSpec, TestCase


class TestSynthesizer:
    """テストケース自動合成.

    Agent 仕様に基づいてテストケースを自動生成します。
    """

    def __init__(
        self,
        llm_client: Any = None,
        min_tests: int = 3,
        max_tests: int = 10,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント
            min_tests: 最小テスト数
            max_tests: 最大テスト数
        """
        self._llm = llm_client or get_llm()
        self._min_tests = min_tests
        self._max_tests = max_tests
        self._logger = logging.getLogger(__name__)

    async def synthesize(
        self,
        agent_spec: AgentSpec,
        *,
        include_edge_cases: bool = True,
        include_error_cases: bool = True,
    ) -> list[TestCase]:
        """テストケースを合成.

        Args:
            agent_spec: Agent 仕様
            include_edge_cases: エッジケースを含めるか
            include_error_cases: エラーケースを含めるか

        Returns:
            生成されたテストケースリスト
        """
        test_cases: list[TestCase] = []

        # 1. 基本テストケースを生成
        basic_tests = await self._generate_basic_tests(agent_spec)
        test_cases.extend(basic_tests)

        # 2. エッジケースを生成
        if include_edge_cases:
            edge_tests = await self._generate_edge_cases(agent_spec)
            test_cases.extend(edge_tests)

        # 3. エラーケースを生成
        if include_error_cases:
            error_tests = await self._generate_error_cases(agent_spec)
            test_cases.extend(error_tests)

        # 4. 能力別テストを生成
        capability_tests = await self._generate_capability_tests(agent_spec)
        test_cases.extend(capability_tests)

        # テスト数を制限
        if len(test_cases) > self._max_tests:
            test_cases = test_cases[:self._max_tests]

        self._logger.info(f"Synthesized {len(test_cases)} test cases for {agent_spec.name}")
        return test_cases

    async def _generate_basic_tests(self, agent_spec: AgentSpec) -> list[TestCase]:
        """基本テストケースを生成.

        Args:
            agent_spec: Agent 仕様

        Returns:
            基本テストケースリスト
        """
        tests = []

        # 正常系テスト
        tests.append(TestCase(
            name=f"test_{agent_spec.name.lower()}_basic_execution",
            description=f"Test basic execution of {agent_spec.name}",
            input_data={"query": "test input"},
            expected_output={"success": True},
            assertions=[
                "result is not None",
                "'success' in result",
                "result['success'] == True",
            ],
            tags=["basic", "smoke"],
        ))

        # 空入力テスト
        tests.append(TestCase(
            name=f"test_{agent_spec.name.lower()}_empty_input",
            description=f"Test {agent_spec.name} with empty input",
            input_data={},
            expected_output={},
            assertions=[
                "result is not None",
            ],
            tags=["basic", "edge"],
        ))

        return tests

    async def _generate_edge_cases(self, agent_spec: AgentSpec) -> list[TestCase]:
        """エッジケースを生成.

        Args:
            agent_spec: Agent 仕様

        Returns:
            エッジケーステストリスト
        """
        tests = []

        # 長い入力テスト
        tests.append(TestCase(
            name=f"test_{agent_spec.name.lower()}_long_input",
            description=f"Test {agent_spec.name} with long input",
            input_data={"query": "x" * 10000},
            expected_output={},
            assertions=[
                "result is not None",
            ],
            tags=["edge", "stress"],
            timeout_seconds=60,
        ))

        # 特殊文字テスト
        tests.append(TestCase(
            name=f"test_{agent_spec.name.lower()}_special_chars",
            description=f"Test {agent_spec.name} with special characters",
            input_data={"query": '!@#$%^&*(){}[]|\\:";<>?,./~`'},
            expected_output={},
            assertions=[
                "result is not None",
            ],
            tags=["edge", "special"],
        ))

        # Unicode テスト
        tests.append(TestCase(
            name=f"test_{agent_spec.name.lower()}_unicode",
            description=f"Test {agent_spec.name} with unicode characters",
            input_data={"query": "こんにちは 世界 🌍 مرحبا"},
            expected_output={},
            assertions=[
                "result is not None",
            ],
            tags=["edge", "unicode"],
        ))

        return tests

    async def _generate_error_cases(self, agent_spec: AgentSpec) -> list[TestCase]:
        """エラーケースを生成.

        Args:
            agent_spec: Agent 仕様

        Returns:
            エラーケーステストリスト
        """
        tests = []

        # 不正な型テスト
        tests.append(TestCase(
            name=f"test_{agent_spec.name.lower()}_invalid_type",
            description=f"Test {agent_spec.name} with invalid input type",
            input_data={"query": 12345},  # 文字列でなく数値
            expected_output={},
            assertions=[
                "result is not None",
            ],
            tags=["error", "type"],
        ))

        # None 値テスト
        tests.append(TestCase(
            name=f"test_{agent_spec.name.lower()}_none_value",
            description=f"Test {agent_spec.name} with None value",
            input_data={"query": None},
            expected_output={},
            assertions=[
                "result is not None",
            ],
            tags=["error", "null"],
        ))

        return tests

    async def _generate_capability_tests(self, agent_spec: AgentSpec) -> list[TestCase]:
        """能力別テストを生成.

        Args:
            agent_spec: Agent 仕様

        Returns:
            能力別テストリスト
        """
        tests = []

        for i, capability in enumerate(agent_spec.capabilities[:5]):
            tests.append(TestCase(
                name=f"test_{agent_spec.name.lower()}_capability_{i+1}",
                description=f"Test capability: {capability}",
                input_data={"query": f"Test {capability}"},
                expected_output={},
                assertions=[
                    "result is not None",
                    "'success' in result or 'error' not in result",
                ],
                tags=["capability", f"cap_{i+1}"],
            ))

        return tests

    async def generate_from_description(
        self,
        description: str,
        num_tests: int = 5,
    ) -> list[TestCase]:
        """説明からテストケースを生成.

        LLM を使用して説明からテストケースを生成します。

        Args:
            description: テスト対象の説明
            num_tests: 生成するテスト数

        Returns:
            生成されたテストケースリスト
        """
        prompt = f"""以下の Agent の説明に基づいて、{num_tests} 個のテストケースを生成してください。

説明: {description}

各テストケースは以下の JSON 形式で回答してください:
[
    {{
        "name": "test_name",
        "description": "テストの説明",
        "input_data": {{"key": "value"}},
        "expected_output": {{"success": true}},
        "assertions": ["assertion1", "assertion2"]
    }}
]"""

        response = await self._llm.generate(prompt)
        content = response.content if hasattr(response, "content") else str(response)

        # JSON 抽出とパース
        import json
        try:
            start = content.find("[")
            end = content.rfind("]") + 1
            if start >= 0 and end > start:
                tests_data = json.loads(content[start:end])
                return [
                    TestCase(
                        name=t.get("name", f"test_{i}"),
                        description=t.get("description", ""),
                        input_data=t.get("input_data", {}),
                        expected_output=t.get("expected_output", {}),
                        assertions=t.get("assertions", []),
                    )
                    for i, t in enumerate(tests_data)
                ]
        except json.JSONDecodeError:
            pass

        # フォールバック
        return [
            TestCase(
                name="test_basic",
                description="Basic test",
                input_data={"query": "test"},
                expected_output={"success": True},
            )
        ]


__all__ = ["TestSynthesizer"]

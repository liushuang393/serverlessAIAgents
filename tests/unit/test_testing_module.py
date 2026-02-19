"""Testing 模块的完整测试.

覆盖 mock_llm.py, agent_test_framework.py, fixtures.py
"""

import asyncio
import unittest


class TestMockResponse(unittest.TestCase):
    """MockResponse 的测试."""

    def test_creation_with_defaults(self):
        """默认值创建测试."""
        from agentflow.testing.mock_llm import MockResponse

        response = MockResponse(content="Hello")
        self.assertEqual(response.content, "Hello")
        self.assertEqual(response.model, "mock-model")
        self.assertIn("prompt_tokens", response.usage)

    def test_to_dict(self):
        """转字典测试."""
        from agentflow.testing.mock_llm import MockResponse

        response = MockResponse(
            content="Test",
            model="gpt-4",
            metadata={"extra": "data"},
        )
        data = response.to_dict()

        self.assertEqual(data["content"], "Test")
        self.assertEqual(data["model"], "gpt-4")
        self.assertEqual(data["extra"], "data")


class TestCallRecord(unittest.TestCase):
    """CallRecord 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.testing.mock_llm import CallRecord, MockResponse

        record = CallRecord(
            messages=[{"role": "user", "content": "Hello"}],
            kwargs={"temperature": 0.5},
            response=MockResponse(content="Hi"),
        )

        self.assertEqual(len(record.messages), 1)
        self.assertEqual(record.kwargs["temperature"], 0.5)


class TestMockLLMProvider(unittest.TestCase):
    """MockLLMProvider 的完整测试."""

    def test_default_response(self):
        """默认响应测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            response = loop.run_until_complete(provider.chat([{"role": "user", "content": "Hi"}]))
            self.assertEqual(response["content"], "Mock response")
        finally:
            loop.close()

    def test_set_response(self):
        """设置响应测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()
        provider.set_response("Custom response")

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            response = loop.run_until_complete(provider.chat([{"role": "user", "content": "test"}]))
            self.assertEqual(response["content"], "Custom response")
        finally:
            loop.close()

    def test_pattern_response(self):
        """模式响应测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()
        provider.add_pattern_response("weather", "It's sunny!")
        provider.add_pattern_response("time", "It's noon.")

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            response = loop.run_until_complete(
                provider.chat([{"role": "user", "content": "What's the weather?"}])
            )
            self.assertEqual(response["content"], "It's sunny!")

            response = loop.run_until_complete(
                provider.chat([{"role": "user", "content": "other question"}])
            )
            self.assertEqual(response["content"], "Mock response")
        finally:
            loop.close()

    def test_sequence_response(self):
        """序列响应测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()
        provider.add_sequence_response("First")
        provider.add_sequence_response("Second")
        provider.add_sequence_response("Third")

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            r1 = loop.run_until_complete(provider.chat([{"role": "user", "content": "test"}]))
            r2 = loop.run_until_complete(provider.chat([{"role": "user", "content": "test"}]))
            r3 = loop.run_until_complete(provider.chat([{"role": "user", "content": "test"}]))

            self.assertEqual(r1["content"], "First")
            self.assertEqual(r2["content"], "Second")
            self.assertEqual(r3["content"], "Third")
        finally:
            loop.close()

    def test_call_count(self):
        """调用计数测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(provider.chat([{"role": "user", "content": "Hello"}]))
            loop.run_until_complete(provider.chat([{"role": "user", "content": "World"}]))

            self.assertEqual(provider.get_call_count(), 2)
        finally:
            loop.close()

    def test_get_calls(self):
        """获取调用记录测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(provider.chat([{"role": "user", "content": "Hello"}]))

            calls = provider.get_calls()
            self.assertEqual(len(calls), 1)
            self.assertEqual(calls[0].messages[0]["content"], "Hello")
        finally:
            loop.close()

    def test_get_last_call(self):
        """获取最后调用测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(provider.chat([{"role": "user", "content": "First"}]))
            loop.run_until_complete(provider.chat([{"role": "user", "content": "Last"}]))

            last = provider.get_last_call()
            self.assertEqual(last.messages[0]["content"], "Last")
        finally:
            loop.close()

    def test_assert_called(self):
        """断言调用测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()

        with self.assertRaises(AssertionError):
            provider.assert_called()

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(provider.chat([{"role": "user", "content": "test"}]))
            provider.assert_called()  # Should not raise
        finally:
            loop.close()

    def test_assert_called_with(self):
        """断言调用内容测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(provider.chat([{"role": "user", "content": "Hello World"}]))

            provider.assert_called_with("Hello")
            provider.assert_called_with("World")

            with self.assertRaises(AssertionError):
                provider.assert_called_with("Goodbye")
        finally:
            loop.close()

    def test_reset(self):
        """重置测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(provider.chat([{"role": "user", "content": "test"}]))
            self.assertEqual(provider.get_call_count(), 1)

            provider.reset()
            self.assertEqual(provider.get_call_count(), 0)
        finally:
            loop.close()

    def test_stream(self):
        """流式响应测试."""
        from agentflow.testing.mock_llm import MockLLMProvider

        provider = MockLLMProvider()
        provider.set_response("Hello World")

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:

            async def collect_stream():
                chunks = []
                async for chunk in provider.stream([{"role": "user", "content": "test"}]):
                    chunks.append(chunk)
                return "".join(chunks)

            result = loop.run_until_complete(collect_stream())
            self.assertEqual(result, "Hello World")
        finally:
            loop.close()

    def test_response_callback(self):
        """响应回调测试."""
        from agentflow.testing.mock_llm import MockLLMProvider, MockResponse

        provider = MockLLMProvider()

        def custom_callback(messages):
            content = messages[-1]["content"]
            return MockResponse(content=f"Echo: {content}")

        provider.set_response_callback(custom_callback)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            response = loop.run_until_complete(provider.chat([{"role": "user", "content": "test"}]))
            self.assertEqual(response["content"], "Echo: test")
        finally:
            loop.close()


class TestCreateMockLLM(unittest.TestCase):
    """create_mock_llm 工厂函数的测试."""

    def test_default_creation(self):
        """默认创建测试."""
        from agentflow.testing.mock_llm import create_mock_llm

        provider = create_mock_llm()

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            response = loop.run_until_complete(provider.chat([{"role": "user", "content": "test"}]))
            self.assertEqual(response["content"], "Mock response")
        finally:
            loop.close()

    def test_creation_with_params(self):
        """带参数创建测试."""
        from agentflow.testing.mock_llm import create_mock_llm

        provider = create_mock_llm(default_response="Custom")

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            response = loop.run_until_complete(provider.chat([{"role": "user", "content": "test"}]))
            self.assertEqual(response["content"], "Custom")
        finally:
            loop.close()


class TestTestContext(unittest.TestCase):
    """TestContext 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.testing.agent_test_framework import TestContext

        ctx = TestContext()
        self.assertIsNotNone(ctx.mock_llm)
        self.assertEqual(ctx.env_vars, {})
        self.assertEqual(ctx.agents, [])


class TestAgentTestCase(unittest.TestCase):
    """AgentTestCase 的测试."""

    def test_subclass_creation(self):
        """子类创建测试."""
        from agentflow.testing.agent_test_framework import AgentTestCase

        class MyTestCase(AgentTestCase):
            pass

        # Should be able to instantiate
        test = MyTestCase()
        self.assertIsInstance(test, unittest.TestCase)


class TestAgentTestRunner(unittest.TestCase):
    """AgentTestRunner 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.testing.agent_test_framework import AgentTestRunner

        runner = AgentTestRunner(verbosity=1)
        self.assertEqual(runner._verbosity, 1)


class TestFixtures(unittest.TestCase):
    """fixtures 模块的测试."""

    def test_clean_env_fixture(self):
        """Clean Env Fixture 测试."""
        import os

        from agentflow.testing.fixtures import clean_env_fixture

        original_val = os.environ.get("TEST_VAR")

        with clean_env_fixture({"TEST_VAR": "test_value"}) as env:
            self.assertEqual(os.environ.get("TEST_VAR"), "test_value")

        # Should be restored
        self.assertEqual(os.environ.get("TEST_VAR"), original_val)

    def test_generate_conftest(self):
        """生成 conftest 测试."""
        import tempfile
        from pathlib import Path

        from agentflow.testing.fixtures import generate_conftest

        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "tests" / "conftest.py"
            result = generate_conftest(str(output_path))

            self.assertTrue(output_path.exists())
            self.assertIn("mock_llm", result)
            self.assertIn("@pytest.fixture", result)

    def test_conftest_template(self):
        """conftest 模板测试."""
        from agentflow.testing.fixtures import CONFTEST_TEMPLATE

        self.assertIn("@pytest.fixture", CONFTEST_TEMPLATE)
        self.assertIn("mock_llm", CONFTEST_TEMPLATE)
        self.assertIn("clean_env", CONFTEST_TEMPLATE)


class TestAgentTestContext(unittest.TestCase):
    """agent_test_context 的测试."""

    def test_import(self):
        """导入测试."""
        from agentflow.testing.agent_test_framework import TestContext, agent_test_context

        self.assertIsNotNone(agent_test_context)
        self.assertIsNotNone(TestContext)


if __name__ == "__main__":
    unittest.main()

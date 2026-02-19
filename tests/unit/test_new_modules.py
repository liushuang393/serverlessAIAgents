"""Tests for new AgentFlow modules (v0.3.0).

Tests for:
- Knowledge (RAG, Document Loader, Vector Search Hooks)
- Observability (Logging, Metrics, Tracing, Sentry)
- Security (API Key, Rate Limiter, Auth, RBAC)
- Testing (Mock LLM, Test Framework)
- Deploy (Docker, Serverless, CI/CD generators)
"""

import asyncio
import tempfile
import unittest


class TestDocumentLoader(unittest.TestCase):
    """ドキュメントローダーのテスト."""

    def test_document_chunk_create(self):
        """DocumentChunk の作成テスト."""
        from agentflow.knowledge.document_loader import DocumentChunk

        chunk = DocumentChunk.create(
            content="Test content",
            source="test.txt",
            chunk_index=0,
            total_chunks=1,
        )

        self.assertIsNotNone(chunk.id)
        self.assertEqual(chunk.content, "Test content")
        self.assertEqual(chunk.source, "test.txt")
        self.assertEqual(chunk.chunk_index, 0)
        self.assertEqual(chunk.total_chunks, 1)

    def test_text_loader_supports(self):
        """TextLoader のサポート判定テスト."""
        from agentflow.knowledge.document_loader import TextLoader

        loader = TextLoader()
        self.assertTrue(loader.supports("test.txt"))
        self.assertTrue(loader.supports("test.text"))
        self.assertFalse(loader.supports("test.pdf"))

    def test_markdown_loader_supports(self):
        """MarkdownLoader のサポート判定テスト."""
        from agentflow.knowledge.document_loader import MarkdownLoader

        loader = MarkdownLoader()
        self.assertTrue(loader.supports("test.md"))
        self.assertTrue(loader.supports("test.markdown"))
        self.assertFalse(loader.supports("test.txt"))


class TestRAGConfig(unittest.TestCase):
    """RAG 設定のテスト."""

    def test_rag_config_defaults(self):
        """RAGConfig のデフォルト値テスト."""
        from agentflow.knowledge.rag_pipeline import RAGConfig

        config = RAGConfig()
        self.assertEqual(config.top_k, 5)
        self.assertEqual(config.min_similarity, 0.3)
        self.assertIsNotNone(config.system_prompt)

    def test_rag_result(self):
        """RAGResult の作成テスト."""
        from agentflow.knowledge.rag_pipeline import RAGResult

        result = RAGResult(
            answer="Test answer",
            sources=[{"id": "1", "content": "source"}],
            query="Test query",
        )

        self.assertEqual(result.answer, "Test answer")
        self.assertEqual(len(result.sources), 1)
        self.assertEqual(result.query, "Test query")


class TestObservabilityLogging(unittest.TestCase):
    """ログモジュールのテスト."""

    def test_log_level_enum(self):
        """LogLevel 列挙型テスト."""
        from agentflow.observability.logging import LogLevel

        self.assertEqual(LogLevel.DEBUG.value, "DEBUG")
        self.assertEqual(LogLevel.INFO.value, "INFO")
        self.assertEqual(LogLevel.ERROR.value, "ERROR")

    def test_logger_creation(self):
        """AgentFlowLogger の作成テスト."""
        from agentflow.observability.logging import AgentFlowLogger

        logger = AgentFlowLogger("test-logger")
        self.assertIsNotNone(logger)


class TestMetrics(unittest.TestCase):
    """メトリクスモジュールのテスト."""

    def test_counter_increment(self):
        """Counter のインクリメントテスト."""
        from agentflow.observability.metrics import Counter

        counter = Counter("test_counter", "Test counter")
        counter.inc()
        self.assertEqual(counter.get(), 1)
        counter.inc(5)
        self.assertEqual(counter.get(), 6)

    def test_gauge_set(self):
        """Gauge の設定テスト."""
        from agentflow.observability.metrics import Gauge

        gauge = Gauge("test_gauge", "Test gauge")
        gauge.set(10)
        self.assertEqual(gauge.get(), 10)
        gauge.inc()
        self.assertEqual(gauge.get(), 11)
        gauge.dec(5)
        self.assertEqual(gauge.get(), 6)

    def test_histogram_observe(self):
        """Histogram の観測テスト."""
        from agentflow.observability.metrics import Histogram

        histogram = Histogram("test_histogram", "Test histogram")
        histogram.observe(0.5)
        histogram.observe(1.0)
        histogram.observe(1.5)

        stats = histogram.get_stats()
        self.assertEqual(stats["count"], 3)
        self.assertEqual(stats["sum"], 3.0)
        self.assertEqual(stats["avg"], 1.0)


class TestTracing(unittest.TestCase):
    """トレーシングモジュールのテスト."""

    def test_span_context_generate(self):
        """SpanContext の生成テスト."""
        from agentflow.observability.tracing import SpanContext

        context = SpanContext.generate()
        self.assertIsNotNone(context.trace_id)
        self.assertIsNotNone(context.span_id)
        self.assertIsNone(context.parent_span_id)

    def test_span_context_with_parent(self):
        """親付きSpanContext の生成テスト."""
        from agentflow.observability.tracing import SpanContext

        parent = SpanContext.generate()
        child = SpanContext.generate(parent)

        self.assertEqual(child.trace_id, parent.trace_id)
        self.assertEqual(child.parent_span_id, parent.span_id)

    def test_tracer_span(self):
        """Tracer のスパンテスト."""
        from agentflow.observability.tracing import Tracer

        tracer = Tracer("test-service")
        with tracer.span("test-operation") as span:
            span.set_attribute("key", "value")
            span.add_event("test-event")

        self.assertEqual(span.status, "ok")
        self.assertIsNotNone(span.end_time)


class TestAPIKeyManager(unittest.TestCase):
    """API Key マネージャーのテスト."""

    def test_generate_api_key(self):
        """API Key 生成テスト."""
        from agentflow.security.api_key import generate_api_key

        key = generate_api_key()
        self.assertTrue(key.startswith("sk-"))
        self.assertGreater(len(key), 10)

    def test_api_key_manager_create_validate(self):
        """API Key の作成と検証テスト."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        raw_key, api_key = manager.create_key("test-key", scopes=["read", "write"])

        # 検証
        validated = manager.validate(raw_key)
        self.assertIsNotNone(validated)
        self.assertEqual(validated.name, "test-key")
        self.assertIn("read", validated.scopes)

    def test_api_key_manager_revoke(self):
        """API Key の無効化テスト."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        raw_key, api_key = manager.create_key("test-key")

        # 無効化
        manager.revoke(api_key.id)

        # 検証失敗
        validated = manager.validate(raw_key)
        self.assertIsNone(validated)


class TestRateLimiter(unittest.TestCase):
    """レート制限のテスト."""

    def test_rate_limiter_allow(self):
        """レート制限（許可）テスト."""
        from agentflow.security.rate_limiter import RateLimiter

        limiter = RateLimiter(requests_per_minute=10)

        # 非同期関数を同期的に実行
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            result = loop.run_until_complete(limiter.check("user-1"))
            self.assertTrue(result)
        finally:
            loop.close()

    def test_rate_limiter_deny(self):
        """レート制限（拒否）テスト."""
        from agentflow.security.rate_limiter import RateLimiter

        limiter = RateLimiter(requests_per_minute=2)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            # 2回は許可
            loop.run_until_complete(limiter.check("user-1"))
            loop.run_until_complete(limiter.check("user-1"))
            # 3回目は拒否
            result = loop.run_until_complete(limiter.check("user-1"))
            self.assertFalse(result)
        finally:
            loop.close()


class TestRBAC(unittest.TestCase):
    """RBAC のテスト."""

    def test_rbac_default_roles(self):
        """デフォルトロールテスト."""
        from agentflow.security.rbac import RBACManager

        rbac = RBACManager()
        roles = rbac.list_roles()
        role_names = [r.name for r in roles]

        self.assertIn("admin", role_names)
        self.assertIn("user", role_names)
        self.assertIn("readonly", role_names)

    def test_rbac_permission_check(self):
        """パーミッションチェックテスト."""
        from agentflow.security.rbac import RBACManager

        rbac = RBACManager()
        rbac.assign_role("user-1", "admin")

        # admin は全権限
        self.assertTrue(rbac.has_permission("user-1", "anything"))
        self.assertTrue(rbac.has_permission("user-1", "read"))
        self.assertTrue(rbac.has_permission("user-1", "write"))


class TestMockLLM(unittest.TestCase):
    """Mock LLM のテスト."""

    def test_mock_llm_response(self):
        """Mock LLM レスポンステスト."""
        from agentflow.testing.mock_llm import create_mock_llm

        mock = create_mock_llm("Hello, I'm a mock!")

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            response = loop.run_until_complete(mock.chat([{"role": "user", "content": "Hi"}]))
            self.assertEqual(response["content"], "Hello, I'm a mock!")
        finally:
            loop.close()

    def test_mock_llm_pattern_response(self):
        """Mock LLM パターンレスポンステスト."""
        from agentflow.testing.mock_llm import MockLLMProvider

        mock = MockLLMProvider()
        mock.set_response("Default response")
        mock.add_pattern_response(r"hello", "Hello pattern!")

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            response = loop.run_until_complete(
                mock.chat([{"role": "user", "content": "hello world"}])
            )
            self.assertEqual(response["content"], "Hello pattern!")
        finally:
            loop.close()

    def test_mock_llm_call_tracking(self):
        """Mock LLM 呼び出し追跡テスト."""
        from agentflow.testing.mock_llm import MockLLMProvider

        mock = MockLLMProvider()

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(mock.chat([{"role": "user", "content": "Test"}]))
            self.assertEqual(mock.get_call_count(), 1)
            mock.assert_called()
            mock.assert_called_with("Test")
        finally:
            loop.close()


class TestDeployGenerators(unittest.TestCase):
    """デプロイジェネレーターのテスト."""

    def test_docker_config_defaults(self):
        """DockerConfig デフォルト値テスト."""
        from agentflow.deploy.docker_generator import DockerConfig

        config = DockerConfig()
        self.assertEqual(config.python_version, "3.13")
        self.assertEqual(config.port, 8000)

    def test_generate_dockerfile(self):
        """Dockerfile 生成テスト."""
        from agentflow.deploy.docker_generator import DockerConfig, generate_dockerfile

        with tempfile.TemporaryDirectory() as tmpdir:
            config = DockerConfig(app_name="test-app")
            path = generate_dockerfile(tmpdir, config)

            self.assertTrue(path.exists())
            content = path.read_text()
            self.assertIn("test-app", content)
            self.assertIn("python:3.13", content)

    def test_generate_github_actions(self):
        """GitHub Actions 生成テスト."""
        from agentflow.deploy.ci_cd_generator import CICDConfig, generate_github_actions

        with tempfile.TemporaryDirectory() as tmpdir:
            config = CICDConfig(app_name="test-app")
            path = generate_github_actions(tmpdir, config)

            self.assertTrue(path.exists())
            content = path.read_text()
            self.assertIn("test-app", content)
            self.assertIn("pytest", content)


if __name__ == "__main__":
    unittest.main()

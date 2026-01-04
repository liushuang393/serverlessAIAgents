# -*- coding: utf-8 -*-
"""Observability 模块的完整测试.

覆盖 logging.py, metrics.py, tracing.py, sentry_integration.py, setup.py
"""

import asyncio
import json
import logging
import tempfile
import threading
import time
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch


class TestLogLevel(unittest.TestCase):
    """LogLevel 枚举的测试."""

    def test_all_levels(self):
        """所有日志级别测试."""
        from agentflow.observability.logging import LogLevel

        self.assertEqual(LogLevel.DEBUG.value, "DEBUG")
        self.assertEqual(LogLevel.INFO.value, "INFO")
        self.assertEqual(LogLevel.WARNING.value, "WARNING")
        self.assertEqual(LogLevel.ERROR.value, "ERROR")
        self.assertEqual(LogLevel.CRITICAL.value, "CRITICAL")


class TestLogConfig(unittest.TestCase):
    """LogConfig 的测试."""

    def test_defaults(self):
        """默认值测试."""
        from agentflow.observability.logging import LogConfig, LogLevel

        config = LogConfig()
        self.assertEqual(config.level, LogLevel.INFO)
        self.assertEqual(config.format, "json")
        self.assertEqual(config.output, "stdout")
        self.assertTrue(config.include_timestamp)
        self.assertTrue(config.include_caller)
        self.assertIn("password", config.mask_patterns)

    def test_custom_values(self):
        """自定义值测试."""
        from agentflow.observability.logging import LogConfig, LogLevel

        config = LogConfig(
            level=LogLevel.DEBUG,
            format="text",
            output="stderr",
        )
        self.assertEqual(config.level, LogLevel.DEBUG)
        self.assertEqual(config.format, "text")


class TestJSONFormatter(unittest.TestCase):
    """JSONFormatter 的测试."""

    def test_format_basic(self):
        """基本格式化测试."""
        from agentflow.observability.logging import JSONFormatter

        formatter = JSONFormatter()
        record = logging.LogRecord(
            name="test",
            level=logging.INFO,
            pathname="test.py",
            lineno=10,
            msg="Test message",
            args=(),
            exc_info=None,
        )

        output = formatter.format(record)
        data = json.loads(output)

        self.assertEqual(data["level"], "INFO")
        self.assertEqual(data["message"], "Test message")
        self.assertEqual(data["logger"], "test")
        self.assertIn("timestamp", data)

    def test_mask_sensitive_data(self):
        """敏感数据掩码测试."""
        from agentflow.observability.logging import JSONFormatter

        formatter = JSONFormatter(mask_patterns=["password", "secret"])

        # Check masking logic
        masked = formatter._mask_value("password", "secret123")
        self.assertEqual(masked, "***MASKED***")

        not_masked = formatter._mask_value("username", "john")
        self.assertEqual(not_masked, "john")


class TestTextFormatter(unittest.TestCase):
    """TextFormatter 的测试."""

    def test_format_with_timestamp(self):
        """带时间戳格式化测试."""
        from agentflow.observability.logging import TextFormatter

        formatter = TextFormatter(include_timestamp=True)
        self.assertIn("asctime", formatter._fmt)

    def test_format_without_timestamp(self):
        """不带时间戳格式化测试."""
        from agentflow.observability.logging import TextFormatter

        formatter = TextFormatter(include_timestamp=False)
        self.assertNotIn("asctime", formatter._fmt)


class TestAgentFlowLogger(unittest.TestCase):
    """AgentFlowLogger 的测试."""

    def test_create_logger(self):
        """创建日志记录器测试."""
        from agentflow.observability.logging import AgentFlowLogger

        logger = AgentFlowLogger("test-logger")
        self.assertIsNotNone(logger)
        self.assertEqual(logger._name, "test-logger")

    def test_log_methods(self):
        """日志方法测试."""
        from agentflow.observability.logging import AgentFlowLogger, LogConfig, LogLevel

        with tempfile.NamedTemporaryFile(mode="w", suffix=".log", delete=False) as f:
            config = LogConfig(level=LogLevel.DEBUG, output=f.name, format="text")
            logger = AgentFlowLogger("test", config)

            logger.debug("Debug message")
            logger.info("Info message")
            logger.warning("Warning message")
            logger.error("Error message")

            # Clean up
            Path(f.name).unlink(missing_ok=True)

    def test_context_manager(self):
        """上下文管理器测试."""
        from agentflow.observability.logging import AgentFlowLogger

        logger = AgentFlowLogger("test")

        with logger.context(request_id="123", user_id="456"):
            # Context should be set
            pass
        # Context should be cleared


class TestSetupLogging(unittest.TestCase):
    """setup_logging 函数的测试."""

    def test_setup_logging(self):
        """设置日志测试."""
        from agentflow.observability.logging import setup_logging, LogLevel

        setup_logging(level=LogLevel.DEBUG, format="json")
        root_logger = logging.getLogger()
        self.assertEqual(root_logger.level, logging.DEBUG)


class TestGetLogger(unittest.TestCase):
    """get_logger 函数的测试."""

    def test_get_logger(self):
        """获取日志记录器测试."""
        from agentflow.observability.logging import get_logger

        logger1 = get_logger("test")
        logger2 = get_logger("test")
        self.assertIs(logger1, logger2)

        logger3 = get_logger("other")
        self.assertIsNot(logger1, logger3)


class TestCounter(unittest.TestCase):
    """Counter 的完整测试."""

    def test_basic_increment(self):
        """基本增量测试."""
        from agentflow.observability.metrics import Counter

        counter = Counter("test_counter", "Test counter")
        self.assertEqual(counter.get(), 0)

        counter.inc()
        self.assertEqual(counter.get(), 1)

        counter.inc(5)
        self.assertEqual(counter.get(), 6)

    def test_increment_with_labels(self):
        """带标签增量测试."""
        from agentflow.observability.metrics import Counter

        counter = Counter("requests", "Requests", label_names=["method"])

        counter.inc(labels={"method": "GET"})
        counter.inc(labels={"method": "POST"})
        counter.inc(labels={"method": "GET"})

        self.assertEqual(counter.get(labels={"method": "GET"}), 2)
        self.assertEqual(counter.get(labels={"method": "POST"}), 1)

    def test_negative_increment_raises(self):
        """负数增量异常测试."""
        from agentflow.observability.metrics import Counter

        counter = Counter("test", "Test")

        with self.assertRaises(ValueError):
            counter.inc(-1)

    def test_properties(self):
        """属性测试."""
        from agentflow.observability.metrics import Counter

        counter = Counter("my_counter", "My description")
        self.assertEqual(counter.name, "my_counter")
        self.assertEqual(counter.description, "My description")


class TestGauge(unittest.TestCase):
    """Gauge 的完整测试."""

    def test_set_and_get(self):
        """设置和获取测试."""
        from agentflow.observability.metrics import Gauge

        gauge = Gauge("test_gauge", "Test gauge")

        gauge.set(100)
        self.assertEqual(gauge.get(), 100)

        gauge.set(50)
        self.assertEqual(gauge.get(), 50)

    def test_inc_and_dec(self):
        """增减测试."""
        from agentflow.observability.metrics import Gauge

        gauge = Gauge("test_gauge", "Test gauge")

        gauge.set(10)
        gauge.inc()
        self.assertEqual(gauge.get(), 11)

        gauge.inc(5)
        self.assertEqual(gauge.get(), 16)

        gauge.dec()
        self.assertEqual(gauge.get(), 15)

        gauge.dec(10)
        self.assertEqual(gauge.get(), 5)

    def test_with_labels(self):
        """带标签测试."""
        from agentflow.observability.metrics import Gauge

        gauge = Gauge("connections", "Active connections", label_names=["server"])

        gauge.set(10, labels={"server": "server1"})
        gauge.set(20, labels={"server": "server2"})

        self.assertEqual(gauge.get(labels={"server": "server1"}), 10)
        self.assertEqual(gauge.get(labels={"server": "server2"}), 20)


class TestHistogram(unittest.TestCase):
    """Histogram 的完整测试."""

    def test_observe(self):
        """观测测试."""
        from agentflow.observability.metrics import Histogram

        histogram = Histogram("request_duration", "Request duration")

        histogram.observe(0.1)
        histogram.observe(0.5)
        histogram.observe(1.0)

        stats = histogram.get_stats()
        self.assertEqual(stats["count"], 3)
        self.assertAlmostEqual(stats["sum"], 1.6)
        self.assertAlmostEqual(stats["avg"], 1.6 / 3)
        self.assertEqual(stats["min"], 0.1)
        self.assertEqual(stats["max"], 1.0)

    def test_time_context_manager(self):
        """时间上下文管理器测试."""
        from agentflow.observability.metrics import Histogram

        histogram = Histogram("duration", "Duration")

        with histogram.time():
            time.sleep(0.01)

        stats = histogram.get_stats()
        self.assertEqual(stats["count"], 1)
        self.assertGreater(stats["sum"], 0.01)

    def test_empty_stats(self):
        """空统计测试."""
        from agentflow.observability.metrics import Histogram

        histogram = Histogram("empty", "Empty")
        stats = histogram.get_stats()

        self.assertEqual(stats["count"], 0)
        self.assertEqual(stats["sum"], 0)
        self.assertEqual(stats["avg"], 0)

    def test_custom_buckets(self):
        """自定义桶测试."""
        from agentflow.observability.metrics import Histogram

        buckets = (0.1, 0.5, 1.0, 5.0)
        histogram = Histogram("custom", "Custom", buckets=buckets)

        histogram.observe(0.2)
        histogram.observe(0.8)

        stats = histogram.get_stats()
        self.assertEqual(stats["buckets"][0.1], 0)
        self.assertEqual(stats["buckets"][0.5], 1)
        self.assertEqual(stats["buckets"][1.0], 2)


class TestMetricsCollector(unittest.TestCase):
    """MetricsCollector 的完整测试."""

    def test_counter_creation(self):
        """创建计数器测试."""
        from agentflow.observability.metrics import MetricsCollector

        collector = MetricsCollector(prefix="test")
        counter = collector.counter("requests", "Total requests")

        self.assertEqual(counter.name, "test_requests")

        # Same name returns same instance
        counter2 = collector.counter("requests")
        self.assertIs(counter, counter2)

    def test_gauge_creation(self):
        """创建 Gauge 测试."""
        from agentflow.observability.metrics import MetricsCollector

        collector = MetricsCollector(prefix="test")
        gauge = collector.gauge("connections", "Active connections")

        self.assertEqual(gauge.name, "test_connections")

    def test_histogram_creation(self):
        """创建 Histogram 测试."""
        from agentflow.observability.metrics import MetricsCollector

        collector = MetricsCollector(prefix="test")
        histogram = collector.histogram("duration", "Duration")

        self.assertEqual(histogram.name, "test_duration")

    def test_collect(self):
        """收集指标测试."""
        from agentflow.observability.metrics import MetricsCollector

        collector = MetricsCollector(prefix="app")
        counter = collector.counter("requests", "Requests")
        counter.inc(10)

        data = collector.collect()
        self.assertIn("counters", data)
        self.assertIn("gauges", data)
        self.assertIn("histograms", data)

    def test_to_prometheus(self):
        """Prometheus 格式输出测试."""
        from agentflow.observability.metrics import MetricsCollector

        collector = MetricsCollector(prefix="app")
        counter = collector.counter("requests", "Total requests")
        counter.inc(100)

        output = collector.to_prometheus()
        self.assertIn("# TYPE app_requests counter", output)
        self.assertIn("app_requests 100", output)


class TestSetupMetrics(unittest.TestCase):
    """setup_metrics 和 get_metrics 的测试."""

    def test_setup_and_get(self):
        """设置和获取测试."""
        from agentflow.observability.metrics import setup_metrics, get_metrics

        collector = setup_metrics(prefix="myapp")
        self.assertEqual(collector._prefix, "myapp")

        retrieved = get_metrics()
        self.assertIsNotNone(retrieved)


class TestSpanContext(unittest.TestCase):
    """SpanContext 的测试."""

    def test_generate_root(self):
        """生成根上下文测试."""
        from agentflow.observability.tracing import SpanContext

        ctx = SpanContext.generate()
        self.assertIsNotNone(ctx.trace_id)
        self.assertIsNotNone(ctx.span_id)
        self.assertIsNone(ctx.parent_span_id)

    def test_generate_child(self):
        """生成子上下文测试."""
        from agentflow.observability.tracing import SpanContext

        parent = SpanContext.generate()
        child = SpanContext.generate(parent)

        self.assertEqual(child.trace_id, parent.trace_id)
        self.assertEqual(child.parent_span_id, parent.span_id)
        self.assertNotEqual(child.span_id, parent.span_id)


class TestSpan(unittest.TestCase):
    """Span 的测试."""

    def test_span_creation(self):
        """Span 创建测试."""
        from agentflow.observability.tracing import Span, SpanContext

        ctx = SpanContext.generate()
        span = Span(name="test-span", context=ctx)

        self.assertEqual(span.name, "test-span")
        self.assertEqual(span.status, "ok")
        self.assertIsNone(span.end_time)

    def test_span_attributes(self):
        """Span 属性测试."""
        from agentflow.observability.tracing import Span, SpanContext

        span = Span(name="test", context=SpanContext.generate())
        span.set_attribute("key", "value")

        self.assertEqual(span.attributes["key"], "value")

    def test_span_events(self):
        """Span 事件测试."""
        from agentflow.observability.tracing import Span, SpanContext

        span = Span(name="test", context=SpanContext.generate())
        span.add_event("event1", {"detail": "test"})

        self.assertEqual(len(span.events), 1)
        self.assertEqual(span.events[0]["name"], "event1")

    def test_span_status(self):
        """Span 状态测试."""
        from agentflow.observability.tracing import Span, SpanContext

        span = Span(name="test", context=SpanContext.generate())
        span.set_status("error", "Something went wrong")

        self.assertEqual(span.status, "error")
        self.assertEqual(span.attributes["status_message"], "Something went wrong")

    def test_span_end(self):
        """Span 结束测试."""
        from agentflow.observability.tracing import Span, SpanContext

        span = Span(name="test", context=SpanContext.generate())
        self.assertIsNone(span.end_time)

        span.end()
        self.assertIsNotNone(span.end_time)

    def test_span_duration(self):
        """Span 持续时间测试."""
        from agentflow.observability.tracing import Span, SpanContext

        span = Span(name="test", context=SpanContext.generate())
        time.sleep(0.01)
        duration = span.duration_ms

        self.assertGreater(duration, 10)

    def test_span_to_dict(self):
        """Span 转字典测试."""
        from agentflow.observability.tracing import Span, SpanContext

        span = Span(name="test", context=SpanContext.generate())
        span.end()

        data = span.to_dict()
        self.assertEqual(data["name"], "test")
        self.assertIn("trace_id", data)
        self.assertIn("span_id", data)
        self.assertIn("start_time", data)
        self.assertIn("end_time", data)


class TestTracer(unittest.TestCase):
    """Tracer 的测试."""

    def test_tracer_creation(self):
        """Tracer 创建测试."""
        from agentflow.observability.tracing import Tracer

        tracer = Tracer("my-service")
        self.assertEqual(tracer._service_name, "my-service")

    def test_span_context_manager(self):
        """Span 上下文管理器测试."""
        from agentflow.observability.tracing import Tracer

        tracer = Tracer("test")

        with tracer.span("operation") as span:
            span.set_attribute("key", "value")

        self.assertEqual(span.status, "ok")
        self.assertIsNotNone(span.end_time)

    def test_span_exception_handling(self):
        """Span 异常处理测试."""
        from agentflow.observability.tracing import Tracer

        tracer = Tracer("test")

        try:
            with tracer.span("failing-operation") as span:
                raise ValueError("Test error")
        except ValueError:
            pass

        self.assertEqual(span.status, "error")
        self.assertEqual(len(span.events), 1)
        self.assertEqual(span.events[0]["name"], "exception")

    def test_nested_spans(self):
        """嵌套 Span 测试."""
        from agentflow.observability.tracing import Tracer

        tracer = Tracer("test")

        with tracer.span("parent") as parent:
            with tracer.span("child") as child:
                pass

        self.assertEqual(child.context.parent_span_id, parent.span_id)

    def test_get_current_span(self):
        """获取当前 Span 测试."""
        from agentflow.observability.tracing import Tracer

        tracer = Tracer("test")

        with tracer.span("operation") as span:
            current = tracer.get_current_span()
            self.assertIs(current, span)

        self.assertIsNone(tracer.get_current_span())

    def test_get_spans(self):
        """获取所有 Span 测试."""
        from agentflow.observability.tracing import Tracer

        tracer = Tracer("test")

        with tracer.span("span1"):
            pass
        with tracer.span("span2"):
            pass

        spans = tracer.get_spans()
        self.assertEqual(len(spans), 2)

    def test_clear(self):
        """清除 Span 测试."""
        from agentflow.observability.tracing import Tracer

        tracer = Tracer("test")

        with tracer.span("span1"):
            pass

        tracer.clear()
        self.assertEqual(len(tracer.get_spans()), 0)


class TestSetupTracing(unittest.TestCase):
    """setup_tracing 和 get_tracer 的测试."""

    def test_setup_and_get(self):
        """设置和获取测试."""
        from agentflow.observability.tracing import setup_tracing, get_tracer

        tracer = setup_tracing(service_name="myapp")
        self.assertEqual(tracer._service_name, "myapp")

        retrieved = get_tracer()
        self.assertIsNotNone(retrieved)


class TestSentryIntegration(unittest.TestCase):
    """Sentry 集成的测试."""

    def test_setup_without_dsn(self):
        """无 DSN 设置测试."""
        from agentflow.observability.sentry_integration import setup_sentry

        result = setup_sentry(dsn=None)
        self.assertFalse(result)

    def test_is_sentry_initialized(self):
        """检查 Sentry 初始化状态."""
        from agentflow.observability.sentry_integration import is_sentry_initialized

        # Without setting up, should be False
        self.assertFalse(is_sentry_initialized())

    def test_capture_exception_not_initialized(self):
        """未初始化时捕获异常测试."""
        from agentflow.observability.sentry_integration import capture_exception

        result = capture_exception(ValueError("test"))
        self.assertIsNone(result)

    def test_capture_message_not_initialized(self):
        """未初始化时捕获消息测试."""
        from agentflow.observability.sentry_integration import capture_message

        result = capture_message("test message")
        self.assertIsNone(result)


class TestSetupObservability(unittest.TestCase):
    """setup_observability 的测试."""

    def test_basic_setup(self):
        """基本设置测试."""
        from agentflow.observability.setup import setup_observability

        result = setup_observability(
            service_name="test-service",
            log_level="INFO",
            enable_metrics=True,
            enable_tracing=True,
        )

        self.assertEqual(result["service_name"], "test-service")
        self.assertTrue(result["logging"])
        self.assertTrue(result["metrics"])
        self.assertTrue(result["tracing"])

    def test_setup_with_string_log_level(self):
        """字符串日志级别设置测试."""
        from agentflow.observability.setup import setup_observability

        result = setup_observability(
            service_name="test",
            log_level="DEBUG",
        )

        self.assertTrue(result["logging"])


if __name__ == "__main__":
    unittest.main()


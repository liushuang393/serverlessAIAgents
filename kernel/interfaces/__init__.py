"""Kernel インターフェース — 外部依存の抽象定義.

Kernel 層が Infrastructure / Shared 層に直接依存しないよう、
Protocol ベースの抽象インターフェースを提供する。
"""
from kernel.interfaces.llm_service import LLMService
from kernel.interfaces.metrics_service import MetricsService
from kernel.interfaces.tool_provider import ToolProviderService

__all__ = ["LLMService", "MetricsService", "ToolProviderService"]

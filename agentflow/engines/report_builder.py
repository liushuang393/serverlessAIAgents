"""shim: agentflow.engines.report_builder -> kernel."""
from kernel.engines.report_builder import ReportBuilder  # noqa: F401

__all__ = ["ReportBuilder"]

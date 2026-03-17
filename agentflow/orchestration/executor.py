"""shim: agentflow.orchestration.executor -> kernel."""
from kernel.executor import ExecutorAgent  # noqa: F401

__all__ = ["ExecutorAgent"]

"""IWorkflowRunner - 後方互換shim.

実体は contracts.core.workflow_runner に移設済み。
このファイルは後方互換のため contracts.core.workflow_runner から re-export する。
"""

from contracts.core.workflow_runner import IWorkflowRunner  # noqa: F401

__all__ = ["IWorkflowRunner"]

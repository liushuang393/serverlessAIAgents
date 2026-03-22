"""patterns パッケージ — Agent合成・分解パターン群."""

from __future__ import annotations

import importlib
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from kernel.patterns.agent_pipeline import AgentConfig, AgentPipeline
    from kernel.patterns.composer import AgentComposer, AgentRole, CompositionConfig, CompositionPattern
    from kernel.patterns.shared_context import SharedContext
    from kernel.patterns.task_decomposer import DecomposedTask, DecompositionPlan, TaskDecomposer, TaskPriority

__all__ = [
    "AgentComposer",
    "AgentConfig",
    "AgentPipeline",
    "AgentRole",
    "CompositionConfig",
    "CompositionPattern",
    "DecomposedTask",
    "DecompositionPlan",
    "SharedContext",
    "TaskDecomposer",
    "TaskPriority",
]

_SUBMODULE_MAP: dict[str, str] = {
    "AgentConfig": "kernel.patterns.agent_pipeline",
    "AgentPipeline": "kernel.patterns.agent_pipeline",
    "AgentComposer": "kernel.patterns.composer",
    "AgentRole": "kernel.patterns.composer",
    "CompositionConfig": "kernel.patterns.composer",
    "CompositionPattern": "kernel.patterns.composer",
    "SharedContext": "kernel.patterns.shared_context",
    "DecomposedTask": "kernel.patterns.task_decomposer",
    "DecompositionPlan": "kernel.patterns.task_decomposer",
    "TaskDecomposer": "kernel.patterns.task_decomposer",
    "TaskPriority": "kernel.patterns.task_decomposer",
}


def __getattr__(name: str) -> object:
    """遅延インポートで循環依存を回避."""
    if name in _SUBMODULE_MAP:
        mod = importlib.import_module(_SUBMODULE_MAP[name])
        return getattr(mod, name)
    msg = f"module 'kernel.patterns' has no attribute {name!r}"
    raise AttributeError(msg)

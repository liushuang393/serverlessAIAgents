"""実行Agent - kernel/executor からの再エクスポートshim.

本体は kernel/executor/service.py に移行済み。
後方互換性のためこのモジュールからも全シンボルをインポート可能。
"""

from kernel.executor.service import (
    ExecutorAgent,
    ExecutorConfig,
    StepResult,
)

__all__ = [
    "ExecutorAgent",
    "ExecutorConfig",
    "StepResult",
]

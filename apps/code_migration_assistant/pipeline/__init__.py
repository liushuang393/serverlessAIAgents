"""Pipeline compatibility surface."""

from .engine import MigrationEngine, PipelineResult, SSEEvent, run_migration_sync
from .project import COBOLFile, COBOLProject


__all__ = [
    "COBOLFile",
    "COBOLProject",
    "MigrationEngine",
    "PipelineResult",
    "SSEEvent",
    "run_migration_sync",
]

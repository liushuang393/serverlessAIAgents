# -*- coding: utf-8 -*-
"""Code Migration Assistant - agentflow ベースのコード移行ツール.

agentflow の Engine パターンを使用したコード移行アシスタント。
Factory パターンにより、設定ベースで言語ペアを切り替え可能。

使用例:
    >>> from apps.code_migration_assistant import CodeMigrationEngine
    >>>
    >>> # Engine を直接使用
    >>> engine = CodeMigrationEngine(migration_type="cobol-to-java")
    >>> result = await engine.run({"source_code": cobol_code})
    >>>
    >>> # Orchestrator を使用（高レベル API）
    >>> from apps.code_migration_assistant import CodeMigrationOrchestrator
    >>> orchestrator = CodeMigrationOrchestrator()
    >>> result = await orchestrator.run({"cobol_code": cobol_code})
"""

from apps.code_migration_assistant.engine import CodeMigrationEngine
from apps.code_migration_assistant.orchestrator import CodeMigrationOrchestrator

__all__ = [
    "CodeMigrationEngine",
    "CodeMigrationOrchestrator",
]


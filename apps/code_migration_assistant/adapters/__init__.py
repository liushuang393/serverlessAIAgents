"""Language Adapters.

拡張可能な言語アダプター。複数のソース言語とターゲット言語をサポート。

ソース言語: COBOL, RPG, PL/I, Struts, ...
ターゲット言語: Java, C#, TypeScript, Spring Boot, ...

使用例:
    # 直接インポート（既存互換）
    from apps.code_migration_assistant.adapters import CobolAdapter, JavaAdapter

    # Factory パターン（推奨）
    from apps.code_migration_assistant.adapters import get_adapter_factory
    factory = get_adapter_factory()
    source_adapter = factory.get_source_adapter(migration_type="cobol-to-java")
    target_adapter = factory.get_target_adapter(migration_type="cobol-to-java")
"""

from apps.code_migration_assistant.adapters.base import (
    AST,
    ExecutionResult,
    SourceLanguageAdapter,
    TargetLanguageAdapter,
)
from apps.code_migration_assistant.adapters.factory import (
    AdapterFactory,
    get_adapter_factory,
)
from apps.code_migration_assistant.adapters.source.cobol_adapter import CobolAdapter
from apps.code_migration_assistant.adapters.target.java_adapter import JavaAdapter


__all__ = [
    "AST",
    # ファクトリー
    "AdapterFactory",
    # 実装アダプター
    "CobolAdapter",
    "ExecutionResult",
    "JavaAdapter",
    # 基底クラス
    "SourceLanguageAdapter",
    "TargetLanguageAdapter",
    "get_adapter_factory",
]


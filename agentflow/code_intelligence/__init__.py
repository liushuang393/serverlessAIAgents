# -*- coding: utf-8 -*-
"""Code Intelligence Layer - 代码智能層.

旧システム翻新を支援する多言語解析、統一AST、変換、迁移追踪機能を提供します。

主要コンポーネント:
- UnifiedAST: 言語非依存の抽象構文木
- CodeParser: 統一解析インターフェース
- CodeTransformer: AST変換インターフェース
- MigrationProject: 迁移プロジェクト追踪
- QualityGateRunner: 品質門

使用例:
    >>> from agentflow.code_intelligence import get_parser, get_transformer
    >>>
    >>> # COBOL を解析
    >>> parser = get_parser("cobol")
    >>> result = parser.parse_file(Path("legacy.cob"))
    >>>
    >>> # Java に変換
    >>> transformer = get_transformer("cobol", "java")
    >>> java_result = transformer.transform(result.ast, context)
"""

from agentflow.code_intelligence.ast.unified_ast import (
    UnifiedAST,
    ASTNode,
    ASTNodeType,
    SymbolInfo,
    ImportInfo,
    TypeInfo,
)
from agentflow.code_intelligence.parsers.base import (
    CodeParser,
    ParseContext,
    ParseResult,
)
from agentflow.code_intelligence.parsers.registry import (
    ParserRegistry,
    get_parser,
    register_parser,
)
from agentflow.code_intelligence.transformers.base import (
    CodeTransformer,
    TransformContext,
    TransformResult,
)
from agentflow.code_intelligence.transformers.registry import (
    TransformerRegistry,
    get_transformer,
    register_transformer,
)
from agentflow.code_intelligence.migration.project import (
    MigrationProject,
    MigrationPhase,
    PhaseStatus,
    SourceFile,
    FileStatus,
    QualityMetrics,
)
from agentflow.code_intelligence.migration.tracker import MigrationTracker
from agentflow.code_intelligence.migration.inventory import (
    CodeInventory,
    InventoryResult,
    FileInfo,
    DependencyInfo,
)
from agentflow.code_intelligence.quality.gates import (
    QualityGate,
    QualityGateRunner,
    QualityReport,
    QualityLevel,
    QualityIssue,
    GateResult,
)
from agentflow.code_intelligence.cicd.pipeline_generator import (
    MigrationPipelineGenerator,
    PipelineConfig,
    PipelineStage,
    CIPlatform,
)

__version__ = "1.0.0"
__author__ = "AgentFlow Team"

__all__ = [
    # AST
    "UnifiedAST",
    "ASTNode",
    "ASTNodeType",
    "SymbolInfo",
    "ImportInfo",
    "TypeInfo",
    # Parser
    "CodeParser",
    "ParseContext",
    "ParseResult",
    "ParserRegistry",
    "get_parser",
    "register_parser",
    # Transformer
    "CodeTransformer",
    "TransformContext",
    "TransformResult",
    "TransformerRegistry",
    "get_transformer",
    "register_transformer",
    # Migration
    "MigrationProject",
    "MigrationPhase",
    "PhaseStatus",
    "SourceFile",
    "FileStatus",
    "QualityMetrics",
    "MigrationTracker",
    # Inventory
    "CodeInventory",
    "InventoryResult",
    "FileInfo",
    "DependencyInfo",
    # Quality
    "QualityGate",
    "QualityGateRunner",
    "QualityReport",
    "QualityLevel",
    "QualityIssue",
    "GateResult",
    # CI/CD
    "MigrationPipelineGenerator",
    "PipelineConfig",
    "PipelineStage",
    "CIPlatform",
]

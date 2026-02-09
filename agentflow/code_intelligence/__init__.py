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
    ASTNode,
    ASTNodeType,
    ImportInfo,
    SymbolInfo,
    TypeInfo,
    UnifiedAST,
)
from agentflow.code_intelligence.cicd.pipeline_generator import (
    CIPlatform,
    MigrationPipelineGenerator,
    PipelineConfig,
    PipelineStage,
)
from agentflow.code_intelligence.migration.inventory import (
    CodeInventory,
    DependencyInfo,
    FileInfo,
    InventoryResult,
)
from agentflow.code_intelligence.migration.project import (
    FileStatus,
    MigrationPhase,
    MigrationProject,
    PhaseStatus,
    QualityMetrics,
    SourceFile,
)
from agentflow.code_intelligence.migration.tracker import MigrationTracker
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
from agentflow.code_intelligence.quality.gates import (
    GateResult,
    QualityGate,
    QualityGateRunner,
    QualityIssue,
    QualityLevel,
    QualityReport,
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


__version__ = "1.0.0"
__author__ = "AgentFlow Team"

__all__ = [
    "ASTNode",
    "ASTNodeType",
    "CIPlatform",
    # Inventory
    "CodeInventory",
    # Parser
    "CodeParser",
    # Transformer
    "CodeTransformer",
    "DependencyInfo",
    "FileInfo",
    "FileStatus",
    "GateResult",
    "ImportInfo",
    "InventoryResult",
    "MigrationPhase",
    # CI/CD
    "MigrationPipelineGenerator",
    # Migration
    "MigrationProject",
    "MigrationTracker",
    "ParseContext",
    "ParseResult",
    "ParserRegistry",
    "PhaseStatus",
    "PipelineConfig",
    "PipelineStage",
    # Quality
    "QualityGate",
    "QualityGateRunner",
    "QualityIssue",
    "QualityLevel",
    "QualityMetrics",
    "QualityReport",
    "SourceFile",
    "SymbolInfo",
    "TransformContext",
    "TransformResult",
    "TransformerRegistry",
    "TypeInfo",
    # AST
    "UnifiedAST",
    "get_parser",
    "get_transformer",
    "register_parser",
    "register_transformer",
]

"""コード分析・移行 Skills パッケージ.

このパッケージはコード分析と移行に必要なスキルセットを提供します。

含まれるスキル:
- repo_connector: リポジトリ接続
- static_analyzer: 静的コード分析
- complexity_scorer: 複雑度評価
- dependency_mapper: 依存関係マッピング
- security_scanner: セキュリティスキャン
- migration_planner: 移行計画策定

ワークフロー:
    リポジトリ接続 → 静的分析 → 複雑度評価 → 依存関係分析 → セキュリティスキャン → 移行計画

使用例:
    >>> from agentflow.skills.builtin.code_analysis import (
    ...     RepoConnector,
    ...     StaticAnalyzer,
    ...     ComplexityScorer,
    ... )
    >>>
    >>> connector = RepoConnector()
    >>> repo = await connector.connect("https://github.com/org/repo.git")
"""

from agentflow.skills.builtin.code_analysis.complexity_scorer import (
    ComplexityMetrics,
    ComplexityReport,
    ComplexityScorer,
    FileComplexity,
)
from agentflow.skills.builtin.code_analysis.dependency_mapper import (
    DependencyGraph,
    DependencyMapper,
    DependencyNode,
)
from agentflow.skills.builtin.code_analysis.migration_planner import (
    MigrationPhase,
    MigrationPlan,
    MigrationPlanner,
    MigrationRisk,
)
from agentflow.skills.builtin.code_analysis.repo_connector import (
    RepoConfig,
    RepoConnector,
    RepoInfo,
)
from agentflow.skills.builtin.code_analysis.security_scanner import (
    SecurityReport,
    SecurityScanner,
    Vulnerability,
    VulnerabilitySeverity,
)
from agentflow.skills.builtin.code_analysis.static_analyzer import (
    AnalysisResult,
    CodeIssue,
    IssueSeverity,
    StaticAnalyzer,
)


__all__ = [
    "AnalysisResult",
    "CodeIssue",
    "ComplexityMetrics",
    "ComplexityReport",
    # 複雑度評価
    "ComplexityScorer",
    "DependencyGraph",
    # 依存関係
    "DependencyMapper",
    "DependencyNode",
    "FileComplexity",
    "IssueSeverity",
    "MigrationPhase",
    "MigrationPlan",
    # 移行計画
    "MigrationPlanner",
    "MigrationRisk",
    "RepoConfig",
    # リポジトリ接続
    "RepoConnector",
    "RepoInfo",
    "SecurityReport",
    # セキュリティ
    "SecurityScanner",
    # 静的分析
    "StaticAnalyzer",
    "Vulnerability",
    "VulnerabilitySeverity",
]

"""AgentFlow Services - 統一サービス層.

このモジュールは、API / CLI / Studio 全ての交互モードが使用する
統一バックエンドサービスを提供します。

設計原則:
- 通用性: 特定フロントエンドに依存しない
- 一貫性: 全モードで同じ業務ロジック
- 柔軟性: 同期/非同期/ストリーム 全対応

アーキテクチャ:
    ┌─────────┐  ┌─────────┐  ┌─────────┐
    │   CLI   │  │   API   │  │ Studio  │
    └────┬────┘  └────┬────┘  └────┬────┘
         │            │            │
         └────────────┼────────────┘
                      │
                      ▼
         ┌────────────────────────┐
         │    Service Layer       │  ← このモジュール
         │  (統一バックエンド)     │
         ├────────────────────────┤
         │ - AgentService         │
         │ - WorkflowService      │
         │ - ToolService          │
         │ - MemoryService        │
         └────────────────────────┘
                      │
                      ▼
         ┌────────────────────────┐
         │    Core Layer          │
         │  - AgentBlock          │
         │  - LLMClient           │
         │  - ToolProvider        │
         └────────────────────────┘

使用例:
    >>> # API から
    >>> from agentflow.services import AgentService
    >>> service = AgentService()
    >>> result = await service.execute(agent_id, input_data)
    >>>
    >>> # CLI から
    >>> for event in service.execute_with_progress(agent_id, input_data):
    ...     print(f"Progress: {event.progress}%")
    >>>
    >>> # Studio から（WebSocket）
    >>> async for event in service.execute_stream(agent_id, input_data):
    ...     await ws.send(event.to_json())
"""

from agentflow.services.agent_service import AgentService
from agentflow.services.auth_service import (
    AuthConfig,
    AuthService,
    AuthToken,
    AuthUser,
)
from agentflow.services.base import (
    ErrorEvent,
    # コールバック
    EventCallback,
    LogEvent,
    ProgressCallback,
    ProgressEvent,
    ResultEvent,
    # 基底クラス
    ServiceBase,
    ServiceError,
    # イベント
    ServiceEvent,
    ServiceEventType,
    ServiceResult,
)
from agentflow.services.chart_service import (
    ChartConfig,
    ChartFormat,
    ChartRecommendation,
    ChartService,
    ChartType,
    DashboardConfig,
    DashboardPanel,
    # 増強: ダッシュボード・ドリルダウン
    DrillDownConfig,
)
from agentflow.services.document_exporter import (
    DocumentExporter,
    ExportConfig,
    ExportFormat,
    TemplateType,
)
from agentflow.services.fewshot_manager import (
    BM25,
    FewshotExample,
    FewshotManager,
    FewshotManagerConfig,
)
from agentflow.services.preview_service import PreviewService
from agentflow.services.publish_service import PublishService

# 新規追加: ナレッジ・データ・可視化サービス
from agentflow.services.rag_service import (
    ChunkStrategy,
    RAGConfig,
    RAGDocument,
    RAGService,
    RerankerType,
)

# 新規追加: NL2SQL 増強コンポーネント（学術研究に基づく）
from agentflow.services.schema_linker import (
    ColumnInfo,
    SchemaLinker,
    SchemaLinkerConfig,
    SchemaLinkResult,
    TableInfo,
)

# 新規追加: 語義層サービス
from agentflow.services.semantic_layer import (
    AggregationType,
    Dimension,
    FilterDSL,
    FilterOperator,
    Metric,
    MetricType,
    OrderByDSL,
    # DSL 中間表現層
    QueryDSL,
    ResolvedQuery,
    SemanticLayerConfig,
    SemanticLayerService,
    SortDirection,
    SQLHints,
    TimeGranularity,
    TimeRangeDSL,
)
from agentflow.services.sql_postprocessor import (
    CorrectionResult,
    PostProcessorConfig,
    PostProcessResult,
    SQLErrorType,
    SQLPostProcessor,
    ValidationLevel,
    ValidationResult,
)
from agentflow.services.suggestion_service import (
    SuggestionConfig,
    # 増強: 優先度
    SuggestionPriority,
    SuggestionService,
    SuggestionType,
)
from agentflow.services.text2sql_service import (
    SQLDialect,
    SQLResult,
    Text2SQLConfig,
    Text2SQLService,
)
from agentflow.services.workflow_service import WorkflowService


__all__ = [
    "BM25",
    # サービス
    "AgentService",
    "AggregationType",
    "AuthConfig",
    # 認証サービス
    "AuthService",
    "AuthToken",
    "AuthUser",
    "ChartConfig",
    "ChartFormat",
    # チャートサービス
    "ChartService",
    "ChartType",
    "ChunkStrategy",
    "ColumnInfo",
    "CorrectionResult",
    "Dimension",
    # ドキュメントエクスポーター
    "DocumentExporter",
    "ErrorEvent",
    # コールバック
    "EventCallback",
    "ExportConfig",
    "ExportFormat",
    "FewshotExample",
    "FewshotManager",
    "FewshotManagerConfig",
    "FilterDSL",
    "FilterOperator",
    "LogEvent",
    "Metric",
    "MetricType",
    "OrderByDSL",
    "PostProcessResult",
    "PostProcessorConfig",
    "PreviewService",
    "ProgressCallback",
    "ProgressEvent",
    "PublishService",
    # DSL 中間表現層
    "QueryDSL",
    "RAGConfig",
    "RAGDocument",
    # RAGサービス
    "RAGService",
    "RerankerType",
    "ResolvedQuery",
    "ResultEvent",
    "SQLDialect",
    "SQLErrorType",
    "SQLHints",
    "SQLPostProcessor",
    "SQLResult",
    "SchemaLinkResult",
    # NL2SQL 増強コンポーネント
    "SchemaLinker",
    "SchemaLinkerConfig",
    "SemanticLayerConfig",
    # 語義層サービス
    "SemanticLayerService",
    # 基底
    "ServiceBase",
    "ServiceError",
    # イベント
    "ServiceEvent",
    "ServiceEventType",
    "ServiceResult",
    "SortDirection",
    "SuggestionConfig",
    # 提案サービス
    "SuggestionService",
    "SuggestionType",
    "TableInfo",
    "TemplateType",
    "Text2SQLConfig",
    # Text2SQLサービス
    "Text2SQLService",
    "TimeGranularity",
    "TimeRangeDSL",
    "ValidationLevel",
    "ValidationResult",
    "WorkflowService",
]

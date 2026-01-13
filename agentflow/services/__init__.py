# -*- coding: utf-8 -*-
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

from agentflow.services.base import (
    # 基底クラス
    ServiceBase,
    ServiceResult,
    ServiceError,
    # イベント
    ServiceEvent,
    ServiceEventType,
    ProgressEvent,
    LogEvent,
    ResultEvent,
    ErrorEvent,
    # コールバック
    EventCallback,
    ProgressCallback,
)

from agentflow.services.agent_service import AgentService
from agentflow.services.workflow_service import WorkflowService
from agentflow.services.publish_service import PublishService
from agentflow.services.preview_service import PreviewService

# 新規追加: ナレッジ・データ・可視化サービス
from agentflow.services.rag_service import (
    RAGService,
    RAGConfig,
    RAGDocument,
    ChunkStrategy,
    RerankerType,
)
from agentflow.services.text2sql_service import (
    Text2SQLService,
    Text2SQLConfig,
    SQLResult,
    SQLDialect,
)
from agentflow.services.chart_service import (
    ChartService,
    ChartConfig,
    ChartType,
    ChartFormat,
)
from agentflow.services.suggestion_service import (
    SuggestionService,
    SuggestionConfig,
    SuggestionType,
)
from agentflow.services.auth_service import (
    AuthService,
    AuthConfig,
    AuthUser,
    AuthToken,
)

__all__ = [
    # 基底
    "ServiceBase",
    "ServiceResult",
    "ServiceError",
    # イベント
    "ServiceEvent",
    "ServiceEventType",
    "ProgressEvent",
    "LogEvent",
    "ResultEvent",
    "ErrorEvent",
    # コールバック
    "EventCallback",
    "ProgressCallback",
    # サービス
    "AgentService",
    "WorkflowService",
    "PublishService",
    "PreviewService",
    # RAGサービス
    "RAGService",
    "RAGConfig",
    "RAGDocument",
    "ChunkStrategy",
    "RerankerType",
    # Text2SQLサービス
    "Text2SQLService",
    "Text2SQLConfig",
    "SQLResult",
    "SQLDialect",
    # チャートサービス
    "ChartService",
    "ChartConfig",
    "ChartType",
    "ChartFormat",
    # 提案サービス
    "SuggestionService",
    "SuggestionConfig",
    "SuggestionType",
    # 認証サービス
    "AuthService",
    "AuthConfig",
    "AuthUser",
    "AuthToken",
]

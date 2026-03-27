"""services パッケージ — shared 層サービス統合エントリポイント.

shared 層サービスの正規 import パス。
旧 platform app 系の残骸ではなく、kernel/ や shared/ 内部から
依存規約に沿って参照するための入口。
"""

from shared.services.base import ServiceResult
from shared.services.chart_service import ChartConfig, ChartService
from shared.services.rag_service import ChunkStrategy, RAGConfig, RAGService, RerankerType
from shared.services.semantic_layer import SemanticLayerConfig, SemanticLayerService
from shared.services.suggestion_service import SuggestionConfig, SuggestionService, SuggestionType
from shared.services.text2sql_service import SQLDialect, Text2SQLConfig, Text2SQLService
from shared.services.weather_service import WeatherConfig, WeatherService
from shared.services.workflow_service import WorkflowService


__all__ = [
    "ChartConfig",
    "ChartService",
    "ChunkStrategy",
    "RAGConfig",
    "RAGService",
    "RerankerType",
    "SQLDialect",
    "SemanticLayerConfig",
    "SemanticLayerService",
    "ServiceResult",
    "SuggestionConfig",
    "SuggestionService",
    "SuggestionType",
    "Text2SQLConfig",
    "Text2SQLService",
    "WeatherConfig",
    "WeatherService",
    "WorkflowService",
]

"""services パッケージ — shared 層サービス統合エントリポイント.

platform/services/ から移設。kernel/ や shared/ 内部から
dependency-violation なしにインポートできる正規パス。
"""

from shared.services.base import ServiceResult
from shared.services.chart_service import ChartConfig, ChartService
from shared.services.rag_service import ChunkStrategy, RAGConfig, RAGService, RerankerType
from shared.services.suggestion_service import SuggestionConfig, SuggestionService
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
    "ServiceResult",
    "SuggestionConfig",
    "SuggestionService",
    "Text2SQLConfig",
    "Text2SQLService",
    "WeatherConfig",
    "WeatherService",
    "WorkflowService",
]

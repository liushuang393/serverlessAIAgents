# -*- coding: utf-8 -*-
"""FAQ Service Interface - Core interface for FAQ system.

Defines the contract for FAQ service implementations.
"""

from __future__ import annotations

from abc import abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, AsyncIterator, Protocol, runtime_checkable


class QueryType(str, Enum):
    """Query type enum."""
    FAQ = "faq"
    SQL = "sql"
    HYBRID = "hybrid"


class ChartType(str, Enum):
    """Chart type enum."""
    BAR = "bar"
    LINE = "line"
    PIE = "pie"
    SCATTER = "scatter"
    TABLE = "table"


@dataclass
class FAQDocument:
    """FAQ document."""
    id: str
    question: str
    answer: str
    category: str = ""
    tags: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class SearchResult:
    """Search result."""
    documents: list[FAQDocument]
    scores: list[float]
    query: str


@dataclass
class SQLQueryResult:
    """SQL query result."""
    sql: str
    data: list[dict[str, Any]]
    columns: list[str]
    row_count: int
    success: bool
    error: str | None = None


@dataclass
class ChartData:
    """Chart data for visualization."""
    chart_type: ChartType
    title: str
    data: dict[str, Any]
    options: dict[str, Any] = field(default_factory=dict)


@dataclass
class FAQResponse:
    """FAQ response."""
    answer: str
    query_type: QueryType
    sources: list[FAQDocument] = field(default_factory=list)
    sql_result: SQLQueryResult | None = None
    chart: ChartData | None = None
    suggestions: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class StreamEvent:
    """Stream event for real-time updates."""
    type: str
    data: dict[str, Any]
    timestamp: float = 0


@runtime_checkable
class IFAQService(Protocol):
    """FAQ Service interface."""

    @abstractmethod
    async def query(
        self,
        question: str,
        context: dict[str, Any] | None = None,
    ) -> FAQResponse:
        """Process FAQ query.
        
        Args:
            question: User question
            context: Additional context
            
        Returns:
            FAQResponse with answer, sources, charts
        """
        ...

    @abstractmethod
    async def query_stream(
        self,
        question: str,
        context: dict[str, Any] | None = None,
    ) -> AsyncIterator[StreamEvent]:
        """Process FAQ query with streaming.
        
        Args:
            question: User question
            context: Additional context
            
        Yields:
            StreamEvent for real-time updates
        """
        ...

    @abstractmethod
    async def add_document(self, document: FAQDocument) -> str:
        """Add FAQ document.
        
        Args:
            document: FAQ document to add
            
        Returns:
            Document ID
        """
        ...

    @abstractmethod
    async def search(
        self,
        query: str,
        top_k: int = 5,
        filters: dict[str, Any] | None = None,
    ) -> SearchResult:
        """Search FAQ documents.
        
        Args:
            query: Search query
            top_k: Number of results
            filters: Optional filters
            
        Returns:
            SearchResult
        """
        ...

    @abstractmethod
    async def execute_sql(
        self,
        question: str,
        schema: dict[str, Any] | None = None,
    ) -> SQLQueryResult:
        """Convert question to SQL and execute.
        
        Args:
            question: Natural language question
            schema: Database schema
            
        Returns:
            SQLQueryResult
        """
        ...


@runtime_checkable
class IRAGProvider(Protocol):
    """RAG provider interface."""

    @abstractmethod
    async def retrieve(
        self,
        query: str,
        top_k: int = 5,
    ) -> list[dict[str, Any]]:
        """Retrieve relevant documents."""
        ...

    @abstractmethod
    async def add(
        self,
        documents: list[str],
        metadatas: list[dict[str, Any]] | None = None,
    ) -> list[str]:
        """Add documents to knowledge base."""
        ...


@runtime_checkable
class IText2SQLProvider(Protocol):
    """Text2SQL provider interface."""

    @abstractmethod
    async def generate_sql(
        self,
        question: str,
        schema: dict[str, Any],
    ) -> str:
        """Generate SQL from natural language."""
        ...

    @abstractmethod
    async def execute(self, sql: str) -> SQLQueryResult:
        """Execute SQL query."""
        ...


@runtime_checkable
class IChartGenerator(Protocol):
    """Chart generator interface."""

    @abstractmethod
    async def generate(
        self,
        data: list[dict[str, Any]],
        columns: list[str],
        question: str,
    ) -> ChartData | None:
        """Generate appropriate chart for data."""
        ...


__all__ = [
    "QueryType",
    "ChartType",
    "FAQDocument",
    "SearchResult",
    "SQLQueryResult",
    "ChartData",
    "FAQResponse",
    "StreamEvent",
    "IFAQService",
    "IRAGProvider",
    "IText2SQLProvider",
    "IChartGenerator",
]

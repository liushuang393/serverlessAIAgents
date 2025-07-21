"""
AI Blocksのコアコンポーネント

このモジュールは、AIエージェント構築のための基本的な抽象インターフェースと
実装を提供します。各コンポーネントは独立して開発・テスト・交換可能です。
"""

from .chunker import ChunkerInterface, SimpleChunker
from .evaluator import EvaluatorInterface, LLMEvaluator

# 具体実装
# 抽象インターフェース
from .memory import MemoryInterface, VectorMemory

# データモデル
from .models import (
    EvaluationResult,
    MemoryItem,
    Message,
    MessageRole,
    ParsedDocument,
    RouteDefinition,
    RouteResult,
    TextChunk,
    ToolDefinition,
    ToolResult,
)
from .parser import HTMLParser, ParserInterface, PDFParser, TextParser
from .router import LLMBasedRouter, RouterInterface, RuleBasedRouter
from .thread import SimpleThread, ThreadInterface
from .tool import ToolInterface, ToolManager

__all__ = [
    # インターフェース
    "MemoryInterface",
    "ThreadInterface",
    "ToolInterface",
    "ParserInterface",
    "ChunkerInterface",
    "RouterInterface",
    "EvaluatorInterface",
    # 実装
    "VectorMemory",
    "SimpleThread",
    "ToolManager",
    "TextParser",
    "HTMLParser",
    "PDFParser",
    "SimpleChunker",
    "RuleBasedRouter",
    "LLMBasedRouter",
    "LLMEvaluator",
    # データモデル
    "Message",
    "MessageRole",
    "MemoryItem",
    "ToolResult",
    "ToolDefinition",
    "ParsedDocument",
    "TextChunk",
    "RouteResult",
    "RouteDefinition",
    "EvaluationResult",
]

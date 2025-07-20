"""
AI Blocksのコアコンポーネント

このモジュールは、AIエージェント構築のための基本的な抽象インターフェースと
実装を提供します。各コンポーネントは独立して開発・テスト・交換可能です。
"""

# 抽象インターフェース
from .memory import MemoryInterface
from .thread import ThreadInterface
from .tool import ToolInterface
from .parser import ParserInterface
from .chunker import ChunkerInterface
from .router import RouterInterface
from .evaluator import EvaluatorInterface

# 具体実装
from .memory import VectorMemory
from .thread import SimpleThread
from .tool import ToolManager
from .parser import TextParser, HTMLParser, PDFParser
from .chunker import SimpleChunker
from .router import RuleBasedRouter, LLMBasedRouter
from .evaluator import LLMEvaluator

# データモデル
from .models import (
    Message,
    MessageRole,
    MemoryItem,
    ToolResult,
    ToolDefinition,
    ParsedDocument,
    TextChunk,
    RouteResult,
    RouteDefinition,
    EvaluationResult,
)

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

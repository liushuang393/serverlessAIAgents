# -*- coding: utf-8 -*-
"""Parser モジュール.

多言語パーサーと統一インターフェース。

サポート言語:
- Legacy: COBOL, VB6, Delphi
- Modern: Java, Python, TypeScript
"""

from agentflow.code_intelligence.parsers.base import (
    CodeParser,
    ParseContext,
    ParseResult,
    ParseError,
)
from agentflow.code_intelligence.parsers.registry import (
    ParserRegistry,
    get_parser,
    register_parser,
)

# Legacy parsers
from agentflow.code_intelligence.parsers.legacy.cobol_parser import CobolParser

# Modern parsers
from agentflow.code_intelligence.parsers.modern.java_parser import JavaParser
from agentflow.code_intelligence.parsers.modern.python_parser import PythonParser

__all__ = [
    # Base
    "CodeParser",
    "ParseContext",
    "ParseResult",
    "ParseError",
    # Registry
    "ParserRegistry",
    "get_parser",
    "register_parser",
    # Legacy parsers
    "CobolParser",
    # Modern parsers
    "JavaParser",
    "PythonParser",
]

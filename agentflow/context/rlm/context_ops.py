"""Context Operations - コンテキスト操作ツール.

LLMが外部保存されたコンテキストにアクセスするためのツール群。
@tool デコレータで登録され、LLMのFunction Callingで呼び出し可能。

ツール一覧:
- ctx_peek: 行範囲を閲覧
- ctx_regex_find: 正規表現検索
- ctx_keyword_find: キーワード検索
- ctx_get_structure: 構造/アウトライン取得

設計原則:
- 決定的操作: LLM呼び出し不要で低コスト
- 行番号付き: LLMが位置を参照しやすい
- コンテキスト付き: 前後の文脈も含める

使用例:
    >>> ops = ContextOps(store)
    >>> lines = ops.ctx_peek("ctx_abc123", start_line=100, num_lines=50)
    >>> matches = ops.ctx_regex_find("ctx_abc123", r"def \\w+")
"""

from __future__ import annotations

import time
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from agentflow.context.rlm.context_store import ContextStore


class ContextOps:
    """コンテキスト操作クラス.

    ContextStoreを操作するツールメソッドを提供。
    RLMコントローラーから使用される。

    使用例:
        >>> store = ContextStore()
        >>> handle = await store.store("長い文書...")
        >>> ops = ContextOps(store)
        >>> lines = ops.ctx_peek(handle.handle_id, 0, 100)
    """

    def __init__(self, store: ContextStore) -> None:
        """初期化.

        Args:
            store: コンテキストストア
        """
        self._store = store

    def ctx_peek(
        self,
        handle_id: str,
        start_line: int = 0,
        num_lines: int = 100,
    ) -> dict[str, Any]:
        """指定行範囲を閲覧.

        Args:
            handle_id: コンテキストハンドルID
            start_line: 開始行（0-indexed）
            num_lines: 取得行数

        Returns:
            結果辞書（content, line_range, total_lines）
        """
        start_time = time.time()

        try:
            content = self._store.peek(handle_id, start_line, num_lines)
            handle = self._store.get_handle(handle_id)

            return {
                "success": True,
                "content": content,
                "start_line": start_line,
                "end_line": start_line + num_lines,
                "total_lines": handle.total_lines if handle else 0,
                "execution_time_ms": (time.time() - start_time) * 1000,
            }
        except KeyError as e:
            return {
                "success": False,
                "error": str(e),
                "execution_time_ms": (time.time() - start_time) * 1000,
            }

    def ctx_regex_find(
        self,
        handle_id: str,
        pattern: str,
        max_matches: int = 20,
        context_lines: int = 2,
    ) -> dict[str, Any]:
        """正規表現で検索.

        Args:
            handle_id: コンテキストハンドルID
            pattern: 正規表現パターン
            max_matches: 最大マッチ数
            context_lines: 前後のコンテキスト行数

        Returns:
            結果辞書（matches, match_count）
        """
        start_time = time.time()

        try:
            matches = self._store.regex_find(handle_id, pattern, max_matches, context_lines)

            # エラーチェック
            if matches and "error" in matches[0]:
                return {
                    "success": False,
                    "error": matches[0]["error"],
                    "execution_time_ms": (time.time() - start_time) * 1000,
                }

            return {
                "success": True,
                "matches": matches,
                "match_count": len(matches),
                "pattern": pattern,
                "execution_time_ms": (time.time() - start_time) * 1000,
            }
        except KeyError as e:
            return {
                "success": False,
                "error": str(e),
                "execution_time_ms": (time.time() - start_time) * 1000,
            }

    def ctx_keyword_find(
        self,
        handle_id: str,
        keywords: list[str],
        operator: str = "OR",
        max_matches: int = 20,
        context_lines: int = 2,
    ) -> dict[str, Any]:
        """キーワードで検索.

        Args:
            handle_id: コンテキストハンドルID
            keywords: キーワードリスト
            operator: "AND" または "OR"
            max_matches: 最大マッチ数
            context_lines: 前後のコンテキスト行数

        Returns:
            結果辞書（matches, match_count）
        """
        start_time = time.time()

        try:
            matches = self._store.keyword_find(
                handle_id, keywords, operator, max_matches, context_lines
            )

            return {
                "success": True,
                "matches": matches,
                "match_count": len(matches),
                "keywords": keywords,
                "operator": operator,
                "execution_time_ms": (time.time() - start_time) * 1000,
            }
        except KeyError as e:
            return {
                "success": False,
                "error": str(e),
                "execution_time_ms": (time.time() - start_time) * 1000,
            }

    def ctx_get_structure(
        self,
        handle_id: str,
        max_depth: int = 3,
    ) -> dict[str, Any]:
        """構造/アウトラインを取得.

        見出し、セクション、コードブロック等の構造情報を取得。

        Args:
            handle_id: コンテキストハンドルID
            max_depth: 最大見出しレベル

        Returns:
            結果辞書（outline, headings, sections, code_blocks）
        """
        start_time = time.time()

        try:
            structure = self._store.get_structure(handle_id)

            return {
                "success": True,
                "outline": structure.to_outline(max_depth),
                "headings": [
                    {"line": h[0], "level": h[1], "text": h[2]} for h in structure.headings
                ],
                "sections": [
                    {"start": s[0], "end": s[1], "title": s[2]} for s in structure.sections
                ],
                "code_blocks": [
                    {"start": c[0], "end": c[1], "language": c[2]} for c in structure.code_blocks
                ],
                "tables": [{"start": t[0], "end": t[1]} for t in structure.tables],
                "execution_time_ms": (time.time() - start_time) * 1000,
            }
        except KeyError as e:
            return {
                "success": False,
                "error": str(e),
                "execution_time_ms": (time.time() - start_time) * 1000,
            }

    def ctx_get_summary(self, handle_id: str) -> dict[str, Any]:
        """コンテキストのサマリを取得.

        Args:
            handle_id: コンテキストハンドルID

        Returns:
            結果辞書（summary, metadata）
        """
        start_time = time.time()

        handle = self._store.get_handle(handle_id)
        if handle is None:
            return {
                "success": False,
                "error": f"Handle not found: {handle_id}",
                "execution_time_ms": (time.time() - start_time) * 1000,
            }

        return {
            "success": True,
            "handle_id": handle.handle_id,
            "total_tokens": handle.total_tokens,
            "total_lines": handle.total_lines,
            "total_chars": handle.total_chars,
            "summary": handle.summary,
            "metadata": handle.metadata,
            "created_at": handle.created_at,
            "execution_time_ms": (time.time() - start_time) * 1000,
        }

    def to_tool_definitions(self) -> list[dict[str, Any]]:
        """OpenAI Function Calling形式のツール定義を生成.

        Returns:
            ツール定義リスト
        """
        return [
            {
                "type": "function",
                "function": {
                    "name": "ctx_peek",
                    "description": "View a range of lines from a stored context. "
                    "Returns lines with line numbers for easy reference.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "handle_id": {
                                "type": "string",
                                "description": "The context handle ID (e.g., ctx_abc123)",
                            },
                            "start_line": {
                                "type": "integer",
                                "description": "Starting line number (0-indexed)",
                                "default": 0,
                            },
                            "num_lines": {
                                "type": "integer",
                                "description": "Number of lines to retrieve",
                                "default": 100,
                            },
                        },
                        "required": ["handle_id"],
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "ctx_regex_find",
                    "description": "Search for patterns in a stored context using regex. "
                    "Returns matching lines with surrounding context.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "handle_id": {
                                "type": "string",
                                "description": "The context handle ID",
                            },
                            "pattern": {
                                "type": "string",
                                "description": "Regular expression pattern to search",
                            },
                            "max_matches": {
                                "type": "integer",
                                "description": "Maximum number of matches to return",
                                "default": 20,
                            },
                            "context_lines": {
                                "type": "integer",
                                "description": "Number of lines before/after to include",
                                "default": 2,
                            },
                        },
                        "required": ["handle_id", "pattern"],
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "ctx_keyword_find",
                    "description": "Search for keywords in a stored context. "
                    "Supports AND/OR operators for multiple keywords.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "handle_id": {
                                "type": "string",
                                "description": "The context handle ID",
                            },
                            "keywords": {
                                "type": "array",
                                "items": {"type": "string"},
                                "description": "Keywords to search for",
                            },
                            "operator": {
                                "type": "string",
                                "enum": ["AND", "OR"],
                                "description": "How to combine keywords",
                                "default": "OR",
                            },
                            "max_matches": {
                                "type": "integer",
                                "description": "Maximum number of matches to return",
                                "default": 20,
                            },
                        },
                        "required": ["handle_id", "keywords"],
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "ctx_get_structure",
                    "description": "Get the structure/outline of a stored context. "
                    "Returns headings, sections, code blocks, and tables.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "handle_id": {
                                "type": "string",
                                "description": "The context handle ID",
                            },
                            "max_depth": {
                                "type": "integer",
                                "description": "Maximum heading depth to include",
                                "default": 3,
                            },
                        },
                        "required": ["handle_id"],
                    },
                },
            },
        ]

    def execute_tool(
        self,
        tool_name: str,
        parameters: dict[str, Any],
    ) -> dict[str, Any]:
        """ツールを実行.

        Args:
            tool_name: ツール名
            parameters: パラメータ辞書

        Returns:
            実行結果辞書
        """
        if tool_name == "ctx_peek":
            return self.ctx_peek(
                handle_id=parameters["handle_id"],
                start_line=parameters.get("start_line", 0),
                num_lines=parameters.get("num_lines", 100),
            )
        if tool_name == "ctx_regex_find":
            return self.ctx_regex_find(
                handle_id=parameters["handle_id"],
                pattern=parameters["pattern"],
                max_matches=parameters.get("max_matches", 20),
                context_lines=parameters.get("context_lines", 2),
            )
        if tool_name == "ctx_keyword_find":
            return self.ctx_keyword_find(
                handle_id=parameters["handle_id"],
                keywords=parameters["keywords"],
                operator=parameters.get("operator", "OR"),
                max_matches=parameters.get("max_matches", 20),
                context_lines=parameters.get("context_lines", 2),
            )
        if tool_name == "ctx_get_structure":
            return self.ctx_get_structure(
                handle_id=parameters["handle_id"],
                max_depth=parameters.get("max_depth", 3),
            )
        if tool_name == "ctx_get_summary":
            return self.ctx_get_summary(
                handle_id=parameters["handle_id"],
            )
        return {
            "success": False,
            "error": f"Unknown tool: {tool_name}",
        }


def create_context_ops_prompt(handles: list[dict[str, Any]]) -> str:
    """コンテキスト操作のシステムプロンプトを生成.

    Args:
        handles: コンテキストハンドル情報リスト

    Returns:
        システムプロンプト文字列
    """
    prompt_parts = [
        "# Context Access Instructions",
        "",
        "You have access to external contexts that are too large to fit in this prompt.",
        "Use the following tools to read and search these contexts:",
        "",
        "## Available Tools",
        "",
        "1. **ctx_peek(handle_id, start_line, num_lines)**",
        "   - View specific line ranges from a context",
        "   - Use to read sections you've identified",
        "",
        "2. **ctx_regex_find(handle_id, pattern, max_matches)**",
        "   - Search using regular expressions",
        "   - Useful for finding specific patterns (function names, imports, etc.)",
        "",
        "3. **ctx_keyword_find(handle_id, keywords, operator)**",
        "   - Search for keywords (AND/OR)",
        "   - Useful for finding content by topic",
        "",
        "4. **ctx_get_structure(handle_id)**",
        "   - Get document outline (headings, sections, code blocks)",
        "   - Use first to understand document structure",
        "",
        "## Strategy",
        "",
        "1. First use ctx_get_structure to understand the document layout",
        "2. Use ctx_keyword_find or ctx_regex_find to locate relevant sections",
        "3. Use ctx_peek to read the identified sections in detail",
        "4. Prefer deterministic operations (regex, keyword) over expensive LLM calls",
        "",
        "## Available Contexts",
        "",
    ]

    for handle in handles:
        prompt_parts.append(f"### {handle['handle_id']}")
        prompt_parts.append(f"- Lines: {handle.get('total_lines', 'unknown')}")
        prompt_parts.append(f"- Tokens: ~{handle.get('total_tokens', 'unknown')}")
        if handle.get("summary"):
            prompt_parts.append(f"- Summary: {handle['summary']}")
        prompt_parts.append("")

    return "\n".join(prompt_parts)

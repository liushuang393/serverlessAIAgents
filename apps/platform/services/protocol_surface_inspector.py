"""Protocol surface inspector (AST ベース).

AST（Python 構文木）を用いて App の実装面から SSE / WS / A2A / MCP の
証拠を抽出し、正規表現依存を減らす。
"""

from __future__ import annotations

import ast
from dataclasses import dataclass, field
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from pathlib import Path


_PROTOCOLS = ("sse", "ws", "a2a", "mcp")
_EXCLUDED_PARTS = {"frontend", "htmlcov", "node_modules", "__pycache__", "tests"}


@dataclass(slots=True, frozen=True)
class ProtocolEvidence:
    """プロトコル検出証拠."""

    protocol: str
    file: str
    line: int
    marker: str

    @property
    def file_line(self) -> str:
        return f"{self.file}:{self.line}"


@dataclass(slots=True)
class ASTParseWarning:
    """AST 解析失敗情報."""

    file: str
    line: int
    message: str
    suggestion: str
    impact: str


@dataclass(slots=True)
class ProtocolSurfaceReport:
    """AST 監査レポート."""

    evidence: dict[str, list[ProtocolEvidence]] = field(
        default_factory=lambda: {name: [] for name in _PROTOCOLS},
    )
    parse_warnings: list[ASTParseWarning] = field(default_factory=list)
    scanned_files: int = 0
    parsed_files: int = 0

    def has(self, protocol: str) -> bool:
        return bool(self.evidence.get(protocol))

    def evidence_locations(self, protocol: str, *, limit: int = 5) -> list[str]:
        items = self.evidence.get(protocol, [])
        ordered = sorted({item.file_line for item in items})
        return ordered[:limit]

    @property
    def parse_failed_files(self) -> int:
        return len(self.parse_warnings)

    @property
    def parse_failed_all(self) -> bool:
        return self.scanned_files > 0 and self.parsed_files == 0


class _ProtocolSurfaceVisitor(ast.NodeVisitor):
    def __init__(self, *, file: str) -> None:
        self._file = file
        self._evidence: dict[str, list[ProtocolEvidence]] = {name: [] for name in _PROTOCOLS}

    @property
    def evidence(self) -> dict[str, list[ProtocolEvidence]]:
        return self._evidence

    def _record(self, protocol: str, node: ast.AST, marker: str) -> None:
        line = getattr(node, "lineno", 1)
        self._evidence[protocol].append(
            ProtocolEvidence(
                protocol=protocol,
                file=self._file,
                line=int(line),
                marker=marker,
            ),
        )

    def visit_Import(self, node: ast.Import) -> None:
        for alias in node.names:
            mod = alias.name
            lowered = mod.lower()
            if "agentflow.protocols.a2a" in lowered:
                self._record("a2a", node, f"import {mod}")
            if "agentflow.protocols.mcp" in lowered:
                self._record("mcp", node, f"import {mod}")
            if lowered.endswith("a2a_card"):
                self._record("a2a", node, f"import {mod}")
            if lowered.endswith(("mcp_client", "mcp_tool")):
                self._record("mcp", node, f"import {mod}")
        self.generic_visit(node)

    def visit_ImportFrom(self, node: ast.ImportFrom) -> None:
        module = (node.module or "").lower()
        imported_names = {name.name for name in node.names}

        if "agentflow.protocols.a2a" in module:
            self._record("a2a", node, f"from {node.module} import ...")
        if "agentflow.protocols.mcp" in module:
            self._record("mcp", node, f"from {node.module} import ...")

        if "agentflow.protocols" in module:
            if "AgentCard" in imported_names:
                self._record("a2a", node, "from agentflow.protocols import AgentCard")
            if {"MCPClient", "MCPTool", "MCPToolClient"} & imported_names:
                self._record("mcp", node, "from agentflow.protocols import MCP*")

        if "fastapi.responses" in module and "StreamingResponse" in imported_names:
            self._record("sse", node, "from fastapi.responses import StreamingResponse")

        self.generic_visit(node)

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        self._visit_function_like(node)
        self.generic_visit(node)

    def visit_AsyncFunctionDef(self, node: ast.AsyncFunctionDef) -> None:
        self._visit_function_like(node)
        self.generic_visit(node)

    def _visit_function_like(
        self,
        node: ast.FunctionDef | ast.AsyncFunctionDef,
    ) -> None:
        for decorator in node.decorator_list:
            call = decorator if isinstance(decorator, ast.Call) else None
            decorated = call.func if call else decorator
            name = _dotted_name(decorated).lower()

            if name.endswith(".websocket") or name == "websocket":
                self._record("ws", decorator, f"decorator {name}")

            if call is not None:
                route_path = _first_literal_str(call)
                if route_path and "/a2a/" in route_path:
                    self._record("a2a", decorator, f"route {route_path}")
                if _has_streaming_response_class(call):
                    self._record("sse", decorator, "response_class=StreamingResponse")

    def visit_Call(self, node: ast.Call) -> None:
        dotted = _dotted_name(node.func)
        lowered = dotted.lower()
        tail = lowered.split(".")[-1]

        if tail == "streamingresponse":
            self._record("sse", node, dotted)
        if tail in {"run_stream", "stream_events"}:
            self._record("sse", node, dotted)
        if tail == "accept" and lowered.endswith(".accept"):
            self._record("ws", node, dotted)
        if tail == "agentcard":
            self._record("a2a", node, dotted)
        if tail in {"mcpclient", "mcptool", "mcptoolclient"}:
            self._record("mcp", node, dotted)
        if ".a2a" in lowered:
            self._record("a2a", node, dotted)
        if ".mcp" in lowered:
            self._record("mcp", node, dotted)

        self.generic_visit(node)

    def visit_Constant(self, node: ast.Constant) -> None:
        value = node.value
        if not isinstance(value, str):
            return
        lowered = value.lower()
        if "text/event-stream" in lowered:
            self._record("sse", node, "text/event-stream")
        if "/a2a/" in lowered:
            self._record("a2a", node, value)


def inspect_protocol_surface(app_dir: Path) -> ProtocolSurfaceReport:
    """アプリ配下 Python ファイルを AST 監査し、プロトコル証拠を返す."""
    report = ProtocolSurfaceReport()

    for py_file in _iter_python_files(app_dir):
        report.scanned_files += 1
        relative = str(py_file.relative_to(app_dir))
        source = py_file.read_text("utf-8", errors="ignore")
        try:
            tree = ast.parse(source, filename=relative)
        except SyntaxError as exc:
            report.parse_warnings.append(
                ASTParseWarning(
                    file=relative,
                    line=exc.lineno or 1,
                    message=exc.msg,
                    suggestion=(
                        "該当ファイルの構文エラーを修正し、AST（Python構文木）で再監査できる状態にしてください"
                    ),
                    impact=("このファイルは SSE/WS/A2A/MCP 実装証拠を抽出できず、契約準拠判定が不確実になります"),
                ),
            )
            continue

        report.parsed_files += 1
        visitor = _ProtocolSurfaceVisitor(file=relative)
        visitor.visit(tree)
        for protocol, items in visitor.evidence.items():
            report.evidence[protocol].extend(items)

    return report


def _iter_python_files(app_dir: Path) -> list[Path]:
    files: list[Path] = []
    for py_file in sorted(app_dir.rglob("*.py")):
        if any(part in _EXCLUDED_PARTS for part in py_file.parts):
            continue
        files.append(py_file)
    return files


def _dotted_name(node: ast.AST) -> str:
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        base = _dotted_name(node.value)
        return f"{base}.{node.attr}" if base else node.attr
    return ""


def _first_literal_str(node: ast.Call) -> str | None:
    if not node.args:
        return None
    first = node.args[0]
    if isinstance(first, ast.Constant) and isinstance(first.value, str):
        return first.value
    return None


def _has_streaming_response_class(node: ast.Call) -> bool:
    for kw in node.keywords:
        if kw.arg != "response_class":
            continue
        target = _dotted_name(kw.value)
        if target.lower().endswith("streamingresponse"):
            return True
    return False


__all__ = [
    "ASTParseWarning",
    "ProtocolEvidence",
    "ProtocolSurfaceReport",
    "inspect_protocol_surface",
]

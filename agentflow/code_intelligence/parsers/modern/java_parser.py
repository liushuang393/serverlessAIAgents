"""Java Parser - Javaパーサー.

Javaソースコードを解析してUnifiedASTを生成します。

使用例:
    >>> parser = JavaParser()
    >>> result = parser.parse(java_source)
    >>> print(result.ast.root.name)  # クラス名
"""

from __future__ import annotations

import logging
import re
import time
from typing import Any

from agentflow.code_intelligence.ast.unified_ast import (
    ASTNode,
    ASTNodeType,
    ImportInfo,
    SymbolInfo,
    TypeInfo,
    UnifiedAST,
)
from agentflow.code_intelligence.parsers.base import (
    CodeParser,
    ParseContext,
    ParseError,
    ParseResult,
)


_logger = logging.getLogger(__name__)


class JavaParser(CodeParser):
    """Javaパーサー.

    正規表現ベースの簡易Javaパーサー。
    本格的な実装ではTree-sitterやJavaCCを使用することを推奨。

    サポート機能:
        - パッケージ宣言
        - インポート文
        - クラス/インターフェース宣言
        - メソッド宣言
        - フィールド宣言
    """

    @property
    def language(self) -> str:
        return "java"

    @property
    def supported_extensions(self) -> list[str]:
        return [".java"]

    # 正規表現パターン
    PACKAGE_PATTERN = re.compile(r"package\s+([\w.]+)\s*;")
    IMPORT_PATTERN = re.compile(r"import\s+(static\s+)?([\w.]+)\s*;")
    CLASS_PATTERN = re.compile(
        r"(public\s+|private\s+|protected\s+)?"
        r"(abstract\s+|final\s+)?"
        r"(class|interface|enum)\s+"
        r"(\w+)"
        r"(?:\s+extends\s+(\w+))?"
        r"(?:\s+implements\s+([\w,\s]+))?"
    )
    METHOD_PATTERN = re.compile(
        r"(public\s+|private\s+|protected\s+)?"
        r"(static\s+)?"
        r"(final\s+)?"
        r"([\w<>\[\],\s]+)\s+"
        r"(\w+)\s*"
        r"\(([^)]*)\)"
    )
    FIELD_PATTERN = re.compile(
        r"(public\s+|private\s+|protected\s+)?"
        r"(static\s+)?"
        r"(final\s+)?"
        r"([\w<>\[\]]+)\s+"
        r"(\w+)\s*"
        r"(?:=\s*[^;]+)?\s*;"
    )

    def parse(
        self,
        source_code: str,
        context: ParseContext | None = None,
    ) -> ParseResult:
        """Javaソースコードを解析.

        Args:
            source_code: Javaソースコード
            context: 解析コンテキスト

        Returns:
            解析結果
        """
        start_time = time.time()
        context = context or ParseContext()
        errors: list[ParseError] = []
        warnings: list[str] = []

        try:
            if not source_code or not source_code.strip():
                return ParseResult(
                    success=False,
                    errors=[ParseError("Source code cannot be empty")],
                )

            # コメントを除去（簡易）
            cleaned = self._remove_comments(source_code)

            # パッケージを抽出
            package = self._extract_package(cleaned)

            # インポートを抽出
            imports = self._extract_imports(cleaned)

            # クラスを抽出
            classes = self._extract_classes(cleaned)

            # シンボルテーブルを構築
            symbols: dict[str, SymbolInfo] = {}
            import_infos: list[ImportInfo] = []

            for imp in imports:
                import_infos.append(
                    ImportInfo(
                        module=imp["module"],
                        is_static=imp.get("is_static", False),
                    )
                )

            # ASTルートを構築
            root = ASTNode(
                node_type=ASTNodeType.PROGRAM,
                name=package or "default",
                metadata={
                    "language": "java",
                    "package": package,
                },
            )

            # クラスノードを追加
            for cls in classes:
                class_node = ASTNode(
                    node_type=ASTNodeType.CLASS,
                    name=cls["name"],
                    metadata={
                        "kind": cls["kind"],
                        "modifiers": cls.get("modifiers", []),
                        "extends": cls.get("extends"),
                        "implements": cls.get("implements", []),
                    },
                )

                # メソッドを追加
                for method in cls.get("methods", []):
                    method_node = ASTNode(
                        node_type=ASTNodeType.FUNCTION,
                        name=method["name"],
                        metadata={
                            "return_type": method.get("return_type"),
                            "parameters": method.get("parameters", []),
                            "modifiers": method.get("modifiers", []),
                        },
                    )
                    class_node.children.append(method_node)

                    # シンボルに追加
                    symbols[f"{cls['name']}.{method['name']}"] = SymbolInfo(
                        name=method["name"],
                        kind="method",
                        type_info=TypeInfo(name=method.get("return_type", "void")),
                        location=cls["name"],
                    )

                # フィールドを追加
                for field in cls.get("fields", []):
                    field_node = ASTNode(
                        node_type=ASTNodeType.VARIABLE,
                        name=field["name"],
                        metadata={
                            "type": field.get("type"),
                            "modifiers": field.get("modifiers", []),
                        },
                    )
                    class_node.children.append(field_node)

                    # シンボルに追加
                    symbols[f"{cls['name']}.{field['name']}"] = SymbolInfo(
                        name=field["name"],
                        kind="field",
                        type_info=TypeInfo(name=field.get("type", "Object")),
                        location=cls["name"],
                    )

                root.children.append(class_node)

                # クラスをシンボルに追加
                symbols[cls["name"]] = SymbolInfo(
                    name=cls["name"],
                    kind=cls["kind"],
                    location=package or "default",
                )

            # UnifiedASTを構築
            ast = UnifiedAST(
                source_language="java",
                root=root,
                symbols=symbols,
                imports=import_infos,
            )

            duration = (time.time() - start_time) * 1000

            return ParseResult(
                success=len(errors) == 0,
                ast=ast,
                errors=errors,
                warnings=warnings,
                metadata={
                    "package": package,
                    "import_count": len(imports),
                    "class_count": len(classes),
                    "duration_ms": duration,
                },
            )

        except Exception as e:
            _logger.exception(f"Java parsing failed: {e}")
            return ParseResult(
                success=False,
                errors=[ParseError(str(e))],
            )

    def _remove_comments(self, source: str) -> str:
        """コメントを除去."""
        # 単一行コメント
        source = re.sub(r"//.*$", "", source, flags=re.MULTILINE)
        # 複数行コメント
        return re.sub(r"/\*.*?\*/", "", source, flags=re.DOTALL)

    def _extract_package(self, source: str) -> str | None:
        """パッケージを抽出."""
        match = self.PACKAGE_PATTERN.search(source)
        return match.group(1) if match else None

    def _extract_imports(self, source: str) -> list[dict[str, Any]]:
        """インポートを抽出."""
        imports = []
        for match in self.IMPORT_PATTERN.finditer(source):
            imports.append(
                {
                    "module": match.group(2),
                    "is_static": bool(match.group(1)),
                }
            )
        return imports

    def _extract_classes(self, source: str) -> list[dict[str, Any]]:
        """クラスを抽出."""
        classes = []

        for match in self.CLASS_PATTERN.finditer(source):
            visibility = (match.group(1) or "").strip()
            modifier = (match.group(2) or "").strip()
            kind = match.group(3)  # class, interface, enum
            name = match.group(4)
            extends = match.group(5)
            implements_str = match.group(6)

            implements = []
            if implements_str:
                implements = [i.strip() for i in implements_str.split(",")]

            modifiers = []
            if visibility:
                modifiers.append(visibility)
            if modifier:
                modifiers.append(modifier)

            # クラス本体を抽出（簡易実装）
            class_start = match.end()
            brace_count = 0
            class_body = ""
            in_body = False

            for _i, char in enumerate(source[class_start:]):
                if char == "{":
                    brace_count += 1
                    in_body = True
                elif char == "}":
                    brace_count -= 1

                if in_body:
                    class_body += char

                if in_body and brace_count == 0:
                    break

            # メソッドとフィールドを抽出
            methods = self._extract_methods(class_body)
            fields = self._extract_fields(class_body)

            classes.append(
                {
                    "name": name,
                    "kind": kind,
                    "modifiers": modifiers,
                    "extends": extends,
                    "implements": implements,
                    "methods": methods,
                    "fields": fields,
                }
            )

        return classes

    def _extract_methods(self, class_body: str) -> list[dict[str, Any]]:
        """メソッドを抽出."""
        methods = []

        for match in self.METHOD_PATTERN.finditer(class_body):
            visibility = (match.group(1) or "").strip()
            is_static = bool(match.group(2))
            is_final = bool(match.group(3))
            return_type = match.group(4).strip()
            name = match.group(5)
            params_str = match.group(6)

            # コンストラクタかどうか
            if return_type == name:
                continue  # コンストラクタはスキップ（簡易実装）

            modifiers = []
            if visibility:
                modifiers.append(visibility)
            if is_static:
                modifiers.append("static")
            if is_final:
                modifiers.append("final")

            # パラメータを解析
            parameters = []
            if params_str.strip():
                for param in params_str.split(","):
                    param = param.strip()
                    if param:
                        parts = param.rsplit(" ", 1)
                        if len(parts) == 2:
                            parameters.append(
                                {
                                    "type": parts[0].strip(),
                                    "name": parts[1].strip(),
                                }
                            )

            methods.append(
                {
                    "name": name,
                    "return_type": return_type,
                    "parameters": parameters,
                    "modifiers": modifiers,
                }
            )

        return methods

    def _extract_fields(self, class_body: str) -> list[dict[str, Any]]:
        """フィールドを抽出."""
        fields = []

        for match in self.FIELD_PATTERN.finditer(class_body):
            visibility = (match.group(1) or "").strip()
            is_static = bool(match.group(2))
            is_final = bool(match.group(3))
            field_type = match.group(4)
            name = match.group(5)

            modifiers = []
            if visibility:
                modifiers.append(visibility)
            if is_static:
                modifiers.append("static")
            if is_final:
                modifiers.append("final")

            fields.append(
                {
                    "name": name,
                    "type": field_type,
                    "modifiers": modifiers,
                }
            )

        return fields


# レジストリに登録
def _register() -> None:
    """パーサーをレジストリに登録."""
    try:
        from agentflow.code_intelligence.parsers.registry import register_parser

        register_parser("java", JavaParser)
    except ImportError:
        pass


_register()


__all__ = ["JavaParser"]

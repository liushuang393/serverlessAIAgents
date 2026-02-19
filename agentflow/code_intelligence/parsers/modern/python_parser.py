"""Python Parser - Pythonパーサー.

Pythonソースコードを解析してUnifiedASTを生成します。

使用例:
    >>> parser = PythonParser()
    >>> result = parser.parse(python_source)
    >>> print(result.ast.root.name)  # モジュール名
"""

from __future__ import annotations

import ast as python_ast
import logging
import time

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
    ParseResult,
)


_logger = logging.getLogger(__name__)


class PythonParser(CodeParser):
    """Pythonパーサー.

    Python標準ライブラリのastモジュールを使用した正確なパーサー。

    サポート機能:
        - インポート文
        - クラス定義
        - 関数定義
        - 変数宣言（型注釈付き）
        - デコレータ
    """

    @property
    def language(self) -> str:
        return "python"

    @property
    def supported_extensions(self) -> list[str]:
        return [".py", ".pyi"]

    def parse(
        self,
        source_code: str,
        context: ParseContext | None = None,
    ) -> ParseResult:
        """Pythonソースコードを解析.

        Args:
            source_code: Pythonソースコード
            context: 解析コンテキスト

        Returns:
            解析結果
        """
        start_time = time.time()
        context = context or ParseContext()
        errors: list[str] = []
        warnings: list[str] = []

        try:
            if not source_code or not source_code.strip():
                return ParseResult(
                    success=False,
                    errors=["Source code cannot be empty"],
                )

            # Python ASTを解析
            try:
                tree = python_ast.parse(source_code)
            except SyntaxError as e:
                return ParseResult(
                    success=False,
                    errors=[f"Syntax error: {e}"],
                )

            # シンボルテーブル
            symbols: dict[str, SymbolInfo] = {}
            import_infos: list[ImportInfo] = []

            # ASTルートを構築
            module_name = context.metadata.get("module_name", "module")
            root = ASTNode(
                node_type=ASTNodeType.PROGRAM,
                name=module_name,
                metadata={
                    "language": "python",
                },
            )

            # ASTをトラバース
            for node in python_ast.iter_child_nodes(tree):
                child_node = self._convert_node(node, symbols, import_infos)
                if child_node:
                    root.children.append(child_node)

            # UnifiedASTを構築
            ast = UnifiedAST(
                source_language="python",
                root=root,
                symbols=symbols,
                imports=import_infos,
            )

            duration = (time.time() - start_time) * 1000

            return ParseResult(
                success=True,
                ast=ast,
                errors=errors,
                warnings=warnings,
                metadata={
                    "import_count": len(import_infos),
                    "symbol_count": len(symbols),
                    "duration_ms": duration,
                },
            )

        except Exception as e:
            _logger.exception(f"Python parsing failed: {e}")
            return ParseResult(
                success=False,
                errors=[str(e)],
            )

    def _convert_node(
        self,
        node: python_ast.AST,
        symbols: dict[str, SymbolInfo],
        imports: list[ImportInfo],
    ) -> ASTNode | None:
        """Python ASTノードをUnifiedAST ノードに変換."""

        if isinstance(node, python_ast.Import):
            for alias in node.names:
                imports.append(
                    ImportInfo(
                        module=alias.name,
                        alias=alias.asname,
                    )
                )
            return None

        if isinstance(node, python_ast.ImportFrom):
            module = node.module or ""
            for alias in node.names:
                imports.append(
                    ImportInfo(
                        module=f"{module}.{alias.name}" if module else alias.name,
                        alias=alias.asname,
                    )
                )
            return None

        if isinstance(node, python_ast.ClassDef):
            class_node = ASTNode(
                node_type=ASTNodeType.CLASS,
                name=node.name,
                start_line=node.lineno,
                end_line=node.end_lineno,
                metadata={
                    "decorators": [self._get_decorator_name(d) for d in node.decorator_list],
                    "bases": [self._get_name(b) for b in node.bases],
                },
            )

            # クラスメンバを追加
            for item in node.body:
                child = self._convert_node(item, symbols, imports)
                if child:
                    class_node.children.append(child)

            # シンボルに追加
            symbols[node.name] = SymbolInfo(
                name=node.name,
                kind="class",
                location=f"line {node.lineno}",
            )

            return class_node

        if isinstance(node, (python_ast.FunctionDef, python_ast.AsyncFunctionDef)):
            is_async = isinstance(node, python_ast.AsyncFunctionDef)

            # 引数を抽出
            parameters = []
            for arg in node.args.args:
                param = {
                    "name": arg.arg,
                    "type": self._get_annotation(arg.annotation) if arg.annotation else None,
                }
                parameters.append(param)

            # 戻り値の型
            return_type = self._get_annotation(node.returns) if node.returns else None

            func_node = ASTNode(
                node_type=ASTNodeType.FUNCTION,
                name=node.name,
                start_line=node.lineno,
                end_line=node.end_lineno,
                metadata={
                    "is_async": is_async,
                    "decorators": [self._get_decorator_name(d) for d in node.decorator_list],
                    "parameters": parameters,
                    "return_type": return_type,
                },
            )

            # シンボルに追加
            symbols[node.name] = SymbolInfo(
                name=node.name,
                kind="async_function" if is_async else "function",
                type_info=TypeInfo(name=return_type) if return_type else None,
                location=f"line {node.lineno}",
            )

            return func_node

        if isinstance(node, python_ast.AnnAssign):
            # 型注釈付き変数
            if isinstance(node.target, python_ast.Name):
                var_name = node.target.id
                var_type = self._get_annotation(node.annotation)

                var_node = ASTNode(
                    node_type=ASTNodeType.VARIABLE,
                    name=var_name,
                    start_line=node.lineno,
                    metadata={
                        "type": var_type,
                        "has_value": node.value is not None,
                    },
                )

                symbols[var_name] = SymbolInfo(
                    name=var_name,
                    kind="variable",
                    type_info=TypeInfo(name=var_type) if var_type else None,
                    location=f"line {node.lineno}",
                )

                return var_node

        elif isinstance(node, python_ast.Assign):
            # 通常の代入（型注釈なし）
            for target in node.targets:
                if isinstance(target, python_ast.Name):
                    var_node = ASTNode(
                        node_type=ASTNodeType.VARIABLE,
                        name=target.id,
                        start_line=node.lineno,
                        metadata={
                            "has_value": True,
                        },
                    )

                    symbols[target.id] = SymbolInfo(
                        name=target.id,
                        kind="variable",
                        location=f"line {node.lineno}",
                    )

                    return var_node

        return None

    def _get_name(self, node: python_ast.AST) -> str:
        """ASTノードから名前を取得."""
        if isinstance(node, python_ast.Name):
            return node.id
        if isinstance(node, python_ast.Attribute):
            return f"{self._get_name(node.value)}.{node.attr}"
        if isinstance(node, python_ast.Subscript):
            return f"{self._get_name(node.value)}[...]"
        return "unknown"

    def _get_annotation(self, node: python_ast.AST | None) -> str | None:
        """型注釈を文字列に変換."""
        if node is None:
            return None
        return self._get_name(node)

    def _get_decorator_name(self, node: python_ast.AST) -> str:
        """デコレータ名を取得."""
        if isinstance(node, python_ast.Call):
            return self._get_name(node.func)
        return self._get_name(node)


# レジストリに登録
def _register() -> None:
    """パーサーをレジストリに登録."""
    try:
        from agentflow.code_intelligence.parsers.registry import register_parser

        register_parser("python", PythonParser)
    except ImportError:
        pass


_register()


__all__ = ["PythonParser"]

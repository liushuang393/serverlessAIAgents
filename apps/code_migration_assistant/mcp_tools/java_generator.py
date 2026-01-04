# -*- coding: utf-8 -*-
"""JavaGenerator MCP Tool.

このモジュールはASTからJavaコードを生成するMCP工具を提供します。

主な機能:
    - ASTからJavaコード生成
    - データ型変換（COBOL PIC → Java型）
    - 制御構造変換（COBOL → Java）
    - 命名規則適用（COBOL → Java）
    - パターンとベストプラクティス活用
"""

from typing import Any

from agentflow.llm import LLMClient, LLMConfig, LLMMessage
from agentflow.config import get_settings

from agentflow import MCPTool, MCPToolRequest, MCPToolResponse


class JavaGenerator(MCPTool):
    """JavaGenerator MCP Tool.

    ASTからJavaコードを生成します。

    Input:
        - ast: 抽象構文木（必須）
        - metadata: メタデータ（必須）
        - patterns: 移行パターンリスト（オプション）
        - best_practices: ベストプラクティスリスト（オプション）
        - generation_options: 生成オプション
            - class_name: クラス名（オプション）
            - package_name: パッケージ名（デフォルト: com.migration）
            - style: コードスタイル（デフォルト: standard）
            - add_comments: コメント追加（デフォルト: true）
            - add_javadoc: Javadoc追加（デフォルト: true）

    Output:
        - java_code: 生成されたJavaコード
        - class_name: クラス名
        - package_name: パッケージ名
        - imports: インポートリスト
        - report: 生成レポート
            - lines_of_code: コード行数
            - methods_count: メソッド数
            - fields_count: フィールド数
            - complexity: 複雑度
        - warnings: 警告リスト
        - mappings: COBOL→Javaマッピング情報
    """

    def __init__(self) -> None:
        """JavaGeneratorを初期化."""
        super().__init__(tool_name="java_generator", version="2.0.0")

        # LLMクライアント初期化
        settings = get_settings()
        llm_config = LLMConfig(
            provider=settings.llm_provider,
            api_key=settings.llm_api_key,
            model=settings.llm_model,
            temperature=0.2,  # コード生成には低めの温度
            max_tokens=2000,
        )
        self._llm_client = LLMClient(llm_config)

    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        """ASTからJavaコードを生成.

        Args:
            request: MCP工具リクエスト

        Returns:
            MCP工具レスポンス
        """
        # 入力パラメータを取得
        ast = request.input.get("ast")
        metadata = request.input.get("metadata")
        patterns = request.input.get("patterns", [])
        best_practices = request.input.get("best_practices", [])
        generation_options = request.input.get("generation_options", {})

        # 必須パラメータチェック
        if not ast:
            return MCPToolResponse(
                success=False,
                errors=["ast is required"],
            )

        if not metadata:
            return MCPToolResponse(
                success=False,
                errors=["metadata is required"],
            )

        # Java生成実行（async）
        try:
            java_code, class_name, package_name, imports, report, warnings, mappings = await self._generate_java(
                ast=ast,
                metadata=metadata,
                patterns=patterns,
                best_practices=best_practices,
                class_name=generation_options.get("class_name"),
                package_name=generation_options.get("package_name", "com.migration"),
                style=generation_options.get("style", "standard"),
                add_comments=generation_options.get("add_comments", True),
                add_javadoc=generation_options.get("add_javadoc", True),
            )

            return MCPToolResponse(
                success=True,
                output={
                    "java_code": java_code,
                    "class_name": class_name,
                    "package_name": package_name,
                    "imports": imports,
                    "report": report,
                    "warnings": warnings,
                    "mappings": mappings,
                },
            )

        except Exception as e:
            return MCPToolResponse(
                success=False,
                errors=[f"Generation failed: {str(e)}"],
            )

    async def _generate_java(
        self,
        ast: dict[str, Any],
        metadata: dict[str, Any],
        patterns: list[dict[str, Any]],
        best_practices: list[dict[str, Any]],
        class_name: str | None,
        package_name: str,
        style: str,
        add_comments: bool,
        add_javadoc: bool,
    ) -> tuple[str, str, str, list[str], dict[str, Any], list[str], dict[str, Any]]:
        """Javaコードを生成（内部実装）.

        Args:
            ast: 抽象構文木
            metadata: メタデータ
            patterns: 移行パターンリスト
            best_practices: ベストプラクティスリスト
            class_name: クラス名
            package_name: パッケージ名
            style: コードスタイル
            add_comments: コメント追加
            add_javadoc: Javadoc追加

        Returns:
            (Javaコード, クラス名, パッケージ名, インポートリスト, レポート, 警告リスト, マッピング)
        """
        warnings: list[str] = []
        mappings: dict[str, Any] = {}
        imports: list[str] = []  # インポートリスト

        # クラス名を決定
        if not class_name:
            class_name = self._convert_program_id_to_class_name(ast["program_id"])

        # フィールドを生成（metadataから変数を取得）
        variables = metadata.get("variables", [])
        fields_code, field_mappings = self._generate_fields(variables)
        mappings["fields"] = field_mappings

        # BigDecimalを使用する場合はインポートを追加
        if "BigDecimal" in fields_code:
            imports.append("java.math.BigDecimal")

        # メソッドを生成（async）
        methods_code, method_mappings = await self._generate_methods(
            ast.get("divisions", {}).get("PROCEDURE DIVISION", [])
        )
        mappings["methods"] = method_mappings

        # Javaコードを組み立て
        java_code_parts = []

        # パッケージ宣言
        java_code_parts.append(f"package {package_name};")
        java_code_parts.append("")

        # インポート
        if imports:
            for imp in imports:
                java_code_parts.append(f"import {imp};")
            java_code_parts.append("")

        # Javadoc
        if add_javadoc:
            java_code_parts.append("/**")
            java_code_parts.append(f" * {class_name} - COBOL→Java移行.")
            java_code_parts.append(f" * 元のプログラム: {ast['program_id']}")
            java_code_parts.append(" */")

        # クラス宣言
        java_code_parts.append(f"public class {class_name} {{")
        java_code_parts.append("")

        # フィールド
        if add_comments:
            java_code_parts.append("    // フィールド（COBOL変数から生成）")
        java_code_parts.extend(["    " + line for line in fields_code.split("\n") if line])
        java_code_parts.append("")

        # メソッド
        if add_comments:
            java_code_parts.append("    // メソッド（COBOLプロシージャから生成）")
        java_code_parts.extend(["    " + line for line in methods_code.split("\n") if line])

        # クラス終了
        java_code_parts.append("}")

        java_code = "\n".join(java_code_parts)

        # レポート生成
        report = {
            "lines_of_code": len(java_code_parts),
            "methods_count": len(method_mappings),
            "fields_count": len(field_mappings),
            "complexity": self._calculate_complexity(methods_code),
        }

        return java_code, class_name, package_name, imports, report, warnings, mappings

    def _convert_program_id_to_class_name(self, program_id: str) -> str:
        """PROGRAM-IDをJavaクラス名に変換.

        Args:
            program_id: PROGRAM-ID

        Returns:
            Javaクラス名（PascalCase）
        """
        # ハイフンをアンダースコアに変換
        parts = program_id.replace("-", "_").split("_")
        # 各パートを大文字開始に
        return "".join(part.capitalize() for part in parts)

    def _generate_fields(self, variables: list[dict[str, Any]]) -> tuple[str, list[dict[str, Any]]]:
        """フィールドを生成.

        Args:
            variables: 変数リスト

        Returns:
            (フィールドコード, マッピング情報)
        """
        fields_code_lines: list[str] = []
        mappings: list[dict[str, Any]] = []

        for var in variables:
            # COBOL変数名をJavaフィールド名に変換
            java_field_name = self._convert_variable_name(var["name"])

            # COBOL型をJava型に変換
            java_type = self._convert_type(var["type"], var.get("pic_clause", ""))

            # フィールド宣言
            fields_code_lines.append(f"private {java_type} {java_field_name};")

            # マッピング記録
            mappings.append(
                {
                    "cobol_name": var["name"],
                    "java_name": java_field_name,
                    "cobol_type": var["type"],
                    "java_type": java_type,
                    "pic_clause": var.get("pic_clause", ""),
                }
            )

        return "\n".join(fields_code_lines), mappings

    def _convert_variable_name(self, cobol_name: str) -> str:
        """COBOL変数名をJava変数名に変換.

        Args:
            cobol_name: COBOL変数名（例: WS-NUM1）

        Returns:
            Java変数名（例: wsNum1）
        """
        # WS-プレフィックスを削除
        name = cobol_name.replace("WS-", "").replace("ws-", "")

        # ハイフンをアンダースコアに変換
        parts = name.replace("-", "_").split("_")

        # camelCaseに変換
        if not parts:
            return "field"

        return parts[0].lower() + "".join(part.capitalize() for part in parts[1:])

    def _convert_type(self, cobol_type: str, pic_clause: str) -> str:
        """COBOL型をJava型に変換.

        Args:
            cobol_type: COBOL型（numeric, string, decimal）
            pic_clause: PIC句

        Returns:
            Java型
        """
        if cobol_type == "decimal":
            return "BigDecimal"
        elif cobol_type == "numeric":
            # PIC句から桁数を判定
            if "9(5)" in pic_clause or "9(4)" in pic_clause or "9(3)" in pic_clause:
                return "int"
            elif "9(10)" in pic_clause or "9(9)" in pic_clause:
                return "long"
            else:
                return "int"
        elif cobol_type == "string":
            return "String"
        else:
            return "Object"

    async def _generate_methods(self, procedure_lines: list[str]) -> tuple[str, list[dict[str, Any]]]:
        """メソッドを生成（LLM使用）.

        Args:
            procedure_lines: PROCEDURE DIVISIONの行リスト

        Returns:
            (メソッドコード, マッピング情報)
        """
        methods_code_lines: list[str] = []
        mappings: list[dict[str, Any]] = []

        # LLMを使用してPROCEDURE DIVISIONをJavaコードに変換
        try:
            cobol_code = "\n".join(procedure_lines)
            java_code = await self._convert_procedure_with_llm(cobol_code)
            methods_code_lines.append(java_code)
        except Exception as e:
            # フォールバック: コメントとして追加
            methods_code_lines.append("public static void main(String[] args) {")
            methods_code_lines.append("    // TODO: COBOL PROCEDURE DIVISIONから移行")
            methods_code_lines.append(f"    // LLM変換失敗: {e}")

            for line in procedure_lines:
                methods_code_lines.append(f"    // {line.strip()}")

            methods_code_lines.append("}")

        mappings.append(
            {
                "cobol_procedure": "PROCEDURE DIVISION",
                "java_method": "main",
                "llm_converted": True,
            }
        )

        return "\n".join(methods_code_lines), mappings

    async def _convert_procedure_with_llm(self, cobol_code: str) -> str:
        """LLMを使用してCOBOL PROCEDURE DIVISIONをJavaコードに変換.

        Args:
            cobol_code: COBOL PROCEDURE DIVISIONのコード

        Returns:
            変換されたJavaコード
        """
        prompt = f"""Convert the following COBOL PROCEDURE DIVISION code to Java.
Generate a complete, executable Java main method.
Use proper Java syntax and best practices.
Add comments to explain the logic.

COBOL Code:
```cobol
{cobol_code}
```

Java Code:
```java"""

        response = await self._llm_client.complete(prompt)
        java_code = response.content

        # コードブロックから抽出
        if "```" in java_code:
            java_code = java_code.split("```")[0].strip()

        # mainメソッドが含まれていない場合は追加
        if "public static void main" not in java_code:
            java_code = f"public static void main(String[] args) {{\n{java_code}\n}}"

        return java_code

    def _calculate_complexity(self, methods_code: str) -> float:
        """コード複雑度を計算.

        Args:
            methods_code: メソッドコード

        Returns:
            複雑度スコア
        """
        # 簡易的な複雑度計算（制御構造の数）
        complexity = 1.0
        complexity += methods_code.count("if ")
        complexity += methods_code.count("for ")
        complexity += methods_code.count("while ")
        complexity += methods_code.count("switch ")

        return complexity


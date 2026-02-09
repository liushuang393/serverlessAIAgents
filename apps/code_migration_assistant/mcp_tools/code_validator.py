# -*- coding: utf-8 -*-
"""CodeValidator MCP Tool.

このモジュールは生成されたJavaコードを検証し、品質スコアとフィードバックを提供するMCPツールを提供します。

主な機能:
    - 構文正確性検証（30点）
    - 意味的等価性検証（40点）
    - コード品質検証（20点）
    - パフォーマンス検証（10点）
    - フィードバックと改善提案生成
"""

import re
from typing import Any

from agentflow import MCPTool, MCPToolRequest, MCPToolResponse


class CodeValidator(MCPTool):
    """CodeValidator MCP Tool.

    生成されたJavaコードを検証し、品質スコアとフィードバックを提供します。

    Input:
        - java_code: Javaコード（必須）
        - ast: 元のCOBOL AST（必須）
        - metadata: 元のCOBOLメタデータ（必須）
        - mappings: COBOL→Javaマッピング情報（オプション）
        - validation_options: 検証オプション
            - strict_mode: 厳格モード（デフォルト: false）
            - check_style: スタイルチェック（デフォルト: true）
            - check_performance: パフォーマンスチェック（デフォルト: true）

    Output:
        - score: 総合スコア（0-100）
        - scores: 各項目のスコア
            - syntax_accuracy: 構文正確性（0-30）
            - semantic_equivalence: 意味的等価性（0-40）
            - code_quality: コード品質（0-20）
            - performance: パフォーマンス（0-10）
        - is_acceptable: 受け入れ可能フラグ（スコア >= 85.0）
        - feedback: フィードバックリスト
        - suggestions: 改善提案リスト
        - errors: エラーリスト
        - warnings: 警告リスト
    """

    def __init__(self) -> None:
        """CodeValidatorを初期化."""
        super().__init__(tool_name="code_validator", version="1.0.0")
        self.acceptance_threshold = 85.0

    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        """Javaコードを検証.

        Args:
            request: MCPツールリクエスト

        Returns:
            MCPツールレスポンス
        """
        # 入力パラメータを取得
        java_code = request.input.get("java_code")
        ast = request.input.get("ast")
        metadata = request.input.get("metadata")
        mappings = request.input.get("mappings", {})
        validation_options = request.input.get("validation_options", {})

        # 必須パラメータチェック
        if not java_code:
            return MCPToolResponse(
                success=False,
                errors=["java_code is required"],
            )

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

        # 検証実行
        try:
            score, scores, is_acceptable, feedback, suggestions, errors, warnings = self._validate_code(
                java_code=java_code,
                ast=ast,
                metadata=metadata,
                mappings=mappings,
                strict_mode=validation_options.get("strict_mode", False),
                check_style=validation_options.get("check_style", True),
                check_performance=validation_options.get("check_performance", True),
            )

            return MCPToolResponse(
                success=True,
                output={
                    "score": score,
                    "scores": scores,
                    "is_acceptable": is_acceptable,
                    "feedback": feedback,
                    "suggestions": suggestions,
                    "errors": errors,
                    "warnings": warnings,
                },
            )

        except Exception as e:
            return MCPToolResponse(
                success=False,
                errors=[f"Validation failed: {str(e)}"],
            )

    def _validate_code(
        self,
        java_code: str,
        ast: dict[str, Any],
        metadata: dict[str, Any],
        mappings: dict[str, Any],
        strict_mode: bool,
        check_style: bool,
        check_performance: bool,
    ) -> tuple[float, dict[str, float], bool, list[str], list[str], list[str], list[str]]:
        """Javaコードを検証（内部実装）.

        Args:
            java_code: Javaコード
            ast: 元のCOBOL AST
            metadata: 元のCOBOLメタデータ
            mappings: COBOL→Javaマッピング情報
            strict_mode: 厳格モード
            check_style: スタイルチェック
            check_performance: パフォーマンスチェック

        Returns:
            (総合スコア, 各項目スコア, 受け入れ可能フラグ, フィードバック, 改善提案, エラー, 警告)
        """
        feedback: list[str] = []
        suggestions: list[str] = []
        errors: list[str] = []
        warnings: list[str] = []

        # 1. 構文正確性検証（30点）
        syntax_score, syntax_feedback = self._check_syntax(java_code, strict_mode)
        feedback.extend(syntax_feedback)

        # 2. 意味的等価性検証（40点）
        semantic_score, semantic_feedback = self._check_semantics(java_code, ast, metadata, mappings)
        feedback.extend(semantic_feedback)

        # 3. コード品質検証（20点）
        quality_score, quality_feedback = self._check_quality(java_code, check_style)
        feedback.extend(quality_feedback)

        # 4. パフォーマンス検証（10点）
        performance_score, performance_feedback = self._check_performance(java_code, check_performance)
        feedback.extend(performance_feedback)

        # 総合スコア計算
        total_score = syntax_score + semantic_score + quality_score + performance_score

        # 受け入れ可能判定
        is_acceptable = total_score >= self.acceptance_threshold

        # スコア詳細
        scores = {
            "syntax_accuracy": syntax_score,
            "semantic_equivalence": semantic_score,
            "code_quality": quality_score,
            "performance": performance_score,
        }

        # 改善提案生成
        if not is_acceptable:
            suggestions.append(f"総合スコアが{self.acceptance_threshold}点未満です。以下の改善を検討してください：")
            if syntax_score < 25:
                suggestions.append("- 構文エラーを修正してください")
            if semantic_score < 30:
                suggestions.append("- COBOL元コードとの意味的等価性を確認してください")
            if quality_score < 15:
                suggestions.append("- コード品質を改善してください（命名規則、コメント、可読性）")
            if performance_score < 7:
                suggestions.append("- パフォーマンスを最適化してください")

        return total_score, scores, is_acceptable, feedback, suggestions, errors, warnings

    def _check_syntax(self, java_code: str, strict_mode: bool) -> tuple[float, list[str]]:
        """構文正確性を検証.

        Args:
            java_code: Javaコード
            strict_mode: 厳格モード

        Returns:
            (スコア, フィードバック)
        """
        score = 30.0  # 最大30点
        feedback: list[str] = []

        # 基本的な構文チェック
        # 1. クラス宣言チェック
        if not re.search(r"public\s+class\s+\w+", java_code):
            score -= 10
            feedback.append("クラス宣言が見つかりません")

        # 2. パッケージ宣言チェック
        if not re.search(r"package\s+[\w.]+;", java_code):
            score -= 5
            feedback.append("パッケージ宣言が見つかりません")

        # 3. 括弧のバランスチェック
        if java_code.count("{") != java_code.count("}"):
            score -= 10
            feedback.append("括弧のバランスが取れていません")

        # 4. セミコロンチェック（簡易）
        lines = java_code.split("\n")
        missing_semicolons = 0
        for line in lines:
            line_stripped = line.strip()
            if line_stripped and not line_stripped.startswith("//") and not line_stripped.startswith("/*"):
                if (
                    line_stripped.endswith("{")
                    or line_stripped.endswith("}")
                    or line_stripped.startswith("package")
                    or line_stripped.startswith("import")
                    or line_stripped.startswith("public")
                    or line_stripped.startswith("private")
                ):
                    continue
                if not line_stripped.endswith(";") and not line_stripped.endswith("{"):
                    missing_semicolons += 1

        if missing_semicolons > 0:
            score -= min(5, missing_semicolons)
            feedback.append(f"セミコロンが不足している可能性があります（{missing_semicolons}箇所）")

        return max(0, score), feedback

    def _check_semantics(
        self,
        java_code: str,
        ast: dict[str, Any],
        metadata: dict[str, Any],
        mappings: dict[str, Any],
    ) -> tuple[float, list[str]]:
        """意味的等価性を検証.

        Args:
            java_code: Javaコード
            ast: 元のCOBOL AST
            metadata: 元のCOBOLメタデータ
            mappings: COBOL→Javaマッピング情報

        Returns:
            (スコア, フィードバック)
        """
        score = 40.0  # 最大40点
        feedback: list[str] = []

        # 1. データ型マッピングチェック（15点）
        field_mappings = mappings.get("fields", [])
        cobol_variables = metadata.get("variables", [])

        if len(field_mappings) < len(cobol_variables):
            missing_count = len(cobol_variables) - len(field_mappings)
            score -= min(10, missing_count * 2)
            feedback.append(f"{missing_count}個の変数がマッピングされていません")

        # 2. ロジック等価性チェック（20点）
        # Javaコード内のTODOコメント数をチェック
        todo_count = java_code.count("TODO")

        if todo_count > 0:
            score -= min(15, todo_count * 3)
            feedback.append(f"{todo_count}個のTODOコメントが残っています（ロジックが未実装）")

        # 3. エラーハンドリングチェック（5点）
        if "try" not in java_code and "catch" not in java_code:
            score -= 5
            feedback.append("エラーハンドリングが実装されていません")

        return max(0, score), feedback

    def _check_quality(self, java_code: str, check_style: bool) -> tuple[float, list[str]]:
        """コード品質を検証.

        Args:
            java_code: Javaコード
            check_style: スタイルチェック

        Returns:
            (スコア, フィードバック)
        """
        score = 20.0  # 最大20点
        feedback: list[str] = []

        if not check_style:
            return score, feedback

        # 1. ベストプラクティスチェック（10点）
        # Javadocチェック
        if "/**" not in java_code:
            score -= 3
            feedback.append("Javadocコメントがありません")

        # 命名規則チェック（クラス名はPascalCase）
        class_match = re.search(r"public\s+class\s+(\w+)", java_code)
        if class_match:
            class_name = class_match.group(1)
            if not class_name[0].isupper():
                score -= 2
                feedback.append("クラス名はPascalCaseで記述してください")

        # 2. コメント・Javadocチェック（5点）
        lines = java_code.split("\n")
        comment_lines = sum(1 for line in lines if line.strip().startswith("//") or line.strip().startswith("/*"))
        code_lines = len([line for line in lines if line.strip() and not line.strip().startswith("//")])

        if code_lines > 0:
            comment_ratio = comment_lines / code_lines
            if comment_ratio < 0.1:
                score -= 3
                feedback.append("コメントが不足しています")

        # 3. 可読性チェック（5点）
        # 行の長さチェック
        long_lines = sum(1 for line in lines if len(line) > 120)
        if long_lines > 0:
            score -= min(3, long_lines)
            feedback.append(f"{long_lines}行が120文字を超えています")

        return max(0, score), feedback

    def _check_performance(self, java_code: str, check_performance: bool) -> tuple[float, list[str]]:
        """パフォーマンスを検証.

        Args:
            java_code: Javaコード
            check_performance: パフォーマンスチェック

        Returns:
            (スコア, フィードバック)
        """
        score = 10.0  # 最大10点
        feedback: list[str] = []

        if not check_performance:
            return score, feedback

        # 1. アルゴリズム効率チェック（5点）
        # ネストされたループチェック
        nested_loops = java_code.count("for") + java_code.count("while")
        if nested_loops > 3:
            score -= 3
            feedback.append("ネストされたループが多すぎます")

        # 2. メモリ使用チェック（3点）
        # 不要なオブジェクト生成チェック（簡易）
        if java_code.count("new ") > 10:
            score -= 2
            feedback.append("オブジェクト生成が多すぎる可能性があります")

        # 3. 最適化チェック（2点）
        # String連結チェック
        if java_code.count("+") > 5 and "String" in java_code:
            score -= 2
            feedback.append("String連結にStringBuilderの使用を検討してください")

        return max(0, score), feedback


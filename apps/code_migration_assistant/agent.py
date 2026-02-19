"""Code Migration Agent.

@agent 装饰器を使用した COBOL→Java 移行 Agent。
LLM が必要な処理（コード翻訳、差分分析、修復提案）を担当。
確定的な処理は @tool 経由で Skill/Adapter に委譲。

使用例:
    >>> from agentflow import AgentClient
    >>> result = await AgentClient.get("CodeMigrationAgent").invoke({
    ...     "cobol_code": "...",
    ...     "source_language": "cobol",
    ...     "target_language": "java",
    ... })
"""

from typing import TYPE_CHECKING, Any

from apps.code_migration_assistant.adapters import CobolAdapter, JavaAdapter

from agentflow import agent, tool
from agentflow.config import get_settings
from agentflow.llm import LLMClient, LLMConfig


if TYPE_CHECKING:
    from apps.code_migration_assistant.adapters.base import AST


@agent
class CodeMigrationAgent:
    """COBOL→Java 移行 Agent.

    職責（LLM が担当）:
    - コードの意味を理解し、等価な Java コードを生成
    - 差分分析の結果から原因を特定し、修復を提案
    - ユーザーとの対話と説明

    職責外（@tool で委譲）:
    - 構文解析（確定的、CobolAdapter）
    - 型マッピング（ルールベース、JavaAdapter）
    - コンパイル・実行（外部ツール）
    - 結果比較（確定的、Skill）
    """

    # システムプロンプト（SKILL.md の内容も自動追加される）
    system_prompt = """あなたは COBOL→Java 移行の専門家です。

## 役割
1. COBOL コードを分析し、等価な Java コードを生成する
2. 移行前後の出力の差異を分析し、原因を特定する
3. 差異が見つかった場合、修正コードを提案する

## 制約
- 推測しない：不明な構文は「TODO: 手動確認必要」とコメント
- 保守性重視：読みやすい Java コードを生成
- テスト可能：依存性注入を考慮した設計

## 利用可能なツール
- parse_source: ソースコード解析（確定的処理）
- generate_skeleton: Java コード骨格生成（確定的処理）
- compile_code: Java コードコンパイル
- compare_outputs: 出力比較（確定的処理）

あなたの主な仕事は、parse_source で得た AST を元に、
PROCEDURE DIVISION の **ロジック部分を Java コードに翻訳する** ことです。
"""

    # 使用する Skill（SKILL.md の内容がプロンプトに追加される）
    # skills = ["cobol-migration"]  # TODO: カスタム Skill 登録後に有効化

    def __init__(self) -> None:
        """初期化."""
        # Adapters（確定的処理を委譲）
        self._source_adapter = CobolAdapter()
        self._target_adapter = JavaAdapter()

        # LLM クライアント（コード翻訳用）
        settings = get_settings()
        self._llm = LLMClient(
            LLMConfig(
                provider=settings.llm_provider,
                api_key=settings.llm_api_key,
                model=settings.llm_model,
                temperature=0.2,  # コード生成は低温度
                max_tokens=4000,
            )
        )

        # 状態管理
        self._current_ast: AST | None = None
        self._current_java_code: str | None = None

    # =========================================================================
    # @tool: 確定的な処理（LLM 不要）
    # =========================================================================

    @tool
    def parse_source(self, code: str, language: str = "cobol") -> dict[str, Any]:
        """ソースコードを解析.

        Args:
            code: ソースコード
            language: 言語（cobol, rpg, ...）

        Returns:
            AST + メタデータ
        """
        if language.lower() != "cobol":
            return {"error": f"Unsupported language: {language}"}

        ast = self._source_adapter.parse(code)
        self._current_ast = ast

        return {
            "program_id": ast.program_id,
            "variables": ast.variables,
            "procedures": ast.procedures,
            "external_calls": self._source_adapter.identify_external_calls(ast),
            "divisions": list(ast.divisions.keys()),
        }

    @tool
    def generate_skeleton(self, class_name: str | None = None) -> dict[str, Any]:
        """Java コード骨格を生成.

        Args:
            class_name: クラス名（省略時は PROGRAM-ID から生成）

        Returns:
            Java コード骨格
        """
        if not self._current_ast:
            return {"error": "No AST available. Call parse_source first."}

        if not class_name:
            class_name = self._to_pascal_case(self._current_ast.program_id)

        skeleton = self._target_adapter.generate_skeleton(self._current_ast, class_name)

        return {
            "class_name": class_name,
            "skeleton": skeleton,
            "type_mappings": [
                {
                    "cobol": v["name"],
                    "java": self._to_camel_case(v["name"]),
                    "type": self._target_adapter.get_type_mapping(
                        v.get("type", ""), v.get("pic_clause", "")
                    ),
                }
                for v in self._current_ast.variables
            ],
        }

    @tool
    def compile_code(self, java_code: str) -> dict[str, Any]:
        """Java コードをコンパイル.

        Args:
            java_code: Java ソースコード

        Returns:
            コンパイル結果
        """
        success, errors = self._target_adapter.compile(java_code)

        if success:
            self._current_java_code = java_code

        return {
            "success": success,
            "errors": errors,
        }

    @tool
    def compare_outputs(self, expected: dict[str, Any], actual: dict[str, Any]) -> dict[str, Any]:
        """出力を比較.

        Args:
            expected: 期待される出力（COBOL 実行結果）
            actual: 実際の出力（Java 実行結果）

        Returns:
            比較結果
        """
        differences: list[dict[str, Any]] = []

        for key in set(expected.keys()) | set(actual.keys()):
            exp_val = expected.get(key)
            act_val = actual.get(key)

            if exp_val != act_val:
                differences.append(
                    {
                        "field": key,
                        "expected": exp_val,
                        "actual": act_val,
                    }
                )

        return {
            "is_equal": len(differences) == 0,
            "differences": differences,
            "match_rate": 1.0 - len(differences) / max(len(expected), 1),
        }

    # =========================================================================
    # ヘルパーメソッド
    # =========================================================================

    def _to_pascal_case(self, name: str) -> str:
        """PascalCase に変換."""
        parts = name.replace("-", "_").split("_")
        return "".join(p.capitalize() for p in parts)

    def _to_camel_case(self, name: str) -> str:
        """camelCase に変換."""
        name = name.replace("WS-", "").replace("ws-", "")
        parts = name.replace("-", "_").split("_")
        if not parts:
            return "field"
        return parts[0].lower() + "".join(p.capitalize() for p in parts[1:])

    # =========================================================================
    # メイン処理（LLM が担当）
    # =========================================================================

    async def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """移行処理を実行.

        Args:
            input_data: 入力データ
                - cobol_code: COBOL ソースコード
                - source_language: ソース言語（デフォルト: cobol）
                - target_language: ターゲット言語（デフォルト: java）

        Returns:
            移行結果
        """
        cobol_code = input_data.get("cobol_code", "")
        if not cobol_code:
            return {"success": False, "error": "cobol_code is required"}

        # Step 1: ソース解析（@tool: 確定的処理）
        parse_result = self.parse_source(cobol_code, "cobol")
        if "error" in parse_result:
            return {"success": False, "error": parse_result["error"], "stage": "parsing"}

        # Step 2: 骨格生成（@tool: 確定的処理）
        skeleton_result = self.generate_skeleton()
        if "error" in skeleton_result:
            return {"success": False, "error": skeleton_result["error"], "stage": "skeleton"}

        # Step 3: LLM によるコード翻訳（コアロジック）
        java_code = await self._translate_with_llm(
            parse_result=parse_result,
            skeleton=skeleton_result["skeleton"],
            class_name=skeleton_result["class_name"],
        )

        # Step 4: コンパイル検証（@tool: 確定的処理）
        compile_result = self.compile_code(java_code)

        # Step 5: コンパイルエラーがあれば LLM で修復
        if not compile_result["success"]:
            java_code = await self._fix_compile_errors(java_code, compile_result["errors"])
            compile_result = self.compile_code(java_code)

        return {
            "success": compile_result["success"],
            "java_code": java_code,
            "class_name": skeleton_result["class_name"],
            "parse_result": parse_result,
            "compile_result": compile_result,
        }

    async def _translate_with_llm(
        self,
        parse_result: dict[str, Any],
        skeleton: str,
        class_name: str,
    ) -> str:
        """LLM でコードを翻訳.

        Args:
            parse_result: 解析結果
            skeleton: Java コード骨格
            class_name: クラス名

        Returns:
            完成した Java コード
        """
        # PROCEDURE DIVISION のコードを取得
        proc_div = ""
        if self._current_ast:
            proc_lines = self._current_ast.divisions.get("PROCEDURE DIVISION", [])
            proc_div = "\n".join(proc_lines)

        prompt = f"""以下の COBOL PROCEDURE DIVISION を Java メソッドに変換してください。

## 入力情報

### COBOL PROCEDURE DIVISION:
```cobol
{proc_div}
```

### 変数マッピング:
{parse_result.get("variables", [])}

### Java コード骨格:
```java
{skeleton}
```

## 要求

1. PROCEDURE DIVISION のロジックを Java メソッドとして実装
2. 骨格の TODO 部分を実際のコードで埋める
3. メインメソッドから適切なエントリポイントを呼び出す
4. 完全な Java クラスを出力（コードブロックなし、純粋な Java コードのみ）

## 出力

Java コードのみを出力してください（```java などのマークダウンは不要）。
"""

        response = await self._llm.complete(prompt)
        java_code = response.content.strip()

        # コードブロックの除去
        if java_code.startswith("```"):
            lines = java_code.split("\n")
            java_code = "\n".join(lines[1:-1] if lines[-1].startswith("```") else lines[1:])

        return java_code

    async def _fix_compile_errors(self, java_code: str, errors: list[str]) -> str:
        """コンパイルエラーを LLM で修復.

        Args:
            java_code: 現在の Java コード
            errors: コンパイルエラーリスト

        Returns:
            修正後の Java コード
        """
        prompt = f"""以下の Java コードにコンパイルエラーがあります。修正してください。

## 現在のコード:
```java
{java_code}
```

## コンパイルエラー:
{chr(10).join(errors)}

## 要求

1. エラーを修正した完全な Java コードを出力
2. コードブロックなし、純粋な Java コードのみ

## 出力

修正後の Java コードのみを出力してください。
"""

        response = await self._llm.complete(prompt)
        fixed_code = response.content.strip()

        # コードブロックの除去
        if fixed_code.startswith("```"):
            lines = fixed_code.split("\n")
            fixed_code = "\n".join(lines[1:-1] if lines[-1].startswith("```") else lines[1:])

        return fixed_code

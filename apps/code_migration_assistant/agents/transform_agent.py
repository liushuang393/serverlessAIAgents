# -*- coding: utf-8 -*-
"""Transform Agent - コード変換.

LLM が全権を持って COBOL → Java 翻訳を実行。
確定性ツールはメソッドとして提供。

Factory パターンにより、設定ベースで言語ペアを切り替え可能。
"""

from typing import Any

from agentflow import agent

from apps.code_migration_assistant.adapters import (
    SourceLanguageAdapter,
    TargetLanguageAdapter,
    get_adapter_factory,
)


# デフォルトプロンプト（設定ファイルがない場合のフォールバック）
_DEFAULT_PROMPT = """あなたは COBOL→Java コード変換の専門家です。

## 役割
COBOL ソースコードを等価な Java コードに翻訳します。

## 入力
- COBOL ソースコード
- 解析済み AST（parse_cobol ツールの結果）

## 出力
- 完全な Java クラス（package, import, class, methods）
- コンパイル可能なコード

## ルール
1. COBOL の WORKING-STORAGE → Java フィールド
2. COBOL の PROCEDURE DIVISION → Java メソッド
3. COBOL の PERFORM → Java メソッド呼び出し or ループ
4. 数値精度を維持（BigDecimal 使用推奨）
5. 不明な構文は TODO コメントで残す

## 重要
- あなたがコード翻訳の全責任を持ちます
- ツールは補助的な確認用です
- 自由に最適な Java コードを生成してください
"""


@agent
class TransformAgent:
    """コード変換 Agent.

    職責：ソース言語コードをターゲット言語コードに翻訳
    LLM が全権を持って翻訳ロジックを実行

    Attributes:
        migration_type: 移行タイプ（例: "cobol-to-java"）
    """

    # system_prompt は動的に設定（__init__ で設定ファイルから読み込み）
    system_prompt: str = _DEFAULT_PROMPT

    def __init__(
        self,
        migration_type: str = "cobol-to-java",
        source_adapter: SourceLanguageAdapter | None = None,
        target_adapter: TargetLanguageAdapter | None = None,
    ) -> None:
        """初期化.

        Args:
            migration_type: 移行タイプ名
            source_adapter: ソースアダプター（DI用、省略時は Factory から取得）
            target_adapter: ターゲットアダプター（DI用、省略時は Factory から取得）
        """
        self._migration_type = migration_type
        self._factory = get_adapter_factory()

        # DI またはファクトリーからアダプターを取得
        if source_adapter is not None:
            self._source_adapter = source_adapter
        else:
            self._source_adapter = self._factory.get_source_adapter(migration_type)

        if target_adapter is not None:
            self._target_adapter = target_adapter
        else:
            self._target_adapter = self._factory.get_target_adapter(migration_type)

        # プロンプトを設定ファイルから読み込み
        prompt = self._factory.get_prompt(migration_type, "transform")
        if prompt:
            self.system_prompt = prompt

    # =========================================================================
    # 確定性ツール（LLM から呼び出し可能）
    # =========================================================================

    def parse_source(self, source_code: str) -> dict[str, Any]:
        """ソースコードを解析（確定的処理）.

        Args:
            source_code: ソースコード

        Returns:
            AST + 変数情報 + プロシージャ情報
        """
        ast = self._source_adapter.parse(source_code)
        return {
            "success": True,
            "language": self._source_adapter.language_name,
            "program_id": ast.program_id,
            "variables": ast.variables,
            "procedures": ast.procedures,
            "divisions": {
                name: lines[:10] if len(lines) > 10 else lines  # 最初の10行
                for name, lines in ast.divisions.items()
            },
            "procedure_code": "\n".join(
                ast.divisions.get("PROCEDURE DIVISION", [])
            ),
        }

    # 後方互換性のためのエイリアス
    def parse_cobol(self, cobol_code: str) -> dict[str, Any]:
        """COBOL コードを解析（parse_source のエイリアス）."""
        return self.parse_source(cobol_code)

    def compile_target(self, target_code: str) -> dict[str, Any]:
        """ターゲットコードをコンパイル（確定的処理）.

        Args:
            target_code: ターゲット言語ソースコード

        Returns:
            コンパイル結果（成功/エラー）
        """
        success, errors = self._target_adapter.compile(target_code)
        return {
            "success": success,
            "language": self._target_adapter.language_name,
            "errors": errors,
        }

    # 後方互換性のためのエイリアス
    def compile_java(self, java_code: str) -> dict[str, Any]:
        """Java コードをコンパイル（compile_target のエイリアス）."""
        return self.compile_target(java_code)

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """コード変換を実行.

        LLM が全責任を持って翻訳。

        Args:
            input_data:
                - source_code: ソースコード（または cobol_code）

        Returns:
            - parse_result: 解析結果
            - source_code: ソースコード
        """
        # source_code または cobol_code を取得（後方互換性）
        source_code = input_data.get("source_code") or input_data.get("cobol_code", "")
        if not source_code:
            return {"success": False, "error": "source_code is required"}

        # Step 1: ソースコード解析（確定的処理）
        parse_result = self.parse_source(source_code)

        # Step 2: LLM にコード翻訳を依頼
        # （AgentClient 経由で呼ばれた場合、LLM が自動的に処理）
        return {
            "success": True,
            "migration_type": self._migration_type,
            "source_language": self._source_adapter.language_name,
            "target_language": self._target_adapter.language_name,
            "parse_result": parse_result,
            "source_code": source_code,
            "message": "Parse complete. LLM should now generate target code.",
        }


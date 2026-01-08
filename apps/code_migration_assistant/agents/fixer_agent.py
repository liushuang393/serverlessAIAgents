# -*- coding: utf-8 -*-
"""Fixer Agent - 自動修復.

LLM が全権を持ってコード修復を実行。
CheckerAgent の差異分析結果を元にターゲットコードを修正。

Factory パターンにより、設定ベースで言語ペアを切り替え可能。
"""

from typing import Any

from agentflow import agent

from apps.code_migration_assistant.adapters import (
    TargetLanguageAdapter,
    get_adapter_factory,
)


# デフォルトプロンプト
_DEFAULT_PROMPT = """あなたはコード修復の専門家です。

## 役割
CheckerAgent が検出した差異を分析し、ターゲットコードを修正します。

## 入力
- 現在のターゲットコード
- 差異レポート（どのフィールドがどう違うか）
- 元のソースコード（参考用）

## 修復戦略
1. **値の不一致**: ロジックのバグを特定・修正
2. **精度差異**: BigDecimal の使用、丸め処理の調整
3. **フォーマット差異**: 出力フォーマットの調整
4. **Missing**: 未実装ロジックの追加

## 出力
- 修正後のターゲットコード
- 修正箇所の説明

## 重要
- あなたがコード修正の全責任を持ちます
- 最小限の変更で修正する
- 修正理由を明確にする
"""


@agent
class FixerAgent:
    """自動修復 Agent.

    職責：
    1. CheckerAgent の差異分析結果を受け取る
    2. 差異の原因を特定
    3. ターゲットコードを修正

    Attributes:
        migration_type: 移行タイプ（例: "cobol-to-java"）
    """

    # system_prompt は __init__ で動的に設定
    system_prompt: str = _DEFAULT_PROMPT

    def __init__(
        self,
        migration_type: str = "cobol-to-java",
        target_adapter: TargetLanguageAdapter | None = None,
    ) -> None:
        """初期化.

        Args:
            migration_type: 移行タイプ名
            target_adapter: ターゲットアダプター（DI用）
        """
        self._migration_type = migration_type
        self._factory = get_adapter_factory()

        # DI またはファクトリーからアダプターを取得
        if target_adapter is not None:
            self._target_adapter = target_adapter
        else:
            self._target_adapter = self._factory.get_target_adapter(migration_type)

        # プロンプトを設定ファイルから読み込み
        prompt = self._factory.get_prompt(migration_type, "fixer")
        if prompt:
            self.system_prompt = prompt

    # =========================================================================
    # 確定性ツール
    # =========================================================================

    def compile_target(self, target_code: str) -> dict[str, Any]:
        """ターゲットコードをコンパイル（確定的処理）.

        Args:
            target_code: ターゲット言語ソースコード

        Returns:
            コンパイル結果
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

    def execute_target(
        self, target_code: str, inputs: dict[str, Any] | None = None
    ) -> dict[str, Any]:
        """ターゲットコードを実行（確定的処理）.

        Args:
            target_code: ターゲット言語ソースコード
            inputs: 入力パラメータ

        Returns:
            実行結果
        """
        result = self._target_adapter.execute(target_code, inputs or {})
        return {
            "success": result.success,
            "language": self._target_adapter.language_name,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "return_code": result.return_code,
        }

    # 後方互換性のためのエイリアス
    def execute_java(
        self, java_code: str, inputs: dict[str, Any] | None = None
    ) -> dict[str, Any]:
        """Java コードを実行（execute_target のエイリアス）."""
        return self.execute_target(java_code, inputs)

    def extract_error_location(self, errors: list[str]) -> list[dict[str, Any]]:
        """エラー位置を抽出（確定的処理）.

        Args:
            errors: コンパイルエラーリスト

        Returns:
            エラー位置情報
        """
        locations = []
        for error in errors:
            # javac エラー形式: ファイル名:行番号: エラー: メッセージ
            if ":" in error:
                parts = error.split(":", 3)
                if len(parts) >= 3:
                    try:
                        locations.append({
                            "file": parts[0],
                            "line": int(parts[1]) if parts[1].isdigit() else 0,
                            "message": parts[2].strip() if len(parts) > 2 else error,
                        })
                    except (ValueError, IndexError):
                        locations.append({"file": "", "line": 0, "message": error})
                else:
                    locations.append({"file": "", "line": 0, "message": error})
            else:
                locations.append({"file": "", "line": 0, "message": error})
        return locations

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """コードを修復.

        Args:
            input_data:
                - target_code: 現在のターゲットコード（または java_code）
                - diff_report: 差異レポート
                - source_code: 元のソースコード（参考用）
                - comparison: 比較結果

        Returns:
            - target_code: ターゲットコード
            - compile_result: コンパイル結果
        """
        # target_code または java_code を取得（後方互換性）
        target_code = input_data.get("target_code") or input_data.get("java_code", "")
        diff_report = input_data.get("diff_report", "")

        if not target_code:
            return {"success": False, "error": "target_code is required"}

        # コンパイルチェック
        compile_result = self.compile_target(target_code)

        return {
            "success": True,
            "migration_type": self._migration_type,
            "target_language": self._target_adapter.language_name,
            "target_code": target_code,
            "diff_report": diff_report,
            "compile_result": compile_result,
            "message": "Context ready. LLM should analyze and fix the code.",
        }


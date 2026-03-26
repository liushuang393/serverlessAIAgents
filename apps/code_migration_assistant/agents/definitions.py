"""Migrationエージェント定義モジュール.

claude-agent-sdk の AgentDefinition を使用して、
各パイプラインステージのエージェントを定義する。
プロンプトは agents/prompts/*.md から読み込み（進化対応）。
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from claude_agent_sdk import AgentDefinition
else:

    @dataclass
    class AgentDefinition:
        """claude_agent_sdk.AgentDefinition の最小互換型."""

        description: str
        prompt: str
        tools: list[str]


# プロンプトファイルのディレクトリ
_PROMPTS_DIR = Path(__file__).parent / "prompts"


def _load_prompt(name: str) -> str:
    """プロンプトファイルを読み込む.

    Args:
        name: プロンプト名（拡張子なし）

    Returns:
        プロンプト文字列
    """
    prompt_file = _PROMPTS_DIR / f"{name}.md"
    if not prompt_file.exists():
        msg = f"プロンプトファイルが見つかりません: {prompt_file}"
        raise FileNotFoundError(msg)
    return prompt_file.read_text(encoding="utf-8")


def get_agent_definition(stage: str) -> AgentDefinition:
    """ステージ名からAgentDefinitionを取得する.

    Args:
        stage: ステージ名（analyzer/designer/transformer/test_generator/verifier/quality_gate）

    Returns:
        AgentDefinition インスタンス

    Raises:
        ValueError: 未知のステージ名の場合
    """
    definitions: dict[str, AgentDefinition] = {
        "analyzer": _get_analyzer_definition(),
        "designer": _get_designer_definition(),
        "transformer": _get_transformer_definition(),
        "test_generator": _get_test_generator_definition(),
        "verifier": _get_verifier_definition(),
        "quality_gate": _get_quality_gate_definition(),
    }
    if stage not in definitions:
        msg = f"未知のステージ: {stage}. 有効値: {list(definitions.keys())}"
        raise ValueError(msg)
    return definitions[stage]


def _get_analyzer_definition() -> AgentDefinition:
    """分析エージェント定義."""
    return AgentDefinition(
        description="COBOLレガシーコード分析専門家。COBOL AST抽出・リスク評価・I/O契約特定を行う。",
        prompt=_load_prompt("analyzer"),
        tools=["Read", "Glob", "Grep", "Bash"],
    )


def _get_designer_definition() -> AgentDefinition:
    """設計エージェント定義."""
    return AgentDefinition(
        description="COBOL→Java Spring Boot移行設計専門家。REST API設計・クラス構造設計を行う。",
        prompt=_load_prompt("designer"),
        tools=["Read", "Glob", "Grep"],
    )


def _get_transformer_definition() -> AgentDefinition:
    """変換エージェント定義."""
    return AgentDefinition(
        description="COBOL→Java Spring Bootコード変換専門家。完全なJavaプロジェクトを生成する。",
        prompt=_load_prompt("transformer"),
        tools=["Read", "Write", "Glob", "Grep"],
    )


def _get_test_generator_definition() -> AgentDefinition:
    """テスト生成エージェント定義."""
    return AgentDefinition(
        description="JUnit 5テストケース生成専門家。ControllerテストとServiceテストを生成する。",
        prompt=_load_prompt("test_generator"),
        tools=["Read", "Write", "Glob", "Grep"],
    )


def _get_verifier_definition() -> AgentDefinition:
    """検証エージェント定義."""
    return AgentDefinition(
        description="COBOL AST vs Java AST差分検証専門家。構造的等価性を検証する。",
        prompt=_load_prompt("verifier"),
        tools=["Read", "Glob", "Grep", "Bash"],
    )


def _get_quality_gate_definition() -> AgentDefinition:
    """品質ゲートエージェント定義."""
    return AgentDefinition(
        description="移行品質最終判定専門家。全ステージ成果物を総合評価して合否を判定する。",
        prompt=_load_prompt("quality_gate"),
        tools=["Read", "Glob", "Grep"],
    )


def list_stages() -> list[str]:
    """利用可能なステージ名一覧を返す."""
    return ["analyzer", "designer", "transformer", "test_generator", "verifier", "quality_gate"]


def reload_prompt(stage: str) -> None:
    """特定ステージのプロンプトを再読み込み（Evolution後に使用）.

    Args:
        stage: プロンプトを再読み込みするステージ名
    """
    # AgentDefinitionはstateless（prompts/*.mdから毎回読み込む）なので
    # この関数は明示的なリロード意図を示すためのドキュメント的な関数
    prompt_file = _PROMPTS_DIR / f"{stage}.md"
    if not prompt_file.exists():
        msg = f"プロンプトファイルが見つかりません: {prompt_file}"
        raise FileNotFoundError(msg)


def get_prompt_path(stage: str) -> Path:
    """ステージのプロンプトファイルパスを返す（Evolution用）.

    Args:
        stage: ステージ名

    Returns:
        プロンプトファイルパス
    """
    return _PROMPTS_DIR / f"{stage}.md"


def get_stage_info() -> list[dict[str, Any]]:
    """全ステージの情報を返す（診断用）."""
    result = []
    for stage in list_stages():
        prompt_path = get_prompt_path(stage)
        result.append(
            {
                "stage": stage,
                "prompt_file": str(prompt_path),
                "prompt_exists": prompt_path.exists(),
                "prompt_size": prompt_path.stat().st_size if prompt_path.exists() else 0,
            }
        )
    return result

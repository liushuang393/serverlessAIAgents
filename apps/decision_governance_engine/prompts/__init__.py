# -*- coding: utf-8 -*-
"""Decision Governance Engine - プロンプト管理モジュール.

全Agentのシステムプロンプトを一元管理し、外部ファイルからの読み込みをサポート。

構成:
    prompts/
    ├── __init__.py          # このファイル
    ├── common_system.md     # 全Agent共通プロンプト
    ├── cognitive_gate.md    # 認知前処理プロンプト
    ├── dao.md              # 道（本質分析）
    ├── fa.md               # 法（戦略選定）
    ├── shu.md              # 術（実行計画）
    ├── qi.md               # 器（技術実装）
    └── review.md           # 検証
"""

from pathlib import Path
from typing import Any

# プロンプトディレクトリのパス
PROMPTS_DIR = Path(__file__).parent


def load_prompt(name: str) -> str:
    """プロンプトファイルを読み込み.

    Args:
        name: プロンプト名（拡張子なし）

    Returns:
        プロンプト内容

    Raises:
        FileNotFoundError: ファイルが見つからない場合
    """
    prompt_path = PROMPTS_DIR / f"{name}.md"
    if not prompt_path.exists():
        msg = f"Prompt file not found: {prompt_path}"
        raise FileNotFoundError(msg)
    return prompt_path.read_text(encoding="utf-8")


def get_common_system_prompt() -> str:
    """全Agent共通システムプロンプトを取得."""
    return load_prompt("common_system")


def get_agent_prompt(agent_name: str) -> str:
    """Agent固有プロンプトを取得.

    Args:
        agent_name: Agent名（例: "dao", "fa", "cognitive_gate"）

    Returns:
        Agent固有プロンプト
    """
    return load_prompt(agent_name)


def build_full_prompt(agent_name: str, user_context: dict[str, Any] | None = None) -> str:
    """完全なプロンプトを構築（共通 + Agent固有）.

    Args:
        agent_name: Agent名
        user_context: ユーザーコンテキスト（テンプレート変数用）

    Returns:
        結合されたプロンプト
    """
    common = get_common_system_prompt()
    agent_specific = get_agent_prompt(agent_name)

    full_prompt = f"{common}\n\n---\n\n{agent_specific}"

    # テンプレート変数を置換
    if user_context:
        for key, value in user_context.items():
            full_prompt = full_prompt.replace(f"{{{{{key}}}}}", str(value))

    return full_prompt


__all__ = [
    "PROMPTS_DIR",
    "load_prompt",
    "get_common_system_prompt",
    "get_agent_prompt",
    "build_full_prompt",
]


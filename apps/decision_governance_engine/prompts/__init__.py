"""Decision Governance Engine - プロンプト管理モジュール（6層プロンプトレイヤー対応版）.

全Agentのシステムプロンプトを一元管理し、外部ファイルからの読み込みをサポート。
kernel/prompts の 6 層モデルに準拠し、L1(CoreSystem) を共通化、
L2(TaskSystem) を Agent ごとに定義する。

パターン: MULTI_STEP（パイプライン処理 — 道→法→術→器→レビュー）

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

from kernel.prompts.models import CoreSystemLayer, TaskSystemLayer


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


# ---------------------------------------------------------------------------
# 6層プロンプトレイヤー定義
# ---------------------------------------------------------------------------

# L1: 全 Agent 共通 CoreSystem
CORE_SYSTEM = CoreSystemLayer(
    role=(
        "あなたは回答生成AIではない。"
        "あなたの役割は「意思決定が成立する前提構造」を検査・制御することである。"
        "「正しい答え」を出すためのものではなく、"
        "「誤った意思決定が行われる確率を下げる」ためのものである。"
    ),
    success_criteria=[
        "構造化データ（JSON）を優先する",
        "具体的な数値・期限・条件を明示する",
    ],
    prohibitions=[
        "結論を急がない — 評価対象・判断レイヤー・動機が未定義なら分析を進めない",
        "一般論禁止 — 教科書的分類・網羅的整理は禁止",
        "迎合禁止 — 迎合・楽観・曖昧な肯定は禁止",
        "曖昧な表現（「適切に」「効果的に」）は禁止",
    ],
    output_principles=[
        "自由文テキストは最小限に",
        "出力は「行動や意思決定を変える」ものでなければ無効である",
    ],
)

# L2: 各 Agent 固有の TaskSystem
DAO_TASK = TaskSystemLayer(
    goal="本質分析（道）：議題の根本構造と因果関係を解明する",
    deliverables=["本質分析レポート"],
)

FA_TASK = TaskSystemLayer(
    goal="戦略選定（法）：分析結果に基づき最適な戦略を選定する",
    deliverables=["戦略選定レポート"],
)

SHU_TASK = TaskSystemLayer(
    goal="実行計画（術）：戦略を具体的な実行ステップに展開する",
    deliverables=["実行計画書"],
)

QI_TASK = TaskSystemLayer(
    goal="技術実装（器）：実行計画を技術的に具体化する",
    deliverables=["技術実装提案"],
)

REVIEW_TASK = TaskSystemLayer(
    goal="検証：全工程の成果物を検証し、品質を判定する",
    deliverables=["検証レポート"],
)

COGNITIVE_GATE_TASK = TaskSystemLayer(
    goal="認知前処理：入力の曖昧さ・矛盾・不足を検出し、分析前に品質を担保する",
    constraints=["不足情報がある場合は必ず明示し補足を要求する"],
)


__all__ = [
    "COGNITIVE_GATE_TASK",
    # 6層レイヤー
    "CORE_SYSTEM",
    "DAO_TASK",
    "FA_TASK",
    "PROMPTS_DIR",
    "QI_TASK",
    "REVIEW_TASK",
    "SHU_TASK",
    "build_full_prompt",
    "get_agent_prompt",
    "get_common_system_prompt",
    "load_prompt",
]

"""プロンプトパターン定義.

4つの具体的パターンを定義し、レイヤー組み合わせを標準化する。
各パターンは「どのレイヤーを使うか」と「レイヤー予算配分比率」を決定する。

パターン:
    SINGLE_TASK — 単発タスク実行（簡単なQ&A）
    MULTI_STEP — 多段タスク実行（計画 + 実行）
    MULTI_TURN — マルチターン会話（圧縮あり）
    TOOL_AUGMENTED — ツール拡張実行
"""

from __future__ import annotations

from dataclasses import dataclass, field

from kernel.prompts.models import (
    LAYER_CONVERSATION_STATE,
    LAYER_CORE_SYSTEM,
    LAYER_MEMORY_PROFILE,
    LAYER_RUNTIME_CONTEXT,
    LAYER_TASK_SYSTEM,
    LAYER_TOOL_ENVIRONMENT,
    PromptPattern,
)


@dataclass(frozen=True)
class PatternConfig:
    """パターン設定.

    Attributes:
        pattern: パターン種別
        required_layers: 必須レイヤー名（必ず注入される）
        optional_layers: オプションレイヤー名（データがあれば注入）
        budget_ratios: レイヤー名→予算比率のマッピング
    """

    pattern: PromptPattern
    required_layers: tuple[str, ...] = field(default_factory=tuple)
    optional_layers: tuple[str, ...] = field(default_factory=tuple)
    budget_ratios: dict[str, float] = field(default_factory=dict)


# パターン1: 単発タスク実行（簡単なQ&A）
# L1(役割) + L2(タスク) + L3(ユーザー入力) のみ
SINGLE_TASK_CONFIG = PatternConfig(
    pattern=PromptPattern.SINGLE_TASK,
    required_layers=(
        LAYER_CORE_SYSTEM,
        LAYER_TASK_SYSTEM,
        LAYER_RUNTIME_CONTEXT,
    ),
    optional_layers=(),
    budget_ratios={
        LAYER_CORE_SYSTEM: 0.25,
        LAYER_TASK_SYSTEM: 0.35,
        LAYER_RUNTIME_CONTEXT: 0.40,
    },
)

# パターン2: 多段タスク実行（計画→実行）
# L1 + L2 + L3 必須、L4(会話状態) + L5(記憶) オプション
MULTI_STEP_CONFIG = PatternConfig(
    pattern=PromptPattern.MULTI_STEP,
    required_layers=(
        LAYER_CORE_SYSTEM,
        LAYER_TASK_SYSTEM,
        LAYER_RUNTIME_CONTEXT,
    ),
    optional_layers=(
        LAYER_CONVERSATION_STATE,
        LAYER_MEMORY_PROFILE,
    ),
    budget_ratios={
        LAYER_CORE_SYSTEM: 0.15,
        LAYER_TASK_SYSTEM: 0.25,
        LAYER_RUNTIME_CONTEXT: 0.20,
        LAYER_CONVERSATION_STATE: 0.25,
        LAYER_MEMORY_PROFILE: 0.15,
    },
)

# パターン3: マルチターン会話（圧縮あり）
# L1 + L3 + L4 必須、L2 + L5 オプション
MULTI_TURN_CONFIG = PatternConfig(
    pattern=PromptPattern.MULTI_TURN,
    required_layers=(
        LAYER_CORE_SYSTEM,
        LAYER_RUNTIME_CONTEXT,
        LAYER_CONVERSATION_STATE,
    ),
    optional_layers=(
        LAYER_TASK_SYSTEM,
        LAYER_MEMORY_PROFILE,
    ),
    budget_ratios={
        LAYER_CORE_SYSTEM: 0.15,
        LAYER_TASK_SYSTEM: 0.10,
        LAYER_RUNTIME_CONTEXT: 0.20,
        LAYER_CONVERSATION_STATE: 0.35,
        LAYER_MEMORY_PROFILE: 0.20,
    },
)

# パターン4: ツール拡張実行
# L1 + L2 + L3 + L6 必須、L5 オプション
TOOL_AUGMENTED_CONFIG = PatternConfig(
    pattern=PromptPattern.TOOL_AUGMENTED,
    required_layers=(
        LAYER_CORE_SYSTEM,
        LAYER_TASK_SYSTEM,
        LAYER_RUNTIME_CONTEXT,
        LAYER_TOOL_ENVIRONMENT,
    ),
    optional_layers=(LAYER_MEMORY_PROFILE,),
    budget_ratios={
        LAYER_CORE_SYSTEM: 0.15,
        LAYER_TASK_SYSTEM: 0.20,
        LAYER_RUNTIME_CONTEXT: 0.20,
        LAYER_TOOL_ENVIRONMENT: 0.30,
        LAYER_MEMORY_PROFILE: 0.15,
    },
)

# パターンレジストリ
PATTERN_REGISTRY: dict[PromptPattern, PatternConfig] = {
    PromptPattern.SINGLE_TASK: SINGLE_TASK_CONFIG,
    PromptPattern.MULTI_STEP: MULTI_STEP_CONFIG,
    PromptPattern.MULTI_TURN: MULTI_TURN_CONFIG,
    PromptPattern.TOOL_AUGMENTED: TOOL_AUGMENTED_CONFIG,
}


def get_pattern_config(pattern: PromptPattern) -> PatternConfig:
    """パターン設定を取得.

    Args:
        pattern: パターン種別

    Returns:
        対応するPatternConfig

    Raises:
        KeyError: 未登録パターンの場合
    """
    if pattern not in PATTERN_REGISTRY:
        msg = f"未登録パターン: {pattern}"
        raise KeyError(msg)
    return PATTERN_REGISTRY[pattern]


def select_pattern(
    *,
    has_tools: bool = False,
    turn_count: int = 0,
    has_plan: bool = False,
) -> PromptPattern:
    """状況からパターンを自動選択.

    判定優先度:
        1. has_tools=True → TOOL_AUGMENTED
        2. has_plan=True → MULTI_STEP
        3. turn_count > 1 → MULTI_TURN
        4. それ以外 → SINGLE_TASK

    Args:
        has_tools: ツールが利用可能か
        turn_count: 現在のターン数
        has_plan: 実行計画があるか

    Returns:
        選択されたPromptPattern
    """
    if has_tools:
        return PromptPattern.TOOL_AUGMENTED
    if has_plan:
        return PromptPattern.MULTI_STEP
    if turn_count > 1:
        return PromptPattern.MULTI_TURN
    return PromptPattern.SINGLE_TASK

"""プロンプトレイヤーデータモデル.

6層プロンプト設計に基づく型安全なデータモデル。
各レイヤーはPydanticモデルとして独立定義され、
PromptAssemblerで合成される。

6層構造:
    L1: CoreSystem — 役割・成功基準・禁止事項（固定、最小限）
    L2: TaskSystem — 目標・成果物・制約（タスクごとに動的）
    L3: RuntimeContext — ユーザーリクエスト・参照情報（毎回動的）
    L4: ConversationState — 確定事項・未解決・次アクション（マルチターン時）
    L5: MemoryProfile — 長期記憶・嗜好・KeyNotes（必要時のみ）
    L6: ToolEnvironment — 利用可能ツール・環境制約（ツール使用時）
"""

from __future__ import annotations

from enum import StrEnum
from typing import Any

from pydantic import BaseModel, Field


class LayerPriority(StrEnum):
    """レイヤー優先度（予算不足時の切り捨て順序）."""

    REQUIRED = "required"  # 必須: 切り捨て不可
    HIGH = "high"  # 高: 最後に切り捨て
    MEDIUM = "medium"  # 中: 予算に応じて
    LOW = "low"  # 低: 最初に切り捨て


class PromptPattern(StrEnum):
    """プロンプトパターン種別."""

    SINGLE_TASK = "single_task"  # 単発タスク実行
    MULTI_STEP = "multi_step"  # 多段タスク実行
    MULTI_TURN = "multi_turn"  # マルチターン会話
    TOOL_AUGMENTED = "tool_augmented"  # ツール拡張実行


# === Layer 1: Core System ===


class CoreSystemLayer(BaseModel):
    """L1: コアシステムレイヤー（固定、最小限）.

    Agentのロール、成功基準、禁止事項を定義する。
    常に注入され、圧縮対象外。
    """

    role: str = Field(..., description="Agentの役割定義")
    success_criteria: list[str] = Field(
        default_factory=list, description="成功基準リスト"
    )
    prohibitions: list[str] = Field(
        default_factory=list, description="禁止事項リスト"
    )
    output_principles: list[str] = Field(
        default_factory=list, description="出力原則リスト"
    )
    priority: LayerPriority = Field(default=LayerPriority.REQUIRED)


# === Layer 2: Task System ===


class TaskSystemLayer(BaseModel):
    """L2: タスクシステムレイヤー（タスクごとに動的）.

    ゴール、成果物、制約、成功基準を定義する。
    タスク開始時に設定され、実行中は固定。
    """

    goal: str = Field(..., description="タスクの目標")
    deliverables: list[str] = Field(
        default_factory=list, description="成果物リスト"
    )
    constraints: list[str] = Field(
        default_factory=list, description="タスク制約"
    )
    task_success_criteria: list[str] = Field(
        default_factory=list, description="タスク固有の成功基準"
    )
    priorities: list[str] = Field(
        default_factory=list, description="優先順位リスト"
    )
    priority: LayerPriority = Field(default=LayerPriority.REQUIRED)


# === Layer 3: Runtime Context ===


class RuntimeContextLayer(BaseModel):
    """L3: ランタイムコンテキストレイヤー（現在の瞬間）.

    ユーザーリクエスト、参照情報、要約を格納。
    毎回の呼び出しで再構築される。
    原文丸ごとではなく、構造化した情報のみ。
    """

    user_request: str = Field(default="", description="ユーザーリクエスト要約")
    extracted_points: list[str] = Field(
        default_factory=list, description="入力から抽出された要点"
    )
    reference_summaries: list[str] = Field(
        default_factory=list, description="参照資料からの要約"
    )
    priority: LayerPriority = Field(default=LayerPriority.HIGH)


# === Layer 4: Conversation State ===


class ConversationStateLayer(BaseModel):
    """L4: 会話状態レイヤー（抽出された状態、完全履歴ではない）.

    多輪対話で本当に必要なのは「全履歴」ではなく「状態遷移の結果」。
    マルチターン時のみ使用。
    """

    decisions: list[str] = Field(
        default_factory=list, description="確定した決定事項"
    )
    open_items: list[str] = Field(
        default_factory=list, description="未解決事項"
    )
    next_actions: list[str] = Field(
        default_factory=list, description="次のアクション"
    )
    continued_constraints: list[str] = Field(
        default_factory=list, description="継続する制約"
    )
    turn_count: int = Field(default=0, description="現在のターン数")
    priority: LayerPriority = Field(default=LayerPriority.MEDIUM)


# === Layer 5: Memory / Profile ===


class MemoryProfileLayer(BaseModel):
    """L5: メモリ/プロファイルレイヤー（長期、オンデマンド）.

    長期保持するが毎回全部入れない。
    必要な時だけ注入する（relevance gating）。
    """

    memory_blocks: list[str] = Field(
        default_factory=list, description="注入される記憶ブロック"
    )
    key_notes: list[str] = Field(
        default_factory=list, description="永続的な重要情報"
    )
    user_preferences: dict[str, str] = Field(
        default_factory=dict, description="ユーザー嗜好"
    )
    priority: LayerPriority = Field(default=LayerPriority.LOW)


# === Layer 6: Tool / Environment ===


class ToolDescription(BaseModel):
    """ツール説明."""

    name: str = Field(..., description="ツール名")
    description: str = Field(..., description="ツールの説明")
    parameters_summary: str = Field(
        default="", description="パラメータの要約"
    )


class ToolEnvironmentLayer(BaseModel):
    """L6: ツール/環境レイヤー（利用可能なケイパビリティ）.

    Agentが今できることの説明。ツール使用パターン時のみ注入。
    """

    available_tools: list[ToolDescription] = Field(
        default_factory=list, description="利用可能ツール"
    )
    environment_info: dict[str, str] = Field(
        default_factory=dict, description="実行環境情報"
    )
    tool_usage_guidelines: str = Field(
        default="", description="ツール使用ガイドライン"
    )
    priority: LayerPriority = Field(default=LayerPriority.MEDIUM)


# === 合成結果 ===


class AssembledPrompt(BaseModel):
    """合成済みプロンプト.

    6層から合成された最終プロンプトとメタデータ。
    """

    system_prompt: str = Field(..., description="合成されたシステムプロンプト")
    pattern: PromptPattern = Field(..., description="使用パターン")
    layers_used: list[str] = Field(
        default_factory=list, description="使用されたレイヤー名"
    )
    token_count: int = Field(default=0, description="合計トークン数")
    token_budget: int = Field(default=0, description="トークン予算")
    layers_truncated: list[str] = Field(
        default_factory=list, description="切り詰められたレイヤー"
    )
    layers_dropped: list[str] = Field(
        default_factory=list, description="予算不足でスキップされたレイヤー"
    )
    metadata: dict[str, Any] = Field(
        default_factory=dict, description="メタデータ"
    )


# === レイヤーセット ===

# レイヤー名定数
LAYER_CORE_SYSTEM = "core_system"
LAYER_TASK_SYSTEM = "task_system"
LAYER_RUNTIME_CONTEXT = "runtime_context"
LAYER_CONVERSATION_STATE = "conversation_state"
LAYER_MEMORY_PROFILE = "memory_profile"
LAYER_TOOL_ENVIRONMENT = "tool_environment"

# レイヤー合成順序（最終プロンプトでの並び順）
LAYER_ORDER: list[str] = [
    LAYER_CORE_SYSTEM,
    LAYER_TASK_SYSTEM,
    LAYER_CONVERSATION_STATE,
    LAYER_MEMORY_PROFILE,
    LAYER_RUNTIME_CONTEXT,
    LAYER_TOOL_ENVIRONMENT,
]


class PromptLayerSet(BaseModel):
    """プロンプトレイヤーセット.

    6層を一括管理する入れ物。Noneのレイヤーは合成時にスキップ。
    """

    core_system: CoreSystemLayer | None = None
    task_system: TaskSystemLayer | None = None
    runtime_context: RuntimeContextLayer | None = None
    conversation_state: ConversationStateLayer | None = None
    memory_profile: MemoryProfileLayer | None = None
    tool_environment: ToolEnvironmentLayer | None = None

    def get_layer(self, name: str) -> BaseModel | None:
        """レイヤー名からレイヤーを取得."""
        return getattr(self, name, None)

    def active_layers(self) -> list[tuple[str, BaseModel]]:
        """None でないレイヤーを合成順序で返す."""
        result: list[tuple[str, BaseModel]] = []
        for name in LAYER_ORDER:
            layer = self.get_layer(name)
            if layer is not None:
                result.append((name, layer))
        return result

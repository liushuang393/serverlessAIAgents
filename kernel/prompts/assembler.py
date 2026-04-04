"""プロンプトアセンブラー.

レイヤーセットをトークン予算内で合成し、最終プロンプトを生成する。

合成アルゴリズム:
    1. PromptLayerSet から有効レイヤーを取得
    2. 各レイヤーをMarkdownテキストにレンダリング
    3. パターン設定の予算比率でレイヤーごとの予算を計算
    4. 優先度順（REQUIRED→HIGH→MEDIUM→LOW）でテキストを追加
    5. 予算超過レイヤーは切り詰めまたはスキップ
    6. AssembledPrompt として返却（監査メタデータ付き）
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

from kernel.prompts.models import (
    LAYER_CONVERSATION_STATE,
    LAYER_CORE_SYSTEM,
    LAYER_MEMORY_PROFILE,
    LAYER_RUNTIME_CONTEXT,
    LAYER_TASK_SYSTEM,
    LAYER_TOOL_ENVIRONMENT,
    AssembledPrompt,
    ConversationStateLayer,
    CoreSystemLayer,
    LayerPriority,
    MemoryProfileLayer,
    PromptLayerSet,
    PromptPattern,
    RuntimeContextLayer,
    TaskSystemLayer,
    ToolEnvironmentLayer,
)
from kernel.prompts.patterns import PATTERN_REGISTRY, PatternConfig

if TYPE_CHECKING:
    from pydantic import BaseModel

logger = logging.getLogger(__name__)

# 優先度順序（低い数字が高い優先度）
_PRIORITY_ORDER: dict[LayerPriority, int] = {
    LayerPriority.REQUIRED: 0,
    LayerPriority.HIGH: 1,
    LayerPriority.MEDIUM: 2,
    LayerPriority.LOW: 3,
}

# デフォルトトークン予算
DEFAULT_TOKEN_BUDGET = 4000


def _estimate_tokens(text: str) -> int:
    """簡易トークン推定.

    日本語/中国語=1.5token/char、ASCII=0.25token/char、その他=1token/char。
    harness.budget.service.SimpleTokenCounter と同じロジック。
    """
    if not text:
        return 0
    cjk_count = 0
    ascii_count = 0
    for ch in text:
        cp = ord(ch)
        # CJK統合漢字 + ひらがな + カタカナ
        if (0x4E00 <= cp <= 0x9FFF) or (0x3040 <= cp <= 0x309F) or (0x30A0 <= cp <= 0x30FF):
            cjk_count += 1
        elif ch.isascii() and (ch.isalnum() or ch.isspace()):
            ascii_count += 1
    other_count = len(text) - cjk_count - ascii_count
    return int(cjk_count * 1.5 + ascii_count * 0.25 + other_count)


def _truncate_text(text: str, max_tokens: int) -> tuple[str, bool]:
    """テキストを予算内に切り詰め.

    Returns:
        (切り詰め後テキスト, 切り詰めが発生したか)
    """
    current = _estimate_tokens(text)
    if current <= max_tokens:
        return text, False

    # 行単位で切り詰め（末尾から削除）
    lines = text.split("\n")
    result_lines: list[str] = []
    used = 0
    for line in lines:
        line_tokens = _estimate_tokens(line)
        if used + line_tokens > max_tokens:
            break
        result_lines.append(line)
        used += line_tokens

    truncated = "\n".join(result_lines)
    if truncated and truncated != text:
        truncated += "\n... (予算超過により切り詰め)"
    return truncated, True


class PromptAssembler:
    """6層プロンプトアセンブラー.

    レイヤーを優先度順に合成し、トークン予算を厳守する。
    """

    def __init__(
        self,
        default_budget: int = DEFAULT_TOKEN_BUDGET,
    ) -> None:
        self._default_budget = default_budget

    def assemble(
        self,
        layers: PromptLayerSet,
        pattern: PromptPattern,
        token_budget: int | None = None,
    ) -> AssembledPrompt:
        """レイヤーセットを合成.

        Args:
            layers: 6層レイヤーセット
            pattern: 使用パターン
            token_budget: トークン予算（None時はデフォルト）

        Returns:
            合成済みプロンプト
        """
        budget = token_budget if token_budget is not None else self._default_budget
        config = PATTERN_REGISTRY.get(pattern)

        # 各レイヤーをレンダリング
        active = layers.active_layers()
        rendered: list[tuple[str, str, LayerPriority]] = []
        for name, layer in active:
            text = self._render_layer(name, layer)
            if text:
                priority = getattr(layer, "priority", LayerPriority.MEDIUM)
                rendered.append((name, text, priority))

        # 優先度でソート（REQUIRED が先頭）
        rendered.sort(key=lambda x: _PRIORITY_ORDER.get(x[2], 99))

        # 予算配分
        ratios = config.budget_ratios if config else {}
        sections: list[str] = []
        layers_used: list[str] = []
        layers_truncated: list[str] = []
        layers_dropped: list[str] = []
        total_used = 0

        for name, text, priority in rendered:
            # レイヤーごとの予算を計算
            ratio = ratios.get(name, 0.1)  # 未定義レイヤーは10%
            layer_budget = int(budget * ratio)

            tokens = _estimate_tokens(text)

            if priority == LayerPriority.REQUIRED:
                # REQUIRED は常に含める（予算超過時は切り詰め）
                if tokens > layer_budget:
                    text, was_truncated = _truncate_text(text, layer_budget)
                    if was_truncated:
                        layers_truncated.append(name)
                sections.append(text)
                layers_used.append(name)
                total_used += _estimate_tokens(text)
            elif total_used + tokens <= budget:
                # 予算内なら追加
                sections.append(text)
                layers_used.append(name)
                total_used += tokens
            elif priority in (LayerPriority.HIGH, LayerPriority.MEDIUM):
                # HIGH/MEDIUM は切り詰めて追加を試みる
                remaining = budget - total_used
                if remaining > 50:  # 最低50トークンの余裕が必要
                    text, was_truncated = _truncate_text(text, remaining)
                    if text and _estimate_tokens(text) > 0:
                        sections.append(text)
                        layers_used.append(name)
                        if was_truncated:
                            layers_truncated.append(name)
                        total_used += _estimate_tokens(text)
                    else:
                        layers_dropped.append(name)
                else:
                    layers_dropped.append(name)
            else:
                # LOW は予算超過時にスキップ
                layers_dropped.append(name)

        system_prompt = "\n\n".join(sections)
        final_tokens = _estimate_tokens(system_prompt)

        return AssembledPrompt(
            system_prompt=system_prompt,
            pattern=pattern,
            layers_used=layers_used,
            token_count=final_tokens,
            token_budget=budget,
            layers_truncated=layers_truncated,
            layers_dropped=layers_dropped,
        )

    def _render_layer(self, name: str, layer: BaseModel) -> str:
        """レイヤーをテキストにレンダリング."""
        renderers = {
            LAYER_CORE_SYSTEM: self._render_core_system,
            LAYER_TASK_SYSTEM: self._render_task_system,
            LAYER_RUNTIME_CONTEXT: self._render_runtime_context,
            LAYER_CONVERSATION_STATE: self._render_conversation_state,
            LAYER_MEMORY_PROFILE: self._render_memory_profile,
            LAYER_TOOL_ENVIRONMENT: self._render_tool_environment,
        }
        renderer = renderers.get(name)
        if renderer is None:
            logger.warning("未知のレイヤー: %s", name)
            return ""
        return renderer(layer)  # type: ignore[arg-type]

    def _render_core_system(self, layer: CoreSystemLayer) -> str:
        """L1: コアシステムのレンダリング."""
        parts: list[str] = [layer.role]

        if layer.success_criteria:
            parts.append("成功基準:")
            parts.extend(f"- {c}" for c in layer.success_criteria)

        if layer.prohibitions:
            parts.append("禁止事項:")
            parts.extend(f"- {p}" for p in layer.prohibitions)

        if layer.output_principles:
            parts.append("出力原則:")
            parts.extend(f"- {o}" for o in layer.output_principles)

        return "\n".join(parts)

    def _render_task_system(self, layer: TaskSystemLayer) -> str:
        """L2: タスクシステムのレンダリング."""
        parts: list[str] = [f"タスク目的: {layer.goal}"]

        if layer.deliverables:
            parts.append("成果物:")
            parts.extend(f"- {d}" for d in layer.deliverables)

        if layer.priorities:
            parts.append("優先順位:")
            for i, p in enumerate(layer.priorities, 1):
                parts.append(f"{i}. {p}")

        if layer.constraints:
            parts.append("制約:")
            parts.extend(f"- {c}" for c in layer.constraints)

        if layer.task_success_criteria:
            parts.append("完了条件:")
            parts.extend(f"- {s}" for s in layer.task_success_criteria)

        return "\n".join(parts)

    def _render_runtime_context(self, layer: RuntimeContextLayer) -> str:
        """L3: ランタイムコンテキストのレンダリング."""
        parts: list[str] = []

        if layer.user_request:
            parts.append(f"現在の入力: {layer.user_request}")

        if layer.extracted_points:
            parts.append("抽出ポイント:")
            parts.extend(f"- {p}" for p in layer.extracted_points)

        if layer.reference_summaries:
            parts.append("参照情報:")
            parts.extend(f"- {r}" for r in layer.reference_summaries)

        return "\n".join(parts)

    def _render_conversation_state(self, layer: ConversationStateLayer) -> str:
        """L4: 会話状態のレンダリング."""
        parts: list[str] = [f"会話状態 (ターン {layer.turn_count}):"]

        if layer.decisions:
            parts.append("確定事項:")
            parts.extend(f"- {d}" for d in layer.decisions)

        if layer.open_items:
            parts.append("未解決事項:")
            parts.extend(f"- {o}" for o in layer.open_items)

        if layer.next_actions:
            parts.append("次アクション:")
            parts.extend(f"- {a}" for a in layer.next_actions)

        if layer.continued_constraints:
            parts.append("継続制約:")
            parts.extend(f"- {c}" for c in layer.continued_constraints)

        return "\n".join(parts)

    def _render_memory_profile(self, layer: MemoryProfileLayer) -> str:
        """L5: メモリ/プロファイルのレンダリング."""
        parts: list[str] = []

        if layer.key_notes:
            parts.append("重要情報:")
            parts.extend(f"- {n}" for n in layer.key_notes)

        if layer.memory_blocks:
            parts.append("関連記憶:")
            parts.extend(f"- {m}" for m in layer.memory_blocks)

        if layer.user_preferences:
            parts.append("適用設定:")
            for k, v in layer.user_preferences.items():
                parts.append(f"- {k}: {v}")

        return "\n".join(parts)

    def _render_tool_environment(self, layer: ToolEnvironmentLayer) -> str:
        """L6: ツール/環境のレンダリング."""
        parts: list[str] = []

        if layer.available_tools:
            parts.append("利用可能ツール:")
            for tool in layer.available_tools:
                desc = f"- {tool.name}: {tool.description}"
                if tool.parameters_summary:
                    desc += f" ({tool.parameters_summary})"
                parts.append(desc)

        if layer.environment_info:
            parts.append("環境情報:")
            for k, v in layer.environment_info.items():
                parts.append(f"- {k}: {v}")

        if layer.tool_usage_guidelines:
            parts.append(f"ガイドライン: {layer.tool_usage_guidelines}")

        return "\n".join(parts)

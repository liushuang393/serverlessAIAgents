"""レイヤービルダー群.

各レイヤーの生成ロジックを提供する。
静的設定、動的生成、既存コンポーネント連携の3種類。

連携先:
    - ContextBuilder (kernel/memory/context_builder.py) → L5
    - TurnBasedCompressor (harness/context/turn_compressor.py) → L4
    - KeyNotesStore (harness/context/key_notes.py) → L4, L5
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING, Any

from kernel.prompts.models import (
    ConversationStateLayer,
    CoreSystemLayer,
    MemoryProfileLayer,
    RuntimeContextLayer,
    TaskSystemLayer,
    ToolDescription,
    ToolEnvironmentLayer,
)

if TYPE_CHECKING:
    from harness.context.key_notes import KeyNotesStore, NoteImportance
    from harness.context.turn_compressor import TurnBasedCompressor
    from kernel.memory.context_builder import ContextBlock, ContextBuilder, MemoryManager

logger = logging.getLogger(__name__)


class CoreSystemBuilder:
    """L1: コアシステムレイヤービルダー.

    Agent定義からCoreSystemLayerを構築する。
    """

    @staticmethod
    def from_config(config: dict[str, Any]) -> CoreSystemLayer:
        """設定辞書からCoreSystemLayerを構築.

        Args:
            config: キーは role, success_criteria, prohibitions, output_principles

        Returns:
            CoreSystemLayer
        """
        return CoreSystemLayer(
            role=config.get("role", ""),
            success_criteria=config.get("success_criteria", []),
            prohibitions=config.get("prohibitions", []),
            output_principles=config.get("output_principles", []),
        )

    @staticmethod
    def from_skill_file(skill_path: Path) -> CoreSystemLayer:
        """SKILL.md ファイルからCoreSystemLayerを構築.

        YAML frontmatter を除外し、本文をroleとして使用する。

        Args:
            skill_path: SKILL.md ファイルパス

        Returns:
            CoreSystemLayer
        """
        content = skill_path.read_text(encoding="utf-8")

        # YAML frontmatter を除外
        if content.startswith("---"):
            parts = content.split("---", 2)
            if len(parts) >= 3:
                content = parts[2].strip()

        return CoreSystemLayer(role=content)

    @staticmethod
    def from_prompt_text(prompt_text: str) -> CoreSystemLayer:
        """プロンプトテキストから直接構築.

        Args:
            prompt_text: システムプロンプトテキスト

        Returns:
            CoreSystemLayer
        """
        return CoreSystemLayer(role=prompt_text)


class TaskSystemBuilder:
    """L2: タスクシステムレイヤービルダー."""

    @staticmethod
    def from_input(
        goal: str,
        deliverables: list[str] | None = None,
        constraints: list[str] | None = None,
        success_criteria: list[str] | None = None,
        priorities: list[str] | None = None,
    ) -> TaskSystemLayer:
        """入力データからTaskSystemLayerを構築.

        Args:
            goal: タスクの目標
            deliverables: 成果物リスト
            constraints: 制約リスト
            success_criteria: 成功基準リスト
            priorities: 優先順位リスト

        Returns:
            TaskSystemLayer
        """
        return TaskSystemLayer(
            goal=goal,
            deliverables=deliverables or [],
            constraints=constraints or [],
            task_success_criteria=success_criteria or [],
            priorities=priorities or [],
        )


class RuntimeContextBuilder:
    """L3: ランタイムコンテキストレイヤービルダー."""

    @staticmethod
    def from_request(
        user_request: str,
        extracted_points: list[str] | None = None,
        reference_summaries: list[str] | None = None,
    ) -> RuntimeContextLayer:
        """ユーザーリクエストからRuntimeContextLayerを構築.

        Args:
            user_request: ユーザーリクエスト
            extracted_points: 抽出された要点
            reference_summaries: 参照資料の要約

        Returns:
            RuntimeContextLayer
        """
        return RuntimeContextLayer(
            user_request=user_request,
            extracted_points=extracted_points or [],
            reference_summaries=reference_summaries or [],
        )


class ConversationStateBuilder:
    """L4: 会話状態レイヤービルダー.

    TurnBasedCompressor / KeyNotesStore と連携して状態を抽出する。
    """

    @staticmethod
    def from_compressor(
        compressor: TurnBasedCompressor,
    ) -> ConversationStateLayer:
        """TurnBasedCompressorから会話状態を構築.

        圧縮済み履歴とKeyNotesから状態を抽出する。

        Args:
            compressor: ターンベース圧縮器

        Returns:
            ConversationStateLayer
        """
        key_notes_store = compressor.get_key_notes_store()
        notes = key_notes_store.get_all_notes()

        # KeyNotesから決定事項・未解決事項を分類
        decisions: list[str] = []
        open_items: list[str] = []
        for note in notes:
            decisions.append(note.content)

        # コンテキストサマリ取得
        context = compressor.get_context_with_notes()

        return ConversationStateLayer(
            decisions=decisions,
            open_items=open_items,
            next_actions=[],
            continued_constraints=[],
            turn_count=compressor.get_turn_count(),
        )

    @staticmethod
    def from_state_dict(
        state: dict[str, Any],
    ) -> ConversationStateLayer:
        """状態辞書から直接構築.

        Args:
            state: decisions, open_items, next_actions, continued_constraints, turn_count

        Returns:
            ConversationStateLayer
        """
        return ConversationStateLayer(
            decisions=state.get("decisions", []),
            open_items=state.get("open_items", []),
            next_actions=state.get("next_actions", []),
            continued_constraints=state.get("continued_constraints", []),
            turn_count=state.get("turn_count", 0),
        )


class MemoryProfileBuilder:
    """L5: メモリ/プロファイルレイヤービルダー.

    ContextBuilder (kernel/memory/context_builder.py) と連携する。
    """

    @staticmethod
    async def from_context_builder(
        context_builder: ContextBuilder,
        user_request: str,
        memory_manager: MemoryManager,
        topic: str | None = None,
    ) -> MemoryProfileLayer:
        """ContextBuilder経由でMemoryProfileLayerを構築.

        ContextBuilder.build() → ContextBlock[] → memory_blocks に変換。

        Args:
            context_builder: コンテキストビルダー
            user_request: ユーザーリクエスト
            memory_manager: メモリマネージャー
            topic: トピックフィルタ

        Returns:
            MemoryProfileLayer
        """
        blocks: list[ContextBlock] = await context_builder.build(
            user_request=user_request,
            memory_manager=memory_manager,
            topic=topic,
        )
        memory_blocks = [block.to_prompt_text() for block in blocks]
        return MemoryProfileLayer(memory_blocks=memory_blocks)

    @staticmethod
    def from_key_notes(
        key_notes_store: KeyNotesStore,
        min_importance: NoteImportance | None = None,
    ) -> MemoryProfileLayer:
        """KeyNotesStoreからMemoryProfileLayerを構築.

        Args:
            key_notes_store: KeyNotesストア
            min_importance: 最低重要度（None時は全件取得）

        Returns:
            MemoryProfileLayer
        """
        if min_importance is not None:
            notes = key_notes_store.get_notes_by_importance(min_importance)
        else:
            notes = key_notes_store.get_all_notes()
        key_note_texts = [note.content for note in notes]
        return MemoryProfileLayer(key_notes=key_note_texts)

    @staticmethod
    def from_blocks(
        memory_blocks: list[str] | None = None,
        key_notes: list[str] | None = None,
        user_preferences: dict[str, str] | None = None,
    ) -> MemoryProfileLayer:
        """直接値からMemoryProfileLayerを構築.

        Args:
            memory_blocks: 記憶ブロック
            key_notes: 重要情報
            user_preferences: ユーザー嗜好

        Returns:
            MemoryProfileLayer
        """
        return MemoryProfileLayer(
            memory_blocks=memory_blocks or [],
            key_notes=key_notes or [],
            user_preferences=user_preferences or {},
        )


class ToolEnvironmentBuilder:
    """L6: ツール/環境レイヤービルダー."""

    @staticmethod
    def from_tools(
        tools: list[dict[str, str]],
        environment: dict[str, str] | None = None,
        guidelines: str = "",
    ) -> ToolEnvironmentLayer:
        """ツール情報からToolEnvironmentLayerを構築.

        Args:
            tools: ツール情報リスト（各要素は name, description, parameters_summary を含む辞書）
            environment: 環境情報
            guidelines: ツール使用ガイドライン

        Returns:
            ToolEnvironmentLayer
        """
        tool_descs = [
            ToolDescription(
                name=t.get("name", ""),
                description=t.get("description", ""),
                parameters_summary=t.get("parameters_summary", ""),
            )
            for t in tools
        ]
        return ToolEnvironmentLayer(
            available_tools=tool_descs,
            environment_info=environment or {},
            tool_usage_guidelines=guidelines,
        )

    @staticmethod
    def from_tool_descriptions(
        tools: list[ToolDescription],
        environment: dict[str, str] | None = None,
        guidelines: str = "",
    ) -> ToolEnvironmentLayer:
        """ToolDescriptionリストから直接構築.

        Args:
            tools: ToolDescriptionリスト
            environment: 環境情報
            guidelines: ツール使用ガイドライン

        Returns:
            ToolEnvironmentLayer
        """
        return ToolEnvironmentLayer(
            available_tools=tools,
            environment_info=environment or {},
            tool_usage_guidelines=guidelines,
        )

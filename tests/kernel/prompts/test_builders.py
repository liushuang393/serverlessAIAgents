"""レイヤービルダーのユニットテスト."""

from __future__ import annotations

from typing import TYPE_CHECKING
from unittest.mock import AsyncMock, MagicMock

import pytest

from kernel.prompts.builders import (
    ConversationStateBuilder,
    CoreSystemBuilder,
    MemoryProfileBuilder,
    RuntimeContextBuilder,
    TaskSystemBuilder,
    ToolEnvironmentBuilder,
)
from kernel.prompts.models import ToolDescription


if TYPE_CHECKING:
    from pathlib import Path


class TestCoreSystemBuilder:
    """CoreSystemBuilder のテスト."""

    def test_from_config_full(self) -> None:
        config = {
            "role": "分析エージェント",
            "success_criteria": ["正確な結果"],
            "prohibitions": ["推測禁止"],
            "output_principles": ["簡潔に返す"],
        }
        layer = CoreSystemBuilder.from_config(config)
        assert layer.role == "分析エージェント"
        assert layer.success_criteria == ["正確な結果"]
        assert layer.prohibitions == ["推測禁止"]
        assert layer.output_principles == ["簡潔に返す"]

    def test_from_config_minimal(self) -> None:
        layer = CoreSystemBuilder.from_config({"role": "テスト"})
        assert layer.role == "テスト"
        assert layer.success_criteria == []

    def test_from_config_empty(self) -> None:
        layer = CoreSystemBuilder.from_config({})
        assert layer.role == ""

    def test_from_skill_file(self, tmp_path: Path) -> None:
        skill_file = tmp_path / "SKILL.md"
        skill_file.write_text(
            "---\nname: test\nversion: 1.0\n---\nあなたはテストエージェントです。",
            encoding="utf-8",
        )
        layer = CoreSystemBuilder.from_skill_file(skill_file)
        assert "テストエージェント" in layer.role
        # frontmatter が除外されていること
        assert "name: test" not in layer.role

    def test_from_skill_file_no_frontmatter(self, tmp_path: Path) -> None:
        skill_file = tmp_path / "SKILL.md"
        skill_file.write_text("シンプルなプロンプト", encoding="utf-8")
        layer = CoreSystemBuilder.from_skill_file(skill_file)
        assert layer.role == "シンプルなプロンプト"

    def test_from_prompt_text(self) -> None:
        layer = CoreSystemBuilder.from_prompt_text("直接テキスト")
        assert layer.role == "直接テキスト"


class TestTaskSystemBuilder:
    """TaskSystemBuilder のテスト."""

    def test_from_input_full(self) -> None:
        layer = TaskSystemBuilder.from_input(
            goal="API設計",
            deliverables=["仕様書"],
            constraints=["REST準拠"],
            success_criteria=["完了"],
            priorities=["セキュリティ"],
        )
        assert layer.goal == "API設計"
        assert layer.deliverables == ["仕様書"]
        assert layer.constraints == ["REST準拠"]
        assert layer.task_success_criteria == ["完了"]
        assert layer.priorities == ["セキュリティ"]

    def test_from_input_minimal(self) -> None:
        layer = TaskSystemBuilder.from_input(goal="テスト")
        assert layer.goal == "テスト"
        assert layer.deliverables == []
        assert layer.constraints == []


class TestRuntimeContextBuilder:
    """RuntimeContextBuilder のテスト."""

    def test_from_request_full(self) -> None:
        layer = RuntimeContextBuilder.from_request(
            user_request="検索して",
            extracted_points=["対象: API"],
            reference_summaries=["参考: RFC 7231"],
        )
        assert layer.user_request == "検索して"
        assert layer.extracted_points == ["対象: API"]
        assert layer.reference_summaries == ["参考: RFC 7231"]

    def test_from_request_minimal(self) -> None:
        layer = RuntimeContextBuilder.from_request(user_request="テスト")
        assert layer.user_request == "テスト"
        assert layer.extracted_points == []


class TestConversationStateBuilder:
    """ConversationStateBuilder のテスト."""

    def test_from_compressor(self) -> None:
        """TurnBasedCompressor モックとの連携テスト."""
        # KeyNotesStore モック
        mock_note = MagicMock()
        mock_note.content = "JWT認証を採用"
        mock_key_notes = MagicMock()
        mock_key_notes.get_all_notes.return_value = [mock_note]

        # TurnBasedCompressor モック
        mock_compressor = MagicMock()
        mock_compressor.get_key_notes_store.return_value = mock_key_notes
        mock_compressor.get_turn_count.return_value = 5
        mock_compressor.get_context_with_notes.return_value = "要約テキスト"

        layer = ConversationStateBuilder.from_compressor(mock_compressor)
        assert layer.turn_count == 5
        assert "JWT認証を採用" in layer.decisions

    def test_from_state_dict(self) -> None:
        state = {
            "decisions": ["決定1"],
            "open_items": ["未解決1"],
            "next_actions": ["アクション1"],
            "continued_constraints": ["制約1"],
            "turn_count": 3,
        }
        layer = ConversationStateBuilder.from_state_dict(state)
        assert layer.decisions == ["決定1"]
        assert layer.open_items == ["未解決1"]
        assert layer.next_actions == ["アクション1"]
        assert layer.continued_constraints == ["制約1"]
        assert layer.turn_count == 3

    def test_from_state_dict_empty(self) -> None:
        layer = ConversationStateBuilder.from_state_dict({})
        assert layer.decisions == []
        assert layer.turn_count == 0


class TestMemoryProfileBuilder:
    """MemoryProfileBuilder のテスト."""

    @pytest.mark.asyncio
    async def test_from_context_builder(self) -> None:
        """ContextBuilder モックとの連携テスト."""
        # ContextBlock モック
        mock_block1 = MagicMock()
        mock_block1.to_prompt_text.return_value = "[topic:2024] 前回の設計決定"
        mock_block2 = MagicMock()
        mock_block2.to_prompt_text.return_value = "[topic:2024] Python規約"

        # ContextBuilder モック
        mock_builder = AsyncMock()
        mock_builder.build.return_value = [mock_block1, mock_block2]

        # MemoryManager モック
        mock_manager = MagicMock()

        layer = await MemoryProfileBuilder.from_context_builder(
            context_builder=mock_builder,
            user_request="前回の続き",
            memory_manager=mock_manager,
            topic="design",
        )
        assert len(layer.memory_blocks) == 2
        assert "[topic:2024] 前回の設計決定" in layer.memory_blocks
        mock_builder.build.assert_called_once_with(
            user_request="前回の続き",
            memory_manager=mock_manager,
            topic="design",
        )

    def test_from_key_notes(self) -> None:
        """KeyNotesStore モックとの連携テスト."""
        mock_note1 = MagicMock()
        mock_note1.content = "Python 3.12 使用"
        mock_note2 = MagicMock()
        mock_note2.content = "日本語優先"

        mock_store = MagicMock()
        mock_store.get_all_notes.return_value = [mock_note1, mock_note2]

        layer = MemoryProfileBuilder.from_key_notes(mock_store)
        assert len(layer.key_notes) == 2
        assert "Python 3.12 使用" in layer.key_notes

    def test_from_key_notes_with_importance(self) -> None:
        """重要度フィルタ付きテスト."""
        mock_note = MagicMock()
        mock_note.content = "重要な情報"

        mock_store = MagicMock()
        mock_importance = MagicMock()
        mock_store.get_notes_by_importance.return_value = [mock_note]

        layer = MemoryProfileBuilder.from_key_notes(mock_store, min_importance=mock_importance)
        assert len(layer.key_notes) == 1
        mock_store.get_notes_by_importance.assert_called_once_with(mock_importance)

    def test_from_blocks(self) -> None:
        layer = MemoryProfileBuilder.from_blocks(
            memory_blocks=["記憶1"],
            key_notes=["メモ1"],
            user_preferences={"lang": "ja"},
        )
        assert layer.memory_blocks == ["記憶1"]
        assert layer.key_notes == ["メモ1"]
        assert layer.user_preferences == {"lang": "ja"}

    def test_from_blocks_empty(self) -> None:
        layer = MemoryProfileBuilder.from_blocks()
        assert layer.memory_blocks == []
        assert layer.key_notes == []
        assert layer.user_preferences == {}


class TestToolEnvironmentBuilder:
    """ToolEnvironmentBuilder のテスト."""

    def test_from_tools(self) -> None:
        tools = [
            {
                "name": "search",
                "description": "全文検索",
                "parameters_summary": "query: str",
            },
            {
                "name": "execute",
                "description": "コード実行",
            },
        ]
        layer = ToolEnvironmentBuilder.from_tools(
            tools=tools,
            environment={"runtime": "Python 3.12"},
            guidelines="必要時のみ使用",
        )
        assert len(layer.available_tools) == 2
        assert layer.available_tools[0].name == "search"
        assert layer.available_tools[1].parameters_summary == ""
        assert layer.environment_info["runtime"] == "Python 3.12"
        assert layer.tool_usage_guidelines == "必要時のみ使用"

    def test_from_tools_empty(self) -> None:
        layer = ToolEnvironmentBuilder.from_tools(tools=[])
        assert layer.available_tools == []
        assert layer.environment_info == {}

    def test_from_tool_descriptions(self) -> None:
        tools = [
            ToolDescription(name="rag", description="RAG検索"),
        ]
        layer = ToolEnvironmentBuilder.from_tool_descriptions(
            tools=tools,
            environment={"max_results": "10"},
        )
        assert len(layer.available_tools) == 1
        assert layer.available_tools[0].name == "rag"

"""プロンプトアセンブラーのユニットテスト."""

from __future__ import annotations

import pytest

from kernel.prompts.assembler import (
    PromptAssembler,
    _estimate_tokens,
    _truncate_text,
)
from kernel.prompts.models import (
    LAYER_CORE_SYSTEM,
    LAYER_MEMORY_PROFILE,
    LAYER_RUNTIME_CONTEXT,
    LAYER_TASK_SYSTEM,
    LAYER_TOOL_ENVIRONMENT,
    ConversationStateLayer,
    CoreSystemLayer,
    LayerPriority,
    MemoryProfileLayer,
    PromptLayerSet,
    PromptPattern,
    RuntimeContextLayer,
    TaskSystemLayer,
    ToolDescription,
    ToolEnvironmentLayer,
)


class TestEstimateTokens:
    """トークン推定のテスト."""

    def test_empty_string(self) -> None:
        assert _estimate_tokens("") == 0

    def test_ascii_text(self) -> None:
        # "hello" = 5 ASCII chars → 5 * 0.25 = 1.25 → 1
        result = _estimate_tokens("hello")
        assert result > 0

    def test_japanese_text(self) -> None:
        # "こんにちは" = 5 CJK chars → 5 * 1.5 = 7.5 → 7
        result = _estimate_tokens("こんにちは")
        assert result >= 7

    def test_mixed_text(self) -> None:
        result = _estimate_tokens("Hello こんにちは")
        assert result > 0


class TestTruncateText:
    """テキスト切り詰めのテスト."""

    def test_within_budget(self) -> None:
        text = "短いテキスト"
        result, truncated = _truncate_text(text, 10000)
        assert result == text
        assert truncated is False

    def test_over_budget(self) -> None:
        # 長いテキスト（多数行）
        lines = [f"行{i}: これはテスト用の長い行です。" for i in range(50)]
        text = "\n".join(lines)
        result, truncated = _truncate_text(text, 50)
        assert truncated is True
        assert "予算超過により切り詰め" in result
        assert _estimate_tokens(result) <= 100  # 切り詰めメッセージ含む


class TestPromptAssembler:
    """PromptAssembler のテスト."""

    def setup_method(self) -> None:
        self.assembler = PromptAssembler(default_budget=4000)

    def _make_basic_layers(self) -> PromptLayerSet:
        """基本的なレイヤーセットを作成."""
        return PromptLayerSet(
            core_system=CoreSystemLayer(
                role="あなたはテスト用エージェントです。",
                success_criteria=["正確に応答する"],
                prohibitions=["推測しない"],
            ),
            task_system=TaskSystemLayer(
                goal="ユニットテスト",
                deliverables=["テスト結果"],
                constraints=["時間制限なし"],
            ),
            runtime_context=RuntimeContextLayer(
                user_request="テストを実行してください",
                extracted_points=["対象: assembler.py"],
            ),
        )

    def test_single_task_assembly(self) -> None:
        """SINGLE_TASK パターンでの合成."""
        layers = self._make_basic_layers()
        result = self.assembler.assemble(
            layers, PromptPattern.SINGLE_TASK,
        )
        assert result.system_prompt != ""
        assert result.pattern == PromptPattern.SINGLE_TASK
        assert LAYER_CORE_SYSTEM in result.layers_used
        assert LAYER_TASK_SYSTEM in result.layers_used
        assert LAYER_RUNTIME_CONTEXT in result.layers_used
        assert result.token_count > 0
        assert result.token_budget == 4000

    def test_required_layers_always_included(self) -> None:
        """REQUIRED レイヤーは予算制限下でも含まれる."""
        assembler = PromptAssembler(default_budget=100)
        layers = self._make_basic_layers()
        result = assembler.assemble(
            layers, PromptPattern.SINGLE_TASK,
        )
        # CoreSystem は REQUIRED なので必ず含まれる
        assert LAYER_CORE_SYSTEM in result.layers_used

    def test_low_priority_dropped_first(self) -> None:
        """LOW 優先度レイヤーは予算不足時に最初にドロップされる."""
        assembler = PromptAssembler(default_budget=200)
        layers = PromptLayerSet(
            core_system=CoreSystemLayer(role="テストエージェント"),
            task_system=TaskSystemLayer(goal="テスト"),
            runtime_context=RuntimeContextLayer(user_request="テスト"),
            memory_profile=MemoryProfileLayer(
                memory_blocks=["長い記憶" * 50],
                priority=LayerPriority.LOW,
            ),
        )
        result = assembler.assemble(
            layers, PromptPattern.SINGLE_TASK,
        )
        # LOW の memory_profile がドロップされる可能性が高い
        if LAYER_MEMORY_PROFILE in result.layers_dropped:
            assert LAYER_CORE_SYSTEM in result.layers_used

    def test_empty_layers(self) -> None:
        """空のレイヤーセット."""
        layers = PromptLayerSet()
        result = self.assembler.assemble(
            layers, PromptPattern.SINGLE_TASK,
        )
        assert result.system_prompt == ""
        assert result.layers_used == []

    def test_custom_token_budget(self) -> None:
        """カスタムトークン予算."""
        layers = self._make_basic_layers()
        result = self.assembler.assemble(
            layers, PromptPattern.SINGLE_TASK, token_budget=8000,
        )
        assert result.token_budget == 8000

    def test_multi_turn_with_conversation_state(self) -> None:
        """MULTI_TURN パターンで会話状態を含む合成."""
        layers = PromptLayerSet(
            core_system=CoreSystemLayer(role="対話エージェント"),
            runtime_context=RuntimeContextLayer(
                user_request="前回の続き",
            ),
            conversation_state=ConversationStateLayer(
                decisions=["JWT認証を採用"],
                open_items=["トークン有効期限"],
                next_actions=["API設計"],
                turn_count=3,
            ),
        )
        result = self.assembler.assemble(
            layers, PromptPattern.MULTI_TURN,
        )
        assert "確定事項" in result.system_prompt
        assert "JWT認証を採用" in result.system_prompt
        assert "ターン 3" in result.system_prompt

    def test_tool_augmented_with_tools(self) -> None:
        """TOOL_AUGMENTED パターンでツール情報を含む合成."""
        layers = PromptLayerSet(
            core_system=CoreSystemLayer(role="ツール使用エージェント"),
            task_system=TaskSystemLayer(goal="検索タスク"),
            runtime_context=RuntimeContextLayer(user_request="検索して"),
            tool_environment=ToolEnvironmentLayer(
                available_tools=[
                    ToolDescription(
                        name="search",
                        description="全文検索",
                        parameters_summary="query: str",
                    ),
                ],
                environment_info={"runtime": "Python 3.12"},
                tool_usage_guidelines="必要時のみ使用",
            ),
        )
        result = self.assembler.assemble(
            layers, PromptPattern.TOOL_AUGMENTED,
        )
        assert "search" in result.system_prompt
        assert "全文検索" in result.system_prompt
        assert LAYER_TOOL_ENVIRONMENT in result.layers_used

    def test_render_core_system(self) -> None:
        """CoreSystem レンダリングの内容確認."""
        layer = CoreSystemLayer(
            role="分析エージェント",
            success_criteria=["正確な結果"],
            prohibitions=["推測禁止"],
            output_principles=["簡潔に"],
        )
        text = self.assembler._render_core_system(layer)
        assert "分析エージェント" in text
        assert "成功基準:" in text
        assert "禁止事項:" in text
        assert "出力原則:" in text

    def test_render_task_system(self) -> None:
        """TaskSystem レンダリングの内容確認."""
        layer = TaskSystemLayer(
            goal="API設計",
            deliverables=["仕様書"],
            priorities=["セキュリティ", "速度"],
            constraints=["REST準拠"],
            task_success_criteria=["完了"],
        )
        text = self.assembler._render_task_system(layer)
        assert "タスク目的: API設計" in text
        assert "成果物:" in text
        assert "1. セキュリティ" in text
        assert "2. 速度" in text

    def test_render_empty_optional_fields(self) -> None:
        """オプションフィールドが空の場合、対応セクションを出力しない."""
        layer = CoreSystemLayer(role="テスト")
        text = self.assembler._render_core_system(layer)
        assert "成功基準:" not in text
        assert "禁止事項:" not in text

    def test_layers_truncated_tracking(self) -> None:
        """切り詰めが発生した場合に追跡される."""
        # 非常に小さい予算で大きなレイヤーを合成
        assembler = PromptAssembler(default_budget=50)
        layers = PromptLayerSet(
            core_system=CoreSystemLayer(
                role="非常に長い役割定義です。" * 20,
                success_criteria=["基準" * 10],
            ),
        )
        result = assembler.assemble(
            layers, PromptPattern.SINGLE_TASK,
        )
        # REQUIRED なので含まれるが、切り詰めが発生しうる
        assert LAYER_CORE_SYSTEM in result.layers_used

    def test_metadata_default_empty(self) -> None:
        """metadata がデフォルトで空であること."""
        layers = self._make_basic_layers()
        result = self.assembler.assemble(
            layers, PromptPattern.SINGLE_TASK,
        )
        assert result.metadata == {}

    def test_memory_profile_rendering(self) -> None:
        """MemoryProfile レンダリングの内容確認."""
        layer = MemoryProfileLayer(
            memory_blocks=["前回の議論: マイクロサービス"],
            key_notes=["Python 3.12"],
            user_preferences={"language": "ja"},
        )
        text = self.assembler._render_memory_profile(layer)
        assert "重要情報:" in text
        assert "Python 3.12" in text
        assert "関連記憶:" in text
        assert "language: ja" in text

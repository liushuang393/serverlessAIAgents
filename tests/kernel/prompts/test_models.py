"""プロンプトレイヤーモデルのユニットテスト."""

from __future__ import annotations

import pytest

from kernel.prompts.models import (
    LAYER_CORE_SYSTEM,
    LAYER_ORDER,
    LAYER_TASK_SYSTEM,
    AssembledPrompt,
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


class TestLayerPriority:
    """LayerPriority enum のテスト."""

    def test_values(self) -> None:
        assert LayerPriority.REQUIRED == "required"
        assert LayerPriority.HIGH == "high"
        assert LayerPriority.MEDIUM == "medium"
        assert LayerPriority.LOW == "low"


class TestPromptPattern:
    """PromptPattern enum のテスト."""

    def test_values(self) -> None:
        assert PromptPattern.SINGLE_TASK == "single_task"
        assert PromptPattern.MULTI_STEP == "multi_step"
        assert PromptPattern.MULTI_TURN == "multi_turn"
        assert PromptPattern.TOOL_AUGMENTED == "tool_augmented"

    def test_all_patterns_count(self) -> None:
        assert len(PromptPattern) == 4


class TestCoreSystemLayer:
    """L1: CoreSystemLayer のテスト."""

    def test_minimal_construction(self) -> None:
        layer = CoreSystemLayer(role="テストエージェント")
        assert layer.role == "テストエージェント"
        assert layer.success_criteria == []
        assert layer.prohibitions == []
        assert layer.output_principles == []
        assert layer.priority == LayerPriority.REQUIRED

    def test_full_construction(self) -> None:
        layer = CoreSystemLayer(
            role="分析エージェント",
            success_criteria=["正確な分析結果を返す"],
            prohibitions=["推測で埋めない"],
            output_principles=["簡潔に返す"],
        )
        assert len(layer.success_criteria) == 1
        assert len(layer.prohibitions) == 1
        assert len(layer.output_principles) == 1


class TestTaskSystemLayer:
    """L2: TaskSystemLayer のテスト."""

    def test_minimal_construction(self) -> None:
        layer = TaskSystemLayer(goal="コードレビュー")
        assert layer.goal == "コードレビュー"
        assert layer.deliverables == []
        assert layer.priority == LayerPriority.REQUIRED

    def test_full_construction(self) -> None:
        layer = TaskSystemLayer(
            goal="API設計",
            deliverables=["OpenAPI仕様書"],
            constraints=["REST準拠"],
            task_success_criteria=["エンドポイント定義完了"],
            priorities=["セキュリティ", "パフォーマンス"],
        )
        assert layer.goal == "API設計"
        assert len(layer.deliverables) == 1
        assert len(layer.priorities) == 2


class TestRuntimeContextLayer:
    """L3: RuntimeContextLayer のテスト."""

    def test_default_construction(self) -> None:
        layer = RuntimeContextLayer()
        assert layer.user_request == ""
        assert layer.extracted_points == []
        assert layer.reference_summaries == []
        assert layer.priority == LayerPriority.HIGH

    def test_with_data(self) -> None:
        layer = RuntimeContextLayer(
            user_request="認証フローを改善したい",
            extracted_points=["JWT使用中", "セッション管理あり"],
            reference_summaries=["OWASP認証ガイドライン要約"],
        )
        assert len(layer.extracted_points) == 2


class TestConversationStateLayer:
    """L4: ConversationStateLayer のテスト."""

    def test_default_construction(self) -> None:
        layer = ConversationStateLayer()
        assert layer.decisions == []
        assert layer.open_items == []
        assert layer.next_actions == []
        assert layer.continued_constraints == []
        assert layer.turn_count == 0
        assert layer.priority == LayerPriority.MEDIUM

    def test_with_state(self) -> None:
        layer = ConversationStateLayer(
            decisions=["JWT認証を採用"],
            open_items=["リフレッシュトークンの有効期限"],
            next_actions=["トークン更新APIを設計"],
            turn_count=3,
        )
        assert len(layer.decisions) == 1
        assert layer.turn_count == 3


class TestMemoryProfileLayer:
    """L5: MemoryProfileLayer のテスト."""

    def test_default_construction(self) -> None:
        layer = MemoryProfileLayer()
        assert layer.memory_blocks == []
        assert layer.key_notes == []
        assert layer.user_preferences == {}
        assert layer.priority == LayerPriority.LOW

    def test_with_memory(self) -> None:
        layer = MemoryProfileLayer(
            memory_blocks=["前回の設計決定: マイクロサービス構成"],
            key_notes=["Python 3.12 使用"],
            user_preferences={"language": "ja", "style": "concise"},
        )
        assert len(layer.memory_blocks) == 1
        assert layer.user_preferences["language"] == "ja"


class TestToolEnvironmentLayer:
    """L6: ToolEnvironmentLayer のテスト."""

    def test_default_construction(self) -> None:
        layer = ToolEnvironmentLayer()
        assert layer.available_tools == []
        assert layer.environment_info == {}
        assert layer.priority == LayerPriority.MEDIUM

    def test_with_tools(self) -> None:
        tool = ToolDescription(
            name="search",
            description="ドキュメント検索",
            parameters_summary="query: str",
        )
        layer = ToolEnvironmentLayer(
            available_tools=[tool],
            environment_info={"runtime": "Python 3.12"},
            tool_usage_guidelines="必要な場合のみツールを使う",
        )
        assert len(layer.available_tools) == 1
        assert layer.available_tools[0].name == "search"


class TestAssembledPrompt:
    """AssembledPrompt のテスト."""

    def test_construction(self) -> None:
        prompt = AssembledPrompt(
            system_prompt="テストプロンプト",
            pattern=PromptPattern.SINGLE_TASK,
            layers_used=[LAYER_CORE_SYSTEM, LAYER_TASK_SYSTEM],
            token_count=100,
            token_budget=4000,
        )
        assert prompt.system_prompt == "テストプロンプト"
        assert prompt.pattern == PromptPattern.SINGLE_TASK
        assert len(prompt.layers_used) == 2
        assert prompt.layers_truncated == []
        assert prompt.layers_dropped == []


class TestPromptLayerSet:
    """PromptLayerSet のテスト."""

    def test_empty_set(self) -> None:
        layer_set = PromptLayerSet()
        assert layer_set.core_system is None
        assert layer_set.active_layers() == []

    def test_partial_set(self) -> None:
        layer_set = PromptLayerSet(
            core_system=CoreSystemLayer(role="テスト"),
            task_system=TaskSystemLayer(goal="目標"),
        )
        active = layer_set.active_layers()
        assert len(active) == 2
        # 合成順序に従う
        assert active[0][0] == LAYER_CORE_SYSTEM
        assert active[1][0] == LAYER_TASK_SYSTEM

    def test_full_set(self) -> None:
        layer_set = PromptLayerSet(
            core_system=CoreSystemLayer(role="テスト"),
            task_system=TaskSystemLayer(goal="目標"),
            runtime_context=RuntimeContextLayer(user_request="要求"),
            conversation_state=ConversationStateLayer(turn_count=1),
            memory_profile=MemoryProfileLayer(key_notes=["メモ"]),
            tool_environment=ToolEnvironmentLayer(),
        )
        active = layer_set.active_layers()
        assert len(active) == 6

    def test_active_layers_order(self) -> None:
        """active_layers() が LAYER_ORDER に従うことを確認."""
        layer_set = PromptLayerSet(
            runtime_context=RuntimeContextLayer(),
            core_system=CoreSystemLayer(role="テスト"),
            tool_environment=ToolEnvironmentLayer(),
        )
        active = layer_set.active_layers()
        names = [name for name, _ in active]
        # LAYER_ORDER 順: core_system → runtime_context → tool_environment
        assert names.index(LAYER_CORE_SYSTEM) < names.index("runtime_context")
        assert names.index("runtime_context") < names.index("tool_environment")

    def test_get_layer(self) -> None:
        layer_set = PromptLayerSet(
            core_system=CoreSystemLayer(role="テスト"),
        )
        assert layer_set.get_layer(LAYER_CORE_SYSTEM) is not None
        assert layer_set.get_layer(LAYER_TASK_SYSTEM) is None

    def test_serialization_roundtrip(self) -> None:
        """model_dump / model_validate の往復テスト."""
        layer_set = PromptLayerSet(
            core_system=CoreSystemLayer(
                role="テスト",
                success_criteria=["基準1"],
            ),
            task_system=TaskSystemLayer(goal="目標"),
        )
        data = layer_set.model_dump()
        restored = PromptLayerSet.model_validate(data)
        assert restored.core_system is not None
        assert restored.core_system.role == "テスト"
        assert restored.task_system is not None
        assert restored.task_system.goal == "目標"


class TestLayerOrder:
    """LAYER_ORDER 定数のテスト."""

    def test_contains_all_layers(self) -> None:
        expected = {
            "core_system",
            "task_system",
            "runtime_context",
            "conversation_state",
            "memory_profile",
            "tool_environment",
        }
        assert set(LAYER_ORDER) == expected

    def test_no_duplicates(self) -> None:
        assert len(LAYER_ORDER) == len(set(LAYER_ORDER))

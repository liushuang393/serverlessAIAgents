# -*- coding: utf-8 -*-
"""AgentBlueprint 単体テスト."""

import pytest
import tempfile
from pathlib import Path

from agentflow.core.blueprint import (
    AgentBlueprint,
    AgentBlueprintModel,
    ConstraintsConfig,
    MemoryConfig,
    SafetyConfig,
    SkillConfig,
    ToolConfig,
    ValidationResult,
)


class TestSkillConfig:
    """SkillConfig テストクラス."""

    def test_create_skill_config(self) -> None:
        """スキル設定を作成できること."""
        config = SkillConfig(
            name="web_scraper",
            enabled=True,
            priority=1,
        )

        assert config.name == "web_scraper"
        assert config.enabled is True
        assert config.priority == 1

    def test_default_values(self) -> None:
        """デフォルト値が正しいこと."""
        config = SkillConfig(name="test")

        assert config.enabled is True
        assert config.priority == 0


class TestToolConfig:
    """ToolConfig テストクラス."""

    def test_create_tool_config(self) -> None:
        """ツール設定を作成できること."""
        config = ToolConfig(
            uri="mcp://browser/scrape",
            enabled=True,
        )

        assert config.uri == "mcp://browser/scrape"
        assert config.enabled is True


class TestMemoryConfig:
    """MemoryConfig テストクラス."""

    def test_create_memory_config(self) -> None:
        """メモリ設定を作成できること."""
        config = MemoryConfig(
            type="enhanced",
            distillation=True,
            max_entries=5000,
        )

        assert config.type == "enhanced"
        assert config.distillation is True
        assert config.max_entries == 5000

    def test_default_values(self) -> None:
        """デフォルト値が正しいこと."""
        config = MemoryConfig()

        assert config.type == "standard"
        assert config.distillation is False


class TestSafetyConfig:
    """SafetyConfig テストクラス."""

    def test_create_safety_config(self) -> None:
        """安全設定を作成できること."""
        config = SafetyConfig(
            hallucination_check=True,
            pii_sanitization=True,
            strict_mode=True,
        )

        assert config.hallucination_check is True
        assert config.strict_mode is True

    def test_default_values(self) -> None:
        """デフォルト値が正しいこと."""
        config = SafetyConfig()

        assert config.hallucination_check is True
        assert config.pii_sanitization is True
        assert config.strict_mode is False


class TestConstraintsConfig:
    """ConstraintsConfig テストクラス."""

    def test_create_constraints_config(self) -> None:
        """制約設定を作成できること."""
        config = ConstraintsConfig(
            allowed_tools=["tool1", "tool2"],
            max_iterations=20,
            timeout_seconds=120.0,
        )

        assert config.allowed_tools == ["tool1", "tool2"]
        assert config.max_iterations == 20
        assert config.timeout_seconds == 120.0


class TestAgentBlueprintModel:
    """AgentBlueprintModel テストクラス."""

    def test_create_model(self) -> None:
        """モデルを作成できること."""
        model = AgentBlueprintModel(
            name="TestAgent",
            description="テスト用Agent",
            version="1.0.0",
        )

        assert model.name == "TestAgent"
        assert model.description == "テスト用Agent"
        assert model.version == "1.0.0"

    def test_normalize_skills_strings(self) -> None:
        """文字列スキルを正規化できること."""
        model = AgentBlueprintModel(
            name="Test",
            skills=["skill1", "skill2"],
        )

        assert len(model.skills) == 2
        assert "skill1" in model.skills

    def test_normalize_skills_dicts(self) -> None:
        """辞書スキルを正規化できること."""
        model = AgentBlueprintModel(
            name="Test",
            skills=[
                {"name": "skill1", "priority": 1},
            ],
        )

        assert len(model.skills) == 1
        skill = model.skills[0]
        assert isinstance(skill, SkillConfig)
        assert skill.name == "skill1"

    def test_normalize_tools_strings(self) -> None:
        """文字列ツールを正規化できること."""
        model = AgentBlueprintModel(
            name="Test",
            tools=["mcp://server/tool"],
        )

        assert len(model.tools) == 1
        assert "mcp://server/tool" in model.tools


class TestAgentBlueprint:
    """AgentBlueprint テストクラス."""

    @pytest.fixture
    def sample_data(self) -> dict:
        """サンプルデータを作成."""
        return {
            "name": "CompetitorAnalyzer",
            "description": "競合分析Agent",
            "version": "1.0.0",
            "skills": ["web_scraper", "data_analyzer"],
            "tools": ["mcp://browser/scrape"],
            "memory": {
                "type": "enhanced",
                "distillation": True,
            },
            "safety": {
                "hallucination_check": True,
                "pii_sanitization": True,
            },
            "constraints": {
                "max_iterations": 10,
                "allowed_domains": ["amazon.com"],
            },
        }

    def test_from_dict(self, sample_data: dict) -> None:
        """辞書からブループリントを作成できること."""
        blueprint = AgentBlueprint.from_dict(sample_data)

        assert blueprint.name == "CompetitorAnalyzer"
        assert blueprint.description == "競合分析Agent"
        assert len(blueprint.skills) == 2

    def test_to_dict(self, sample_data: dict) -> None:
        """辞書に変換できること."""
        blueprint = AgentBlueprint.from_dict(sample_data)
        data = blueprint.to_dict()

        assert data["name"] == "CompetitorAnalyzer"
        assert "skills" in data
        assert "tools" in data

    def test_from_yaml(self, sample_data: dict) -> None:
        """YAMLファイルからロードできること."""
        import yaml

        with tempfile.NamedTemporaryFile(
            mode="w",
            suffix=".yaml",
            delete=False,
            encoding="utf-8",
        ) as f:
            yaml.dump(sample_data, f, allow_unicode=True)
            f.flush()

            blueprint = AgentBlueprint.from_yaml(f.name)

            assert blueprint.name == "CompetitorAnalyzer"

            # クリーンアップ
            Path(f.name).unlink()

    def test_to_yaml(self, sample_data: dict) -> None:
        """YAMLファイルに保存できること."""
        blueprint = AgentBlueprint.from_dict(sample_data)

        with tempfile.NamedTemporaryFile(
            mode="w",
            suffix=".yaml",
            delete=False,
            encoding="utf-8",
        ) as f:
            blueprint.to_yaml(f.name)

            # ファイルが作成されたことを確認
            assert Path(f.name).exists()

            # 内容を確認
            loaded = AgentBlueprint.from_yaml(f.name)
            assert loaded.name == blueprint.name

            # クリーンアップ
            Path(f.name).unlink()

    def test_validate_valid_blueprint(self, sample_data: dict) -> None:
        """有効なブループリントの検証が通ること."""
        blueprint = AgentBlueprint.from_dict(sample_data)
        result = blueprint.validate()

        assert result.valid is True
        assert len(result.errors) == 0

    def test_validate_missing_name(self) -> None:
        """名前がないブループリントの検証が失敗すること."""
        blueprint = AgentBlueprint.from_dict({
            "description": "説明のみ",
        })
        result = blueprint.validate()

        assert result.valid is False
        assert any("名" in e for e in result.errors)

    def test_get_skill_names(self, sample_data: dict) -> None:
        """スキル名リストを取得できること."""
        blueprint = AgentBlueprint.from_dict(sample_data)
        names = blueprint.get_skill_names()

        assert "web_scraper" in names
        assert "data_analyzer" in names

    def test_get_tool_uris(self, sample_data: dict) -> None:
        """ツールURIリストを取得できること."""
        blueprint = AgentBlueprint.from_dict(sample_data)
        uris = blueprint.get_tool_uris()

        assert "mcp://browser/scrape" in uris

    def test_properties(self, sample_data: dict) -> None:
        """プロパティが正しく動作すること."""
        blueprint = AgentBlueprint.from_dict(sample_data)

        assert blueprint.name == "CompetitorAnalyzer"
        assert blueprint.version == "1.0.0"
        assert blueprint.memory.type == "enhanced"
        assert blueprint.safety.hallucination_check is True
        assert blueprint.constraints.max_iterations == 10

    def test_repr(self, sample_data: dict) -> None:
        """文字列表現が正しいこと."""
        blueprint = AgentBlueprint.from_dict(sample_data)
        repr_str = repr(blueprint)

        assert "CompetitorAnalyzer" in repr_str
        assert "1.0.0" in repr_str

    @pytest.mark.asyncio
    async def test_to_agent(self, sample_data: dict) -> None:
        """Agentをインスタンス化できること."""
        blueprint = AgentBlueprint.from_dict(sample_data)
        agent = await blueprint.to_agent()

        assert agent is not None
        # 動的生成されたAgentはrun メソッドを持つ
        assert hasattr(agent, "run")

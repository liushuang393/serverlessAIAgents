"""AgentFlow テンプレートシステムのユニットテスト."""

from __future__ import annotations

import tempfile
from pathlib import Path

import pytest

from agentflow.templates.template_manager import (
    TemplateManager,
    TemplateMetadata,
    TemplateParameter,
)


@pytest.fixture
def template_manager():
    """テンプレートマネージャーを作成."""
    # パッケージ内のテンプレートディレクトリを使用
    return TemplateManager()


@pytest.fixture
def temp_output_dir():
    """一時出力ディレクトリを作成."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


class TestTemplateManager:
    """TemplateManager のテスト."""

    def test_list_templates(self, template_manager):
        """テンプレート一覧を取得."""
        templates = template_manager.list_templates()
        
        assert len(templates) >= 3
        assert all(isinstance(t, TemplateMetadata) for t in templates)
        
        # テンプレート ID を確認
        template_ids = [t.id for t in templates]
        assert "invoice-processor" in template_ids
        assert "chatbot" in template_ids
        assert "data-pipeline" in template_ids

    def test_get_template(self, template_manager):
        """テンプレートメタデータを取得."""
        metadata = template_manager.get_template("invoice-processor")
        
        assert metadata is not None
        assert metadata.id == "invoice-processor"
        assert metadata.name == "Invoice Processor"
        assert metadata.category == "document"
        assert len(metadata.parameters) > 0

    def test_get_template_not_found(self, template_manager):
        """存在しないテンプレートを取得."""
        metadata = template_manager.get_template("nonexistent")
        assert metadata is None

    def test_generate_project_invoice_processor(
        self, template_manager, temp_output_dir
    ):
        """invoice-processor テンプレートからプロジェクトを生成."""
        output_dir = temp_output_dir / "invoice-processor"
        
        parameters = {
            "agent_name": "my-invoice-processor",
            "agent_description": "My custom invoice processor",
            "author": "Test User",
            "email": "test@example.com",
            "database_type": "postgresql",
            "enable_ocr": True,
            "supported_languages": ["ja", "en", "zh"],
        }
        
        template_manager.generate_project(
            "invoice-processor",
            output_dir,
            parameters,
        )
        
        # 生成されたファイルを確認
        assert output_dir.exists()
        assert (output_dir / "agent.yaml").exists()
        assert (output_dir / "agent.py").exists()
        assert (output_dir / "README.md").exists()
        
        # agent.yaml の内容を確認
        agent_yaml = (output_dir / "agent.yaml").read_text(encoding="utf-8")
        assert "my-invoice-processor" in agent_yaml
        assert "postgresql" in agent_yaml
        assert "ocr_engine" in agent_yaml

    def test_generate_project_chatbot(self, template_manager, temp_output_dir):
        """chatbot テンプレートからプロジェクトを生成."""
        output_dir = temp_output_dir / "chatbot"
        
        parameters = {
            "agent_name": "my-chatbot",
            "agent_description": "My custom chatbot",
            "author": "Test User",
            "email": "test@example.com",
            "llm_provider": "anthropic",
            "model_name": "claude-3-opus",
            "enable_memory": True,
            "max_history_length": 20,
        }
        
        template_manager.generate_project(
            "chatbot",
            output_dir,
            parameters,
        )
        
        # 生成されたファイルを確認
        assert output_dir.exists()
        assert (output_dir / "agent.yaml").exists()
        assert (output_dir / "agent.py").exists()
        assert (output_dir / "README.md").exists()
        
        # agent.yaml の内容を確認
        agent_yaml = (output_dir / "agent.yaml").read_text(encoding="utf-8")
        assert "my-chatbot" in agent_yaml
        assert "anthropic" in agent_yaml
        assert "claude-3-opus" in agent_yaml

    def test_generate_project_data_pipeline(
        self, template_manager, temp_output_dir
    ):
        """data-pipeline テンプレートからプロジェクトを生成."""
        output_dir = temp_output_dir / "data-pipeline"
        
        parameters = {
            "agent_name": "my-data-pipeline",
            "agent_description": "My custom data pipeline",
            "author": "Test User",
            "email": "test@example.com",
            "input_format": "parquet",
            "output_format": "json",
            "enable_validation": True,
            "enable_transformation": True,
        }
        
        template_manager.generate_project(
            "data-pipeline",
            output_dir,
            parameters,
        )
        
        # 生成されたファイルを確認
        assert output_dir.exists()
        assert (output_dir / "agent.yaml").exists()
        assert (output_dir / "agent.py").exists()
        assert (output_dir / "README.md").exists()
        
        # agent.yaml の内容を確認
        agent_yaml = (output_dir / "agent.yaml").read_text(encoding="utf-8")
        assert "my-data-pipeline" in agent_yaml
        assert "parquet" in agent_yaml
        assert "json" in agent_yaml

    def test_generate_project_template_not_found(
        self, template_manager, temp_output_dir
    ):
        """存在しないテンプレートからプロジェクトを生成."""
        output_dir = temp_output_dir / "test"
        
        with pytest.raises(ValueError, match="Template not found"):
            template_manager.generate_project(
                "nonexistent",
                output_dir,
                {},
            )

    def test_generate_project_output_exists(
        self, template_manager, temp_output_dir
    ):
        """既存のディレクトリにプロジェクトを生成."""
        output_dir = temp_output_dir / "test"
        output_dir.mkdir()
        
        with pytest.raises(FileExistsError, match="already exists"):
            template_manager.generate_project(
                "chatbot",
                output_dir,
                {
                    "agent_name": "test",
                    "agent_description": "test",
                    "author": "test",
                    "email": "test@example.com",
                    "llm_provider": "openai",
                    "model_name": "gpt-4",
                },
            )

    def test_validate_parameters_required(self, template_manager):
        """必須パラメーターの検証."""
        # デフォルト値のない必須パラメーターを持つメタデータを作成
        metadata = TemplateMetadata(
            id="test",
            name="Test",
            description="Test template",
            category="test",
            author="Test",
            version="1.0.0",
            parameters=[
                TemplateParameter(
                    name="required_param",
                    type="string",
                    description="Required parameter",
                    required=True,
                ),
            ],
        )

        # 必須パラメーターが不足
        with pytest.raises(ValueError, match="Required parameter missing"):
            template_manager._validate_parameters(metadata, {})

    def test_validate_parameters_type(self, template_manager):
        """パラメーター型の検証."""
        metadata = TemplateMetadata(
            id="test",
            name="Test",
            description="Test template",
            category="test",
            author="Test",
            version="1.0.0",
            parameters=[
                TemplateParameter(
                    name="count",
                    type="int",
                    description="Count",
                    required=True,
                ),
            ],
        )
        
        # 型が不正
        with pytest.raises(ValueError, match="must be int"):
            template_manager._validate_parameters(
                metadata,
                {"count": "not_an_int"},
            )

    def test_validate_parameters_choices(self, template_manager):
        """パラメーター選択肢の検証."""
        metadata = TemplateMetadata(
            id="test",
            name="Test",
            description="Test template",
            category="test",
            author="Test",
            version="1.0.0",
            parameters=[
                TemplateParameter(
                    name="format",
                    type="string",
                    description="Format",
                    required=True,
                    choices=["json", "yaml"],
                ),
            ],
        )
        
        # 選択肢にない値
        with pytest.raises(ValueError, match="must be one of"):
            template_manager._validate_parameters(
                metadata,
                {"format": "xml"},
            )

    def test_validate_parameters_default(self, template_manager):
        """デフォルト値の使用."""
        metadata = TemplateMetadata(
            id="test",
            name="Test",
            description="Test template",
            category="test",
            author="Test",
            version="1.0.0",
            parameters=[
                TemplateParameter(
                    name="timeout",
                    type="int",
                    description="Timeout",
                    required=False,
                    default=30,
                ),
            ],
        )
        
        # デフォルト値が使用される
        validated = template_manager._validate_parameters(metadata, {})
        assert validated["timeout"] == 30


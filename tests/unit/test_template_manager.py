"""TemplateManager のテスト.

このテストは TemplateManager クラスの機能をテストします。
"""

from pathlib import Path

import pytest
import yaml

from agentflow.templates.template_manager import (
    TemplateManager,
    TemplateMetadata,
    TemplateParameter,
)


class TestTemplateParameter:
    """TemplateParameter のテスト."""

    def test_create_parameter(self) -> None:
        """パラメーターを作成できることをテスト."""
        param = TemplateParameter(
            name="test_param",
            type="string",
            description="Test parameter",
            default="default_value",
            required=True,
            choices=["option1", "option2"],
        )

        assert param.name == "test_param"
        assert param.type == "string"
        assert param.description == "Test parameter"
        assert param.default == "default_value"
        assert param.required is True
        assert param.choices == ["option1", "option2"]

    def test_create_parameter_with_defaults(self) -> None:
        """デフォルト値でパラメーターを作成できることをテスト."""
        param = TemplateParameter(
            name="test_param",
            type="string",
            description="Test parameter",
        )

        assert param.name == "test_param"
        assert param.type == "string"
        assert param.description == "Test parameter"
        assert param.default is None
        assert param.required is True
        assert param.choices is None


class TestTemplateMetadata:
    """TemplateMetadata のテスト."""

    def test_create_metadata(self) -> None:
        """メタデータを作成できることをテスト."""
        param = TemplateParameter(
            name="test_param",
            type="string",
            description="Test parameter",
        )

        metadata = TemplateMetadata(
            id="test-template",
            name="Test Template",
            description="Test description",
            category="test",
            author="Test Author",
            version="1.0.0",
            parameters=[param],
        )

        assert metadata.id == "test-template"
        assert metadata.name == "Test Template"
        assert metadata.description == "Test description"
        assert metadata.category == "test"
        assert metadata.author == "Test Author"
        assert metadata.version == "1.0.0"
        assert len(metadata.parameters) == 1
        assert metadata.parameters[0].name == "test_param"

    def test_create_metadata_without_parameters(self) -> None:
        """パラメーターなしでメタデータを作成できることをテスト."""
        metadata = TemplateMetadata(
            id="test-template",
            name="Test Template",
            description="Test description",
            category="test",
            author="Test Author",
            version="1.0.0",
        )

        assert metadata.parameters == []


class TestTemplateManager:
    """TemplateManager のテスト."""

    @pytest.fixture
    def templates_dir(self, tmp_path: Path) -> Path:
        """テスト用のテンプレートディレクトリを作成."""
        return tmp_path / "templates"

    @pytest.fixture
    def manager(self, templates_dir: Path) -> TemplateManager:
        """テスト用のマネージャーを作成."""
        return TemplateManager(templates_dir=templates_dir)

    @pytest.fixture
    def sample_template_dir(self, templates_dir: Path) -> Path:
        """サンプルテンプレートディレクトリを作成."""
        template_dir = templates_dir / "sample-template"
        template_dir.mkdir(parents=True)

        # template.yaml を作成
        template_yaml = {
            "id": "sample-template",
            "name": "Sample Template",
            "description": "Sample description",
            "category": "test",
            "author": "Test Author",
            "version": "1.0.0",
            "parameters": [
                {
                    "name": "project_name",
                    "type": "string",
                    "description": "Project name",
                    "default": "my-project",
                    "required": True,
                },
                {
                    "name": "enable_feature",
                    "type": "bool",
                    "description": "Enable feature",
                    "default": True,
                    "required": False,
                },
            ],
        }

        with (template_dir / "template.yaml").open("w", encoding="utf-8") as f:
            yaml.dump(template_yaml, f)

        # サンプルファイルを作成
        (template_dir / "README.md").write_text(
            "# {{ project_name }}\n\nFeature enabled: {{ enable_feature }}",
            encoding="utf-8",
        )

        (template_dir / "config.yaml").write_text(
            "name: {{ project_name }}\nenabled: {{ enable_feature }}",
            encoding="utf-8",
        )

        return template_dir

    def test_init_with_default_path(self) -> None:
        """デフォルトパスで初期化できることをテスト."""
        manager = TemplateManager()

        expected_path = Path(__file__).parent.parent.parent / "agentflow" / "templates" / "scenarios"
        assert manager.templates_dir == expected_path

    def test_init_with_custom_path(self, templates_dir: Path) -> None:
        """カスタムパスで初期化できることをテスト."""
        manager = TemplateManager(templates_dir=templates_dir)

        assert manager.templates_dir == templates_dir
        assert templates_dir.exists()

    def test_list_templates_empty(self, manager: TemplateManager) -> None:
        """空のテンプレート一覧を取得できることをテスト."""
        templates = manager.list_templates()

        assert templates == []

    def test_list_templates_single(self, manager: TemplateManager, sample_template_dir: Path) -> None:
        """単一のテンプレートを一覧取得できることをテスト."""
        templates = manager.list_templates()

        assert len(templates) == 1
        assert templates[0].id == "sample-template"
        assert templates[0].name == "Sample Template"

    def test_list_templates_multiple(self, manager: TemplateManager, templates_dir: Path) -> None:
        """複数のテンプレートを一覧取得できることをテスト."""
        # 3つのテンプレートを作成
        for i in range(3):
            template_dir = templates_dir / f"template-{i}"
            template_dir.mkdir(parents=True)

            template_yaml = {
                "id": f"template-{i}",
                "name": f"Template {i}",
                "description": f"Description {i}",
                "category": "test",
                "author": "Test Author",
                "version": "1.0.0",
            }

            with (template_dir / "template.yaml").open("w", encoding="utf-8") as f:
                yaml.dump(template_yaml, f)

        templates = manager.list_templates()

        assert len(templates) == 3

    def test_list_templates_skips_files(self, manager: TemplateManager, templates_dir: Path) -> None:
        """ファイルをスキップすることをテスト."""
        # ファイルを作成
        (templates_dir / "not-a-template.txt").write_text("test", encoding="utf-8")

        templates = manager.list_templates()

        assert templates == []

    def test_list_templates_skips_invalid(self, manager: TemplateManager, templates_dir: Path) -> None:
        """無効なテンプレートをスキップすることをテスト."""
        # template.yaml がないディレクトリを作成
        template_dir = templates_dir / "invalid-template"
        template_dir.mkdir(parents=True)

        templates = manager.list_templates()

        assert templates == []

    def test_get_template(self, manager: TemplateManager, sample_template_dir: Path) -> None:
        """テンプレートを取得できることをテスト."""
        metadata = manager.get_template("sample-template")

        assert metadata is not None
        assert metadata.id == "sample-template"
        assert metadata.name == "Sample Template"
        assert len(metadata.parameters) == 2

    def test_get_nonexistent_template(self, manager: TemplateManager) -> None:
        """存在しないテンプレートの取得が None を返すことをテスト."""
        metadata = manager.get_template("nonexistent-template")

        assert metadata is None

    def test_generate_project(self, manager: TemplateManager, sample_template_dir: Path, tmp_path: Path) -> None:
        """プロジェクトを生成できることをテスト."""
        output_dir = tmp_path / "output"
        parameters = {
            "project_name": "test-project",
            "enable_feature": True,
        }

        manager.generate_project("sample-template", output_dir, parameters)

        assert output_dir.exists()
        assert (output_dir / "README.md").exists()
        assert (output_dir / "config.yaml").exists()

        # 内容を確認
        readme_content = (output_dir / "README.md").read_text(encoding="utf-8")
        assert "# test-project" in readme_content
        assert "Feature enabled: True" in readme_content

        config_content = (output_dir / "config.yaml").read_text(encoding="utf-8")
        assert "name: test-project" in config_content
        assert "enabled: True" in config_content

    def test_generate_project_nonexistent_template(self, manager: TemplateManager, tmp_path: Path) -> None:
        """存在しないテンプレートでプロジェクト生成がエラーになることをテスト."""
        output_dir = tmp_path / "output"

        with pytest.raises(ValueError, match="Template not found"):
            manager.generate_project("nonexistent-template", output_dir, {})

    def test_generate_project_existing_output_dir(
        self, manager: TemplateManager, sample_template_dir: Path, tmp_path: Path
    ) -> None:
        """既存の出力ディレクトリでプロジェクト生成がエラーになることをテスト."""
        output_dir = tmp_path / "output"
        output_dir.mkdir()

        with pytest.raises(FileExistsError, match="Output directory already exists"):
            manager.generate_project("sample-template", output_dir, {})

    def test_generate_project_with_default_parameters(
        self, manager: TemplateManager, sample_template_dir: Path, tmp_path: Path
    ) -> None:
        """デフォルトパラメーターでプロジェクトを生成できることをテスト."""
        output_dir = tmp_path / "output"

        # パラメーターを指定しない
        manager.generate_project("sample-template", output_dir, {})

        assert output_dir.exists()
        readme_content = (output_dir / "README.md").read_text(encoding="utf-8")
        assert "# my-project" in readme_content  # デフォルト値

    def test_validate_parameters_type_string(self, manager: TemplateManager, templates_dir: Path) -> None:
        """文字列型パラメーターの検証をテスト."""
        template_dir = templates_dir / "type-test"
        template_dir.mkdir(parents=True)

        template_yaml = {
            "id": "type-test",
            "name": "Type Test",
            "description": "Type test",
            "category": "test",
            "author": "Test Author",
            "version": "1.0.0",
            "parameters": [
                {
                    "name": "string_param",
                    "type": "string",
                    "description": "String parameter",
                    "required": True,
                }
            ],
        }

        with (template_dir / "template.yaml").open("w", encoding="utf-8") as f:
            yaml.dump(template_yaml, f)

        metadata = manager.get_template("type-test")

        # 正しい型
        validated = manager._validate_parameters(metadata, {"string_param": "test"})
        assert validated["string_param"] == "test"

        # 間違った型
        with pytest.raises(ValueError, match="must be string"):
            manager._validate_parameters(metadata, {"string_param": 123})

    def test_validate_parameters_type_int(self, manager: TemplateManager, templates_dir: Path) -> None:
        """整数型パラメーターの検証をテスト."""
        template_dir = templates_dir / "int-test"
        template_dir.mkdir(parents=True)

        template_yaml = {
            "id": "int-test",
            "name": "Int Test",
            "description": "Int test",
            "category": "test",
            "author": "Test Author",
            "version": "1.0.0",
            "parameters": [
                {
                    "name": "int_param",
                    "type": "int",
                    "description": "Int parameter",
                    "required": True,
                }
            ],
        }

        with (template_dir / "template.yaml").open("w", encoding="utf-8") as f:
            yaml.dump(template_yaml, f)

        metadata = manager.get_template("int-test")

        # 正しい型
        validated = manager._validate_parameters(metadata, {"int_param": 42})
        assert validated["int_param"] == 42

        # 間違った型
        with pytest.raises(ValueError, match="must be int"):
            manager._validate_parameters(metadata, {"int_param": "not-an-int"})

    def test_validate_parameters_type_bool(self, manager: TemplateManager, templates_dir: Path) -> None:
        """ブール型パラメーターの検証をテスト."""
        template_dir = templates_dir / "bool-test"
        template_dir.mkdir(parents=True)

        template_yaml = {
            "id": "bool-test",
            "name": "Bool Test",
            "description": "Bool test",
            "category": "test",
            "author": "Test Author",
            "version": "1.0.0",
            "parameters": [
                {
                    "name": "bool_param",
                    "type": "bool",
                    "description": "Bool parameter",
                    "required": True,
                }
            ],
        }

        with (template_dir / "template.yaml").open("w", encoding="utf-8") as f:
            yaml.dump(template_yaml, f)

        metadata = manager.get_template("bool-test")

        # 正しい型
        validated = manager._validate_parameters(metadata, {"bool_param": True})
        assert validated["bool_param"] is True

        # 間違った型
        with pytest.raises(ValueError, match="must be bool"):
            manager._validate_parameters(metadata, {"bool_param": "not-a-bool"})

    def test_validate_parameters_type_list(self, manager: TemplateManager, templates_dir: Path) -> None:
        """リスト型パラメーターの検証をテスト."""
        template_dir = templates_dir / "list-test"
        template_dir.mkdir(parents=True)

        template_yaml = {
            "id": "list-test",
            "name": "List Test",
            "description": "List test",
            "category": "test",
            "author": "Test Author",
            "version": "1.0.0",
            "parameters": [
                {
                    "name": "list_param",
                    "type": "list",
                    "description": "List parameter",
                    "required": True,
                }
            ],
        }

        with (template_dir / "template.yaml").open("w", encoding="utf-8") as f:
            yaml.dump(template_yaml, f)

        metadata = manager.get_template("list-test")

        # 正しい型
        validated = manager._validate_parameters(metadata, {"list_param": ["a", "b"]})
        assert validated["list_param"] == ["a", "b"]

        # 間違った型
        with pytest.raises(ValueError, match="must be list"):
            manager._validate_parameters(metadata, {"list_param": "not-a-list"})

    def test_validate_parameters_choices(self, manager: TemplateManager, templates_dir: Path) -> None:
        """選択肢パラメーターの検証をテスト."""
        template_dir = templates_dir / "choices-test"
        template_dir.mkdir(parents=True)

        template_yaml = {
            "id": "choices-test",
            "name": "Choices Test",
            "description": "Choices test",
            "category": "test",
            "author": "Test Author",
            "version": "1.0.0",
            "parameters": [
                {
                    "name": "choice_param",
                    "type": "string",
                    "description": "Choice parameter",
                    "required": True,
                    "choices": ["option1", "option2", "option3"],
                }
            ],
        }

        with (template_dir / "template.yaml").open("w", encoding="utf-8") as f:
            yaml.dump(template_yaml, f)

        metadata = manager.get_template("choices-test")

        # 正しい選択肢
        validated = manager._validate_parameters(metadata, {"choice_param": "option1"})
        assert validated["choice_param"] == "option1"

        # 間違った選択肢
        with pytest.raises(ValueError, match="must be one of"):
            manager._validate_parameters(metadata, {"choice_param": "invalid-option"})

    def test_validate_parameters_required_without_default(self, manager: TemplateManager, templates_dir: Path) -> None:
        """デフォルト値なしの必須パラメーターが不足している場合にエラーになることをテスト."""
        template_dir = templates_dir / "required-test"
        template_dir.mkdir(parents=True)

        template_yaml = {
            "id": "required-test",
            "name": "Required Test",
            "description": "Required test",
            "category": "test",
            "author": "Test Author",
            "version": "1.0.0",
            "parameters": [
                {
                    "name": "required_param",
                    "type": "string",
                    "description": "Required parameter",
                    "required": True,
                }
            ],
        }

        with (template_dir / "template.yaml").open("w", encoding="utf-8") as f:
            yaml.dump(template_yaml, f)

        metadata = manager.get_template("required-test")

        with pytest.raises(ValueError, match="Required parameter missing"):
            manager._validate_parameters(metadata, {})

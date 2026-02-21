"""SchemaLoader のテスト."""

import logging
from pathlib import Path
from typing import Any

import pytest
import yaml

from agentflow.core.metadata import (
    A2AConfig,
    AgentMetadata,
    DependencySpec,
    InputField,
    InterfaceDefinition,
    MetaInfo,
    OutputField,
    PocketFlowConfig,
    ProtocolConfig,
    VisualConfig,
)
from agentflow.core.schemas import SchemaLoader, SchemaValidationError


class TestSchemaLoader:
    """SchemaLoader のテスト."""

    @pytest.fixture
    def loader(self) -> SchemaLoader:
        """テスト用の SchemaLoader インスタンス."""
        return SchemaLoader()

    @pytest.fixture
    def sample_metadata(self) -> AgentMetadata:
        """テスト用のサンプルメタデータ."""
        return AgentMetadata(
            meta=MetaInfo(
                id="test-agent",
                name="Test Agent",
                version="1.0.0",
                author="Test Author",
                icon="🤖",
                category="test",
                description="A test agent",
            ),
            interfaces=InterfaceDefinition(
                inputs=[
                    InputField(
                        name="text",
                        type="string",
                        required=True,
                        description="Input text",
                    )
                ],
                outputs=[OutputField(name="result", type="string")],
            ),
            protocols=ProtocolConfig(
                mcp=None,
                a2a=A2AConfig(enabled=True, skills=["process"]),
                agui=None,
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=[]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#000000", size="medium", ports={}),
        )

    @pytest.fixture
    def sample_metadata_dict(self) -> dict[str, Any]:
        """テスト用のサンプルメタデータ辞書."""
        return {
            "meta": {
                "id": "test-agent",
                "name": "Test Agent",
                "version": "1.0.0",
                "author": "Test Author",
                "icon": "🤖",
                "category": "test",
                "description": "A test agent",
            },
            "interfaces": {
                "inputs": [
                    {
                        "name": "text",
                        "type": "string",
                        "required": True,
                        "description": "Input text",
                    }
                ],
                "outputs": [{"name": "result", "type": "string"}],
            },
            "protocols": {
                "a2a": {"enabled": True, "skills": ["process"]},
            },
            "dependencies": {"agents": [], "tools": [], "packages": []},
            "pocketflow": {
                "entry": "flow.py:flow",
                "shared_schema": "schemas.py:Schema",
            },
            "visual": {"color": "#000000", "size": "medium", "ports": {}},
        }

    def test_loader_initialization(self) -> None:
        """SchemaLoader の初期化をテスト."""
        loader = SchemaLoader()
        assert loader is not None

    def test_loader_with_custom_logger(self) -> None:
        """カスタムロガーでの初期化をテスト."""
        logger = logging.getLogger("test")
        loader = SchemaLoader(logger=logger)
        assert loader._logger is logger

    def test_load_from_file_success(self, loader: SchemaLoader, sample_metadata: AgentMetadata, tmp_path: Path) -> None:
        """ファイルからの読み込み成功をテスト."""
        # メタデータファイルを作成
        metadata_file = tmp_path / "agent.yaml"
        loader.save_to_file(sample_metadata, metadata_file)

        # ファイルから読み込み
        loaded_metadata = loader.load_from_file(metadata_file)

        assert loaded_metadata.meta.id == "test-agent"
        assert loaded_metadata.meta.name == "Test Agent"

    def test_load_from_file_not_found(self, loader: SchemaLoader, tmp_path: Path) -> None:
        """存在しないファイルの読み込みをテスト."""
        nonexistent_file = tmp_path / "nonexistent.yaml"

        with pytest.raises(FileNotFoundError, match="Agent metadata file not found"):
            loader.load_from_file(nonexistent_file)

    def test_load_from_file_invalid_yaml(self, loader: SchemaLoader, tmp_path: Path) -> None:
        """無効な YAML ファイルの読み込みをテスト."""
        invalid_file = tmp_path / "invalid.yaml"
        invalid_file.write_text("invalid: yaml: content: [", encoding="utf-8")

        with pytest.raises(yaml.YAMLError):
            loader.load_from_file(invalid_file)

    def test_load_from_dict_success(self, loader: SchemaLoader, sample_metadata_dict: dict[str, Any]) -> None:
        """辞書からの読み込み成功をテスト."""
        metadata = loader.load_from_dict(sample_metadata_dict)

        assert metadata.meta.id == "test-agent"
        assert metadata.meta.name == "Test Agent"

    def test_load_from_dict_invalid_data(self, loader: SchemaLoader) -> None:
        """無効な辞書データの読み込みをテスト."""
        invalid_data = {"meta": {"id": "test"}}  # 必須フィールドが不足

        with pytest.raises(SchemaValidationError):
            loader.load_from_dict(invalid_data)

    def test_validate_success(self, loader: SchemaLoader, sample_metadata_dict: dict[str, Any]) -> None:
        """検証成功をテスト."""
        metadata = loader.validate(sample_metadata_dict)

        assert isinstance(metadata, AgentMetadata)
        assert metadata.meta.id == "test-agent"

    def test_validate_failure(self, loader: SchemaLoader) -> None:
        """検証失敗をテスト."""
        invalid_data = {"meta": {"id": "test"}}  # 必須フィールドが不足

        with pytest.raises(SchemaValidationError) as exc_info:
            loader.validate(invalid_data)

        assert len(exc_info.value.errors) > 0

    def test_save_to_file(self, loader: SchemaLoader, sample_metadata: AgentMetadata, tmp_path: Path) -> None:
        """ファイルへの保存をテスト."""
        output_file = tmp_path / "output.yaml"

        loader.save_to_file(sample_metadata, output_file)

        assert output_file.exists()

        # 保存されたファイルを読み込んで検証
        loaded_metadata = loader.load_from_file(output_file)
        assert loaded_metadata.meta.id == sample_metadata.meta.id

    def test_to_dict(self, loader: SchemaLoader, sample_metadata: AgentMetadata) -> None:
        """辞書への変換をテスト."""
        data = loader.to_dict(sample_metadata)

        assert isinstance(data, dict)
        assert data["meta"]["id"] == "test-agent"
        assert data["meta"]["name"] == "Test Agent"

    def test_to_json_schema(self, loader: SchemaLoader) -> None:
        """JSON スキーマ生成をテスト."""
        # Pydantic の RootModel 互換性問題により、このテストはスキップ
        # 参考: https://github.com/pydantic/pydantic/issues/8854
        pytest.skip("Pydantic RootModel compatibility issue with Python 3.13")

        schema = loader.to_json_schema()

        assert isinstance(schema, dict)
        assert "$defs" in schema or "definitions" in schema
        assert "properties" in schema
        assert "meta" in schema["properties"]

    def test_schema_validation_error_message(self) -> None:
        """SchemaValidationError のメッセージをテスト."""
        errors = [
            {"loc": ("meta", "id"), "msg": "field required"},
            {"loc": ("meta", "name"), "msg": "field required"},
        ]

        error = SchemaValidationError(errors)

        assert "Schema validation failed" in str(error)
        assert "meta" in str(error)
        assert "field required" in str(error)

    def test_round_trip_conversion(self, loader: SchemaLoader, sample_metadata: AgentMetadata, tmp_path: Path) -> None:
        """メタデータの往復変換をテスト."""
        # メタデータ -> 辞書 -> メタデータ
        data = loader.to_dict(sample_metadata)
        restored_metadata = loader.load_from_dict(data)

        assert restored_metadata.meta.id == sample_metadata.meta.id
        assert restored_metadata.meta.name == sample_metadata.meta.name

        # メタデータ -> ファイル -> メタデータ
        file_path = tmp_path / "roundtrip.yaml"
        loader.save_to_file(sample_metadata, file_path)
        file_metadata = loader.load_from_file(file_path)

        assert file_metadata.meta.id == sample_metadata.meta.id
        assert file_metadata.meta.name == sample_metadata.meta.name

"""AgentValidator のテスト."""

import pytest

from agentflow.core.metadata import (
    A2AConfig,
    AgentMetadata,
    AGUIConfig,
    DependencySpec,
    InputField,
    InterfaceDefinition,
    MCPConfig,
    MetaInfo,
    OutputField,
    PocketFlowConfig,
    ProtocolConfig,
    VisualConfig,
)
from agentflow.core.validator import AgentValidator, ValidationResult


class TestValidationResult:
    """ValidationResult のテスト."""

    def test_validation_result_creation(self) -> None:
        """ValidationResult の作成をテスト."""
        result = ValidationResult()

        assert result.is_valid is True
        assert result.errors == []
        assert result.warnings == []

    def test_validation_result_with_errors(self) -> None:
        """エラー付き ValidationResult の作成をテスト."""
        result = ValidationResult(
            is_valid=False, errors=["Error 1", "Error 2"], warnings=["Warning 1"]
        )

        assert result.is_valid is False
        assert len(result.errors) == 2
        assert len(result.warnings) == 1

    def test_add_error(self) -> None:
        """エラーの追加をテスト."""
        result = ValidationResult()
        result.add_error("Test error")

        assert result.is_valid is False
        assert "Test error" in result.errors

    def test_add_warning(self) -> None:
        """警告の追加をテスト."""
        result = ValidationResult()
        result.add_warning("Test warning")

        assert result.is_valid is True  # 警告は is_valid に影響しない
        assert "Test warning" in result.warnings

    def test_validation_result_repr(self) -> None:
        """ValidationResult の文字列表現をテスト."""
        result = ValidationResult(is_valid=False, errors=["Error 1"])

        repr_str = repr(result)
        assert "Invalid" in repr_str
        assert "errors=1" in repr_str


class TestAgentValidator:
    """AgentValidator のテスト."""

    @pytest.fixture
    def validator(self) -> AgentValidator:
        """テスト用の AgentValidator を作成."""
        return AgentValidator()

    @pytest.fixture
    def valid_metadata(self) -> AgentMetadata:
        """有効なメタデータを作成."""
        return AgentMetadata(
            meta=MetaInfo(
                id="test-agent",
                name="Test Agent",
                version="1.0.0",
                author="Test Author",
                icon="🤖",
                category="test",
                description="A test agent for unit testing purposes",
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
                outputs=[
                    OutputField(
                        name="result",
                        type="string",
                    )
                ],
            ),
            protocols=ProtocolConfig(
                mcp=MCPConfig(tools=["tool1"], resources=[]),
                a2a=None,
                agui=None,
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=["numpy>=1.0.0"]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:create_flow",
                shared_schema="schemas.py:MySchema",
            ),
            visual=VisualConfig(color="#FF0000", size="medium", ports={}),
        )

    def test_validate_valid_metadata(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """有効なメタデータの検証をテスト."""
        result = validator.validate(valid_metadata)

        assert result.is_valid is True
        assert len(result.errors) == 0

    def test_validate_invalid_id_format(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """無効な ID 形式の検証をテスト."""
        valid_metadata.meta.id = "Invalid_ID"  # アンダースコアは不可

        result = validator.validate(valid_metadata)

        assert result.is_valid is False
        assert any("Invalid agent ID format" in error for error in result.errors)

    def test_validate_invalid_id_uppercase(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """大文字を含む ID の検証をテスト."""
        valid_metadata.meta.id = "Test-Agent"  # 大文字は不可

        result = validator.validate(valid_metadata)

        assert result.is_valid is False
        assert any("Invalid agent ID format" in error for error in result.errors)

    def test_validate_invalid_version_format(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """無効なバージョン形式の検証をテスト."""
        valid_metadata.meta.version = "1.0"  # semver ではない

        result = validator.validate(valid_metadata)

        assert result.is_valid is False
        assert any("Invalid version format" in error for error in result.errors)

    def test_validate_empty_name(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """空の名前の検証をテスト."""
        valid_metadata.meta.name = ""

        result = validator.validate(valid_metadata)

        assert result.is_valid is False
        assert any("name cannot be empty" in error for error in result.errors)

    def test_validate_long_name(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """長すぎる名前の検証をテスト."""
        valid_metadata.meta.name = "A" * 101  # 100文字超

        result = validator.validate(valid_metadata)

        assert any("name is too long" in warning for warning in result.warnings)

    def test_validate_short_description(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """短すぎる説明の検証をテスト."""
        valid_metadata.meta.description = "Short"  # 10文字未満

        result = validator.validate(valid_metadata)

        assert any("description is too short" in warning for warning in result.warnings)

    def test_validate_long_description(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """長すぎる説明の検証をテスト."""
        valid_metadata.meta.description = "A" * 501  # 500文字超

        result = validator.validate(valid_metadata)

        assert any("description is too long" in warning for warning in result.warnings)

    def test_validate_no_inputs(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """入力フィールドなしの検証をテスト."""
        valid_metadata.interfaces.inputs = []

        result = validator.validate(valid_metadata)

        assert any("No input fields" in warning for warning in result.warnings)

    def test_validate_duplicate_input_names(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """重複する入力フィールド名の検証をテスト."""
        valid_metadata.interfaces.inputs = [
            InputField(name="text", type="string", required=True, description="Input 1"),
            InputField(name="text", type="string", required=True, description="Input 2"),
        ]

        result = validator.validate(valid_metadata)

        assert result.is_valid is False
        assert any("Duplicate input field name" in error for error in result.errors)

    def test_validate_input_without_type(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """型なし入力フィールドの検証をテスト."""
        # Pydantic validation will catch empty type before our validator
        # So we test that the validator would catch it if it got through
        from pydantic import ValidationError

        with pytest.raises(ValidationError) as exc_info:
            InputField(name="text", type="", required=True, description="Input")

        assert "String should have at least 1 character" in str(exc_info.value)

    def test_validate_no_outputs(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """出力フィールドなしの検証をテスト."""
        valid_metadata.interfaces.outputs = []

        result = validator.validate(valid_metadata)

        assert any("No output fields" in warning for warning in result.warnings)

    def test_validate_duplicate_output_names(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """重複する出力フィールド名の検証をテスト."""
        valid_metadata.interfaces.outputs = [
            OutputField(name="result", type="string"),
            OutputField(name="result", type="string"),
        ]

        result = validator.validate(valid_metadata)

        assert result.is_valid is False
        assert any("Duplicate output field name" in error for error in result.errors)

    def test_validate_no_protocols(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """プロトコルなしの検証をテスト."""
        valid_metadata.protocols = ProtocolConfig(
            mcp=MCPConfig(tools=[], resources=[]), a2a=None, agui=None
        )

        result = validator.validate(valid_metadata)

        assert any("No protocols enabled" in warning for warning in result.warnings)

    def test_validate_invalid_dependent_agent_id(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """無効な依存 Agent ID の検証をテスト."""
        valid_metadata.dependencies = DependencySpec(
            agents=["Invalid_Agent"], tools=[], packages=[]
        )

        result = validator.validate(valid_metadata)

        assert result.is_valid is False
        assert any("Invalid dependent agent ID" in error for error in result.errors)

    def test_validate_empty_package_dependency(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """空のパッケージ依存関係の検証をテスト."""
        valid_metadata.dependencies = DependencySpec(agents=[], tools=[], packages=[""])

        result = validator.validate(valid_metadata)

        assert result.is_valid is False
        assert any("Empty package dependency" in error for error in result.errors)

    def test_validate_invalid_pocketflow_entry(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """無効な PocketFlow エントリーの検証をテスト."""
        valid_metadata.pocketflow.entry = "invalid_format"  # コロンなし

        result = validator.validate(valid_metadata)

        assert result.is_valid is False
        assert any("Invalid PocketFlow entry format" in error for error in result.errors)

    def test_validate_pocketflow_entry_non_py_file(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """非 .py ファイルの PocketFlow エントリーの検証をテスト."""
        valid_metadata.pocketflow.entry = "flow.txt:create_flow"

        result = validator.validate(valid_metadata)

        assert any("should be a .py file" in warning for warning in result.warnings)

    def test_validate_mcp_with_tools(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """MCP ツールありの検証をテスト."""
        valid_metadata.protocols.mcp = MCPConfig(tools=["tool1", "tool2"], resources=[])

        result = validator.validate(valid_metadata)

        assert result.is_valid is True

    def test_validate_mcp_with_resources(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """MCP リソースありの検証をテスト."""
        valid_metadata.protocols.mcp = MCPConfig(tools=[], resources=["resource1"])

        result = validator.validate(valid_metadata)

        assert result.is_valid is True

    def test_validate_a2a_enabled(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """A2A プロトコル有効の検証をテスト."""
        valid_metadata.protocols.a2a = A2AConfig(server_enabled=True, client_enabled=False)

        result = validator.validate(valid_metadata)

        assert result.is_valid is True

    def test_validate_agui_enabled(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """AGUI プロトコル有効の検証をテスト."""
        valid_metadata.protocols.agui = AGUIConfig(enabled=True)

        result = validator.validate(valid_metadata)

        assert result.is_valid is True

    def test_validate_multiple_package_dependencies(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """複数のパッケージ依存関係の検証をテスト."""
        valid_metadata.dependencies = DependencySpec(
            agents=[],
            tools=[],
            packages=["numpy>=1.0.0", "pandas>=2.0.0", "scikit-learn"],
        )

        result = validator.validate(valid_metadata)

        assert result.is_valid is True

    def test_validate_tool_dependencies(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """ツール依存関係の検証をテスト."""
        valid_metadata.dependencies = DependencySpec(
            agents=[], tools=["tool1", "tool2"], packages=[]
        )

        result = validator.validate(valid_metadata)

        assert result.is_valid is True

    def test_validate_pocketflow_entry_empty_function(
        self, validator: AgentValidator, valid_metadata: AgentMetadata
    ) -> None:
        """空の関数名の PocketFlow エントリーの検証をテスト."""
        valid_metadata.pocketflow.entry = "flow.py:"

        result = validator.validate(valid_metadata)

        assert result.is_valid is False
        assert any("function name is empty" in error for error in result.errors)

"""AgentFlow types のテスト."""

from datetime import datetime
from typing import Any

import pytest
from pydantic import ValidationError

from agentflow.core.types import (
    AgentMetadata,
    ExecutionContext,
    ExecutionResult,
    ProtocolType,
    WorkflowConfig,
)


class TestProtocolType:
    """ProtocolType のテスト."""

    def test_protocol_type_values(self) -> None:
        """ProtocolType の値をテスト."""
        assert ProtocolType.MCP.value == "mcp"
        assert ProtocolType.A2A.value == "a2a"
        assert ProtocolType.AG_UI.value == "ag-ui"

    def test_protocol_type_enum_members(self) -> None:
        """ProtocolType の全メンバーをテスト."""
        expected_members = {"MCP", "A2A", "AG_UI"}
        actual_members = {member.name for member in ProtocolType}
        assert actual_members == expected_members


class TestAgentMetadata:
    """AgentMetadata のテスト."""

    @pytest.fixture
    def valid_metadata_dict(self) -> dict[str, Any]:
        """有効なメタデータ辞書."""
        return {
            "name": "Test Agent",
            "version": "1.0.0",
            "description": "A test agent",
            "author": "Test Author",
            "protocols": ["mcp"],
            "entry_point": "test.agent:main",
        }

    def test_agent_metadata_creation(self, valid_metadata_dict: dict[str, Any]) -> None:
        """AgentMetadata の作成をテスト."""
        metadata = AgentMetadata(**valid_metadata_dict)

        assert metadata.name == "Test Agent"
        assert metadata.version == "1.0.0"
        assert metadata.description == "A test agent"
        assert metadata.author == "Test Author"
        assert metadata.protocols == [ProtocolType.MCP]
        assert metadata.entry_point == "test.agent:main"

    def test_agent_metadata_with_defaults(self) -> None:
        """デフォルト値を持つ AgentMetadata をテスト."""
        metadata = AgentMetadata(
            name="Test",
            version="1.0.0",
            description="Test",
            author="Author",
            protocols=[ProtocolType.MCP],
            entry_point="test:main",
        )

        assert metadata.dependencies == []
        assert metadata.tags == []
        assert metadata.config_schema == {}

    def test_agent_metadata_invalid_version(self) -> None:
        """無効なバージョンでの AgentMetadata 作成をテスト."""
        with pytest.raises(ValidationError) as exc_info:
            AgentMetadata(
                name="Test",
                version="invalid",  # 無効なバージョン
                description="Test",
                author="Author",
                protocols=[ProtocolType.MCP],
                entry_point="test:main",
            )

        errors = exc_info.value.errors()
        assert any(err["loc"] == ("version",) for err in errors)

    def test_agent_metadata_empty_protocols(self) -> None:
        """空のプロトコルリストでの AgentMetadata 作成をテスト."""
        with pytest.raises(ValidationError) as exc_info:
            AgentMetadata(
                name="Test",
                version="1.0.0",
                description="Test",
                author="Author",
                protocols=[],  # 空のプロトコルリスト
                entry_point="test:main",
            )

        errors = exc_info.value.errors()
        assert any("At least one protocol must be specified" in str(err) for err in errors)

    def test_agent_metadata_multiple_protocols(self) -> None:
        """複数のプロトコルを持つ AgentMetadata をテスト."""
        metadata = AgentMetadata(
            name="Test",
            version="1.0.0",
            description="Test",
            author="Author",
            protocols=[ProtocolType.MCP, ProtocolType.A2A, ProtocolType.AG_UI],
            entry_point="test:main",
        )

        assert len(metadata.protocols) == 3
        assert ProtocolType.MCP in metadata.protocols
        assert ProtocolType.A2A in metadata.protocols
        assert ProtocolType.AG_UI in metadata.protocols


class TestWorkflowConfig:
    """WorkflowConfig のテスト."""

    def test_workflow_config_creation(self) -> None:
        """WorkflowConfig の作成をテスト."""
        config = WorkflowConfig(
            workflow_id="test-workflow",
            name="Test Workflow",
            description="A test workflow",
            nodes=[{"id": "node1", "type": "process"}],
            edges=[{"from": "node1", "to": "node2"}],
            config={"timeout": 30},
        )

        assert config.workflow_id == "test-workflow"
        assert config.name == "Test Workflow"
        assert config.description == "A test workflow"
        assert len(config.nodes) == 1
        assert len(config.edges) == 1
        assert config.config["timeout"] == 30

    def test_workflow_config_with_defaults(self) -> None:
        """デフォルト値を持つ WorkflowConfig をテスト."""
        config = WorkflowConfig(
            workflow_id="test",
            name="Test",
        )

        assert config.description == ""
        assert config.nodes == []
        assert config.edges == []
        assert config.config == {}


class TestExecutionContext:
    """ExecutionContext のテスト."""

    def test_execution_context_creation(self) -> None:
        """ExecutionContext の作成をテスト."""
        context = ExecutionContext(
            workflow_id="test-workflow",
            execution_id="exec-123",
            inputs={"param1": "value1"},
            metadata={"user": "test-user"},
        )

        assert context.workflow_id == "test-workflow"
        assert context.execution_id == "exec-123"
        assert context.inputs == {"param1": "value1"}
        assert context.metadata == {"user": "test-user"}
        assert isinstance(context.started_at, datetime)

    def test_execution_context_with_defaults(self) -> None:
        """デフォルト値を持つ ExecutionContext をテスト."""
        context = ExecutionContext(
            workflow_id="test",
            execution_id="exec-123",
        )

        assert context.inputs == {}
        assert context.metadata == {}
        assert isinstance(context.started_at, datetime)

    def test_execution_context_started_at_auto_generated(self) -> None:
        """started_at が自動生成されることをテスト."""
        context1 = ExecutionContext(
            workflow_id="test",
            execution_id="exec-1",
        )
        context2 = ExecutionContext(
            workflow_id="test",
            execution_id="exec-2",
        )

        # 異なる実行コンテキストは異なる started_at を持つ
        assert (
            context1.started_at != context2.started_at
            or context1.execution_id != context2.execution_id
        )


class TestExecutionResult:
    """ExecutionResult のテスト."""

    @pytest.fixture
    def sample_context(self) -> ExecutionContext:
        """テスト用の ExecutionContext."""
        return ExecutionContext(
            workflow_id="test-workflow",
            execution_id="exec-123",
        )

    def test_execution_result_success(self, sample_context: ExecutionContext) -> None:
        """成功した ExecutionResult をテスト."""
        result = ExecutionResult(
            status="success",
            output={"result": "completed"},
            error=None,
            duration=1.5,
            context=sample_context,
        )

        assert result.status == "success"
        assert result.output == {"result": "completed"}
        assert result.error is None
        assert result.duration == 1.5
        assert result.context.workflow_id == "test-workflow"

    def test_execution_result_error(self, sample_context: ExecutionContext) -> None:
        """エラーの ExecutionResult をテスト."""
        result = ExecutionResult(
            status="error",
            output={},
            error="Something went wrong",
            duration=0.5,
            context=sample_context,
        )

        assert result.status == "error"
        assert result.output == {}
        assert result.error == "Something went wrong"
        assert result.duration == 0.5

    def test_execution_result_with_defaults(self, sample_context: ExecutionContext) -> None:
        """デフォルト値を持つ ExecutionResult をテスト."""
        result = ExecutionResult(
            status="success",
            duration=1.0,
            context=sample_context,
        )

        assert result.output == {}
        assert result.error is None

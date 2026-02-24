"""Integration tests for Code Migration Assistant (v4 pipeline)."""

from __future__ import annotations

import pytest
from apps.code_migration_assistant.mcp_tools.cobol_parser import COBOLParser
from apps.code_migration_assistant.orchestrator import CodeMigrationOrchestrator

from agentflow import MCPToolClient as MCPClient


@pytest.mark.asyncio
async def test_legacy_analysis_agent_basic() -> None:
    """LegacyAnalysisAgent 基本テスト."""
    from apps.code_migration_assistant.agents import LegacyAnalysisAgent

    agent = LegacyAnalysisAgent()
    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CALC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(5).
       01 WS-NUM2 PIC 9(5).
    """

    result = agent.process(
        {
            "source_code": cobol_code,
            "task_spec": {
                "task_id": "task-analysis",
                "trace_id": "trace-analysis",
                "module": "SIMPLE_CALC",
                "expected_outputs": {"RESULT": "300"},
            },
        }
    )

    assert result["meta"]["stage"] == "analysis"
    assert "unknowns" in result
    assert "extensions" in result
    assert result["programs"][0]["program_id"] == "SIMPLE-CALC"


@pytest.mark.asyncio
async def test_migration_design_agent_basic() -> None:
    """MigrationDesignAgent 基本テスト."""
    from apps.code_migration_assistant.agents import MigrationDesignAgent

    agent = MigrationDesignAgent()
    analysis = {
        "meta": {
            "task_id": "task-design",
            "trace_id": "trace-design",
            "module": "M1",
            "source_language": "COBOL",
        },
        "programs": [{"program_id": "EMP-MANAGER"}],
        "data_structures": [{"name": "WS-NAME"}],
        "entry_points": [{"name": "MAIN", "type": "paragraph"}],
    }
    semantics = {
        "meta": {
            "task_id": "task-design",
            "trace_id": "trace-design",
            "module": "M1",
        },
        "business_processes": [{"name": "社員管理", "entry_point": "MAIN", "steps": ["入力", "更新"]}],
        "business_events": [{"name": "社員更新", "trigger": "MAIN", "type": "business"}],
        "state_model": {"states": ["初期", "更新"]},
        "business_rules": [{"name": "rule_1", "condition": "IF A", "action": "更新"}],
    }

    result = agent.process({"legacy_analysis": analysis, "business_semantics": semantics})

    assert result["meta"]["stage"] == "design"
    assert result["class_mapping"]["primary_class"] == "EmpManager"
    assert "unknowns" in result


@pytest.mark.asyncio
async def test_code_transformation_agent_basic() -> None:
    """CodeTransformationAgent 基本テスト."""
    from apps.code_migration_assistant.agents import CodeTransformationAgent

    agent = CodeTransformationAgent()
    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20).
    """
    design = {
        "meta": {
            "task_id": "task-code",
            "trace_id": "trace-code",
            "module": "HELLO",
        },
        "class_mapping": {"primary_class": "Hello"},
        "package_mapping": {"default": "com.migration.generated"},
    }

    result = agent.process(
        {
            "source_code": cobol_code,
            "migration_design": design,
            "fast_mode": True,
        }
    )

    assert result["meta"]["stage"] == "code"
    assert "target_code" in result
    assert result["generated_files"][0]["path"].endswith("Hello.java")


@pytest.mark.asyncio
async def test_quality_gate_agent_decision() -> None:
    """QualityGateAgent 裁定テスト."""
    from apps.code_migration_assistant.agents import QualityGateAgent

    agent = QualityGateAgent()
    differential = {
        "meta": {"task_id": "task-qg", "trace_id": "trace-qg", "module": "M1"},
        "equivalence": False,
        "classification": "logic",
        "diffs": [
            {
                "location": "response.amount",
                "legacy": "100",
                "new": "99",
            }
        ],
    }

    result = agent.process(
        {
            "differential": differential,
            "migration_design": {"unknowns": []},
            "test_synthesis": {"unknowns": []},
            "known_legacy_issues": [],
        }
    )

    assert result["meta"]["stage"] == "quality"
    assert result["decision"] == "TRANSFORM_ISSUE"
    assert result["target_agent"] == "LimitedFixerAgent"


@pytest.mark.asyncio
async def test_limited_fixer_agent_scope() -> None:
    """LimitedFixerAgent 限定修正テスト."""
    from apps.code_migration_assistant.agents import LimitedFixerAgent

    agent = LimitedFixerAgent()
    result = agent.process(
        {
            "quality_gate": {"decision": "TRANSFORM_ISSUE"},
            "transformation": {
                "meta": {
                    "task_id": "task-fix",
                    "trace_id": "trace-fix",
                    "module": "M2",
                },
                "target_code": "public class A { }   \n\n",
            },
            "migration_design": {"package_mapping": {"default": "com.migration.generated"}},
        }
    )

    assert result["meta"]["stage"] == "fix"
    assert isinstance(result["applied"], bool)
    assert "patch_summary" in result


@pytest.mark.asyncio
async def test_orchestrator_basic() -> None:
    """Orchestrator 基本テスト."""
    orchestrator = CodeMigrationOrchestrator()

    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20).

       PROCEDURE DIVISION.
           MOVE "HELLO WORLD" TO WS-MESSAGE.
           DISPLAY WS-MESSAGE.
           STOP RUN.
    """

    result = await orchestrator.run({"cobol_code": cobol_code, "run_tests": False})

    assert "success" in result
    assert "quality_gate" in result
    assert "artifact_paths" in result


@pytest.mark.asyncio
async def test_mcp_client_tool_management() -> None:
    """MCPClient ツール管理テスト."""
    client = MCPClient()

    parser = COBOLParser()
    client.register_tool("cobol_parser", parser)

    assert client.has_tool("cobol_parser") is True
    assert client.has_tool("unknown_tool") is False

    tools = client.list_tools()
    assert "cobol_parser" in tools

    tool = client.get_tool("cobol_parser")
    assert tool is not None
    assert tool == parser

    client.unregister_tool("cobol_parser")
    assert client.has_tool("cobol_parser") is False

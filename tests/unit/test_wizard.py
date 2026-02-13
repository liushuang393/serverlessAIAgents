# -*- coding: utf-8 -*-
"""Unit tests for the AgentWizard module.

Tests cover:
- Data models (AgentSpec, CapabilityGap, TestCase, etc.)
- GapAnalysis functionality
- TestSuiteResult metrics
- Serialization/deserialization
"""

import pytest
from datetime import datetime, UTC

from agentflow.wizard.models import (
    AgentSpec,
    CapabilityGap,
    EngineType,
    GapAnalysis,
    GapType,
    TestCase,
    TestResult,
    TestSuiteResult,
    ValidationResult,
    ValidationStatus,
    WizardConfig,
)


class TestWizardConfig:
    """Tests for WizardConfig dataclass."""

    def test_default_config(self) -> None:
        """Test default configuration values."""
        config = WizardConfig()
        assert config.auto_test is True
        assert config.auto_publish is False
        assert config.min_confidence == 0.7
        assert config.max_retries == 3
        assert config.enable_learning is True
        assert config.test_coverage_threshold == 0.8
        assert config.quality_threshold == 0.7

    def test_custom_config(self) -> None:
        """Test custom configuration."""
        config = WizardConfig(
            auto_test=False,
            auto_publish=True,
            min_confidence=0.9,
            max_retries=5,
        )
        assert config.auto_test is False
        assert config.auto_publish is True
        assert config.min_confidence == 0.9
        assert config.max_retries == 5


class TestAgentSpec:
    """Tests for AgentSpec dataclass."""

    def test_agent_spec_creation(self) -> None:
        """Test creating an AgentSpec."""
        spec = AgentSpec(
            name="PDFAnalyzer",
            description="Analyzes PDF documents and extracts key information",
            capabilities=["pdf_parsing", "text_extraction", "summarization"],
            required_skills=["pdf_skill", "summarize_skill"],
            required_tools=["pdf_reader"],
            system_prompt="You are a PDF analysis assistant.",
            engine_type=EngineType.PIPELINE,
            confidence=0.85,
        )
        assert spec.name == "PDFAnalyzer"
        assert spec.description == "Analyzes PDF documents and extracts key information"
        assert "pdf_parsing" in spec.capabilities
        assert spec.engine_type == EngineType.PIPELINE
        assert spec.confidence == 0.85

    def test_agent_spec_to_dict(self) -> None:
        """Test serialization to dict."""
        spec = AgentSpec(
            name="TestAgent",
            description="Test description",
            engine_type=EngineType.SIMPLE,
            confidence=0.7,
        )
        data = spec.to_dict()
        assert data["name"] == "TestAgent"
        assert data["description"] == "Test description"
        assert data["engine_type"] == "simple"
        assert data["confidence"] == 0.7

    def test_agent_spec_from_dict(self) -> None:
        """Test deserialization from dict."""
        data = {
            "name": "FromDict",
            "description": "Created from dict",
            "capabilities": ["cap1", "cap2"],
            "engine_type": "pipeline",
            "confidence": 0.9,
        }
        spec = AgentSpec.from_dict(data)
        assert spec.name == "FromDict"
        assert spec.engine_type == EngineType.PIPELINE
        assert spec.confidence == 0.9
        assert len(spec.capabilities) == 2

    def test_agent_spec_roundtrip(self) -> None:
        """Test roundtrip serialization."""
        original = AgentSpec(
            name="RoundTrip",
            description="Test roundtrip",
            capabilities=["a", "b"],
            required_skills=["skill1"],
            engine_type=EngineType.RAG,
            confidence=0.75,
        )
        data = original.to_dict()
        restored = AgentSpec.from_dict(data)
        assert restored.name == original.name
        assert restored.capabilities == original.capabilities
        assert restored.engine_type == original.engine_type


class TestCapabilityGap:
    """Tests for CapabilityGap dataclass."""

    def test_capability_gap_creation(self) -> None:
        """Test creating a CapabilityGap."""
        gap = CapabilityGap(
            gap_id="gap-001",
            gap_type=GapType.SKILL,
            description="Missing PDF parsing skill",
            frequency=5,
            severity=0.8,
            suggested_solution="Install pdf_skill from marketplace",
            auto_fixable=True,
            source="skill_engine",
        )
        assert gap.gap_id == "gap-001"
        assert gap.gap_type == GapType.SKILL
        assert gap.frequency == 5
        assert gap.severity == 0.8
        assert gap.auto_fixable is True

    def test_capability_gap_defaults(self) -> None:
        """Test default values for CapabilityGap."""
        gap = CapabilityGap(
            gap_id="gap-002",
            gap_type=GapType.TOOL,
            description="Missing tool",
        )
        assert gap.frequency == 1
        assert gap.severity == 0.5
        assert gap.auto_fixable is False
        assert gap.source == ""

    def test_capability_gap_to_dict(self) -> None:
        """Test serialization of CapabilityGap."""
        gap = CapabilityGap(
            gap_id="gap-003",
            gap_type=GapType.AGENT,
            description="Missing coordinator agent",
            severity=0.9,
        )
        data = gap.to_dict()
        assert data["gap_id"] == "gap-003"
        assert data["gap_type"] == "agent"
        assert data["severity"] == 0.9
        assert "detected_at" in data


class TestGapAnalysis:
    """Tests for GapAnalysis dataclass."""

    def test_gap_analysis_empty(self) -> None:
        """Test empty GapAnalysis."""
        analysis = GapAnalysis()
        assert len(analysis.gaps) == 0
        assert analysis.total_count == 0
        assert analysis.auto_fixable_count == 0

    def test_gap_analysis_with_gaps(self) -> None:
        """Test GapAnalysis with gaps."""
        gaps = [
            CapabilityGap(
                gap_id="g1", gap_type=GapType.SKILL, description="Gap 1", auto_fixable=True
            ),
            CapabilityGap(
                gap_id="g2", gap_type=GapType.TOOL, description="Gap 2", auto_fixable=False
            ),
            CapabilityGap(
                gap_id="g3", gap_type=GapType.SKILL, description="Gap 3", auto_fixable=True
            ),
        ]
        analysis = GapAnalysis(
            gaps=gaps,
            total_count=3,
            auto_fixable_count=2,
            recommendations=["Install missing skills"],
        )
        assert len(analysis.gaps) == 3
        assert analysis.total_count == 3
        assert analysis.auto_fixable_count == 2
        assert len(analysis.recommendations) == 1

    def test_gap_analysis_to_dict(self) -> None:
        """Test GapAnalysis serialization."""
        gap = CapabilityGap(gap_id="g1", gap_type=GapType.PATTERN, description="Test")
        analysis = GapAnalysis(
            gaps=[gap],
            total_count=1,
            priority_gaps=[gap],
        )
        data = analysis.to_dict()
        assert data["total_count"] == 1
        assert len(data["gaps"]) == 1
        assert len(data["priority_gaps"]) == 1


class TestTestCase:
    """Tests for TestCase dataclass."""

    def test_test_case_creation(self) -> None:
        """Test creating a TestCase."""
        test_case = TestCase(
            name="test_pdf_parsing",
            description="Test PDF parsing functionality",
            input_data={"file_path": "/tmp/test.pdf"},
            expected_output={"status": "success", "pages": 5},
            assertions=["output.status == 'success'", "output.pages > 0"],
            tags=["pdf", "unit"],
            timeout_seconds=60,
        )
        assert test_case.name == "test_pdf_parsing"
        assert test_case.timeout_seconds == 60
        assert len(test_case.assertions) == 2
        assert "pdf" in test_case.tags

    def test_test_case_defaults(self) -> None:
        """Test default values for TestCase."""
        test_case = TestCase(name="simple_test")
        assert test_case.description == ""
        assert test_case.input_data == {}
        assert test_case.expected_output == {}
        assert test_case.timeout_seconds == 30


class TestTestResult:
    """Tests for TestResult dataclass."""

    def test_test_result_passed(self) -> None:
        """Test a passing TestResult."""
        result = TestResult(
            test_name="test_success",
            passed=True,
            duration_ms=150.5,
            output={"result": "ok"},
            assertions_passed=3,
            assertions_failed=0,
        )
        assert result.passed is True
        assert result.error is None
        assert result.assertions_passed == 3
        assert result.assertions_failed == 0

    def test_test_result_failed(self) -> None:
        """Test a failing TestResult."""
        result = TestResult(
            test_name="test_failure",
            passed=False,
            duration_ms=50.0,
            error="AssertionError: Expected 5, got 3",
            assertions_passed=2,
            assertions_failed=1,
        )
        assert result.passed is False
        assert result.error is not None
        assert "AssertionError" in result.error

    def test_test_result_to_dict(self) -> None:
        """Test TestResult serialization."""
        result = TestResult(
            test_name="test_serialize",
            passed=True,
            duration_ms=100.0,
        )
        data = result.to_dict()
        assert data["test_name"] == "test_serialize"
        assert data["passed"] is True
        assert data["duration_ms"] == 100.0


class TestTestSuiteResult:
    """Tests for TestSuiteResult dataclass."""

    def test_test_suite_empty(self) -> None:
        """Test empty TestSuiteResult."""
        suite = TestSuiteResult()
        assert suite.total_tests == 0
        assert suite.success_rate == 0.0
        assert suite.all_passed is False

    def test_test_suite_all_passed(self) -> None:
        """Test TestSuiteResult with all tests passing."""
        results = [
            TestResult(test_name="test1", passed=True),
            TestResult(test_name="test2", passed=True),
            TestResult(test_name="test3", passed=True),
        ]
        suite = TestSuiteResult(
            total_tests=3,
            passed_tests=3,
            failed_tests=0,
            results=results,
            coverage=0.95,
        )
        assert suite.success_rate == 1.0
        assert suite.all_passed is True

    def test_test_suite_partial_failure(self) -> None:
        """Test TestSuiteResult with some failures."""
        results = [
            TestResult(test_name="test1", passed=True),
            TestResult(test_name="test2", passed=False),
            TestResult(test_name="test3", passed=True),
        ]
        suite = TestSuiteResult(
            total_tests=3,
            passed_tests=2,
            failed_tests=1,
            results=results,
        )
        assert suite.success_rate == pytest.approx(0.666, rel=0.01)
        assert suite.all_passed is False

    def test_test_suite_to_dict(self) -> None:
        """Test TestSuiteResult serialization."""
        suite = TestSuiteResult(
            total_tests=5,
            passed_tests=4,
            failed_tests=1,
            coverage=0.88,
            duration_ms=500.0,
        )
        data = suite.to_dict()
        assert data["total_tests"] == 5
        assert data["passed_tests"] == 4
        assert data["coverage"] == 0.88


class TestValidationResult:
    """Tests for ValidationResult dataclass."""

    def test_validation_passed(self) -> None:
        """Test a passing validation."""
        result = ValidationResult(
            valid=True,
            status=ValidationStatus.PASSED,
            score=0.95,
            errors=[],
            warnings=[],
            suggestions=["All checks passed"],
        )
        assert result.valid is True
        assert result.status == ValidationStatus.PASSED
        assert result.score == 0.95
        assert len(result.errors) == 0

    def test_validation_failed(self) -> None:
        """Test a failing validation."""
        result = ValidationResult(
            valid=False,
            status=ValidationStatus.FAILED,
            score=0.45,
            errors=["Quality score below threshold", "Missing required tests"],
            warnings=[],
        )
        assert result.valid is False
        assert result.status == ValidationStatus.FAILED
        assert result.score == 0.45
        assert len(result.errors) == 2

    def test_validation_warning(self) -> None:
        """Test a warning validation."""
        result = ValidationResult(
            valid=True,
            status=ValidationStatus.WARNING,
            score=0.75,
            warnings=["Coverage below recommended threshold"],
        )
        assert result.status == ValidationStatus.WARNING
        assert len(result.warnings) == 1

    def test_validation_to_dict(self) -> None:
        """Test ValidationResult serialization."""
        result = ValidationResult(
            valid=True,
            status=ValidationStatus.PASSED,
            score=0.9,
        )
        data = result.to_dict()
        assert data["valid"] is True
        assert data["status"] == "passed"
        assert data["score"] == 0.9


class TestEngineType:
    """Tests for EngineType enum."""

    def test_engine_types(self) -> None:
        """Test all engine types exist."""
        assert EngineType.SIMPLE.value == "simple"
        assert EngineType.PIPELINE.value == "pipeline"
        assert EngineType.GATE.value == "gate"
        assert EngineType.RAG.value == "rag"

    def test_engine_type_from_string(self) -> None:
        """Test creating EngineType from string."""
        assert EngineType("simple") == EngineType.SIMPLE
        assert EngineType("pipeline") == EngineType.PIPELINE


class TestGapType:
    """Tests for GapType enum."""

    def test_gap_types(self) -> None:
        """Test all gap types exist."""
        assert GapType.SKILL.value == "skill"
        assert GapType.TOOL.value == "tool"
        assert GapType.AGENT.value == "agent"
        assert GapType.PATTERN.value == "pattern"
        assert GapType.KNOWLEDGE.value == "knowledge"

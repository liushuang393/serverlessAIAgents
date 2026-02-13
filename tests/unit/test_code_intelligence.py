# -*- coding: utf-8 -*-
"""Unit tests for code_intelligence module."""

import pytest
import asyncio
from pathlib import Path


class TestUnifiedAST:
    """Tests for UnifiedAST module."""

    def test_ast_node_creation(self) -> None:
        """Test ASTNode creation."""
        from agentflow.code_intelligence.ast.unified_ast import (
            ASTNode,
            ASTNodeType,
        )

        node = ASTNode(
            type=ASTNodeType.PROGRAM,
            name="TestProgram",
        )
        assert node.name == "TestProgram"
        assert node.type == ASTNodeType.PROGRAM
        assert len(node.children) == 0

    def test_ast_node_add_child(self) -> None:
        """Test adding child nodes."""
        from agentflow.code_intelligence.ast.unified_ast import (
            ASTNode,
            ASTNodeType,
        )

        parent = ASTNode(type=ASTNodeType.PROGRAM, name="Parent")
        child = ASTNode(type=ASTNodeType.FUNCTION, name="Child")
        parent.add_child(child)

        assert len(parent.children) == 1
        assert child.parent == parent

    def test_unified_ast_creation(self) -> None:
        """Test UnifiedAST creation."""
        from agentflow.code_intelligence.ast.unified_ast import (
            UnifiedAST,
            ASTNode,
            ASTNodeType,
        )

        root = ASTNode(type=ASTNodeType.PROGRAM, name="Main")
        ast = UnifiedAST(source_language="java", root=root)

        assert ast.source_language == "java"
        assert ast.root.name == "Main"


class TestQualityGates:
    """Tests for Quality Gates module."""

    @pytest.mark.asyncio
    async def test_syntax_validator(self) -> None:
        """Test SyntaxValidator gate."""
        from agentflow.code_intelligence.quality.gates import (
            SyntaxValidator,
            QualityLevel,
        )

        validator = SyntaxValidator()
        result = await validator.validate("source", "target code", {})

        assert result.gate_name == "syntax_validator"
        assert isinstance(result.passed, bool)

    @pytest.mark.asyncio
    async def test_quality_gate_runner(self) -> None:
        """Test QualityGateRunner."""
        from agentflow.code_intelligence.quality.gates import (
            QualityGateRunner,
            SyntaxValidator,
            SemanticValidator,
        )

        runner = QualityGateRunner()
        runner.add_gate(SyntaxValidator())
        runner.add_gate(SemanticValidator())

        report = await runner.run_all("source", "target code here", {})

        assert report.score >= 0
        assert report.score <= 100
        assert len(report.gate_results) == 2

    @pytest.mark.asyncio
    async def test_empty_target_fails(self) -> None:
        """Test that empty target code fails validation."""
        from agentflow.code_intelligence.quality.gates import (
            SyntaxValidator,
            QualityLevel,
        )

        validator = SyntaxValidator()
        result = await validator.validate("source", "", {})

        assert not result.passed
        assert any(i.level == QualityLevel.CRITICAL for i in result.issues)


class TestMigrationProject:
    """Tests for Migration module."""

    def test_project_creation(self) -> None:
        """Test MigrationProject creation."""
        from agentflow.code_intelligence.migration.project import (
            MigrationProject,
            MigrationPhase,
        )

        project = MigrationProject(
            name="test-migration",
            source_language="cobol",
            target_language="java",
        )

        assert project.name == "test-migration"
        assert project.source_language == "cobol"
        assert len(project.phases) == len(MigrationPhase)

    def test_add_file(self) -> None:
        """Test adding files to project."""
        from agentflow.code_intelligence.migration.project import (
            MigrationProject,
            SourceFile,
        )

        project = MigrationProject(
            name="test",
            source_language="cobol",
            target_language="java",
        )
        file = SourceFile(path="main.cob", loc=100)
        project.add_file(file)

        assert len(project.source_files) == 1
        assert project.get_progress()["total_files"] == 1

    def test_phase_management(self) -> None:
        """Test phase start and complete."""
        from agentflow.code_intelligence.migration.project import (
            MigrationProject,
            MigrationPhase,
            PhaseStatus,
        )

        project = MigrationProject(
            name="test",
            source_language="cobol",
            target_language="java",
        )

        project.start_phase(MigrationPhase.ANALYSIS)
        record = project.get_phase_record(MigrationPhase.ANALYSIS)

        assert record is not None
        assert record.status == PhaseStatus.IN_PROGRESS
        assert project.current_phase == MigrationPhase.ANALYSIS


class TestPipelineGenerator:
    """Tests for CI/CD Pipeline Generator."""

    def test_github_pipeline(self) -> None:
        """Test GitHub Actions pipeline generation."""
        from agentflow.code_intelligence.cicd.pipeline_generator import (
            MigrationPipelineGenerator,
            PipelineConfig,
            CIPlatform,
        )

        config = PipelineConfig(
            project_name="test-migration",
            source_language="cobol",
            target_language="java",
        )
        generator = MigrationPipelineGenerator()
        yaml_content = generator.generate(config, CIPlatform.GITHUB)

        assert "test-migration" in yaml_content
        assert "cobol" in yaml_content.lower()
        assert "jobs:" in yaml_content

    def test_gitlab_pipeline(self) -> None:
        """Test GitLab CI pipeline generation."""
        from agentflow.code_intelligence.cicd.pipeline_generator import (
            MigrationPipelineGenerator,
            PipelineConfig,
            CIPlatform,
        )

        config = PipelineConfig(
            project_name="test-project",
            source_language="vb6",
            target_language="csharp",
        )
        generator = MigrationPipelineGenerator()
        yaml_content = generator.generate(config, CIPlatform.GITLAB)

        assert "stages:" in yaml_content
        assert "test-project" in yaml_content


class TestSkillsVersioning:
    """Tests for Skills Versioning module."""

    def test_skill_version_parsing(self) -> None:
        """Test SkillVersion parsing."""
        from agentflow.skills.versioning.skill_version import SkillVersion

        v = SkillVersion.parse("1.2.3")
        assert v.major == 1
        assert v.minor == 2
        assert v.patch == 3
        assert str(v) == "1.2.3"

    def test_skill_version_comparison(self) -> None:
        """Test SkillVersion comparison."""
        from agentflow.skills.versioning.skill_version import SkillVersion

        v1 = SkillVersion.parse("1.2.3")
        v2 = SkillVersion.parse("1.2.4")
        v3 = SkillVersion.parse("2.0.0")

        assert v1 < v2
        assert v2 < v3
        assert v1 < v3

    def test_skill_version_bump(self) -> None:
        """Test version bumping."""
        from agentflow.skills.versioning.skill_version import SkillVersion

        v = SkillVersion.parse("1.2.3")

        assert str(v.bump_patch()) == "1.2.4"
        assert str(v.bump_minor()) == "1.3.0"
        assert str(v.bump_major()) == "2.0.0"

    def test_diff_engine(self) -> None:
        """Test DiffEngine."""
        from agentflow.skills.versioning.diff_engine import DiffEngine

        engine = DiffEngine()
        old = "line1\nline2\nline3"
        new = "line1\nmodified\nline3\nline4"

        result = engine.diff(old, new)

        assert result.has_changes
        assert result.lines_added > 0 or result.lines_removed > 0

    def test_in_memory_version_store(self) -> None:
        """Test InMemoryVersionStore."""
        from agentflow.skills.versioning.skill_version import (
            SkillVersion,
            SkillSnapshot,
        )
        from agentflow.skills.versioning.version_store import InMemoryVersionStore

        store = InMemoryVersionStore()
        snapshot = SkillSnapshot(
            skill_name="test-skill",
            version=SkillVersion(1, 0, 0),
            content="def hello(): pass",
        )

        store.save(snapshot)
        loaded = store.load("test-skill", "1.0.0")

        assert loaded is not None
        assert loaded.skill_name == "test-skill"
        assert loaded.content == "def hello(): pass"

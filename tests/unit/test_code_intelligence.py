"""Unit tests for code_intelligence module."""

import pytest


class TestUnifiedAST:
    """Tests for UnifiedAST module."""

    def test_ast_node_creation(self) -> None:
        """Test ASTNode creation."""
        from apps.dev_studio.code_intelligence.ast.unified_ast import (
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
        from apps.dev_studio.code_intelligence.ast.unified_ast import (
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
        from apps.dev_studio.code_intelligence.ast.unified_ast import (
            ASTNode,
            ASTNodeType,
            UnifiedAST,
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
        from apps.dev_studio.code_intelligence.quality.gates import (
            SyntaxValidator,
        )

        validator = SyntaxValidator()
        result = await validator.validate("source", "target code", {})

        assert result.gate_name == "syntax_validator"
        assert isinstance(result.passed, bool)

    @pytest.mark.asyncio
    async def test_quality_gate_runner(self) -> None:
        """Test QualityGateRunner."""
        from apps.dev_studio.code_intelligence.quality.gates import (
            QualityGateRunner,
            SemanticValidator,
            SyntaxValidator,
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
        from apps.dev_studio.code_intelligence.quality.gates import (
            QualityLevel,
            SyntaxValidator,
        )

        validator = SyntaxValidator()
        result = await validator.validate("source", "", {})

        assert not result.passed
        assert any(i.level == QualityLevel.CRITICAL for i in result.issues)


class TestMigrationProject:
    """Tests for Migration module."""

    def test_project_creation(self) -> None:
        """Test MigrationProject creation."""
        from apps.dev_studio.code_intelligence.migration.project import (
            MigrationPhase,
            MigrationProject,
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
        from apps.dev_studio.code_intelligence.migration.project import (
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
        from apps.dev_studio.code_intelligence.migration.project import (
            MigrationPhase,
            MigrationProject,
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
        from apps.dev_studio.code_intelligence.cicd.pipeline_generator import (
            CIPlatform,
            MigrationPipelineGenerator,
            PipelineConfig,
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
        from apps.dev_studio.code_intelligence.cicd.pipeline_generator import (
            CIPlatform,
            MigrationPipelineGenerator,
            PipelineConfig,
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
        from kernel.skills.versioning.skill_version import SkillVersion

        v = SkillVersion.parse("1.2.3")
        assert v.major == 1
        assert v.minor == 2
        assert v.patch == 3
        assert str(v) == "1.2.3"

    def test_skill_version_comparison(self) -> None:
        """Test SkillVersion comparison."""
        from kernel.skills.versioning.skill_version import SkillVersion

        v1 = SkillVersion.parse("1.2.3")
        v2 = SkillVersion.parse("1.2.4")
        v3 = SkillVersion.parse("2.0.0")

        assert v1 < v2
        assert v2 < v3
        assert v1 < v3

    def test_skill_version_bump(self) -> None:
        """Test version bumping."""
        from kernel.skills.versioning.skill_version import SkillVersion

        v = SkillVersion.parse("1.2.3")

        assert str(v.bump_patch()) == "1.2.4"
        assert str(v.bump_minor()) == "1.3.0"
        assert str(v.bump_major()) == "2.0.0"

    def test_diff_engine(self) -> None:
        """Test DiffEngine."""
        from kernel.skills.versioning.diff_engine import DiffEngine

        engine = DiffEngine()
        old = "line1\nline2\nline3"
        new = "line1\nmodified\nline3\nline4"

        result = engine.diff(old, new)

        assert result.has_changes
        assert result.lines_added > 0 or result.lines_removed > 0

    def test_in_memory_version_store(self) -> None:
        """Test InMemoryVersionStore."""
        from kernel.skills.versioning.skill_version import (
            SkillSnapshot,
            SkillVersion,
        )
        from kernel.skills.versioning.version_store import InMemoryVersionStore

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


class TestPythonParser:
    """Tests for PythonParser."""

    def test_python_parser_file_extensions(self) -> None:
        """file_extensions プロパティが正しい値を返すか確認."""
        from apps.dev_studio.code_intelligence.parsers.modern.python_parser import PythonParser

        parser = PythonParser()
        assert ".py" in parser.file_extensions
        assert ".pyi" in parser.file_extensions

    def test_python_parser_can_parse_py(self) -> None:
        """can_parse が .py ファイルを認識するか確認."""
        from pathlib import Path

        from apps.dev_studio.code_intelligence.parsers.modern.python_parser import PythonParser

        parser = PythonParser()
        assert parser.can_parse(Path("script.py"))
        assert parser.can_parse(Path("module.pyi"))
        assert not parser.can_parse(Path("Main.java"))

    def test_python_parser_parse_class(self) -> None:
        """クラス定義を正しくパースするか確認."""
        from apps.dev_studio.code_intelligence.parsers.modern.python_parser import PythonParser

        parser = PythonParser()
        code = """
class MyClass:
    def __init__(self) -> None:
        pass

    def hello(self) -> str:
        return "hello"
"""
        result = parser.parse(code)
        assert result.success
        assert result.ast is not None
        assert "MyClass" in result.ast.symbols

    def test_python_parser_parse_function(self) -> None:
        """関数定義を正しくパースするか確認."""
        from apps.dev_studio.code_intelligence.parsers.modern.python_parser import PythonParser

        parser = PythonParser()
        code = "def add(a: int, b: int) -> int:\n    return a + b\n"
        result = parser.parse(code)
        assert result.success
        assert "add" in result.ast.symbols  # type: ignore[union-attr]
        assert result.ast.symbols["add"].kind == "function"  # type: ignore[union-attr]

    def test_python_parser_import_extraction(self) -> None:
        """インポート文を正しく抽出するか確認."""
        from apps.dev_studio.code_intelligence.parsers.modern.python_parser import PythonParser

        parser = PythonParser()
        code = "import os\nfrom pathlib import Path\n"
        result = parser.parse(code)
        assert result.success
        assert len(result.ast.imports) >= 1  # type: ignore[union-attr]

    def test_python_parser_empty_code(self) -> None:
        """空のコードでエラーを返すか確認."""
        from apps.dev_studio.code_intelligence.parsers.modern.python_parser import PythonParser

        parser = PythonParser()
        result = parser.parse("")
        assert not result.success
        assert len(result.errors) > 0

    def test_python_parser_syntax_error(self) -> None:
        """構文エラーを検出できるか確認."""
        from apps.dev_studio.code_intelligence.parsers.modern.python_parser import PythonParser

        parser = PythonParser()
        result = parser.parse("def broken(:\n    pass\n")
        assert not result.success


class TestJavaParser:
    """Tests for JavaParser."""

    def test_java_parser_file_extensions(self) -> None:
        """file_extensions プロパティが正しい値を返すか確認."""
        from apps.dev_studio.code_intelligence.parsers.modern.java_parser import JavaParser

        parser = JavaParser()
        assert ".java" in parser.file_extensions

    def test_java_parser_can_parse_java(self) -> None:
        """can_parse が .java ファイルを認識するか確認."""
        from pathlib import Path

        from apps.dev_studio.code_intelligence.parsers.modern.java_parser import JavaParser

        parser = JavaParser()
        assert parser.can_parse(Path("Main.java"))
        assert not parser.can_parse(Path("script.py"))

    def test_java_parser_parse_class(self) -> None:
        """クラス定義を正しくパースするか確認."""
        from apps.dev_studio.code_intelligence.parsers.modern.java_parser import JavaParser

        parser = JavaParser()
        code = """
package com.example;

public class HelloWorld {
    private String message;

    public void hello() {
        System.out.println(message);
    }
}
"""
        result = parser.parse(code)
        assert result.success
        assert result.ast is not None
        assert "HelloWorld" in result.ast.symbols

    def test_java_parser_import_extraction(self) -> None:
        """インポート文を正しく抽出するか確認."""
        from apps.dev_studio.code_intelligence.parsers.modern.java_parser import JavaParser

        parser = JavaParser()
        code = "import java.util.List;\nimport java.util.ArrayList;\npublic class Foo {}\n"
        result = parser.parse(code)
        assert result.success
        assert len(result.ast.imports) >= 2  # type: ignore[union-attr]

    def test_java_parser_empty_code(self) -> None:
        """空のコードでエラーを返すか確認."""
        from apps.dev_studio.code_intelligence.parsers.modern.java_parser import JavaParser

        parser = JavaParser()
        result = parser.parse("   ")
        assert not result.success


class TestCodeIntelligenceRouter:
    """Tests for the code_intelligence FastAPI router."""

    def test_list_parsers_endpoint(self) -> None:
        """GET /parsers が正しいパーサー一覧を返すか確認."""
        import asyncio

        from apps.dev_studio.code_intelligence.router import list_parsers

        result = asyncio.run(list_parsers())
        assert "parsers" in result
        languages = [p["language"] for p in result["parsers"]]
        assert "python" in languages
        assert "java" in languages

    def test_parse_endpoint_python(self) -> None:
        """POST /parse が Python コードを正しく解析するか確認."""
        import asyncio

        from apps.dev_studio.code_intelligence.router import ParseRequest, parse_code

        req = ParseRequest(
            source_code="def greet(name: str) -> str:\n    return f'Hello {name}'\n",
            language="python",
        )
        result = asyncio.run(parse_code(req))
        assert result.success
        assert result.language == "python"

    def test_parse_endpoint_unknown_language(self) -> None:
        """未サポート言語でHTTPExceptionが発生するか確認."""
        import asyncio

        from apps.dev_studio.code_intelligence.router import ParseRequest, parse_code
        from fastapi import HTTPException

        req = ParseRequest(source_code="print('hi')", language="cobol")
        with pytest.raises(HTTPException) as exc_info:
            asyncio.run(parse_code(req))
        assert exc_info.value.status_code == 400

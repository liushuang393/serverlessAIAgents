"""Skills システムのテスト.

このテストは Skill 基類、メタデータ、ローダー、および自動進化コンポーネントをテストします。
"""

from pathlib import Path
from textwrap import dedent

import pytest

from agentflow.skills import (
    Skill,
    SkillLoader,
    SkillMetadata,
    SkillRegistry,
    SkillMatcher,
    MatchResult,
    SkillValidator,
    ValidationResult,
    SkillPersister,
)


class TestSkillMetadata:
    """SkillMetadata のテスト."""

    def test_from_dict(self) -> None:
        """辞書から SkillMetadata を作成できることをテスト."""
        data = {
            "name": "test-skill",
            "description": "Test description",
            "version": "2.0.0",
            "author": "Test Author",
            "tags": ["test", "example"],
            "dependencies": ["dep1"],
        }
        metadata = SkillMetadata.from_dict(data)
        assert metadata.name == "test-skill"
        assert metadata.description == "Test description"
        assert metadata.version == "2.0.0"
        assert metadata.author == "Test Author"
        assert metadata.tags == ["test", "example"]
        assert metadata.dependencies == ["dep1"]

    def test_from_dict_with_defaults(self) -> None:
        """デフォルト値で SkillMetadata を作成できることをテスト."""
        metadata = SkillMetadata.from_dict({})
        assert metadata.name == "unknown"
        assert metadata.description == ""
        assert metadata.version == "1.0.0"
        assert metadata.tags == []

    def test_from_dict_with_extra_fields(self) -> None:
        """追加フィールドが extra に格納されることをテスト."""
        data = {"name": "test", "custom_field": "custom_value"}
        metadata = SkillMetadata.from_dict(data)
        assert metadata.extra.get("custom_field") == "custom_value"

    def test_from_dict_type_coercion(self) -> None:
        """型変換が正しく行われることをテスト."""
        data = {"name": 123, "version": 2.0, "tags": "single_tag"}
        metadata = SkillMetadata.from_dict(data)
        assert metadata.name == "123"
        assert metadata.version == "2.0"
        assert metadata.tags == ["single_tag"]

    def test_from_dict_non_dict_raises(self) -> None:
        """辞書以外の入力で TypeError が発生することをテスト."""
        with pytest.raises(TypeError, match="must be a dict"):
            SkillMetadata.from_dict("not a dict")  # type: ignore[arg-type]

    def test_from_dict_claude_code_cli_kebab_case(self) -> None:
        """Claude Code CLI kebab-case fields are parsed correctly."""
        data = {
            "name": "cli-skill",
            "allowed-tools": ["Bash", "Read", "Write"],
            "context": "fork",
            "agent": True,
            "user-invocable": True,
            "disable-model-invocation": True,
            "argument-hint": "<file-path>",
            "hooks": {"pre-tool-use": "echo checking"},
        }
        m = SkillMetadata.from_dict(data)
        assert m.allowed_tools == ["Bash", "Read", "Write"]
        assert m.context == "fork"
        assert m.agent is True
        assert m.user_invocable is True
        assert m.disable_model_invocation is True
        assert m.argument_hint == "<file-path>"
        assert m.hooks == {"pre-tool-use": "echo checking"}

    def test_from_dict_claude_code_cli_snake_case(self) -> None:
        """Claude Code CLI snake_case fields are parsed (backward compat)."""
        data = {
            "name": "cli-skill",
            "allowed_tools": ["Bash"],
            "user_invocable": True,
            "disable_model_invocation": True,
            "argument_hint": "<url>",
        }
        m = SkillMetadata.from_dict(data)
        assert m.allowed_tools == ["Bash"]
        assert m.user_invocable is True
        assert m.disable_model_invocation is True
        assert m.argument_hint == "<url>"

    def test_to_dict_claude_code_cli_kebab_output(self) -> None:
        """to_dict() emits kebab-case keys for CLI fields."""
        m = SkillMetadata(
            name="cli-skill",
            allowed_tools=["Bash", "Read"],
            context="fork",
            agent=True,
            user_invocable=True,
            disable_model_invocation=True,
            argument_hint="<path>",
            hooks={"pre-tool-use": "lint"},
        )
        d = m.to_dict()
        assert d["allowed-tools"] == ["Bash", "Read"]
        assert d["context"] == "fork"
        assert d["agent"] is True
        assert d["user-invocable"] is True
        assert d["disable-model-invocation"] is True
        assert d["argument-hint"] == "<path>"
        assert d["hooks"] == {"pre-tool-use": "lint"}

    def test_to_dict_omits_empty_cli_fields(self) -> None:
        """to_dict() omits CLI fields when they are default/empty."""
        m = SkillMetadata(name="minimal")
        d = m.to_dict()
        assert "allowed-tools" not in d
        assert "context" not in d
        assert "agent" not in d
        assert "user-invocable" not in d
        assert "hooks" not in d

    def test_cli_fields_round_trip(self) -> None:
        """from_dict(to_dict(m)) preserves Claude Code CLI field values."""
        original = SkillMetadata(
            name="round-trip",
            allowed_tools=["Bash", "Read", "Write"],
            context="fork",
            agent=True,
            user_invocable=True,
            disable_model_invocation=True,
            argument_hint="<file>",
            hooks={"post-tool-use": "test"},
        )
        restored = SkillMetadata.from_dict(original.to_dict())
        assert restored.allowed_tools == original.allowed_tools
        assert restored.context == original.context
        assert restored.agent == original.agent
        assert restored.user_invocable == original.user_invocable
        assert restored.disable_model_invocation == original.disable_model_invocation
        assert restored.argument_hint == original.argument_hint
        assert restored.hooks == original.hooks

    def test_cli_fields_defaults_when_absent(self) -> None:
        """CLI fields have correct defaults when not provided."""
        m = SkillMetadata.from_dict({"name": "no-cli"})
        assert m.allowed_tools == []
        assert m.context == ""
        assert m.agent is False
        assert m.user_invocable is False
        assert m.disable_model_invocation is False
        assert m.argument_hint == ""
        assert m.hooks == {}

    def test_cli_fields_not_in_extra(self) -> None:
        """CLI fields should NOT leak into the extra dict."""
        data = {
            "name": "test",
            "allowed-tools": ["Bash"],
            "user-invocable": True,
            "unknown-field": "value",
        }
        m = SkillMetadata.from_dict(data)
        assert "allowed-tools" not in m.extra
        assert "user-invocable" not in m.extra
        assert m.extra == {"unknown-field": "value"}


class TestSkill:
    """Skill のテスト."""

    @pytest.fixture
    def skill_dir(self, tmp_path: Path) -> Path:
        """テスト用 Skill ディレクトリを作成."""
        skill_path = tmp_path / "test-skill"
        skill_path.mkdir()
        return skill_path

    def test_load_skill_with_frontmatter(self, skill_dir: Path) -> None:
        """frontmatter 付き Skill を読み込めることをテスト."""
        skill_file = skill_dir / "SKILL.md"
        skill_file.write_text(
            dedent("""
            ---
            name: my-skill
            description: My skill description
            version: 1.2.0
            ---

            # Instructions
            Do something useful.
            """).strip(),
            encoding="utf-8",
        )

        skill = Skill.load(skill_dir)
        assert skill.name == "my-skill"
        assert skill.metadata.description == "My skill description"
        assert skill.metadata.version == "1.2.0"
        assert "Instructions" in skill.instructions

    def test_load_skill_without_frontmatter(self, skill_dir: Path) -> None:
        """frontmatter なし Skill を読み込めることをテスト."""
        skill_file = skill_dir / "SKILL.md"
        skill_file.write_text("# Just instructions\nDo this.", encoding="utf-8")

        skill = Skill.load(skill_dir)
        assert skill.name == skill_dir.name
        assert "Just instructions" in skill.instructions

    def test_load_skill_file_not_found(self, tmp_path: Path) -> None:
        """SKILL.md が見つからない場合 FileNotFoundError が発生することをテスト."""
        with pytest.raises(FileNotFoundError, match="SKILL.md not found"):
            Skill.load(tmp_path / "nonexistent")

    def test_load_skill_invalid_yaml(self, skill_dir: Path) -> None:
        """不正な YAML で ValueError が発生することをテスト."""
        skill_file = skill_dir / "SKILL.md"
        # タブとスペースの混在で無効な YAML を作成
        skill_file.write_text(
            "---\nname: test\n  bad_indent:\n\t\tmixed_tabs\n---\n",
            encoding="utf-8",
        )

        with pytest.raises(ValueError, match="Invalid YAML"):
            Skill.load(skill_dir)

    def test_skill_repr(self, skill_dir: Path) -> None:
        """__repr__ が正しく動作することをテスト."""
        skill_file = skill_dir / "SKILL.md"
        skill_file.write_text("---\nname: repr-test\nversion: 3.0.0\n---\nContent", encoding="utf-8")
        skill = Skill.load(skill_dir)
        assert "repr-test" in repr(skill)
        assert "3.0.0" in repr(skill)


class TestSkillRegistry:
    """SkillRegistry のテスト."""

    def test_singleton(self) -> None:
        """シングルトンであることをテスト."""
        r1 = SkillRegistry()
        r2 = SkillRegistry()
        assert r1 is r2

    def test_register_and_get_skill(self, tmp_path: Path) -> None:
        """Skill を登録・取得できることをテスト."""
        registry = SkillRegistry()
        registry.clear()  # 他のテストの影響をクリア

        skill_dir = tmp_path / "test-skill"
        skill_dir.mkdir()
        (skill_dir / "SKILL.md").write_text("---\nname: test\n---\nContent", encoding="utf-8")

        skill = Skill.load(skill_dir)
        registry.register("test", skill)
        assert registry.get("test") is skill


class TestSkillMatcher:
    """SkillMatcher のテスト."""

    @pytest.fixture
    def sample_skills(self, tmp_path: Path) -> list[Skill]:
        """テスト用 Skill リストを作成."""
        skills = []

        # PDF スキル
        pdf_dir = tmp_path / "pdf-extractor"
        pdf_dir.mkdir()
        (pdf_dir / "SKILL.md").write_text(
            dedent("""
            ---
            name: pdf-extractor
            description: Extract text from PDF files
            triggers:
              - pdf
              - extract text
            tags:
              - document
            ---
            # Instructions
            Use pdfplumber to extract text.
            """).strip(),
            encoding="utf-8",
        )
        skills.append(Skill.load(pdf_dir))

        # Excel スキル
        excel_dir = tmp_path / "excel-reader"
        excel_dir.mkdir()
        (excel_dir / "SKILL.md").write_text(
            dedent("""
            ---
            name: excel-reader
            description: Read Excel spreadsheets
            triggers:
              - excel
              - spreadsheet
            tags:
              - document
            ---
            # Instructions
            Use openpyxl to read Excel files.
            """).strip(),
            encoding="utf-8",
        )
        skills.append(Skill.load(excel_dir))

        return skills

    def test_match_by_trigger(self, sample_skills: list[Skill]) -> None:
        """トリガーワードでマッチすることをテスト."""
        matcher = SkillMatcher(sample_skills)
        results = matcher.match("I need to read a pdf file")

        assert len(results) > 0
        assert results[0].skill.name == "pdf-extractor"
        assert results[0].score > 0.3

    def test_match_by_tag(self, sample_skills: list[Skill]) -> None:
        """タグでマッチすることをテスト."""
        matcher = SkillMatcher(sample_skills)
        results = matcher.match("document processing")

        # 両方のスキルが document タグを持つ
        assert len(results) >= 1

    def test_match_empty_query(self, sample_skills: list[Skill]) -> None:
        """空クエリで空リストを返すことをテスト."""
        matcher = SkillMatcher(sample_skills)
        results = matcher.match("")
        assert results == []

    def test_find_best(self, sample_skills: list[Skill]) -> None:
        """find_best で最適な Skill を取得できることをテスト."""
        matcher = SkillMatcher(sample_skills)
        best = matcher.find_best("excel spreadsheet")

        assert best is not None
        assert best.name == "excel-reader"

    def test_has_match(self, sample_skills: list[Skill]) -> None:
        """has_match でマッチ存在確認できることをテスト."""
        matcher = SkillMatcher(sample_skills)
        assert matcher.has_match("pdf") is True
        assert matcher.has_match("completely unrelated query xyz") is False

    def test_add_remove_skill(self, sample_skills: list[Skill]) -> None:
        """Skill の追加・削除ができることをテスト."""
        matcher = SkillMatcher()
        assert len(matcher.skills) == 0

        matcher.add_skill(sample_skills[0])
        assert len(matcher.skills) == 1

        removed = matcher.remove_skill("pdf-extractor")
        assert removed is True
        assert len(matcher.skills) == 0


class TestSkillValidator:
    """SkillValidator のテスト."""

    @pytest.fixture
    def valid_skill(self, tmp_path: Path) -> Skill:
        """有効な Skill を作成."""
        skill_dir = tmp_path / "valid-skill"
        skill_dir.mkdir()
        (skill_dir / "SKILL.md").write_text(
            dedent("""
            ---
            name: valid-skill
            description: A valid skill with proper description for testing purposes
            version: 1.0.0
            triggers:
              - test
            tags:
              - testing
            ---
            # Instructions
            This is a valid skill.
            """).strip(),
            encoding="utf-8",
        )
        return Skill.load(skill_dir)

    @pytest.fixture
    def invalid_skill(self, tmp_path: Path) -> Skill:
        """無効な Skill を作成."""
        skill_dir = tmp_path / "invalid-skill"
        skill_dir.mkdir()
        (skill_dir / "SKILL.md").write_text(
            dedent("""
            ---
            name: Invalid_Name
            description: Short
            version: bad
            ---

            """).strip(),
            encoding="utf-8",
        )
        return Skill.load(skill_dir)

    def test_validate_valid_skill(self, valid_skill: Skill) -> None:
        """有効な Skill が検証に合格することをテスト."""
        validator = SkillValidator()
        result = validator.validate(valid_skill)

        assert result.valid is True
        assert len(result.errors) == 0

    def test_validate_invalid_skill(self, invalid_skill: Skill) -> None:
        """無効な Skill で警告が出ることをテスト."""
        validator = SkillValidator()
        result = validator.validate(invalid_skill)

        # name が kebab-case ではない、description が短い、triggers がない
        assert len(result.warnings) > 0

    def test_validate_strict_mode(self, invalid_skill: Skill) -> None:
        """厳格モードで警告がエラーになることをテスト."""
        validator = SkillValidator(strict=True)
        result = validator.validate(invalid_skill)

        assert result.valid is False

    def test_security_check(self, tmp_path: Path) -> None:
        """セキュリティチェックが機能することをテスト."""
        skill_dir = tmp_path / "dangerous-skill"
        skill_dir.mkdir()
        (skill_dir / "SKILL.md").write_text(
            dedent("""
            ---
            name: dangerous-skill
            description: A dangerous skill for testing security checks
            ---
            # Instructions
            Run this dangerous command:
            rm -rf /
            """).strip(),
            encoding="utf-8",
        )
        skill = Skill.load(skill_dir)

        validator = SkillValidator()
        result = validator.validate(skill)

        assert result.valid is False
        assert any("Security risk" in e for e in result.errors)

    def test_is_valid_shortcut(self, valid_skill: Skill) -> None:
        """is_valid ショートカットが機能することをテスト."""
        validator = SkillValidator()
        assert validator.is_valid(valid_skill) is True


class TestSkillPersister:
    """SkillPersister のテスト."""

    @pytest.fixture
    def persister(self, tmp_path: Path) -> SkillPersister:
        """テスト用 Persister を作成."""
        learned_dir = tmp_path / "learned"
        project_dir = tmp_path / "project"
        return SkillPersister(learned_dir=learned_dir, project_dir=project_dir)

    @pytest.fixture
    def test_skill(self, tmp_path: Path) -> Skill:
        """テスト用 Skill を作成."""
        skill_dir = tmp_path / "test-skill"
        skill_dir.mkdir()
        (skill_dir / "SKILL.md").write_text(
            dedent("""
            ---
            name: test-skill
            description: A test skill for persister testing purposes
            triggers:
              - test
            ---
            # Instructions
            Test instructions here.
            """).strip(),
            encoding="utf-8",
        )
        return Skill.load(skill_dir)

    def test_save_skill(self, persister: SkillPersister, test_skill: Skill) -> None:
        """Skill を保存できることをテスト."""
        path = persister.save(test_skill, scope="learned")

        assert path.exists()
        assert (path / "SKILL.md").exists()

    def test_save_duplicate_raises(self, persister: SkillPersister, test_skill: Skill) -> None:
        """重複保存で FileExistsError が発生することをテスト."""
        persister.save(test_skill, scope="learned")

        with pytest.raises(FileExistsError):
            persister.save(test_skill, scope="learned")

    def test_save_force_overwrites(self, persister: SkillPersister, test_skill: Skill) -> None:
        """force=True で上書きできることをテスト."""
        persister.save(test_skill, scope="learned")
        path = persister.save(test_skill, scope="learned", force=True)

        assert path.exists()

    def test_delete_skill(self, persister: SkillPersister, test_skill: Skill) -> None:
        """Skill を削除できることをテスト."""
        persister.save(test_skill, scope="learned")

        deleted = persister.delete("test-skill", scope="learned")
        assert deleted is True

        deleted_again = persister.delete("test-skill", scope="learned")
        assert deleted_again is False

    def test_list_learned(self, persister: SkillPersister, test_skill: Skill) -> None:
        """学習済み Skill リストを取得できることをテスト."""
        persister.save(test_skill, scope="learned")

        learned = persister.list_learned()
        assert "test-skill" in learned


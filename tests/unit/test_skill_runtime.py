"""SkillRuntime のテスト - シェルスクリプト対応."""

import stat
from pathlib import Path

from agentflow.skills.core.base import Skill
from agentflow.skills.core.runtime import SkillRuntime


def _make_skill(tmp_path: Path, scripts: dict[str, str]) -> Skill:
    """テスト用 Skill を作成（scripts/ ディレクトリ付き）."""
    skill_dir = tmp_path / "test-skill"
    skill_dir.mkdir(exist_ok=True)
    (skill_dir / "SKILL.md").write_text("---\nname: test-skill\n---\nInstructions", encoding="utf-8")
    scripts_dir = skill_dir / "scripts"
    scripts_dir.mkdir(exist_ok=True)
    for name, content in scripts.items():
        script_file = scripts_dir / name
        script_file.write_text(content, encoding="utf-8")
        if name.endswith(".sh"):
            script_file.chmod(script_file.stat().st_mode | stat.S_IEXEC)
    return Skill.load(skill_dir)


class TestResolveScriptPath:
    """_resolve_script_path のテスト."""

    def test_finds_sh_file(self, tmp_path: Path) -> None:
        """シェルスクリプトが検索で見つかること."""
        skill = _make_skill(tmp_path, {"hello.sh": "#!/bin/bash\necho hi"})
        runtime = SkillRuntime()
        path = runtime._resolve_script_path(skill, "hello")
        assert path is not None
        assert path.suffix == ".sh"

    def test_finds_py_file(self, tmp_path: Path) -> None:
        """Python スクリプトが検索で見つかること."""
        skill = _make_skill(tmp_path, {"hello.py": "def hello(d): return d"})
        runtime = SkillRuntime()
        path = runtime._resolve_script_path(skill, "hello")
        assert path is not None
        assert path.suffix == ".py"

    def test_py_preferred_over_sh(self, tmp_path: Path) -> None:
        """.py is checked before .sh when both exist."""
        skill = _make_skill(
            tmp_path,
            {
                "run.py": "def run(d): return d",
                "run.sh": "#!/bin/bash\necho hi",
            },
        )
        runtime = SkillRuntime()
        path = runtime._resolve_script_path(skill, "run")
        assert path is not None
        assert path.suffix == ".py"

    def test_exact_match_with_extension(self, tmp_path: Path) -> None:
        """Exact match including extension works."""
        skill = _make_skill(tmp_path, {"run.sh": "#!/bin/bash\necho hi"})
        runtime = SkillRuntime()
        path = runtime._resolve_script_path(skill, "run.sh")
        assert path is not None

    def test_returns_none_for_missing(self, tmp_path: Path) -> None:
        """存在しないスクリプトは None を返す."""
        skill = _make_skill(tmp_path, {"other.py": "def other(d): pass"})
        runtime = SkillRuntime()
        path = runtime._resolve_script_path(skill, "missing")
        assert path is None


class TestListScripts:
    """list_scripts のテスト."""

    def test_lists_both_py_and_sh(self, tmp_path: Path) -> None:
        """.py と .sh の両方がリストされること."""
        skill = _make_skill(
            tmp_path,
            {
                "generate.py": "def generate(d): pass",
                "setup.sh": "#!/bin/bash\necho setup",
                "_internal.py": "# private",
            },
        )
        runtime = SkillRuntime()
        scripts = runtime.list_scripts(skill)
        assert sorted(scripts) == ["generate", "setup"]

    def test_excludes_other_extensions(self, tmp_path: Path) -> None:
        """Other extensions are excluded."""
        skill = _make_skill(
            tmp_path,
            {
                "run.py": "def run(d): pass",
                "data.json": "{}",
                "notes.txt": "text",
            },
        )
        runtime = SkillRuntime()
        scripts = runtime.list_scripts(skill)
        assert scripts == ["run"]


class TestExecuteShellScript:
    """_execute_shell_script のテスト."""

    async def test_simple_echo_script(self, tmp_path: Path) -> None:
        """Simple shell script that outputs JSON."""
        skill = _make_skill(
            tmp_path,
            {
                "echo_json.sh": '#!/bin/bash\necho \'{"result": "ok"}\'',
            },
        )
        runtime = SkillRuntime()
        result = await runtime._execute_shell_script(
            skill,
            skill.path / "scripts" / "echo_json.sh",  # type: ignore[union-attr]
            {"input": "test"},
        )
        assert result.success is True
        assert result.output == {"result": "ok"}

    async def test_script_reads_stdin(self, tmp_path: Path) -> None:
        """Shell script reads JSON from stdin."""
        script = '#!/bin/bash\nread input\necho "$input"'
        skill = _make_skill(tmp_path, {"passthrough.sh": script})
        runtime = SkillRuntime()
        input_data = {"key": "value"}
        result = await runtime._execute_shell_script(
            skill,
            skill.path / "scripts" / "passthrough.sh",  # type: ignore[union-attr]
            input_data,
        )
        assert result.success is True
        assert result.output == input_data

    async def test_script_failure(self, tmp_path: Path) -> None:
        """Non-zero exit code marks result as failed."""
        skill = _make_skill(
            tmp_path,
            {
                "fail.sh": "#!/bin/bash\necho 'error msg' >&2\nexit 1",
            },
        )
        runtime = SkillRuntime()
        result = await runtime._execute_shell_script(
            skill,
            skill.path / "scripts" / "fail.sh",  # type: ignore[union-attr]
            {},
        )
        assert result.success is False
        assert result.error is not None
        assert "error msg" in result.stderr

    async def test_non_json_stdout(self, tmp_path: Path) -> None:
        """Non-JSON stdout is captured as raw output."""
        skill = _make_skill(
            tmp_path,
            {
                "text.sh": '#!/bin/bash\necho "plain text"',
            },
        )
        runtime = SkillRuntime()
        result = await runtime._execute_shell_script(
            skill,
            skill.path / "scripts" / "text.sh",  # type: ignore[union-attr]
            {},
        )
        assert result.success is True
        assert "plain text" in result.output.get("raw", "")


class TestExecuteScriptRouting:
    """execute_script routing .sh vs .py のテスト."""

    async def test_routes_sh_to_shell_executor(self, tmp_path: Path) -> None:
        """Shell scripts are routed through _execute_shell_script."""
        skill = _make_skill(
            tmp_path,
            {
                "greet.sh": '#!/bin/bash\necho \'{"greeting": "hello"}\'',
            },
        )
        runtime = SkillRuntime()
        result = await runtime.execute_script(skill, "greet", {})
        assert result.success is True
        assert result.output.get("greeting") == "hello"

    async def test_routes_py_to_direct_executor(self, tmp_path: Path) -> None:
        """Python scripts are routed through _execute_directly."""
        skill = _make_skill(
            tmp_path,
            {
                "add.py": "def add(data):\n    return {'sum': data['a'] + data['b']}",
            },
        )
        runtime = SkillRuntime()
        result = await runtime.execute_script(skill, "add", {"a": 1, "b": 2})
        assert result.success is True
        assert result.output.get("sum") == 3

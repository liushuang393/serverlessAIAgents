"""SkillRuntime テスト.

Skill実行ランタイムの単体テスト。
"""

from pathlib import Path

import pytest

from agentflow.skills.base import Skill
from agentflow.skills.runtime import ScriptResult, SkillRuntime


class TestScriptResult:
    """ScriptResult テストクラス."""

    def test_create_success_result(self) -> None:
        """成功結果の作成."""
        result = ScriptResult(
            success=True,
            output={"keywords": ["python", "ai"]},
            duration_ms=100.0,
        )
        assert result.success is True
        assert result.output["keywords"] == ["python", "ai"]
        assert result.error is None

    def test_create_error_result(self) -> None:
        """エラー結果の作成."""
        result = ScriptResult(
            success=False,
            error="Script not found",
        )
        assert result.success is False
        assert result.error == "Script not found"


class TestSkillRuntime:
    """SkillRuntime テストクラス."""

    def test_init_default(self) -> None:
        """デフォルト初期化."""
        runtime = SkillRuntime()
        assert runtime._sandbox_provider is None
        assert runtime._timeout == 60.0

    def test_init_with_sandbox(self) -> None:
        """サンドボックス指定初期化."""
        runtime = SkillRuntime(sandbox_provider="docker", timeout=30.0)
        assert runtime._sandbox_provider == "docker"
        assert runtime._timeout == 30.0

    @pytest.mark.asyncio
    async def test_execute_script_not_found(self) -> None:
        """存在しないスクリプトの実行."""
        runtime = SkillRuntime()

        # ダミーSkill（パスなし）
        skill = Skill.create(
            name="test-skill",
            description="Test",
            instructions="Test instructions",
        )

        result = await runtime.execute_script(skill, "nonexistent", {})
        assert result.success is False
        assert "not found" in (result.error or "").lower()

    @pytest.mark.asyncio
    async def test_execute_script_market_trend(self) -> None:
        """market-trend-analysisスキルのスクリプト実行."""
        runtime = SkillRuntime()

        # 実際のSkillをロード
        skill_path = Path(__file__).parent.parent.parent.parent / "agentflow/skills/builtin/market-trend-analysis"
        if not skill_path.exists():
            pytest.skip("market-trend-analysis skill not found")

        skill = Skill.load(skill_path)
        assert skill is not None

        # スクリプト一覧確認
        scripts = runtime.list_scripts(skill)
        assert "validate_input" in scripts
        assert "extract_keywords" in scripts
        assert "generate_report_skeleton" in scripts

    @pytest.mark.asyncio
    async def test_execute_validate_input_script(self) -> None:
        """validate_inputスクリプトの実行."""
        runtime = SkillRuntime()

        skill_path = Path(__file__).parent.parent.parent.parent / "agentflow/skills/builtin/market-trend-analysis"
        if not skill_path.exists():
            pytest.skip("market-trend-analysis skill not found")

        skill = Skill.load(skill_path)
        assert skill is not None

        # 有効な入力データ（idとtitleは必須）
        input_data = {
            "articles": [
                {
                    "id": "article-001",
                    "title": "AI Trends",
                    "content": "Python and AI are popular.",
                    "published_at": "2024-01-01",
                    "source": "news",
                }
            ]
        }

        result = await runtime.execute_script(skill, "validate_input", input_data)
        assert result.success is True
        assert result.output.get("valid") is True

    @pytest.mark.asyncio
    async def test_execute_extract_keywords_script(self) -> None:
        """extract_keywordsスクリプトの実行."""
        runtime = SkillRuntime()

        skill_path = Path(__file__).parent.parent.parent.parent / "agentflow/skills/builtin/market-trend-analysis"
        if not skill_path.exists():
            pytest.skip("market-trend-analysis skill not found")

        skill = Skill.load(skill_path)
        assert skill is not None

        input_data = {
            "articles": [
                {"title": "Python AI", "content": "Python and machine learning.", "keywords": []},
                {"title": "AI Trends", "content": "AI and deep learning.", "keywords": []},
            ],
            "min_frequency": 1,
            "top_n": 5,
        }

        result = await runtime.execute_script(skill, "extract_keywords", input_data)
        assert result.success is True
        assert "keywords" in result.output

    def test_list_scripts_no_path(self) -> None:
        """パスなしSkillのスクリプト一覧."""
        runtime = SkillRuntime()
        skill = Skill.create(
            name="test",
            description="Test",
            instructions="Test",
        )
        scripts = runtime.list_scripts(skill)
        assert scripts == []

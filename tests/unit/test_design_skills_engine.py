"""DesignSkillsEngine のテスト."""

from unittest.mock import AsyncMock, MagicMock, patch


class TestDesignSkillsEngine:
    """DesignSkillsEngine 統合テスト（LLM + ComfyUI モック）."""

    def _make_mock_comfyui(self):
        """モックComfyUIクライアントを生成."""
        client = MagicMock()
        client.health_check = AsyncMock(return_value=True)
        client.build_workflow_payload = MagicMock(return_value={"prompt": {}})
        client.queue_prompt = AsyncMock(return_value="prompt-001")
        client.poll_until_complete = AsyncMock(return_value={
            "outputs": {
                "7": {
                    "images": [{
                        "filename": "out.png",
                        "subfolder": "",
                        "type": "output",
                    }]
                }
            }
        })
        client.get_image = AsyncMock(return_value=b"\x89PNG")
        client.close = AsyncMock()
        return client

    async def test_engine_initializes(self) -> None:
        """エンジンが正常に初期化されること."""
        from apps.design_skills_engine.engine import DesignSkillsEngine

        engine = DesignSkillsEngine(llm_client=None)
        assert engine is not None

    async def test_engine_has_correct_config(self) -> None:
        """エンジンの設定が正しいこと."""
        from apps.design_skills_engine.engine import DesignSkillsEngine

        engine = DesignSkillsEngine(llm_client=None)
        assert engine._config.name == "design-skills-engine"
        assert engine._config.timeout_seconds == 600


class TestDesignSkillsSkill:
    """Design Skills スキル登録テスト."""

    def test_skill_md_exists(self) -> None:
        """SKILL.mdが存在すること."""
        from pathlib import Path
        skill_path = Path("apps/design_skills_engine/skills/design-skills/SKILL.md")
        assert skill_path.exists()

    def test_skill_md_has_required_fields(self) -> None:
        """SKILL.mdに必須フィールドが含まれること."""
        from pathlib import Path
        skill_path = Path("apps/design_skills_engine/skills/design-skills/SKILL.md")
        content = skill_path.read_text()
        assert "name:" in content
        assert "description:" in content
        assert "triggers:" in content

    def test_skill_module_exists(self) -> None:
        """スキルモジュールディレクトリが存在すること."""
        from pathlib import Path
        design_skill_dir = Path("apps/design_skills_engine/skills/design-skills")
        assert design_skill_dir.exists()
        assert (design_skill_dir / "__init__.py").exists()
        assert (design_skill_dir / "design_skills.py").exists()

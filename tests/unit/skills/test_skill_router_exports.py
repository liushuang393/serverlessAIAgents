"""SkillRouter エクスポート経路の回帰テスト."""

import pytest

from agentflow.skills import SkillRouter


@pytest.mark.asyncio
async def test_exported_skill_router_loads_builtin_skills() -> None:
    """agentflow.skills から導出される SkillRouter が built-in を読み込める."""
    router = SkillRouter()
    await router.initialize()
    assert router.skill_count > 0

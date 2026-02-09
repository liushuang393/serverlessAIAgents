"""PromptPlannerAgent のテスト."""

import pytest


class TestPromptPlannerAgent:
    """PromptPlannerAgent ユニットテスト（ルールベース、LLM不使用）."""

    def _make_agent(self):
        """テスト用Agentインスタンスを生成."""
        from agentflow.skills.builtin.design_skills.agents.prompt_planner_agent import (
            PromptPlannerAgent,
        )
        return PromptPlannerAgent(llm_client=None)

    def _make_sample_intent(self):
        """テスト用IntentAnalysisを生成."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            DesignCategory,
            ImageRole,
            IntentAnalysis,
        )
        return IntentAnalysis(
            category=DesignCategory.PRODUCT_PHOTOGRAPHY,
            subject="outdoor bluetooth speaker",
            key_features=["waterproof", "high quality audio", "portable"],
            target_audience="outdoor enthusiasts",
            style_direction="tech, dark, minimal",
            image_roles=[
                ImageRole.HERO,
                ImageRole.FEATURE,
                ImageRole.FEATURE,
                ImageRole.DETAIL,
            ],
        )

    def test_agent_name(self) -> None:
        """Agent名が正しいこと."""
        agent = self._make_agent()
        assert agent.name == "PromptPlannerAgent"

    async def test_generates_prompt_plan(self) -> None:
        """プロンプト計画が正しく生成されること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import PromptPlanInput

        agent = self._make_agent()
        intent = self._make_sample_intent()
        input_data = PromptPlanInput(
            intent=intent,
            brand_colors=["#000000", "#1E90FF"],
            aspect_ratio="1:1",
        )
        result = await agent.process(input_data)

        assert result.design_concept  # 空でないこと
        assert result.global_style is not None
        assert len(result.images) == 4  # intent.image_rolesの数と一致
        assert result.global_style.negative_prompt  # ネガティブプロンプトが設定されていること

    async def test_all_images_share_seed(self) -> None:
        """全画像が同じ一貫性シードを共有すること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import PromptPlanInput

        agent = self._make_agent()
        intent = self._make_sample_intent()
        input_data = PromptPlanInput(
            intent=intent,
            brand_colors=[],
        )
        result = await agent.process(input_data)

        # 一貫性シードが設定されていること
        assert result.consistency_seed > 0
        # 全画像が一貫性シードを使用すること
        for img in result.images:
            assert img.seed == result.consistency_seed

    async def test_hero_image_prompt_contains_subject(self) -> None:
        """ヒーロー画像のプロンプトに被写体が含まれること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            ImageRole,
            PromptPlanInput,
        )

        agent = self._make_agent()
        intent = self._make_sample_intent()
        input_data = PromptPlanInput(intent=intent)
        result = await agent.process(input_data)

        hero = [img for img in result.images if img.role == ImageRole.HERO]
        assert len(hero) == 1
        assert intent.subject.lower() in hero[0].prompt.lower()

    async def test_feature_images_use_different_features(self) -> None:
        """フィーチャー画像が異なる特徴キーワードを使用すること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            ImageRole,
            PromptPlanInput,
        )

        agent = self._make_agent()
        intent = self._make_sample_intent()
        input_data = PromptPlanInput(intent=intent)
        result = await agent.process(input_data)

        features = [img for img in result.images if img.role == ImageRole.FEATURE]
        assert len(features) == 2
        # 異なる特徴が割り当てられていること
        assert features[0].prompt != features[1].prompt

    async def test_aspect_ratio_resolution(self) -> None:
        """アスペクト比が正しい解像度に変換されること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import PromptPlanInput

        agent = self._make_agent()
        intent = self._make_sample_intent()
        input_data = PromptPlanInput(intent=intent, aspect_ratio="16:9")
        result = await agent.process(input_data)

        # 16:9 → 1344x768
        assert result.images[0].width == 1344
        assert result.images[0].height == 768

    async def test_dark_style_lighting(self) -> None:
        """ダーク系スタイル指定時のライティングが適切であること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import PromptPlanInput

        agent = self._make_agent()
        intent = self._make_sample_intent()  # style_direction = "tech, dark, minimal"
        input_data = PromptPlanInput(intent=intent)
        result = await agent.process(input_data)

        assert "high contrast" in result.global_style.lighting

    async def test_brand_colors_in_palette(self) -> None:
        """ブランドカラーがパレットに反映されること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import PromptPlanInput

        agent = self._make_agent()
        intent = self._make_sample_intent()
        input_data = PromptPlanInput(
            intent=intent,
            brand_colors=["#FF0000", "#00FF00"],
        )
        result = await agent.process(input_data)

        assert "#FF0000" in result.global_style.color_palette
        assert "#00FF00" in result.global_style.color_palette

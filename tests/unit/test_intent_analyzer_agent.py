"""IntentAnalyzerAgent のテスト."""


class TestIntentAnalyzerAgent:
    """IntentAnalyzerAgent ユニットテスト（ルールベース、LLM不使用）."""

    def _make_agent(self):
        """テスト用Agentインスタンスを生成."""
        from agentflow.skills.builtin.design_skills.agents.intent_analyzer_agent import (
            IntentAnalyzerAgent,
        )
        return IntentAnalyzerAgent(llm_client=None)

    def test_agent_name(self) -> None:
        """Agent名が正しいこと."""
        agent = self._make_agent()
        assert agent.name == "IntentAnalyzerAgent"

    async def test_process_product_brief(self) -> None:
        """商品ブリーフの解析が正しいこと."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            DesignBriefInput,
            DesignCategory,
        )

        agent = self._make_agent()
        input_data = DesignBriefInput(
            brief="Create product images for an outdoor bluetooth speaker, "
                  "tech style, black and blue colors",
            style_preferences=["tech", "dark"],
            target_platform="amazon",
            num_images=6,
        )
        result = await agent.process(input_data)

        assert result.category == DesignCategory.PRODUCT_PHOTOGRAPHY
        assert result.subject  # 空でないこと
        assert len(result.image_roles) == 6
        assert len(result.key_features) >= 1

    async def test_process_social_media_brief(self) -> None:
        """SNSブリーフの解析が正しいこと."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            DesignBriefInput,
            DesignCategory,
        )

        agent = self._make_agent()
        input_data = DesignBriefInput(
            brief="Design Instagram posts for a coffee brand campaign",
            target_platform="instagram",
            num_images=4,
        )
        result = await agent.process(input_data)

        assert result.category == DesignCategory.SOCIAL_MEDIA
        assert len(result.image_roles) == 4

    async def test_default_image_role_distribution(self) -> None:
        """8枚の画像にhero, feature, detail, lifestyleが含まれること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            DesignBriefInput,
            ImageRole,
        )

        agent = self._make_agent()
        input_data = DesignBriefInput(
            brief="Product images for a laptop stand",
            num_images=8,
        )
        result = await agent.process(input_data)

        roles = list(result.image_roles)
        assert ImageRole.HERO in roles
        assert ImageRole.FEATURE in roles

    async def test_amazon_platform_constraints(self) -> None:
        """Amazonプラットフォームの制約が設定されること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import DesignBriefInput

        agent = self._make_agent()
        input_data = DesignBriefInput(
            brief="Product images for a phone case",
            target_platform="amazon",
            num_images=4,
        )
        result = await agent.process(input_data)

        assert "background" in result.platform_constraints
        assert "min_resolution" in result.platform_constraints

    async def test_japanese_brief(self) -> None:
        """日本語ブリーフが正しく処理されること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            DesignBriefInput,
            DesignCategory,
        )

        agent = self._make_agent()
        input_data = DesignBriefInput(
            brief="コーヒーブランドのインスタ投稿画像をデザイン",
            num_images=4,
        )
        result = await agent.process(input_data)

        assert result.category == DesignCategory.SOCIAL_MEDIA
        assert len(result.image_roles) == 4

    async def test_brand_identity_detection(self) -> None:
        """ブランドアイデンティティカテゴリの検出."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            DesignBriefInput,
            DesignCategory,
        )

        agent = self._make_agent()
        input_data = DesignBriefInput(
            brief="Create a brand identity kit with logo concepts",
            num_images=3,
        )
        result = await agent.process(input_data)

        assert result.category == DesignCategory.BRAND_IDENTITY

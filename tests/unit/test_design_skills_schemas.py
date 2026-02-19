"""Design Skills Engine スキーマのテスト."""

import pytest
from pydantic import ValidationError


class TestDesignBriefInput:
    """DesignBriefInput スキーマテスト."""

    def test_minimal_input(self) -> None:
        """最小限の入力でインスタンス生成できること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import DesignBriefInput

        brief = DesignBriefInput(brief="Bluetoothスピーカーの商品画像を生成")
        assert brief.brief == "Bluetoothスピーカーの商品画像を生成"
        assert brief.style_preferences == []
        assert brief.target_platform == ""
        assert brief.num_images == 8

    def test_full_input(self) -> None:
        """全フィールド指定でインスタンス生成できること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import DesignBriefInput

        brief = DesignBriefInput(
            brief="Bluetoothスピーカーの商品画像",
            style_preferences=["tech", "dark", "minimal"],
            target_platform="amazon",
            num_images=6,
            brand_colors=["#000000", "#1E90FF"],
            aspect_ratio="1:1",
        )
        assert brief.num_images == 6
        assert len(brief.brand_colors) == 2

    def test_num_images_lower_bound(self) -> None:
        """num_imagesが0以下の場合バリデーションエラー."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import DesignBriefInput

        with pytest.raises(ValidationError):
            DesignBriefInput(brief="test", num_images=0)

    def test_num_images_upper_bound(self) -> None:
        """num_imagesが20超の場合バリデーションエラー."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import DesignBriefInput

        with pytest.raises(ValidationError):
            DesignBriefInput(brief="test", num_images=25)


class TestGlobalStyle:
    """GlobalStyle スキーマテスト."""

    def test_creation(self) -> None:
        """正常にインスタンス生成できること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import GlobalStyle

        style = GlobalStyle(
            color_palette=["black", "dark blue", "neon accent"],
            lighting="studio lighting, high contrast",
            camera_angle="product photography, center frame",
            mood="professional, tech-forward",
            negative_prompt="blurry, low quality, text, watermark",
        )
        assert len(style.color_palette) == 3
        assert "studio" in style.lighting

    def test_default_base_model(self) -> None:
        """デフォルトベースモデルが設定されていること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import GlobalStyle

        style = GlobalStyle(
            color_palette=["black"],
            lighting="studio",
            camera_angle="front",
            mood="pro",
            negative_prompt="blurry",
        )
        assert style.base_model == "sd_xl_base_1.0.safetensors"


class TestImageSpec:
    """ImageSpec スキーマテスト."""

    def test_creation(self) -> None:
        """正常にインスタンス生成できること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            ImageRole,
            ImageSpec,
        )

        spec = ImageSpec(
            image_id="img_001",
            role=ImageRole.HERO,
            prompt="Speaker on black background",
            seed=12345,
            weight=1.0,
        )
        assert spec.role == ImageRole.HERO
        assert spec.seed == 12345

    def test_default_values(self) -> None:
        """デフォルト値が正しく設定されること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            ImageRole,
            ImageSpec,
        )

        spec = ImageSpec(
            image_id="img_001",
            role=ImageRole.HERO,
            prompt="test",
        )
        assert spec.seed == -1
        assert spec.width == 1024
        assert spec.height == 1024
        assert spec.cfg_scale == 7.0
        assert spec.steps == 30

    def test_cfg_scale_bounds(self) -> None:
        """cfg_scaleの範囲外でバリデーションエラー."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            ImageRole,
            ImageSpec,
        )

        with pytest.raises(ValidationError):
            ImageSpec(
                image_id="img_001",
                role=ImageRole.HERO,
                prompt="test",
                cfg_scale=0.5,
            )


class TestPromptPlanOutput:
    """PromptPlanOutput スキーマテスト."""

    def test_creation(self) -> None:
        """プロンプト計画を正常に生成できること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
            PromptPlanOutput,
        )

        plan = PromptPlanOutput(
            design_concept="テクノ風商品撮影",
            global_style=GlobalStyle(
                color_palette=["black"],
                lighting="studio",
                camera_angle="front",
                mood="professional",
                negative_prompt="blurry",
            ),
            images=[
                ImageSpec(
                    image_id="img_001",
                    role=ImageRole.HERO,
                    prompt="Speaker on black background",
                    seed=42,
                ),
            ],
            consistency_seed=42,
        )
        assert len(plan.images) == 1
        assert plan.consistency_seed == 42


class TestWorkflowResult:
    """WorkflowResult スキーマテスト."""

    def test_creation(self) -> None:
        """ワークフロー結果を正常に生成できること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            GeneratedImage,
            ImageRole,
            WorkflowResult,
        )

        result = WorkflowResult(
            images=[
                GeneratedImage(
                    image_id="img_001",
                    role=ImageRole.HERO,
                    file_path="/output/hero_001.png",
                    prompt_used="Speaker on black background",
                    seed_used=42,
                ),
            ],
            output_directory="/output",
            total_generation_time_seconds=45.2,
        )
        assert len(result.images) == 1
        assert result.total_generation_time_seconds == 45.2

    def test_errors_default_empty(self) -> None:
        """エラーリストのデフォルトが空であること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import WorkflowResult

        result = WorkflowResult(
            images=[],
            output_directory="/output",
            total_generation_time_seconds=0.0,
        )
        assert result.errors == []


class TestPromptPlanInput:
    """PromptPlanInput スキーマテスト."""

    def test_creation(self) -> None:
        """正常に入力を生成できること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            DesignCategory,
            ImageRole,
            IntentAnalysis,
            PromptPlanInput,
        )

        intent = IntentAnalysis(
            category=DesignCategory.PRODUCT_PHOTOGRAPHY,
            subject="bluetooth speaker",
            key_features=["waterproof"],
            style_direction="tech, dark",
            image_roles=[ImageRole.HERO],
        )
        plan_input = PromptPlanInput(intent=intent)
        assert plan_input.intent.subject == "bluetooth speaker"
        assert plan_input.aspect_ratio == "1:1"


class TestWorkflowExecutorInput:
    """WorkflowExecutorInput スキーマテスト."""

    def test_creation(self) -> None:
        """正常に入力を生成できること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
            PromptPlanOutput,
            WorkflowExecutorInput,
        )

        plan = PromptPlanOutput(
            design_concept="Test",
            global_style=GlobalStyle(
                color_palette=["black"],
                lighting="studio",
                camera_angle="front",
                mood="pro",
                negative_prompt="blurry",
            ),
            images=[
                ImageSpec(
                    image_id="img_001",
                    role=ImageRole.HERO,
                    prompt="test",
                    seed=42,
                )
            ],
            consistency_seed=42,
        )
        executor_input = WorkflowExecutorInput(prompt_plan=plan)
        assert len(executor_input.prompt_plan.images) == 1
        assert executor_input.output_directory == "/tmp/design_output"

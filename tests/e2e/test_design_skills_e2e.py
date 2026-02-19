"""Design Skills Engine - E2Eテスト.

実際のComfyUIサーバーに対するインテグレーションテスト。
ComfyUI未起動時はすべて自動スキップされる。

実行方法:
    pytest tests/e2e/test_design_skills_e2e.py -v --no-cov
"""

from pathlib import Path

import httpx
import pytest

from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
    DesignBriefInput,
    GlobalStyle,
    ImageRole,
    ImageSpec,
    PromptPlanInput,
)
from agentflow.skills.builtin.design_skills.tools.comfyui_client import ComfyUIClient


# PNG マジックバイト (8 bytes)
_PNG_MAGIC = b"\x89PNG\r\n\x1a\n"

# 最小有効画像サイズ (1KB)
_MIN_IMAGE_SIZE = 1024

# テスト用の高速生成パラメータ
_TEST_WIDTH = 512
_TEST_HEIGHT = 512
_TEST_STEPS = 10
_TEST_SEED = 42
_TEST_MODEL = "sd_xl_base_1.0.safetensors"


def _make_style() -> GlobalStyle:
    """テスト用グローバルスタイルを生成."""
    return GlobalStyle(
        color_palette=["black", "blue"],
        lighting="studio lighting",
        camera_angle="front view",
        mood="professional",
        negative_prompt="blurry, low quality, distorted",
        base_model=_TEST_MODEL,
    )


def _make_spec(
    image_id: str = "test_hero",
    role: ImageRole = ImageRole.HERO,
    prompt: str = "a sleek bluetooth speaker on white background, product photography",
) -> ImageSpec:
    """テスト用画像仕様を生成."""
    return ImageSpec(
        image_id=image_id,
        role=role,
        prompt=prompt,
        seed=_TEST_SEED,
        width=_TEST_WIDTH,
        height=_TEST_HEIGHT,
        steps=_TEST_STEPS,
    )


def _extract_images_from_history(history: dict) -> list[dict]:
    """ComfyUI履歴から画像情報を抽出."""
    images: list[dict] = []
    outputs = history.get("outputs", {})
    for node_output in outputs.values():
        if "images" in node_output:
            images.extend(node_output["images"])
    return images


# =============================================================================
# テストクラス
# =============================================================================


@pytest.mark.e2e
class TestComfyUIConnection:
    """ComfyUIサーバー接続テスト."""

    async def test_health_check(self, comfyui_client: ComfyUIClient) -> None:
        """サーバーの到達可能性を確認."""
        result = await comfyui_client.health_check()
        assert result is True


@pytest.mark.e2e
class TestSingleImageGeneration:
    """単一画像生成テスト."""

    async def test_generate_hero_image(
        self,
        comfyui_client: ComfyUIClient,
        e2e_output_dir: Path,
    ) -> None:
        """HERO画像を1枚生成し、PNGとして検証."""
        style = _make_style()
        spec = _make_spec()

        # ワークフロー構築 → キューイング → 完了待機
        workflow = comfyui_client.build_workflow_payload(style, spec)
        prompt_id = await comfyui_client.queue_prompt(workflow)
        assert prompt_id, "prompt_id が空"

        history = await comfyui_client.poll_until_complete(prompt_id)
        assert history, "履歴が空"

        # 画像取得 + 検証
        images = _extract_images_from_history(history)
        assert len(images) >= 1, "画像が生成されなかった"

        image_info = images[0]
        image_bytes = await comfyui_client.get_image(
            filename=image_info["filename"],
            subfolder=image_info.get("subfolder", ""),
            folder_type=image_info.get("type", "output"),
        )

        # PNGマジックバイト検証
        assert image_bytes[:8] == _PNG_MAGIC, "PNG形式ではない"
        # 最小サイズ検証 (1KB以上)
        assert len(image_bytes) > _MIN_IMAGE_SIZE, f"画像が小さすぎる: {len(image_bytes)} bytes"

        # ローカル保存
        output_path = e2e_output_dir / "hero.png"
        output_path.write_bytes(image_bytes)
        assert output_path.exists()


@pytest.mark.e2e
class TestFullPipeline:
    """IntentAnalyzer → PromptPlanner → ComfyUI の全パイプラインテスト."""

    async def test_brief_to_images(
        self,
        comfyui_client: ComfyUIClient,
    ) -> None:
        """自然言語ブリーフからComfyUI画像生成まで通しで実行."""
        from agentflow.skills.builtin.design_skills.agents.intent_analyzer_agent import (
            IntentAnalyzerAgent,
        )
        from agentflow.skills.builtin.design_skills.agents.prompt_planner_agent import (
            PromptPlannerAgent,
        )

        # Step 1: 意図解析
        analyzer = IntentAnalyzerAgent()
        brief = DesignBriefInput(
            brief="A minimalist bluetooth speaker, black, studio shot",
            num_images=1,
            style_preferences=["minimal", "dark"],
        )
        intent = await analyzer.process(brief)
        assert intent is not None
        assert intent.subject

        # Step 2: プロンプト計画
        planner = PromptPlannerAgent()
        plan_input = PromptPlanInput(intent=intent)
        plan = await planner.process(plan_input)
        assert plan is not None
        assert len(plan.images) >= 1

        # Step 3: ComfyUI実行 (最初の画像のみ、高速パラメータで上書き)
        img_spec = plan.images[0]
        fast_spec = ImageSpec(
            image_id=img_spec.image_id,
            role=img_spec.role,
            prompt=img_spec.prompt,
            seed=_TEST_SEED,
            width=_TEST_WIDTH,
            height=_TEST_HEIGHT,
            steps=_TEST_STEPS,
        )

        workflow = comfyui_client.build_workflow_payload(plan.global_style, fast_spec)
        prompt_id = await comfyui_client.queue_prompt(workflow)
        history = await comfyui_client.poll_until_complete(prompt_id)

        images = _extract_images_from_history(history)
        assert len(images) >= 1, "パイプラインで画像が生成されなかった"

        image_bytes = await comfyui_client.get_image(
            filename=images[0]["filename"],
            subfolder=images[0].get("subfolder", ""),
            folder_type=images[0].get("type", "output"),
        )
        assert image_bytes[:8] == _PNG_MAGIC


@pytest.mark.e2e
@pytest.mark.slow
class TestBatchGeneration:
    """バッチ画像生成テスト."""

    async def test_generate_three_images(
        self,
        comfyui_client: ComfyUIClient,
    ) -> None:
        """3種類の役割で画像を順次生成."""
        style = _make_style()
        roles = [
            (ImageRole.HERO, "bluetooth speaker hero shot, centered, white background"),
            (ImageRole.FEATURE, "bluetooth speaker waterproof feature, water splash"),
            (ImageRole.LIFESTYLE, "bluetooth speaker outdoor camping scene"),
        ]

        generated_images: list[bytes] = []
        for i, (role, prompt) in enumerate(roles):
            spec = _make_spec(
                image_id=f"batch_{role.value.lower()}",
                role=role,
                prompt=prompt,
            )
            # シードをずらして異なる画像を生成
            spec = ImageSpec(
                image_id=spec.image_id,
                role=spec.role,
                prompt=spec.prompt,
                seed=_TEST_SEED + i,
                width=_TEST_WIDTH,
                height=_TEST_HEIGHT,
                steps=_TEST_STEPS,
            )

            workflow = comfyui_client.build_workflow_payload(style, spec)
            prompt_id = await comfyui_client.queue_prompt(workflow)
            history = await comfyui_client.poll_until_complete(prompt_id)

            images = _extract_images_from_history(history)
            assert len(images) >= 1, f"{role.value} 画像が生成されなかった"

            image_bytes = await comfyui_client.get_image(
                filename=images[0]["filename"],
                subfolder=images[0].get("subfolder", ""),
                folder_type=images[0].get("type", "output"),
            )
            assert image_bytes[:8] == _PNG_MAGIC
            assert len(image_bytes) > _MIN_IMAGE_SIZE
            generated_images.append(image_bytes)

        assert len(generated_images) == 3


@pytest.mark.e2e
class TestErrorHandling:
    """エラーハンドリングテスト."""

    async def test_invalid_model_name(
        self,
        comfyui_client: ComfyUIClient,
    ) -> None:
        """存在しないチェックポイント名でエラーになることを確認."""
        style = GlobalStyle(
            color_palette=["red"],
            lighting="flat",
            camera_angle="front",
            mood="neutral",
            negative_prompt="blurry",
            base_model="nonexistent_model_12345.safetensors",
        )
        spec = _make_spec()

        workflow = comfyui_client.build_workflow_payload(style, spec)
        with pytest.raises(httpx.HTTPStatusError):
            await comfyui_client.queue_prompt(workflow)

    async def test_timeout_handling(
        self,
        comfyui_url: str,
    ) -> None:
        """極端に短いタイムアウトでTimeoutErrorが発生することを確認."""
        # 0.001秒のタイムアウトで接続自体がタイムアウトするはず
        client = ComfyUIClient(base_url=comfyui_url, timeout=0.001)
        try:
            style = _make_style()
            spec = _make_spec()
            workflow = client.build_workflow_payload(style, spec)
            with pytest.raises((TimeoutError, httpx.TimeoutException)):
                await client.queue_prompt(workflow)
        finally:
            await client.close()

"""PromptPlannerAgent - 構造化プロンプト計画生成Agent.

IntentAnalysis を受け取り、完全な PromptPlan に変換:
- グローバルスタイル(全画像で統一)
- 個別画像プロンプト(役割別に最適化)
- 一貫性制御(共有シード、LoRA)

使用例:
    >>> agent = PromptPlannerAgent()
    >>> result = await agent.run({
    ...     "intent": intent_analysis,
    ...     "brand_colors": ["#000000", "#1E90FF"],
    ... })
"""

import logging
import random
from typing import Any, ClassVar

from agentflow import ResilientAgent
from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
    GlobalStyle,
    ImageRole,
    ImageSpec,
    IntentAnalysis,
    PromptPlanInput,
    PromptPlanOutput,
)
from agentflow.utils import extract_json


class PromptPlannerAgent(ResilientAgent[PromptPlanInput, PromptPlanOutput]):
    """デザイン意図から構造化プロンプト計画を生成.

    Lovartの「デザイナーの頭脳」に相当する部分。
    全画像で統一されたスタイルと、役割別に最適化されたプロンプトを生成。
    """

    name = "PromptPlannerAgent"
    temperature = 0.5

    # 役割別プロンプトテンプレート(Stable Diffusion最適化済み)
    ROLE_TEMPLATES: ClassVar[dict[ImageRole, str]] = {
        ImageRole.HERO: (
            "{subject}, center frame, main product shot, "
            "dramatic composition, studio backdrop, hero image"
        ),
        ImageRole.FEATURE: (
            "{subject}, {feature}, feature highlight, focused detail, contextual setting"
        ),
        ImageRole.DETAIL: (
            "{subject}, extreme close-up, macro detail, texture visible, studio lighting"
        ),
        ImageRole.LIFESTYLE: (
            "{subject}, in use, real-world setting, "
            "natural environment, candid feel, lifestyle photography"
        ),
        ImageRole.COMPARISON: (
            "{subject}, size comparison, with everyday objects, scale reference, flat lay"
        ),
        ImageRole.INFOGRAPHIC: (
            "{subject}, clean background, space for text overlay, "
            "minimal composition, infographic style"
        ),
    }

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    def _parse_input(self, input_data: dict[str, Any]) -> PromptPlanInput:
        """入力辞書をPromptPlanInputにパース.
        
        PipelineEngine から渡される場合、intent_analyzer_result フィールドに
        IntentAnalysis が含まれているため、それを intent フィールドに変換する。
        """
        # PipelineEngine からの入力を処理
        if "intent_analyzer_result" in input_data and "intent" not in input_data:
            intent_result = input_data["intent_analyzer_result"]
            # IntentAnalysis オブジェクトまたは辞書を intent フィールドに設定
            if isinstance(intent_result, dict):
                input_data["intent"] = IntentAnalysis(**intent_result)
            else:
                input_data["intent"] = intent_result
        # PipelineEngine が結果を直接マージした場合の処理
        # IntentAnalysis のフィールドが直接 input_data に含まれている場合
        elif "intent" not in input_data and "category" in input_data and "subject" in input_data:
            # IntentAnalysis のフィールドを抽出して intent オブジェクトを構築
            intent_fields = {
                "category": input_data.get("category"),
                "subject": input_data.get("subject"),
                "key_features": input_data.get("key_features", []),
                "target_audience": input_data.get("target_audience", ""),
                "style_direction": input_data.get("style_direction", ""),
                "image_roles": input_data.get("image_roles", []),
                "platform_constraints": input_data.get("platform_constraints", {}),
            }
            input_data["intent"] = IntentAnalysis(**intent_fields)
        
        return PromptPlanInput(**input_data)

    async def process(self, input_data: PromptPlanInput) -> PromptPlanOutput:
        """プロンプト計画を生成."""
        if self._llm:
            return await self._plan_with_llm(input_data)
        return self._plan_rule_based(input_data)

    def _build_global_style(self, input_data: PromptPlanInput) -> GlobalStyle:
        """意図解析結果からグローバルスタイルを構築."""
        intent = input_data.intent

        # カラーパレット
        palette = input_data.brand_colors[:5] if input_data.brand_colors else ["neutral tones"]

        # スタイル依存のライティング
        style_lower = intent.style_direction.lower()
        if "dark" in style_lower or "tech" in style_lower or "テクノ" in style_lower:
            lighting = "studio lighting, high contrast, dramatic shadows"
        elif "warm" in style_lower or "cozy" in style_lower or "暖色" in style_lower:
            lighting = "warm golden hour lighting, soft shadows"
        elif "ミニマル" in style_lower or "minimal" in style_lower:
            lighting = "flat lighting, soft diffused light, minimal shadows"
        else:
            lighting = "clean studio lighting, even illumination"

        # カテゴリ別カメラアングル
        camera_angles = {
            "PRODUCT_PHOTOGRAPHY": "product photography, 45-degree angle",
            "SOCIAL_MEDIA": "eye-level, lifestyle photography",
            "BRAND_IDENTITY": "centered, symmetrical composition",
            "ADVERTISING": "dynamic angle, attention-grabbing composition",
            "PACKAGING": "flat lay, overhead shot",
            "UI_MOCKUP": "isometric view, clean perspective",
        }
        camera = camera_angles.get(
            intent.category.value,
            "professional photography",
        )

        return GlobalStyle(
            color_palette=palette,
            lighting=lighting,
            camera_angle=camera,
            mood=intent.style_direction,
            negative_prompt=(
                "blurry, low quality, text, watermark, deformed, "
                "ugly, oversaturated, cropped, out of frame, "
                "bad anatomy, bad proportions, duplicate"
            ),
        )

    def _build_image_specs(
        self,
        input_data: PromptPlanInput,
        seed: int,
    ) -> list[ImageSpec]:
        """個別画像仕様リストを構築."""
        intent = input_data.intent
        specs: list[ImageSpec] = []
        feature_idx = 0

        for i, role in enumerate(intent.image_roles):
            template = self.ROLE_TEMPLATES.get(role, "{subject}")

            # FEATURE役割には特徴キーワードを割り当て
            feature = ""
            if role == ImageRole.FEATURE and intent.key_features:
                feature = intent.key_features[feature_idx % len(intent.key_features)]
                feature_idx += 1

            prompt = template.format(
                subject=intent.subject,
                feature=feature,
            )

            # アスペクト比を解像度に変換
            width, height = self._resolve_dimensions(input_data.aspect_ratio)

            specs.append(
                ImageSpec(
                    image_id=f"img_{i + 1:03d}",
                    role=role,
                    prompt=prompt,
                    seed=seed,
                    width=width,
                    height=height,
                )
            )

        return specs

    def _resolve_dimensions(self, aspect_ratio: str) -> tuple[int, int]:
        """アスペクト比文字列をピクセル解像度に変換.

        SDXL推奨解像度に基づく。
        """
        ratio_map: dict[str, tuple[int, int]] = {
            "1:1": (1024, 1024),
            "16:9": (1344, 768),
            "9:16": (768, 1344),
            "4:3": (1152, 896),
            "3:4": (896, 1152),
            "4:5": (896, 1120),
        }
        return ratio_map.get(aspect_ratio, (1024, 1024))

    def _plan_rule_based(self, input_data: PromptPlanInput) -> PromptPlanOutput:
        """ルールベースのプロンプト計画生成."""
        global_style = self._build_global_style(input_data)
        seed = random.randint(1, 2**31 - 1)
        images = self._build_image_specs(input_data, seed)

        return PromptPlanOutput(
            design_concept=(
                f"{input_data.intent.category.value.replace('_', ' ').title()} "
                f"for {input_data.intent.subject}"
            ),
            global_style=global_style,
            images=images,
            consistency_seed=seed,
        )

    async def _plan_with_llm(self, input_data: PromptPlanInput) -> PromptPlanOutput:
        """LLMを使用したプロンプト計画生成(ベースを強化).

        ルールベースで基本計画を生成後、LLMでプロンプトを最適化。
        """
        base_plan = self._plan_rule_based(input_data)

        system_prompt = """あなたはStable Diffusionプロンプトの専門家です。
基本プロンプト計画を受け取り、各画像のプロンプトをより高品質に最適化してください。
構造は変えず、プロンプトテキストのみを改善してください。

JSON形式で出力: {"enhanced_prompts": [{"image_id": "...", "prompt": "..."}]}"""

        prompts_summary = "\n".join(
            f"- {img.image_id} ({img.role.value}): {img.prompt}" for img in base_plan.images
        )
        user_prompt = f"""【被写体】{input_data.intent.subject}
【スタイル】{input_data.intent.style_direction}
【特徴】{", ".join(input_data.intent.key_features)}

【基本プロンプト】
{prompts_summary}"""

        try:
            response = await self._call_llm(f"{system_prompt}\n\n{user_prompt}")
            data = extract_json(response)

            if data and "enhanced_prompts" in data:
                prompt_map = {
                    ep["image_id"]: ep["prompt"]
                    for ep in data["enhanced_prompts"]
                    if "image_id" in ep and "prompt" in ep
                }
                for img in base_plan.images:
                    if img.image_id in prompt_map:
                        img.prompt = prompt_map[img.image_id]
        except Exception:
            self._logger.warning("LLMプロンプト最適化に失敗、ルールベースを使用")

        return base_plan

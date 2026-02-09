"""IntentAnalyzerAgent - デザイン意図理解Agent.

自然言語のデザインブリーフを解析し、以下を抽出:
- デザインカテゴリ(商品、ブランド、SNS等)
- メイン被写体と主要特徴
- ターゲットオーディエンスとスタイル方向性
- セット内の画像役割配分

使用例:
    >>> agent = IntentAnalyzerAgent()
    >>> result = await agent.run({
    ...     "brief": "アウトドアBluetoothスピーカーの商品画像",
    ...     "num_images": 8,
    ... })
"""

import logging
from typing import Any, ClassVar

from agentflow import ResilientAgent
from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
    DesignBriefInput,
    DesignCategory,
    ImageRole,
    IntentAnalysis,
)
from agentflow.utils import extract_json


# 被写体抽出時の前方コンテキスト単語数
_SUBJECT_CONTEXT_WORDS = 3


class IntentAnalyzerAgent(ResilientAgent[DesignBriefInput, IntentAnalysis]):
    """デザインブリーフを解析し、構造化された意図を抽出.

    LLM未使用時はルールベースにフォールバック。
    LLM使用時はより精度の高い解析を実行。
    """

    name = "IntentAnalyzerAgent"
    temperature = 0.3

    # カテゴリ判定キーワード
    CATEGORY_KEYWORDS: ClassVar[dict[DesignCategory, list[str]]] = {
        DesignCategory.PRODUCT_PHOTOGRAPHY: [
            "product",
            "商品",
            "goods",
            "item",
            "speaker",
            "phone",
            "amazon",
            "shopify",
            "e-commerce",
            "ecommerce",
            "プロダクト",
            "製品",
            "商品写真",
        ],
        DesignCategory.BRAND_IDENTITY: [
            "brand",
            "logo",
            "identity",
            "ブランド",
            "CI",
            "ロゴ",
        ],
        DesignCategory.SOCIAL_MEDIA: [
            "instagram",
            "tiktok",
            "social",
            "post",
            "story",
            "SNS",
            "ソーシャル",
            "投稿",
            "インスタ",
        ],
        DesignCategory.ADVERTISING: [
            "ad",
            "advertisement",
            "campaign",
            "banner",
            "広告",
            "キャンペーン",
            "バナー",
        ],
        DesignCategory.PACKAGING: [
            "package",
            "packaging",
            "box",
            "label",
            "パッケージ",
            "箱",
            "ラベル",
        ],
        DesignCategory.UI_MOCKUP: [
            "UI",
            "mockup",
            "wireframe",
            "app design",
            "画面",
            "モックアップ",
            "ワイヤーフレーム",
        ],
    }

    # デフォルト役割配分テンプレート
    ROLE_DISTRIBUTIONS: ClassVar[dict[int, list[ImageRole]]] = {
        1: [ImageRole.HERO],
        2: [ImageRole.HERO, ImageRole.FEATURE],
        3: [ImageRole.HERO, ImageRole.FEATURE, ImageRole.DETAIL],
        4: [ImageRole.HERO, ImageRole.FEATURE, ImageRole.FEATURE, ImageRole.LIFESTYLE],
    }

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    def _parse_input(self, input_data: dict[str, Any]) -> DesignBriefInput:
        """入力辞書をDesignBriefInputにパース."""
        return DesignBriefInput(**input_data)

    async def process(self, input_data: DesignBriefInput) -> IntentAnalysis:
        """デザインブリーフを解析."""
        if self._llm:
            return await self._analyze_with_llm(input_data)
        return self._analyze_rule_based(input_data)

    def _infer_category(self, brief: str, platform: str) -> DesignCategory:
        """ブリーフテキストとプラットフォームからデザインカテゴリを推定.

        優先順位: プラットフォーム固有 > キーワードスコアリング
        """
        text = f"{brief} {platform}".lower()

        # プラットフォーム固有の判定 -- 最優先
        platform_map: dict[str, DesignCategory] = {
            "instagram": DesignCategory.SOCIAL_MEDIA,
            "tiktok": DesignCategory.SOCIAL_MEDIA,
            "twitter": DesignCategory.SOCIAL_MEDIA,
            "amazon": DesignCategory.PRODUCT_PHOTOGRAPHY,
            "shopify": DesignCategory.PRODUCT_PHOTOGRAPHY,
        }
        if platform.lower() in platform_map:
            return platform_map[platform.lower()]

        # キーワードスコアリングでマッチ数を判定
        scores: dict[DesignCategory, int] = {}
        for category, keywords in self.CATEGORY_KEYWORDS.items():
            score = sum(1 for kw in keywords if kw.lower() in text)
            if score > 0:
                scores[category] = score

        if scores:
            return max(scores, key=scores.get)  # type: ignore[arg-type]
        return DesignCategory.PRODUCT_PHOTOGRAPHY

    def _distribute_roles(self, num_images: int) -> list[ImageRole]:
        """画像枚数に応じて役割を配分."""
        if num_images in self.ROLE_DISTRIBUTIONS:
            return self.ROLE_DISTRIBUTIONS[num_images]

        # 5枚以上: 1 hero + features + details + lifestyles
        roles: list[ImageRole] = [ImageRole.HERO]
        remaining = num_images - 1
        feature_count = min(remaining // 2, 4)
        detail_count = min((remaining - feature_count) // 2, 3)
        lifestyle_count = remaining - feature_count - detail_count

        roles.extend([ImageRole.FEATURE] * feature_count)
        roles.extend([ImageRole.DETAIL] * detail_count)
        roles.extend([ImageRole.LIFESTYLE] * lifestyle_count)
        return roles[:num_images]

    def _extract_subject(self, brief: str) -> str:
        """ブリーフからメイン被写体を抽出."""
        lower = brief.lower()
        # 「for a/an X」パターンを探索
        for marker in ["for a ", "for an ", "of a ", "of an ", "for "]:
            if marker in lower:
                idx = lower.index(marker) + len(marker)
                words = brief[idx:].split()[:5]
                return " ".join(words).rstrip(",.")
        # 日本語パターン: 「〜の」「〜を」
        for marker in ["の商品", "の画像", "の写真", "を生成", "を作成"]:
            if marker in brief:
                idx = brief.index(marker)
                # markerの前の部分を取得
                prefix = brief[:idx]
                words = prefix.split()
                if len(words) > _SUBJECT_CONTEXT_WORDS:
                    return " ".join(words[-_SUBJECT_CONTEXT_WORDS:])
                return prefix
        # フォールバック -- 最初の数単語
        return " ".join(brief.split()[:6])

    def _extract_features(self, brief: str) -> list[str]:
        """ブリーフから主要特徴を抽出."""
        features: list[str] = []
        # 特徴を示すキーワード
        feature_indicators = [
            "emphasize",
            "highlight",
            "featuring",
            "with",
            "強調",
            "特徴",
            "アピール",
        ]
        lower = brief.lower()
        for indicator in feature_indicators:
            if indicator in lower:
                idx = lower.index(indicator) + len(indicator)
                chunk = brief[idx:].split(",")[0].strip()
                if chunk:
                    features.append(chunk[:50])
        if not features:
            features.append("メインプロダクトビュー")
        return features[:5]

    def _analyze_rule_based(self, input_data: DesignBriefInput) -> IntentAnalysis:
        """ルールベース解析(LLM不使用時)."""
        category = self._infer_category(input_data.brief, input_data.target_platform)
        subject = self._extract_subject(input_data.brief)
        features = self._extract_features(input_data.brief)
        roles = self._distribute_roles(input_data.num_images)

        # スタイル方向性を構築
        style_parts: list[str] = []
        if input_data.style_preferences:
            style_parts.extend(input_data.style_preferences)
        style_parts.append(f"{category.value.lower().replace('_', ' ')} style")
        style_direction = ", ".join(style_parts)

        # プラットフォーム固有の制約
        platform_constraints: dict[str, str] = {}
        if input_data.target_platform == "amazon":
            platform_constraints["background"] = "白背景推奨"
            platform_constraints["min_resolution"] = "1000x1000"
        elif input_data.target_platform == "instagram":
            platform_constraints["aspect_ratio"] = "1:1 または 4:5"
            platform_constraints["safe_zone"] = "外側10%にテキスト配置を避ける"

        return IntentAnalysis(
            category=category,
            subject=subject,
            key_features=features,
            target_audience="",
            style_direction=style_direction,
            image_roles=roles,
            platform_constraints=platform_constraints,
        )

    async def _analyze_with_llm(self, input_data: DesignBriefInput) -> IntentAnalysis:
        """LLMを使用した高精度解析."""
        system_prompt = """あなたはデザイン意図分析の専門家です。
デザインブリーフを受け取り、以下を構造化JSONで出力してください:

1. category: PRODUCT_PHOTOGRAPHY, BRAND_IDENTITY, SOCIAL_MEDIA,
   ADVERTISING, PACKAGING, UI_MOCKUP のいずれか
2. subject: メインの被写体(最大100文字)
3. key_features: 強調すべき特徴(最大5つの配列)
4. target_audience: ターゲット層
5. style_direction: スタイル方向性(最大200文字)
6. image_roles: 画像役割の配列(HERO, FEATURE, DETAIL, LIFESTYLE, COMPARISON, INFOGRAPHIC)

JSON形式のみで出力してください。"""

        user_prompt = f"""【デザインブリーフ】
{input_data.brief}

【スタイル指定】
{", ".join(input_data.style_preferences) if input_data.style_preferences else "なし"}

【プラットフォーム】
{input_data.target_platform or "汎用"}

【画像枚数】
{input_data.num_images}枚"""

        response = await self._call_llm(f"{system_prompt}\n\n{user_prompt}")
        data = extract_json(response)

        if data is None:
            return self._analyze_rule_based(input_data)

        try:
            return IntentAnalysis(
                category=DesignCategory(data.get("category", "PRODUCT_PHOTOGRAPHY")),
                subject=data.get("subject", "")[:100],
                key_features=data.get("key_features", ["メインビュー"])[:5],
                target_audience=data.get("target_audience", "")[:100],
                style_direction=data.get("style_direction", "")[:200],
                image_roles=[ImageRole(r) for r in data.get("image_roles", ["HERO"])][
                    : input_data.num_images
                ],
                platform_constraints=data.get("platform_constraints", {}),
            )
        except (ValueError, KeyError):
            return self._analyze_rule_based(input_data)

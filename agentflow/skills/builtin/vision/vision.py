# -*- coding: utf-8 -*-
"""Vision Skill - 画像認識・分析機能.

GPT-4V / Claude Vision / Gemini Vision を使用した画像認識スキル。

Example:
    >>> from agentflow.skills import VisionSkill
    >>>
    >>> # スキル作成
    >>> vision = VisionSkill()
    >>>
    >>> # 画像分析
    >>> result = await vision.analyze_image(
    ...     image_url="https://example.com/image.jpg",
    ...     prompt="この画像に何が写っていますか？",
    ... )
    >>> print(result["description"])
    >>>
    >>> # OCR（テキスト抽出）
    >>> text = await vision.extract_text(image_url="https://example.com/doc.png")
    >>> print(text)
"""

from __future__ import annotations

import base64
import logging
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any

import httpx

from agentflow import get_llm


logger = logging.getLogger(__name__)


class VisionProvider(str, Enum):
    """Vision プロバイダー."""

    OPENAI = "openai"  # GPT-4V
    ANTHROPIC = "anthropic"  # Claude Vision
    GOOGLE = "google"  # Gemini Vision
    AUTO = "auto"  # 自動検出


@dataclass
class VisionConfig:
    """Vision スキル設定.

    Attributes:
        provider: Vision プロバイダー
        model: モデル名（プロバイダー固有）
        max_tokens: 最大出力トークン数
        detail: 画像詳細度（low, high, auto）
        timeout: タイムアウト（秒）
    """

    provider: VisionProvider = VisionProvider.AUTO
    model: str | None = None
    max_tokens: int = 1024
    detail: str = "auto"
    timeout: int = 60


@dataclass
class VisionResult:
    """Vision 分析結果.

    Attributes:
        description: 画像の説明
        objects: 検出されたオブジェクト
        text: 抽出されたテキスト（OCR）
        labels: ラベル/タグ
        confidence: 信頼度スコア
        raw_response: 生のレスポンス
    """

    description: str = ""
    objects: list[str] = field(default_factory=list)
    text: str = ""
    labels: list[str] = field(default_factory=list)
    confidence: float = 0.0
    raw_response: dict[str, Any] = field(default_factory=dict)


class VisionSkill:
    """画像認識・分析スキル.

    GPT-4V / Claude Vision / Gemini Vision を使用した
    マルチモーダル画像分析機能を提供。

    Features:
    - 画像説明生成
    - オブジェクト検出
    - OCR（テキスト抽出）
    - 画像比較
    - カスタムプロンプト分析
    """

    # プロバイダー別デフォルトモデル
    _DEFAULT_MODELS = {
        VisionProvider.OPENAI: "gpt-4o",
        VisionProvider.ANTHROPIC: "claude-3-5-sonnet-20241022",
        VisionProvider.GOOGLE: "gemini-1.5-pro",
    }

    def __init__(self, config: VisionConfig | None = None) -> None:
        """Vision スキルを初期化.

        Args:
            config: Vision 設定
        """
        self._config = config or VisionConfig()
        self._logger = logging.getLogger("vision_skill")
        self._http_client = httpx.AsyncClient(timeout=self._config.timeout)

    async def analyze_image(
        self,
        image_url: str | None = None,
        image_path: str | Path | None = None,
        image_base64: str | None = None,
        prompt: str = "この画像を詳しく説明してください。",
        *,
        extract_objects: bool = False,
        extract_text: bool = False,
    ) -> VisionResult:
        """画像を分析.

        Args:
            image_url: 画像 URL
            image_path: ローカル画像パス
            image_base64: Base64 エンコード画像
            prompt: 分析プロンプト
            extract_objects: オブジェクト検出を行うか
            extract_text: OCR を行うか

        Returns:
            分析結果

        Raises:
            ValueError: 画像ソースが指定されていない場合
        """
        # 画像データを準備
        image_data = await self._prepare_image(image_url, image_path, image_base64)

        # プロンプトを構築
        full_prompt = prompt
        if extract_objects:
            full_prompt += "\n\n画像内のオブジェクトをリストアップしてください。"
        if extract_text:
            full_prompt += "\n\n画像内のテキストを全て抽出してください。"

        # LLM で分析
        try:
            llm = get_llm(temperature=0.3)
            response = await self._call_vision_api(llm, image_data, full_prompt)

            return self._parse_response(response, extract_objects, extract_text)

        except Exception as e:
            self._logger.error(f"Vision analysis failed: {e}", exc_info=True)
            raise

    async def extract_text(
        self,
        image_url: str | None = None,
        image_path: str | Path | None = None,
        image_base64: str | None = None,
        language: str = "auto",
    ) -> str:
        """画像からテキストを抽出（OCR）.

        Args:
            image_url: 画像 URL
            image_path: ローカル画像パス
            image_base64: Base64 エンコード画像
            language: 言語ヒント（auto, ja, en, zh 等）

        Returns:
            抽出されたテキスト
        """
        lang_hint = ""
        if language != "auto":
            lang_hint = f"（言語: {language}）"

        prompt = f"""この画像に含まれるテキストを全て抽出してください{lang_hint}。
テキストのみを出力し、説明は不要です。
レイアウトを可能な限り保持してください。"""

        result = await self.analyze_image(
            image_url=image_url,
            image_path=image_path,
            image_base64=image_base64,
            prompt=prompt,
        )
        return result.description

    async def describe_image(
        self,
        image_url: str | None = None,
        image_path: str | Path | None = None,
        image_base64: str | None = None,
        style: str = "detailed",
    ) -> str:
        """画像の説明を生成.

        Args:
            image_url: 画像 URL
            image_path: ローカル画像パス
            image_base64: Base64 エンコード画像
            style: 説明スタイル（detailed, brief, technical, creative）

        Returns:
            画像の説明
        """
        style_prompts = {
            "detailed": "この画像を詳細に説明してください。",
            "brief": "この画像を1-2文で簡潔に説明してください。",
            "technical": "この画像を技術的な観点から分析してください。",
            "creative": "この画像からインスピレーションを得て、創造的な説明を書いてください。",
        }

        prompt = style_prompts.get(style, style_prompts["detailed"])

        result = await self.analyze_image(
            image_url=image_url,
            image_path=image_path,
            image_base64=image_base64,
            prompt=prompt,
        )
        return result.description

    async def compare_images(
        self,
        images: list[dict[str, str]],
        comparison_prompt: str = "これらの画像を比較し、類似点と相違点を説明してください。",
    ) -> str:
        """複数の画像を比較.

        Args:
            images: 画像リスト（各要素は url, path, base64 のいずれかを含む dict）
            comparison_prompt: 比較プロンプト

        Returns:
            比較結果
        """
        # 全画像を準備
        image_data_list = []
        for img in images:
            data = await self._prepare_image(
                img.get("url"),
                img.get("path"),
                img.get("base64"),
            )
            image_data_list.append(data)

        # マルチ画像分析
        try:
            llm = get_llm(temperature=0.3)
            response = await self._call_vision_api_multi(
                llm, image_data_list, comparison_prompt
            )
            return response.get("content", "")
        except Exception as e:
            self._logger.error(f"Image comparison failed: {e}", exc_info=True)
            raise

    async def detect_objects(
        self,
        image_url: str | None = None,
        image_path: str | Path | None = None,
        image_base64: str | None = None,
    ) -> list[str]:
        """画像内のオブジェクトを検出.

        Args:
            image_url: 画像 URL
            image_path: ローカル画像パス
            image_base64: Base64 エンコード画像

        Returns:
            検出されたオブジェクトのリスト
        """
        prompt = """この画像に写っているオブジェクトを全てリストアップしてください。
各オブジェクトを1行に1つずつ、箇条書きで出力してください。
説明は不要です。"""

        result = await self.analyze_image(
            image_url=image_url,
            image_path=image_path,
            image_base64=image_base64,
            prompt=prompt,
        )

        # 箇条書きをパース
        objects = []
        for line in result.description.split("\n"):
            line = line.strip().lstrip("-•*").strip()
            if line:
                objects.append(line)
        return objects

    async def _prepare_image(
        self,
        image_url: str | None,
        image_path: str | Path | None,
        image_base64: str | None,
    ) -> dict[str, Any]:
        """画像データを準備.

        Args:
            image_url: 画像 URL
            image_path: ローカル画像パス
            image_base64: Base64 エンコード画像

        Returns:
            画像データ dict

        Raises:
            ValueError: 画像ソースが指定されていない場合
        """
        if image_url:
            return {"type": "url", "url": image_url}

        if image_path:
            path = Path(image_path)
            if not path.exists():
                msg = f"Image file not found: {path}"
                raise ValueError(msg)

            with open(path, "rb") as f:
                data = base64.b64encode(f.read()).decode()

            # MIME タイプを推定
            suffix = path.suffix.lower()
            mime_types = {
                ".jpg": "image/jpeg",
                ".jpeg": "image/jpeg",
                ".png": "image/png",
                ".gif": "image/gif",
                ".webp": "image/webp",
            }
            mime_type = mime_types.get(suffix, "image/jpeg")

            return {"type": "base64", "data": data, "mime_type": mime_type}

        if image_base64:
            return {"type": "base64", "data": image_base64, "mime_type": "image/jpeg"}

        msg = "No image source provided. Specify image_url, image_path, or image_base64."
        raise ValueError(msg)

    async def _call_vision_api(
        self,
        llm: Any,
        image_data: dict[str, Any],
        prompt: str,
    ) -> dict[str, Any]:
        """Vision API を呼び出し.

        Args:
            llm: LLM インスタンス
            image_data: 画像データ
            prompt: プロンプト

        Returns:
            API レスポンス
        """
        # OpenAI 形式のメッセージを構築
        content: list[dict[str, Any]] = [{"type": "text", "text": prompt}]

        if image_data["type"] == "url":
            content.append({
                "type": "image_url",
                "image_url": {
                    "url": image_data["url"],
                    "detail": self._config.detail,
                },
            })
        else:
            data_url = f"data:{image_data['mime_type']};base64,{image_data['data']}"
            content.append({
                "type": "image_url",
                "image_url": {
                    "url": data_url,
                    "detail": self._config.detail,
                },
            })

        messages = [{"role": "user", "content": content}]

        return await llm.chat(messages, max_tokens=self._config.max_tokens)

    async def _call_vision_api_multi(
        self,
        llm: Any,
        image_data_list: list[dict[str, Any]],
        prompt: str,
    ) -> dict[str, Any]:
        """複数画像の Vision API を呼び出し.

        Args:
            llm: LLM インスタンス
            image_data_list: 画像データリスト
            prompt: プロンプト

        Returns:
            API レスポンス
        """
        content: list[dict[str, Any]] = [{"type": "text", "text": prompt}]

        for image_data in image_data_list:
            if image_data["type"] == "url":
                content.append({
                    "type": "image_url",
                    "image_url": {
                        "url": image_data["url"],
                        "detail": self._config.detail,
                    },
                })
            else:
                data_url = f"data:{image_data['mime_type']};base64,{image_data['data']}"
                content.append({
                    "type": "image_url",
                    "image_url": {
                        "url": data_url,
                        "detail": self._config.detail,
                    },
                })

        messages = [{"role": "user", "content": content}]

        return await llm.chat(messages, max_tokens=self._config.max_tokens)

    def _parse_response(
        self,
        response: dict[str, Any],
        extract_objects: bool,
        extract_text: bool,
    ) -> VisionResult:
        """レスポンスをパース.

        Args:
            response: API レスポンス
            extract_objects: オブジェクト抽出フラグ
            extract_text: テキスト抽出フラグ

        Returns:
            パース結果
        """
        content = response.get("content", "")

        result = VisionResult(
            description=content,
            raw_response=response,
        )

        # オブジェクト抽出
        if extract_objects:
            objects = []
            for line in content.split("\n"):
                line = line.strip().lstrip("-•*").strip()
                if line and len(line) < 50:  # 短い行をオブジェクトとして扱う
                    objects.append(line)
            result.objects = objects

        # テキスト抽出（OCR 結果は description に含まれる）
        if extract_text:
            result.text = content

        return result

    async def close(self) -> None:
        """HTTP クライアントをクローズ."""
        await self._http_client.aclose()


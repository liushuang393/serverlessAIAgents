"""OpenAI 画像生成 非同期HTTPクライアント.

ComfyUI が利用不可の場合のフォールバックとして使用:
- gpt-image-1 モデルによる画像生成
- SDXL解像度からOpenAI対応解像度へのマッピング

参照: OpenAI Images API (POST /v1/images/generations)

使用例:
    >>> client = OpenAIImageClient()
    >>> image_bytes = await client.generate_image(
    ...     prompt="a professional product photo of a bluetooth speaker",
    ...     width=1024,
    ...     height=1024,
    ... )
"""

from __future__ import annotations

import logging
import os
from typing import TYPE_CHECKING

from infrastructure.llm.gateway import LiteLLMGateway
from kernel.runtime import get_runtime_context


if TYPE_CHECKING:
    from pathlib import Path


logger = logging.getLogger(__name__)


def _gateway_metadata() -> dict[str, str]:
    """現在の RuntimeContext から metadata を抽出する."""
    context = get_runtime_context()
    if context is None:
        return {}
    payload: dict[str, str] = {}
    app_name = context.metadata.get("app_name")
    if isinstance(app_name, str) and app_name.strip():
        payload["app_name"] = app_name.strip()
    agent_name = context.metadata.get("agent_name")
    if isinstance(agent_name, str) and agent_name.strip():
        payload["agent_name"] = agent_name.strip()
    return payload


# OpenAI gpt-image-1 対応サイズ
_OPENAI_SIZES = frozenset({"1024x1024", "1536x1024", "1024x1536"})

# SDXL解像度 → OpenAI対応サイズ マッピング
_SIZE_MAP: dict[tuple[int, int], str] = {
    (1024, 1024): "1024x1024",
    (1344, 768): "1536x1024",  # 16:9 landscape
    (768, 1344): "1024x1536",  # 9:16 portrait
    (1152, 896): "1536x1024",  # 4:3 landscape
    (896, 1152): "1024x1536",  # 3:4 portrait
    (896, 1120): "1024x1536",  # 4:5 portrait
}

# HTTP status codes
_HTTP_OK = 200


def map_size_to_openai(width: int, height: int) -> str:
    """SDXL幅/高さをOpenAI対応サイズにマッピング.

    Args:
        width: 画像幅(px)
        height: 画像高さ(px)

    Returns:
        OpenAI Images API対応のサイズ文字列
    """
    if (width, height) in _SIZE_MAP:
        return _SIZE_MAP[(width, height)]
    if width > height:
        return "1536x1024"
    if height > width:
        return "1024x1536"
    return "1024x1024"


class OpenAIImageClient:
    """OpenAI Images API 非同期クライアント.

    ComfyUI がローカルで利用不可な場合にフォールバックとして使用。
    gpt-image-1 モデルで画像を生成する。

    Args:
        api_key: OpenAI APIキー(デフォルト: OPENAI_API_KEY 環境変数)
        model: 画像生成モデル名(デフォルト: OPENAI_IMAGE_MODEL 環境変数 or gpt-image-1)
        timeout: HTTPタイムアウト(秒)
    """

    def __init__(
        self,
        api_key: str | None = None,
        model: str | None = None,
        timeout: float = 120.0,
    ) -> None:
        """初期化."""
        self._api_key = api_key or os.getenv("OPENAI_API_KEY", "")
        self._model = model or os.getenv("OPENAI_IMAGE_MODEL", "gpt-image-1")
        self._gateway = LiteLLMGateway()
        self._role = os.getenv("IMAGE_ROLE", "cheap")
        self._model_alias = os.getenv("OPENAI_IMAGE_MODEL_ALIAS")
        self._timeout = timeout
        self._logger = logging.getLogger("design_skills.openai_image_client")

    async def close(self) -> None:
        """互換API（no-op）."""

    async def health_check(self) -> bool:
        """OpenAI APIの利用可能性を確認.

        APIキーが設定されており、APIに到達可能であればTrueを返す。

        Returns:
            APIが利用可能ならTrue、そうでなければFalse
        """
        return bool(self._api_key or os.getenv("OPENAI_API_KEY"))

    async def generate_image(
        self,
        prompt: str,
        *,
        width: int = 1024,
        height: int = 1024,
        quality: str = "high",
        output_format: str = "png",
    ) -> bytes:
        """画像を生成してバイトデータを返す.

        Args:
            prompt: 画像生成プロンプト
            width: 希望する画像幅(OpenAI対応サイズにマッピングされる)
            height: 希望する画像高さ(OpenAI対応サイズにマッピングされる)
            quality: 品質レベル('low', 'medium', 'high')
            output_format: 出力フォーマット('png', 'jpeg', 'webp')

        Returns:
            生成画像のバイトデータ

        Raises:
            httpx.HTTPStatusError: API呼び出しが失敗した場合
        """
        size = map_size_to_openai(width, height)

        self._logger.info(f"OpenAI画像生成: model={self._model}, size={size}, quality={quality}")
        current = os.getenv("OPENAI_API_KEY")
        inserted = False
        if not current and self._api_key:
            os.environ["OPENAI_API_KEY"] = self._api_key
            inserted = True

        try:
            return await self._gateway.generate_image(
                role=self._role,
                prompt=prompt,
                size=size,
                quality=quality,
                output_format=output_format,
                model_alias=self._model_alias,
                model=self._model,
                metadata=_gateway_metadata(),
            )
        finally:
            if inserted:
                os.environ.pop("OPENAI_API_KEY", None)

    async def generate_and_save(
        self,
        prompt: str,
        output_path: Path,
        *,
        width: int = 1024,
        height: int = 1024,
        quality: str = "high",
    ) -> Path:
        """画像を生成してファイルに保存.

        Args:
            prompt: 画像生成プロンプト
            output_path: 保存先パス
            width: 希望する画像幅
            height: 希望する画像高さ
            quality: 品質レベル

        Returns:
            保存したファイルのパス
        """
        image_bytes = await self.generate_image(
            prompt,
            width=width,
            height=height,
            quality=quality,
        )
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_bytes(image_bytes)
        self._logger.info(f"OpenAI経由で画像を保存: {output_path}")
        return output_path

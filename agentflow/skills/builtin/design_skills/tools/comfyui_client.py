"""ComfyUI 非同期HTTPクライアント.

ComfyUI サーバーとの通信を担当:
- ワークフロープロンプトのキューイング
- 完了までのポーリング
- 生成画像のダウンロード

参照: ComfyUI API (POST /prompt, GET /history, GET /view)

使用例:
    >>> client = ComfyUIClient(base_url="http://localhost:8188")
    >>> workflow = client.build_workflow_payload(style, spec)
    >>> prompt_id = await client.queue_prompt(workflow)
    >>> history = await client.poll_until_complete(prompt_id)
    >>> image_bytes = await client.get_image("output.png")
"""

import asyncio
import logging
import os
import time
from typing import Any

import httpx

from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
    GlobalStyle,
    ImageSpec,
)


logger = logging.getLogger(__name__)

# HTTP status codes
_HTTP_OK = 200


class ComfyUIClient:
    """ComfyUI サーバー向け非同期HTTPクライアント.

    Args:
        base_url: ComfyUI サーバーURL(デフォルト: COMFYUI_URL 環境変数)
        timeout: HTTPタイムアウト(秒)
    """

    def __init__(
        self,
        base_url: str | None = None,
        timeout: float = 120.0,
    ) -> None:
        """初期化."""
        self._base_url = base_url or os.getenv("COMFYUI_URL", "http://localhost:8188")
        self._http_client = httpx.AsyncClient(
            base_url=self._base_url,
            timeout=timeout,
        )
        self._logger = logging.getLogger("design_skills.comfyui_client")

    async def close(self) -> None:
        """HTTPクライアントを閉じる."""
        await self._http_client.aclose()

    def build_workflow_payload(
        self,
        style: GlobalStyle,
        spec: ImageSpec,
    ) -> dict[str, Any]:
        """スタイル+画像仕様からComfyUI APIワークフローJSONを構築.

        標準SDXL txt2imgワークフローを構成:
        - CheckpointLoaderSimple → モデル読込
        - CLIPTextEncode(正)→ グローバル+画像固有プロンプトのマージ
        - CLIPTextEncode(負)→ グローバル+画像固有ネガティブ
        - KSampler → 生成パラメータ
        - VAEDecode → 潜在空間デコード
        - SaveImage → 出力保存

        Args:
            style: グローバルスタイル定義
            spec: 個別画像仕様

        Returns:
            ComfyUI互換のワークフロー辞書
        """
        # ポジティブプロンプトのマージ -- グローバルコンテキスト + 画像固有
        style_context = (
            f"{', '.join(style.color_palette)} color scheme, "
            f"{style.lighting}, {style.camera_angle}, {style.mood}"
        )
        positive_prompt = f"{spec.prompt}, {style_context}"

        # ネガティブプロンプトのマージ
        negative_prompt = style.negative_prompt
        if spec.extra_negative:
            negative_prompt = f"{negative_prompt}, {spec.extra_negative}"

        workflow: dict[str, Any] = {
            "1": {
                "class_type": "CheckpointLoaderSimple",
                "inputs": {
                    "ckpt_name": style.base_model,
                },
            },
            "2": {
                "class_type": "CLIPTextEncode",
                "inputs": {
                    "text": positive_prompt,
                    "clip": ["1", 1],
                },
            },
            "3": {
                "class_type": "CLIPTextEncode",
                "inputs": {
                    "text": negative_prompt,
                    "clip": ["1", 1],
                },
            },
            "4": {
                "class_type": "EmptyLatentImage",
                "inputs": {
                    "width": spec.width,
                    "height": spec.height,
                    "batch_size": 1,
                },
            },
            "5": {
                "class_type": "KSampler",
                "inputs": {
                    "model": ["1", 0],
                    "positive": ["2", 0],
                    "negative": ["3", 0],
                    "latent_image": ["4", 0],
                    "seed": spec.seed,
                    "steps": spec.steps,
                    "cfg": spec.cfg_scale,
                    "sampler_name": spec.sampler,
                    "scheduler": "normal",
                    "denoise": 1.0,
                },
            },
            "6": {
                "class_type": "VAEDecode",
                "inputs": {
                    "samples": ["5", 0],
                    "vae": ["1", 2],
                },
            },
            "7": {
                "class_type": "SaveImage",
                "inputs": {
                    "images": ["6", 0],
                    "filename_prefix": spec.image_id,
                },
            },
        }
        return workflow

    async def queue_prompt(self, workflow: dict[str, Any]) -> str:
        """ワークフローをキューに投入.

        Args:
            workflow: ComfyUIワークフロー辞書(build_workflow_payloadの戻り値)

        Returns:
            ComfyUIからのprompt_id

        Raises:
            httpx.HTTPStatusError: サーバーがエラーを返した場合
        """
        # ComfyUI APIは {"prompt": workflow} 形式を期待
        payload = {"prompt": workflow}
        response = await self._http_client.post("/prompt", json=payload)
        
        # エラー時は詳細をログ出力
        if response.status_code != _HTTP_OK:
            try:
                error_detail = response.json()
                self._logger.error(f"ComfyUI エラー応答: {error_detail}")
            except Exception:
                self._logger.error(f"ComfyUI エラー応答 (raw): {response.text}")
        
        response.raise_for_status()
        data = response.json()
        prompt_id: str = data["prompt_id"]
        self._logger.info(f"プロンプトをキューに投入: {prompt_id}")
        return prompt_id

    async def poll_until_complete(
        self,
        prompt_id: str,
        poll_interval: float = 1.0,
        max_wait: float = 300.0,
    ) -> dict[str, Any]:
        """プロンプト完了までComfyUI履歴をポーリング.

        Args:
            prompt_id: 待機するプロンプトID
            poll_interval: ポーリング間隔(秒)
            max_wait: 最大待機時間(秒)

        Returns:
            完了したプロンプトの履歴エントリ

        Raises:
            TimeoutError: max_waitを超過した場合
        """
        start = time.monotonic()
        while (time.monotonic() - start) < max_wait:
            response = await self._http_client.get(f"/history/{prompt_id}")
            if response.status_code == _HTTP_OK:
                history = response.json()
                if prompt_id in history:
                    return history[prompt_id]
            await asyncio.sleep(poll_interval)

        msg = f"ComfyUIプロンプト {prompt_id} が {max_wait}秒以内に完了しませんでした"
        raise TimeoutError(msg)

    async def get_image(
        self,
        filename: str,
        subfolder: str = "",
        folder_type: str = "output",
    ) -> bytes:
        """生成画像をComfyUIからダウンロード.

        Args:
            filename: 画像ファイル名
            subfolder: 出力ディレクトリ内のサブフォルダ
            folder_type: フォルダタイプ('output', 'input', 'temp')

        Returns:
            画像バイトデータ
        """
        params = {
            "filename": filename,
            "subfolder": subfolder,
            "type": folder_type,
        }
        response = await self._http_client.get("/view", params=params)
        response.raise_for_status()
        return response.content

    async def health_check(self) -> bool:
        """ComfyUIサーバーの到達可能性を確認.

        Returns:
            サーバーが応答すればTrue、そうでなければFalse
        """
        try:
            response = await self._http_client.get("/prompt")
        except httpx.HTTPError:
            return False
        else:
            return response.status_code == _HTTP_OK

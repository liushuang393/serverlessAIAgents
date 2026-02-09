"""ComfyUI クライアントのテスト."""

from unittest.mock import AsyncMock, MagicMock, patch


class TestComfyUIClient:
    """ComfyUI クライアントテスト（HTTPモック使用）."""

    def _make_client(self):
        """テスト用クライアントインスタンスを生成."""
        from agentflow.skills.builtin.design_skills.tools.comfyui_client import ComfyUIClient

        return ComfyUIClient(base_url="http://localhost:8188")

    async def test_build_workflow_payload(self) -> None:
        """ImageSpecからワークフローJSONを正しく構築できること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
        )

        client = self._make_client()
        style = GlobalStyle(
            color_palette=["black"],
            lighting="studio lighting",
            camera_angle="front",
            mood="professional",
            negative_prompt="blurry, low quality",
        )
        spec = ImageSpec(
            image_id="img_001",
            role=ImageRole.HERO,
            prompt="Speaker on black background",
            seed=42,
            width=1024,
            height=1024,
            cfg_scale=7.0,
            steps=30,
            sampler="euler_ancestral",
        )
        payload = client.build_workflow_payload(style, spec)

        assert isinstance(payload, dict)
        assert "prompt" in payload

        # KSampler ノードの検証
        ksampler = None
        for node in payload["prompt"].values():
            if node.get("class_type") == "KSampler":
                ksampler = node
                break
        assert ksampler is not None
        assert ksampler["inputs"]["seed"] == 42
        assert ksampler["inputs"]["steps"] == 30
        assert ksampler["inputs"]["cfg"] == 7.0

    async def test_build_workflow_merges_prompts(self) -> None:
        """グローバルスタイルと画像固有プロンプトがマージされること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
        )

        client = self._make_client()
        style = GlobalStyle(
            color_palette=["black", "neon"],
            lighting="dramatic",
            camera_angle="45-degree",
            mood="futuristic",
            negative_prompt="blurry",
        )
        spec = ImageSpec(
            image_id="img_001",
            role=ImageRole.HERO,
            prompt="Speaker hero shot",
            seed=42,
        )
        payload = client.build_workflow_payload(style, spec)

        # ポジティブプロンプトにスタイル情報が含まれること
        positive_node = payload["prompt"]["2"]
        assert "Speaker hero shot" in positive_node["inputs"]["text"]
        assert "dramatic" in positive_node["inputs"]["text"]

    async def test_queue_prompt_success(self) -> None:
        """プロンプトキューイングが成功し、prompt_idが返ること."""
        client = self._make_client()
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"prompt_id": "abc-123"}
        mock_response.raise_for_status = MagicMock()

        with patch.object(client, "_http_client") as mock_http:
            mock_http.post = AsyncMock(return_value=mock_response)
            prompt_id = await client.queue_prompt({"prompt": {}})

        assert prompt_id == "abc-123"

    async def test_get_image_returns_bytes(self) -> None:
        """生成画像の取得でバイトデータが返ること."""
        client = self._make_client()
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.content = b"\x89PNG fake image data"
        mock_response.raise_for_status = MagicMock()

        with patch.object(client, "_http_client") as mock_http:
            mock_http.get = AsyncMock(return_value=mock_response)
            data = await client.get_image("test.png", "subfolder", "output")

        assert data == b"\x89PNG fake image data"

    async def test_health_check_success(self) -> None:
        """ヘルスチェック成功時にTrueが返ること."""
        client = self._make_client()
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"exec_info": {"queue_remaining": 0}}

        with patch.object(client, "_http_client") as mock_http:
            mock_http.get = AsyncMock(return_value=mock_response)
            healthy = await client.health_check()

        assert healthy is True

    async def test_health_check_failure(self) -> None:
        """ヘルスチェック失敗時にFalseが返ること."""
        import httpx

        client = self._make_client()

        with patch.object(client, "_http_client") as mock_http:
            mock_http.get = AsyncMock(side_effect=httpx.ConnectError("refused"))
            healthy = await client.health_check()

        assert healthy is False

    async def test_build_workflow_extra_negative(self) -> None:
        """画像固有のネガティブプロンプトがマージされること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
        )

        client = self._make_client()
        style = GlobalStyle(
            color_palette=["black"],
            lighting="studio",
            camera_angle="front",
            mood="pro",
            negative_prompt="blurry",
        )
        spec = ImageSpec(
            image_id="img_001",
            role=ImageRole.DETAIL,
            prompt="close-up",
            seed=42,
            extra_negative="text overlay",
        )
        payload = client.build_workflow_payload(style, spec)

        negative_node = payload["prompt"]["3"]
        assert "blurry" in negative_node["inputs"]["text"]
        assert "text overlay" in negative_node["inputs"]["text"]

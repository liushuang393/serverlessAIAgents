"""E2Eテスト用フィクスチャ.

ComfyUIサーバーの可用性を検出し、サーバーが利用できない場合は
テストを自動スキップする。
"""

import os
from pathlib import Path

import httpx
import pytest

from agentflow.skills.builtin.design_skills.tools.comfyui_client import ComfyUIClient


@pytest.fixture(scope="session")
def comfyui_url() -> str:
    """ComfyUIサーバーURLを環境変数から取得."""
    return os.getenv("COMFYUI_URL", "http://localhost:8188")


@pytest.fixture(scope="session")
async def comfyui_available(comfyui_url: str) -> bool:
    """ComfyUIサーバーの到達可能性を一度だけ確認."""
    try:
        async with httpx.AsyncClient(timeout=5.0) as client:
            response = await client.get(f"{comfyui_url}/prompt")
            return response.status_code == 200
    except httpx.HTTPError:
        return False


@pytest.fixture(autouse=True)
def skip_if_no_comfyui(request: pytest.FixtureRequest) -> None:
    """ComfyUI依存のe2eテストのみ、ComfyUI未起動時に自動スキップ."""
    if not request.node.get_closest_marker("e2e"):
        return
    if request.node.get_closest_marker("real_llm"):
        return

    comfyui_available = request.getfixturevalue("comfyui_available")
    if not comfyui_available:
        pytest.skip("ComfyUI server unavailable")


@pytest.fixture
async def comfyui_client(comfyui_url: str) -> ComfyUIClient:
    """ComfyUIクライアントを生成し、テスト後にクローズ."""
    client = ComfyUIClient(base_url=comfyui_url, timeout=300.0)
    yield client  # type: ignore[misc]
    await client.close()


@pytest.fixture
def e2e_output_dir(tmp_path: Path) -> Path:
    """E2Eテスト用の一時出力ディレクトリ."""
    output = tmp_path / "e2e_output"
    output.mkdir()
    return output

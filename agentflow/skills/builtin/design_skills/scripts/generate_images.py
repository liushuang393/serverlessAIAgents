#!/usr/bin/env python3
"""Standalone image generation script with ComfyUI/OpenAI fallback.

Reads JSON config from stdin, generates an image via ComfyUI (local) or
OpenAI gpt-image-1 (cloud fallback), and writes result JSON to stdout.

Fallback priority:
  1. ComfyUI (local GPU) - if server is running
  2. OpenAI gpt-image-1 (cloud) - if OPENAI_API_KEY is set

This script does NOT import from agentflow -- it is fully self-contained
so it works when the skill folder is copied to .claude/skills/.

Dependencies: httpx (pip install httpx)

Usage:
    echo '{"prompt": "a cat", "width": 1024, "height": 1024}' | python generate_images.py
"""

from __future__ import annotations

import base64
import json
import os
import sys
import time
import uuid
from pathlib import Path

import httpx


COMFYUI_URL = os.getenv("COMFYUI_URL", "http://localhost:8188")
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY", "")
OPENAI_IMAGE_MODEL = os.getenv("OPENAI_IMAGE_MODEL", "gpt-image-1")
OPENAI_API_URL = "https://api.openai.com/v1"

DEFAULT_MODEL = "sd_xl_base_1.0.safetensors"
DEFAULT_NEGATIVE = (
    "blurry, low quality, text, watermark, deformed, ugly, "
    "oversaturated, cropped, out of frame, bad anatomy, "
    "bad proportions, duplicate"
)
DEFAULT_STEPS = 20
DEFAULT_CFG = 7.0
DEFAULT_WIDTH = 1024
DEFAULT_HEIGHT = 1024
POLL_INTERVAL = 1.0
MAX_WAIT = 300.0

# HTTP status codes
_HTTP_OK = 200

# OpenAI supported sizes
_OPENAI_SIZE_MAP: dict[tuple[int, int], str] = {
    (1024, 1024): "1024x1024",
    (1344, 768): "1536x1024",
    (768, 1344): "1024x1536",
    (1152, 896): "1536x1024",
    (896, 1152): "1024x1536",
    (896, 1120): "1024x1536",
}


# =========================================================================
# ComfyUI backend
# =========================================================================


def build_workflow(config: dict) -> dict:
    """Build a ComfyUI SDXL txt2img workflow payload."""
    prompt = config["prompt"]
    negative = config.get("negative_prompt", DEFAULT_NEGATIVE)
    width = config.get("width", DEFAULT_WIDTH)
    height = config.get("height", DEFAULT_HEIGHT)
    seed = config.get("seed", int(time.time()) % (2**32))
    steps = config.get("steps", DEFAULT_STEPS)
    cfg = config.get("cfg_scale", DEFAULT_CFG)
    model = config.get("model", DEFAULT_MODEL)
    image_id = config.get("image_id", f"img_{uuid.uuid4().hex[:8]}")

    return {
        "prompt": {
            "1": {
                "class_type": "CheckpointLoaderSimple",
                "inputs": {"ckpt_name": model},
            },
            "2": {
                "class_type": "CLIPTextEncode",
                "inputs": {"text": prompt, "clip": ["1", 1]},
            },
            "3": {
                "class_type": "CLIPTextEncode",
                "inputs": {"text": negative, "clip": ["1", 1]},
            },
            "4": {
                "class_type": "EmptyLatentImage",
                "inputs": {"width": width, "height": height, "batch_size": 1},
            },
            "5": {
                "class_type": "KSampler",
                "inputs": {
                    "model": ["1", 0],
                    "positive": ["2", 0],
                    "negative": ["3", 0],
                    "latent_image": ["4", 0],
                    "seed": seed,
                    "steps": steps,
                    "cfg": cfg,
                    "sampler_name": "euler",
                    "scheduler": "normal",
                    "denoise": 1.0,
                },
            },
            "6": {
                "class_type": "VAEDecode",
                "inputs": {"samples": ["5", 0], "vae": ["1", 2]},
            },
            "7": {
                "class_type": "SaveImage",
                "inputs": {"images": ["6", 0], "filename_prefix": image_id},
            },
        }
    }


def queue_prompt(client: httpx.Client, workflow: dict) -> str:
    """Submit workflow to ComfyUI queue."""
    resp = client.post("/prompt", json=workflow)
    resp.raise_for_status()
    return resp.json()["prompt_id"]


def poll_until_complete(
    client: httpx.Client,
    prompt_id: str,
) -> dict:
    """Poll ComfyUI history until prompt completes."""
    start = time.monotonic()
    while (time.monotonic() - start) < MAX_WAIT:
        resp = client.get(f"/history/{prompt_id}")
        if resp.status_code == _HTTP_OK:
            history = resp.json()
            if prompt_id in history:
                return history[prompt_id]
        time.sleep(POLL_INTERVAL)
    msg = f"Timeout: prompt {prompt_id} did not complete within {MAX_WAIT}s"
    raise TimeoutError(msg)


def download_image(
    client: httpx.Client,
    filename: str,
    output_dir: Path,
    subfolder: str = "",
) -> Path:
    """Download generated image from ComfyUI."""
    params = {"filename": filename, "subfolder": subfolder, "type": "output"}
    resp = client.get("/view", params=params)
    resp.raise_for_status()
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / filename
    output_path.write_bytes(resp.content)
    return output_path


# =========================================================================
# OpenAI backend
# =========================================================================


def _map_size_to_openai(width: int, height: int) -> str:
    """Map SDXL dimensions to closest OpenAI supported size."""
    if (width, height) in _OPENAI_SIZE_MAP:
        return _OPENAI_SIZE_MAP[(width, height)]
    if width > height:
        return "1536x1024"
    if height > width:
        return "1024x1536"
    return "1024x1024"


def generate_via_openai(
    client: httpx.Client,
    config: dict,
    output_dir: Path,
) -> dict:
    """Generate image via OpenAI Images API."""
    prompt = config["prompt"]
    width = config.get("width", DEFAULT_WIDTH)
    height = config.get("height", DEFAULT_HEIGHT)
    seed = config.get("seed", int(time.time()) % (2**32))
    image_id = config.get("image_id", f"img_{uuid.uuid4().hex[:8]}")

    size = _map_size_to_openai(width, height)

    payload = {
        "model": OPENAI_IMAGE_MODEL,
        "prompt": prompt,
        "n": 1,
        "size": size,
        "quality": "high",
        "output_format": "png",
    }

    resp = client.post("/images/generations", json=payload)
    resp.raise_for_status()

    data = resp.json()
    b64_data = data["data"][0]["b64_json"]
    image_bytes = base64.b64decode(b64_data)

    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / f"{image_id}.png"
    output_path.write_bytes(image_bytes)

    return {
        "success": True,
        "image_path": str(output_path),
        "backend": "openai",
        "model": OPENAI_IMAGE_MODEL,
        "seed": seed,
    }


# =========================================================================
# Common helpers
# =========================================================================


def _read_config() -> dict:
    """Read and validate JSON config from stdin.

    Raises:
        SystemExit: On invalid input.
    """
    raw_input = sys.stdin.read().strip()
    if not raw_input:
        json.dump({"success": False, "error": "No input provided"}, sys.stdout)
        sys.exit(1)

    try:
        config = json.loads(raw_input)
    except json.JSONDecodeError as e:
        json.dump({"success": False, "error": f"Invalid JSON: {e}"}, sys.stdout)
        sys.exit(1)

    if "prompt" not in config:
        json.dump(
            {"success": False, "error": "Missing required field: prompt"},
            sys.stdout,
        )
        sys.exit(1)

    return config


def _check_comfyui_health(client: httpx.Client) -> bool:
    """Check if ComfyUI server is reachable."""
    try:
        health = client.get("/prompt")
    except httpx.HTTPError:
        return False
    return health.status_code == _HTTP_OK


def _check_openai_available() -> bool:
    """Check if OpenAI API key is configured."""
    return bool(OPENAI_API_KEY)


def _extract_images(
    client: httpx.Client,
    history: dict,
    output_dir: Path,
) -> Path | None:
    """Extract and download output images from ComfyUI history."""
    outputs = history.get("outputs", {})
    image_path = None
    for _node_id, node_output in outputs.items():
        for img_info in node_output.get("images", []):
            filename = img_info.get("filename", "")
            if filename:
                image_path = download_image(
                    client, filename, output_dir, img_info.get("subfolder", "")
                )
    return image_path


# =========================================================================
# Main entry point
# =========================================================================


def main() -> None:
    """Entry point: read JSON from stdin, generate image, write result JSON.

    Tries ComfyUI (local) first. Falls back to OpenAI if unavailable.
    """
    config = _read_config()

    output_dir = Path(config.get("output_dir", "/tmp/design_output"))
    seed = config.get("seed", int(time.time()) % (2**32))
    config["seed"] = seed

    # --- Try ComfyUI first ---
    try:
        with httpx.Client(base_url=COMFYUI_URL, timeout=120.0) as comfyui_client:
            if _check_comfyui_health(comfyui_client):
                workflow = build_workflow(config)
                prompt_id = queue_prompt(comfyui_client, workflow)
                history = poll_until_complete(comfyui_client, prompt_id)
                image_path = _extract_images(comfyui_client, history, output_dir)

                if image_path:
                    result = {
                        "success": True,
                        "image_path": str(image_path),
                        "backend": "comfyui",
                        "prompt_id": prompt_id,
                        "seed": seed,
                    }
                else:
                    result = {
                        "success": False,
                        "error": "No output images found in ComfyUI response",
                        "prompt_id": prompt_id,
                    }
                json.dump(result, sys.stdout, ensure_ascii=False)
                return
    except (httpx.HTTPError, TimeoutError) as e:
        # ComfyUI failed, try OpenAI fallback
        _comfyui_error = str(e)
    else:
        # health check returned False, proceed to fallback
        _comfyui_error = "ComfyUI not running"

    # --- Fallback to OpenAI ---
    if not _check_openai_available():
        json.dump(
            {
                "success": False,
                "error": (
                    f"ComfyUI unavailable ({_comfyui_error}). "
                    "OpenAI fallback also unavailable: OPENAI_API_KEY not set. "
                    "Please start ComfyUI or set OPENAI_API_KEY."
                ),
            },
            sys.stdout,
            ensure_ascii=False,
        )
        sys.exit(1)

    try:
        with httpx.Client(
            base_url=OPENAI_API_URL,
            timeout=120.0,
            headers={
                "Authorization": f"Bearer {OPENAI_API_KEY}",
                "Content-Type": "application/json",
            },
        ) as openai_client:
            result = generate_via_openai(openai_client, config, output_dir)
    except httpx.HTTPStatusError as e:
        result = {
            "success": False,
            "error": f"OpenAI API error: {e.response.status_code} - {e.response.text}",
        }
    except httpx.HTTPError as e:
        result = {"success": False, "error": f"OpenAI HTTP error: {e}"}

    json.dump(result, sys.stdout, ensure_ascii=False)


if __name__ == "__main__":
    main()

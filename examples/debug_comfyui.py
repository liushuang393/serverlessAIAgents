#!/usr/bin/env python3
"""ComfyUI API デバッグスクリプト."""

import asyncio
import json

import httpx


async def test_comfyui_workflow():
    """ComfyUI ワークフローをテスト."""
    # 最小限のワークフロー
    workflow = {
        "1": {
            "class_type": "CheckpointLoaderSimple",
            "inputs": {
                "ckpt_name": "sd_xl_base_1.0.safetensors",
            },
        },
        "2": {
            "class_type": "CLIPTextEncode",
            "inputs": {
                "text": "a black bluetooth speaker",
                "clip": ["1", 1],
            },
        },
        "3": {
            "class_type": "CLIPTextEncode",
            "inputs": {
                "text": "blurry, low quality",
                "clip": ["1", 1],
            },
        },
        "4": {
            "class_type": "EmptyLatentImage",
            "inputs": {
                "width": 1024,
                "height": 1024,
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
                "seed": 12345,
                "steps": 20,
                "cfg": 7.0,
                "sampler_name": "euler",
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
                "filename_prefix": "test",
            },
        },
    }

    payload = {"prompt": workflow}

    print("送信するペイロード:")
    print(json.dumps(payload, indent=2, ensure_ascii=False))
    print()

    async with httpx.AsyncClient(base_url="http://localhost:8188", timeout=30.0) as client:
        print("ComfyUI に送信中...")
        response = await client.post("/prompt", json=payload)

        print(f"ステータスコード: {response.status_code}")
        print()

        if response.status_code == 200:
            print("✓ 成功!")
            print(json.dumps(response.json(), indent=2, ensure_ascii=False))
        else:
            print("✗ エラー!")
            print("応答ヘッダー:")
            print(dict(response.headers))
            print()
            print("応答ボディ:")
            try:
                print(json.dumps(response.json(), indent=2, ensure_ascii=False))
            except Exception:
                print(response.text)


if __name__ == "__main__":
    asyncio.run(test_comfyui_workflow())

#!/usr/bin/env python3
"""ComfyUI で利用可能なモデルをリスト."""

import asyncio
import json

import httpx


async def list_models():
    """ComfyUI で利用可能なモデルをリスト."""
    async with httpx.AsyncClient(base_url="http://localhost:8188", timeout=30.0) as client:
        # オブジェクト情報を取得
        response = await client.get("/object_info")
        
        if response.status_code != 200:
            print(f"エラー: {response.status_code}")
            return
        
        data = response.json()
        
        # CheckpointLoaderSimple の情報を取得
        if "CheckpointLoaderSimple" in data:
            checkpoint_info = data["CheckpointLoaderSimple"]
            if "input" in checkpoint_info and "required" in checkpoint_info["input"]:
                ckpt_name_info = checkpoint_info["input"]["required"].get("ckpt_name")
                if ckpt_name_info:
                    available_models = ckpt_name_info[0]
                    print("利用可能なチェックポイント:")
                    if available_models:
                        for model in available_models:
                            print(f"  - {model}")
                    else:
                        print("  (なし)")
                    print()
                    print(f"合計: {len(available_models)} モデル")
                else:
                    print("ckpt_name 情報が見つかりません")
            else:
                print("CheckpointLoaderSimple の入力情報が見つかりません")
        else:
            print("CheckpointLoaderSimple が見つかりません")
            print()
            print("利用可能なノードタイプ:")
            for node_type in sorted(data.keys())[:20]:
                print(f"  - {node_type}")
            print(f"  ... (合計 {len(data)} ノードタイプ)")


if __name__ == "__main__":
    asyncio.run(list_models())

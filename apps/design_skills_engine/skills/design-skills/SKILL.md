---
name: design-skills
description: Lovart風マルチ画像生成スキル - 自然言語デザインブリーフからComfyUI経由で統一スタイルの画像セットを生成
version: 1.0.0
triggers: [design, image generation, product images, brand design, ComfyUI, デザイン, 画像生成, 商品画像]
requirements: [httpx>=0.24.0]
tags: [design, image-generation, comfyui, creative]
examples:
  - "Bluetoothスピーカーの商品画像を8枚生成"
  - "コーヒーブランドのInstagram投稿画像をデザイン"
  - "Generate product images for a laptop stand"
---

# Design Skills

自然言語のデザインブリーフから、プロフェッショナルなマルチ画像デザインセットを生成。

## 概要

Lovartの「Design Skills」思想をAgentFlowフレームワークで実装:

1. **意図解析（IntentAnalyzer）** - デザインブリーフを理解
2. **プロンプト計画（PromptPlanner）** - 統一スタイル + 個別プロンプトを生成
3. **ワークフロー実行（WorkflowExecutor）** - ComfyUI経由で画像生成

## 前提条件

- ComfyUI サーバーが起動中（デフォルト: http://localhost:8188）
- 異なるアドレスの場合は `COMFYUI_URL` 環境変数を設定

## 使用方法

```python
from apps.design_skills_engine.engine import DesignSkillsEngine

engine = DesignSkillsEngine()
result = await engine.run({
    "brief": "アウトドアBluetoothスピーカーの商品画像、テクノ風、黒系",
    "num_images": 8,
    "target_platform": "amazon",
})
```

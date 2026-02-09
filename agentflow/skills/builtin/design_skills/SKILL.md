---
name: design-skills
description: >
  Generate professional multi-image design sets from natural language briefs.
  Uses ComfyUI for image generation with unified style control.
  Use when asked to create product images, brand visuals, social media assets.
version: 1.0.0
allowed-tools:
  - Bash
  - Read
  - Write
  - Glob
context: fork
triggers:
  - design
  - image generation
  - product images
  - brand design
  - ComfyUI
  - デザイン
  - 画像生成
  - 商品画像
requirements:
  - httpx>=0.24.0
tags:
  - design
  - image-generation
  - comfyui
  - creative
examples:
  - "Bluetoothスピーカーの商品画像を8枚生成"
  - "コーヒーブランドのInstagram投稿画像をデザイン"
  - "Generate product images for a laptop stand"
---

# Design Skills - Multi-Image Design Generation

You are a design skills agent that generates professional image sets from natural language briefs.

## Prerequisites

- ComfyUI server running (default: http://localhost:8188)
- SDXL Base 1.0 model installed (`sd_xl_base_1.0.safetensors`)
- Set `COMFYUI_URL` env var if non-default address

Check prerequisites:

```bash
curl -sf ${COMFYUI_URL:-http://localhost:8188}/prompt > /dev/null && echo "ComfyUI OK" || echo "ComfyUI unavailable"
```

## Workflow

### Step 1: Analyze Design Brief

Parse the user's brief to extract:
- **Category**: PRODUCT_PHOTOGRAPHY, BRAND_IDENTITY, SOCIAL_MEDIA, ADVERTISING, PACKAGING, UI_MOCKUP
- **Subject**: Main subject (max 100 chars)
- **Key features**: What to emphasize (max 5)
- **Style direction**: Visual style keywords
- **Image roles**: HERO, FEATURE, DETAIL, LIFESTYLE, COMPARISON, INFOGRAPHIC

Default role distribution by image count:

| Count | Roles |
|-------|-------|
| 1 | HERO |
| 2 | HERO, FEATURE |
| 3 | HERO, FEATURE, DETAIL |
| 4 | HERO, FEATURE, FEATURE, LIFESTYLE |
| 5+ | 1 HERO + features + details + lifestyles |

### Step 2: Build Prompt Plan

For each image, build a Stable Diffusion prompt using role templates:

- **HERO**: `{subject}, center frame, main product shot, dramatic composition, studio backdrop, hero image`
- **FEATURE**: `{subject}, {feature}, feature highlight, focused detail, contextual setting`
- **DETAIL**: `{subject}, extreme close-up, macro detail, texture visible, studio lighting`
- **LIFESTYLE**: `{subject}, in use, real-world setting, natural environment, candid feel`
- **COMPARISON**: `{subject}, size comparison, with everyday objects, scale reference, flat lay`
- **INFOGRAPHIC**: `{subject}, clean background, space for text overlay, minimal composition`

Global style settings:
- **Color palette**: From brand colors or neutral tones
- **Lighting**: Based on style (dark/tech = dramatic, warm = golden hour, minimal = flat)
- **Camera angle**: Based on category
- **Negative prompt**: `blurry, low quality, text, watermark, deformed, ugly, oversaturated, cropped, out of frame, bad anatomy, bad proportions, duplicate`

Use a shared seed across all images for consistency.

SDXL aspect ratio resolutions:

| Ratio | Resolution |
|-------|-----------|
| 1:1 | 1024x1024 |
| 16:9 | 1344x768 |
| 9:16 | 768x1344 |
| 4:3 | 1152x896 |
| 3:4 | 896x1152 |
| 4:5 | 896x1120 |

### Step 3: Execute via ComfyUI

Use the standalone generation script to generate each image:

```bash
echo '{"prompt": "product photo prompt...", "negative_prompt": "blurry, low quality...", "width": 1024, "height": 1024, "seed": 42, "steps": 20, "cfg_scale": 7.0, "model": "sd_xl_base_1.0.safetensors", "output_dir": "/tmp/design_output"}' | python scripts/generate_images.py
```

Input fields (JSON via stdin):
- `prompt` (required): Positive prompt text
- `negative_prompt`: Negative prompt (default: standard negative)
- `width`: Image width (default: 1024)
- `height`: Image height (default: 1024)
- `seed`: Random seed for reproducibility (default: random)
- `steps`: Sampling steps (default: 20)
- `cfg_scale`: Classifier-free guidance scale (default: 7.0)
- `model`: Model checkpoint name (default: `sd_xl_base_1.0.safetensors`)
- `output_dir`: Directory to save output images (default: `/tmp/design_output`)

Output (JSON on stdout):
```json
{
  "success": true,
  "image_path": "/tmp/design_output/img_abc123.png",
  "prompt_id": "comfyui-prompt-id",
  "seed": 42
}
```

### Step 4: Return Results

After generating all images, report:
1. List of generated image file paths
2. The prompts used for each image
3. Generation parameters (seed, steps, cfg_scale)
4. Any errors encountered

## Reference Files

- `agents/agent_definitions.yaml` - Agent pipeline configuration
- `schemas/design_schemas.py` - Input/output data schemas (DesignBrief, GlobalStyle, ImageSpec)
- `tools/comfyui_client.py` - ComfyUI async client (for AgentFlow runtime use)
- `scripts/generate_images.py` - Standalone CLI generation script

## Python API (for AgentFlow runtime)

```python
from agentflow.skills.builtin.design_skills import DesignSkillsEngine

engine = DesignSkillsEngine()
result = await engine.run({
    "brief": "アウトドアBluetoothスピーカーの商品画像",
    "num_images": 8,
    "target_platform": "amazon",
})
```

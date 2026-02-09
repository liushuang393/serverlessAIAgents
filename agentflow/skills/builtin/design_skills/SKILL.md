---
name: design-skills
description: >
  Generate professional multi-image design sets from natural language briefs.
  Uses ComfyUI for image generation with unified style control.
  Use when asked to create product images, brand visuals, social media assets.
version: 1.0.1
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
allowed-tools:
  - Bash
  - Read
  - Write
  - Glob
context: fork
user-invocable: true
---

# Design Skills - Multi-Image Design Generation

You are a design skills agent that generates professional image sets from natural language briefs using ComfyUI.

## Quick Start

1. **Check ComfyUI availability** (REQUIRED before generation):
   ```bash
   curl -sf ${COMFYUI_URL:-http://localhost:8188}/system_stats > /dev/null && echo "✓ ComfyUI ready" || echo "✗ ComfyUI unavailable - please start ComfyUI server"
   ```

2. **Verify SDXL model** (REQUIRED):
   ```bash
   curl -sf ${COMFYUI_URL:-http://localhost:8188}/object_info | grep -q "sd_xl_base_1.0.safetensors" && echo "✓ SDXL model found" || echo "✗ SDXL model missing - please install sd_xl_base_1.0.safetensors"
   ```

3. **Generate images** using the Python API or standalone script (see below)

## Prerequisites

- **ComfyUI server**: Must be running at `http://localhost:8188` (or set `COMFYUI_URL` env var)
- **SDXL Base 1.0 model**: `sd_xl_base_1.0.safetensors` must be installed in ComfyUI's models directory
- **Python packages**: `httpx>=0.24.0` (auto-installed with agentflow)

## Usage Methods

### Method 1: Python API (Recommended for AgentFlow)

```python
from agentflow.skills.builtin.design_skills import DesignSkillsEngine

engine = DesignSkillsEngine()
result = await engine.run({
    "brief": "アウトドアBluetoothスピーカーの商品画像、黒系、テクノ風",
    "num_images": 8,
    "target_platform": "amazon",  # optional
    "output_directory": "/tmp/design_output",  # optional
})

# Result structure:
# {
#   "images": [
#     {"path": "/tmp/design_output/img_001.png", "role": "HERO", "prompt": "..."},
#     ...
#   ],
#   "global_style": {...},
#   "metadata": {...}
# }
```

### Method 2: Standalone Script (For CLI/Shell usage)

```bash
# Generate a single image
echo '{
  "prompt": "bluetooth speaker, black, studio lighting, product photography",
  "negative_prompt": "blurry, low quality, text, watermark",
  "width": 1024,
  "height": 1024,
  "seed": 42,
  "steps": 20,
  "cfg_scale": 7.0,
  "model": "sd_xl_base_1.0.safetensors",
  "output_dir": "/tmp/design_output"
}' | python agentflow/skills/builtin/design_skills/scripts/generate_images.py

# Output (JSON on stdout):
# {
#   "success": true,
#   "image_path": "/tmp/design_output/img_abc123.png",
#   "prompt_id": "comfyui-prompt-id",
#   "seed": 42
# }
```

**Script Parameters:**
- `prompt` (required): Positive prompt text
- `negative_prompt`: Negative prompt (default: standard negative)
- `width`: Image width (default: 1024)
- `height`: Image height (default: 1024)
- `seed`: Random seed for reproducibility (default: random)
- `steps`: Sampling steps (default: 20)
- `cfg_scale`: CFG scale (default: 7.0)
- `model`: Model checkpoint name (default: `sd_xl_base_1.0.safetensors`)
- `output_dir`: Output directory (default: `/tmp/design_output`)

**Error Handling:**
- If ComfyUI is unavailable: Returns `{"success": false, "error": "ComfyUI connection failed"}`
- If model not found: Returns `{"success": false, "error": "Model not found: ..."}`
- Script exits with code 1 on error

## Design Workflow (For Multi-Image Generation)

When generating multiple images from a natural language brief, follow this workflow:

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

**SDXL Aspect Ratio Resolutions:**

| Ratio | Resolution |
|-------|-----------|
| 1:1 | 1024x1024 |
| 16:9 | 1344x768 |
| 9:16 | 768x1344 |
| 4:3 | 1152x896 |
| 3:4 | 896x1152 |
| 4:5 | 896x1120 |

### Step 3: Execute via ComfyUI

**For each image in the plan:**

1. Build the prompt using the role template + global style
2. Call the standalone script or Python API
3. Verify the output image exists
4. Collect metadata (path, prompt, seed, etc.)

**Example batch generation:**

```bash
# Generate HERO image
echo '{"prompt": "bluetooth speaker, center frame, hero shot, studio backdrop", "seed": 42, "width": 1024, "height": 1024}' | python scripts/generate_images.py

# Generate FEATURE image
echo '{"prompt": "bluetooth speaker, waterproof feature, water splash", "seed": 42, "width": 1024, "height": 1024}' | python scripts/generate_images.py

# Generate LIFESTYLE image
echo '{"prompt": "bluetooth speaker, outdoor camping scene, natural environment", "seed": 42, "width": 1344, "height": 768}' | python scripts/generate_images.py
```

### Step 4: Return Results

After generating all images, provide a summary:

```markdown
## Generated Images

1. **HERO** - `/tmp/design_output/img_001.png`
   - Prompt: "bluetooth speaker, center frame, hero shot, studio backdrop"
   - Seed: 42, Size: 1024x1024

2. **FEATURE** - `/tmp/design_output/img_002.png`
   - Prompt: "bluetooth speaker, waterproof feature, water splash"
   - Seed: 42, Size: 1024x1024

3. **LIFESTYLE** - `/tmp/design_output/img_003.png`
   - Prompt: "bluetooth speaker, outdoor camping scene"
   - Seed: 42, Size: 1344x768

## Generation Parameters
- Model: sd_xl_base_1.0.safetensors
- Steps: 20, CFG Scale: 7.0
- Shared Seed: 42 (for consistency)

## Next Steps
- Review images in output directory
- Adjust prompts if needed and regenerate
- Use different seeds for variations
```

## Troubleshooting

### ComfyUI Connection Failed
```bash
# Check if ComfyUI is running
curl -sf http://localhost:8188/system_stats

# If not running, start ComfyUI server
# (Refer to ComfyUI documentation for installation)
```

### Model Not Found
```bash
# List available models
curl -sf http://localhost:8188/object_info | grep -o '"[^"]*\.safetensors"'

# Download SDXL Base 1.0 if missing
# Place in ComfyUI's models/checkpoints/ directory
```

### Generation Timeout
- Increase timeout in script (default: 300s)
- Check ComfyUI queue status: `curl http://localhost:8188/queue`
- Reduce image size or steps for faster generation

## Reference Files

- `agents/agent_definitions.yaml` - Agent pipeline configuration
- `schemas/design_schemas.py` - Input/output data schemas
- `tools/comfyui_client.py` - ComfyUI async client
- `scripts/generate_images.py` - Standalone CLI script

## Testing

```bash
# Run unit tests
pytest --no-cov tests/unit/test_design_skills_schemas.py
pytest --no-cov tests/unit/test_design_skills_engine.py

# Run E2E tests (requires ComfyUI running)
pytest --no-cov -m e2e tests/e2e/test_design_skills_e2e.py
```

# Design Skills 使用指南

本指南演示如何使用 `design_skills` 生成设计图。

## ⚠️ 重要：两种方法的区别

### ✅ 方法1: Python API - 完整的4步流程（推荐）

**执行完整的设计工作流：**

| 步骤 | 名称 | 功能 | 输入 | 输出 |
|------|------|------|------|------|
| 1 | IntentAnalyzer | 意图分析 | 自然语言 brief | 结构化意图 |
| 2 | PromptPlanner | 提示词规划 | 结构化意图 | 生成计划 |
| 3 | WorkflowExecutor | ComfyUI 执行 | 生成计划 | 图片文件 |
| 4 | Result Aggregation | 结果汇总 | 所有图片 | 完整结果 |

**适用场景：**
- ✅ 你想让 AI 分析和规划设计
- ✅ 需要生成多张风格一致的图片
- ✅ 更喜欢用自然语言而不是技术提示词
- ✅ 需要完整的元数据和结果汇总

**示例：**
```python
# 输入：自然语言
"一个黑色的蓝牙音箱，科技风格，工作室拍摄"

# 输出：8张风格一致的图片
# - HERO 图（主图）
# - FEATURE 图（特写）
# - DETAIL 图（细节）
# - LIFESTYLE 图（生活场景）
# ...
```

---

### ⚠️ 方法2: Standalone Script - 仅直接生成（快速测试）

**只执行第3步（ComfyUI 生成）：**

| 步骤 | 状态 | 说明 |
|------|------|------|
| 1. 意图分析 | ❌ 跳过 | 你需要自己分析需求 |
| 2. 提示词规划 | ❌ 跳过 | 你需要自己写提示词 |
| 3. ComfyUI 执行 | ✅ 执行 | 直接生成图片 |
| 4. 结果汇总 | ❌ 跳过 | 只返回单张图片路径 |

**你需要自己提供：**
- ✍️ 完整的提示词文本
- 📐 图片尺寸（width, height）
- ⚙️ 所有生成参数（steps, cfg_scale, seed, etc.）

**适用场景：**
- ✅ 你已经知道确切的提示词
- ✅ 只需要快速生成单张图片
- ✅ 测试 ComfyUI 连接
- ✅ 不需要 AI 分析或规划

**示例：**
```bash
# 输入：完整的技术参数
{
  "prompt": "bluetooth speaker, black, studio lighting, product photography",
  "width": 1024,
  "height": 1024,
  "seed": 42
}

# 输出：单张图片
/tmp/design_output/img_abc123.png
```

---

## 前提条件

在开始之前，请确保：

1. **ComfyUI 服务器运行中**
   ```bash
   # 检查 ComfyUI 状态
   curl -sf http://localhost:8188/system_stats
   ```

2. **SDXL Base 1.0 模型已安装**
   ```bash
   # 检查模型
   curl -sf http://localhost:8188/object_info | grep sd_xl_base_1.0.safetensors
   ```

3. **Python 依赖已安装**
   ```bash
   pip install httpx
   # 或安装完整的 agentflow
   pip install -e .
   ```

---

## 方法1: Python API - 完整的4步流程 ✅

### 工作流程详解

#### Step 1: 意图分析（IntentAnalyzer）

```python
# 输入：自然语言
input_brief = "一个黑色的蓝牙音箱，科技风格，工作室拍摄"

# AI 分析后输出：结构化意图
intent = {
  "category": "PRODUCT_PHOTOGRAPHY",
  "subject": "bluetooth speaker",
  "key_features": ["black", "tech style", "studio shot"],
  "style_direction": ["minimal", "dark", "tech"],
  "image_roles": ["HERO", "FEATURE", "DETAIL", "LIFESTYLE"]
}
```

#### Step 2: 提示词规划（PromptPlanner）

```python
# 输入：结构化意图
# AI 规划后输出：完整生成计划

plan = {
  "global_style": {
    "color_palette": ["black", "blue"],
    "lighting": "dramatic",
    "camera_angle": "front view",
    "negative_prompt": "blurry, low quality, ...",
    "base_model": "sd_xl_base_1.0.safetensors"
  },
  "images": [
    {
      "image_id": "hero_001",
      "role": "HERO",
      "prompt": "bluetooth speaker, center frame, hero shot, studio backdrop",
      "width": 1024,
      "height": 1024,
      "seed": 42
    },
    {
      "image_id": "feature_001",
      "role": "FEATURE",
      "prompt": "bluetooth speaker, waterproof feature, water splash",
      "width": 1024,
      "height": 1024,
      "seed": 42
    },
    # ... 更多图片
  ]
}
```

#### Step 3: ComfyUI 执行（WorkflowExecutor）

```python
# 为每张图片生成
for image_spec in plan.images:
    workflow = build_workflow(global_style, image_spec)
    image = await comfyui_client.generate(workflow)
    # 保存到 /tmp/design_output/
```

#### Step 4: 结果汇总

```python
# 返回完整结果
result = {
  "images": [
    {"path": "/tmp/design_output/hero_001.png", "role": "HERO", "prompt": "..."},
    {"path": "/tmp/design_output/feature_001.png", "role": "FEATURE", "prompt": "..."},
    {"path": "/tmp/design_output/detail_001.png", "role": "DETAIL", "prompt": "..."},
  ],
  "global_style": {
    "color_palette": ["black", "blue"],
    "lighting": "dramatic",
    ...
  },
  "metadata": {
    "total_images": 8,
    "seed": 42,
    "model": "sd_xl_base_1.0.safetensors"
  }
}
```

### 基本用法

```python
import asyncio
from agentflow.skills.builtin.design_skills import DesignSkillsEngine

async def generate_images():
    engine = DesignSkillsEngine()
    
    # 只需要提供自然语言描述
    result = await engine.run({
        "brief": "一个黑色的蓝牙音箱，科技风格，工作室拍摄",
        "num_images": 3,
        "output_directory": "/tmp/design_output"
    })
    
    # AI 会自动完成 4 步流程
    print(f"生成了 {len(result['images'])} 张图片")
    for img in result['images']:
        print(f"  - {img['role']}: {img['path']}")

asyncio.run(generate_images())
```

### 完整参数示例

```python
result = await engine.run({
    "brief": "蓝牙音箱商品图，黑色，科技风",
    "num_images": 8,
    "target_platform": "amazon",  # 可选: amazon, instagram, etc.
    "style_preferences": ["minimal", "dark", "tech"],  # 可选
    "brand_colors": ["#000000", "#0066CC"],  # 可选
    "aspect_ratio": "16:9",  # 可选: 1:1, 16:9, 9:16, 4:3, 3:4, 4:5
    "output_directory": "/tmp/design_output"
})
```

### 使用测试脚本

```bash
# 运行完整测试（包含4步流程）
python examples/test_design_skills.py
```

---

## 方法2: Standalone Script - 仅直接生成 ⚠️

### 基本用法

```bash
# 你需要自己写完整的提示词
echo '{
  "prompt": "bluetooth speaker, black, studio lighting, product photography",
  "width": 1024,
  "height": 1024,
  "seed": 42
}' | python agentflow/skills/builtin/design_skills/scripts/generate_images.py
```

### 完整参数示例

```bash
echo '{
  "prompt": "bluetooth speaker, black, studio lighting, product photography",
  "negative_prompt": "blurry, low quality, text, watermark, deformed",
  "width": 1024,
  "height": 1024,
  "seed": 42,
  "steps": 20,
  "cfg_scale": 7.0,
  "model": "sd_xl_base_1.0.safetensors",
  "output_dir": "/tmp/design_output",
  "image_id": "my_speaker"
}' | python agentflow/skills/builtin/design_skills/scripts/generate_images.py
```

### 输出格式

成功时：
```json
{
  "success": true,
  "image_path": "/tmp/design_output/my_speaker_00001_.png",
  "prompt_id": "abc123-def456",
  "seed": 42
}
```

失败时：
```json
{
  "success": false,
  "error": "ComfyUI connection failed"
}
```

### 快速测试脚本

```bash
# 使用提供的快速测试脚本
chmod +x examples/quick_test_design.sh
./examples/quick_test_design.sh
```

---

## 方法对比总结

| 特性 | 方法1: Python API | 方法2: Standalone Script |
|------|-------------------|--------------------------|
| **意图分析** | ✅ 自动 | ❌ 需要手动 |
| **提示词规划** | ✅ 自动 | ❌ 需要手动 |
| **图片生成** | ✅ 自动 | ✅ 自动 |
| **结果汇总** | ✅ 自动 | ❌ 无 |
| **输入方式** | 自然语言 | 技术参数 |
| **输出内容** | 多张图片 + 元数据 | 单张图片路径 |
| **适用场景** | 完整设计流程 | 快速测试 |
| **学习曲线** | 简单 | 需要了解 SD 参数 |

---

## 批量生成示例

### 使用方法1（推荐）

```python
import asyncio
from agentflow.skills.builtin.design_skills import DesignSkillsEngine

async def batch_generate():
    engine = DesignSkillsEngine()
    
    # 定义多个设计需求
    briefs = [
        {
            "brief": "蓝牙音箱商品图，黑色，科技风",
            "num_images": 4,
            "output_directory": "/tmp/design_output/speaker_black"
        },
        {
            "brief": "蓝牙音箱商品图，白色，简约风",
            "num_images": 4,
            "output_directory": "/tmp/design_output/speaker_white"
        },
        {
            "brief": "蓝牙音箱生活场景图，户外露营",
            "num_images": 3,
            "output_directory": "/tmp/design_output/speaker_lifestyle"
        }
    ]
    
    # AI 会为每个 brief 自动完成 4 步流程
    for i, brief in enumerate(briefs, 1):
        print(f"\n生成第 {i}/{len(briefs)} 组图片...")
        result = await engine.run(brief)
        print(f"✓ 完成: {len(result['images'])} 张图片")

asyncio.run(batch_generate())
```

### 使用方法2

```bash
#!/bin/bash
# 你需要为每张图片手动写提示词

SEED=42

# HERO 图片
echo '{
  "prompt": "bluetooth speaker, center frame, hero shot, studio backdrop",
  "seed": '$SEED',
  "width": 1024,
  "height": 1024,
  "image_id": "hero"
}' | python agentflow/skills/builtin/design_skills/scripts/generate_images.py

# FEATURE 图片
echo '{
  "prompt": "bluetooth speaker, waterproof feature, water splash",
  "seed": '$SEED',
  "width": 1024,
  "height": 1024,
  "image_id": "feature"
}' | python agentflow/skills/builtin/design_skills/scripts/generate_images.py

# LIFESTYLE 图片
echo '{
  "prompt": "bluetooth speaker, outdoor camping scene, natural environment",
  "seed": '$SEED',
  "width": 1344,
  "height": 768,
  "image_id": "lifestyle"
}' | python agentflow/skills/builtin/design_skills/scripts/generate_images.py
```

---

## 常见问题

### 1. ComfyUI 连接失败

**错误信息:**
```json
{"success": false, "error": "ComfyUI unreachable at http://localhost:8188"}
```

**解决方法:**
```bash
# 检查 ComfyUI 是否运行
curl -sf http://localhost:8188/system_stats

# 如果使用非默认地址，设置环境变量
export COMFYUI_URL=http://192.168.1.100:8188
```

### 2. 模型未找到

**错误信息:**
```json
{"success": false, "error": "Model not found: sd_xl_base_1.0.safetensors"}
```

**解决方法:**
```bash
# 检查可用模型
curl -sf http://localhost:8188/object_info | grep -o '"[^"]*\.safetensors"'

# 下载 SDXL Base 1.0 模型
# 放置到 ComfyUI 的 models/checkpoints/ 目录
```

### 3. 生成超时

**错误信息:**
```json
{"success": false, "error": "Timeout: prompt did not complete within 300s"}
```

**解决方法:**
- 减少图片尺寸（例如从 1024x1024 改为 512x512）
- 减少采样步数（例如从 20 改为 10）
- 检查 ComfyUI 队列状态: `curl http://localhost:8188/queue`

---

## 高级用法

### 1. 自定义 SDXL 分辨率

```bash
# 1:1 正方形
echo '{"prompt": "...", "width": 1024, "height": 1024}' | python scripts/generate_images.py

# 16:9 横屏
echo '{"prompt": "...", "width": 1344, "height": 768}' | python scripts/generate_images.py

# 9:16 竖屏
echo '{"prompt": "...", "width": 768, "height": 1344}' | python scripts/generate_images.py
```

### 2. 使用相同种子生成一致的图片

```bash
# 使用固定种子确保可重现
SEED=42
echo '{"prompt": "...", "seed": '$SEED'}' | python scripts/generate_images.py
```

### 3. 调整生成质量

```bash
# 高质量（慢）
echo '{"prompt": "...", "steps": 30, "cfg_scale": 8.0}' | python scripts/generate_images.py

# 快速预览（快）
echo '{"prompt": "...", "steps": 10, "cfg_scale": 6.0}' | python scripts/generate_images.py
```

---

## 推荐使用流程

1. **首次测试**: 使用方法2（Standalone Script）快速验证 ComfyUI 连接
   ```bash
   ./examples/quick_test_design.sh
   ```

2. **正式使用**: 使用方法1（Python API）获得完整的 AI 设计流程
   ```bash
   python examples/test_design_skills.py
   ```

3. **集成到应用**: 在你的 Python 代码中使用 `DesignSkillsEngine`

---

## 参考资料

- [SKILL.md](../agentflow/skills/builtin/design_skills/SKILL.md) - 完整文档
- [EVALUATION.md](../agentflow/skills/builtin/design_skills/EVALUATION.md) - 评估报告
- [ComfyUI 文档](https://github.com/comfyanonymous/ComfyUI)
- [SDXL 模型](https://huggingface.co/stabilityai/stable-diffusion-xl-base-1.0)

---

## 下一步

1. ✅ 运行快速测试脚本验证环境
2. ✅ 尝试使用 Python API 生成完整的设计图集
3. ✅ 调整参数优化生成质量
4. ✅ 集成到你的应用中

祝你使用愉快！🎨

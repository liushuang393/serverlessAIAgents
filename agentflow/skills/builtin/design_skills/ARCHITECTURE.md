# Design Skills 架构说明

## 概述

Design Skills 是一个完整的 AI 设计图生成系统，支持从自然语言到图片的端到端流程。

## 架构图

```
用户输入（自然语言）
    ↓
┌─────────────────────────────────────────────────────────┐
│  DesignSkillsEngine (PipelineEngine)                    │
│                                                          │
│  Step 1: IntentAnalyzerAgent                            │
│    输入: "一个黑色的蓝牙音箱，科技风格，工作室拍摄"      │
│    输出: 结构化意图 (category, subject, features, etc.) │
│                                                          │
│  Step 2: PromptPlannerAgent                             │
│    输入: 结构化意图                                      │
│    输出: 生成计划 (global_style + image_specs)          │
│                                                          │
│  Step 3: WorkflowExecutorAgent                          │
│    输入: 生成计划                                        │
│    ├─ 后端检测                                          │
│    │   ├─ ComfyUI 可用? → 使用本地 GPU                 │
│    │   └─ ComfyUI 不可用? → 使用 OpenAI (fallback)     │
│    │                                                     │
│    ├─ ComfyUIClient (本地后端)                         │
│    │   ├─ build_workflow_payload()                     │
│    │   ├─ queue_prompt()                               │
│    │   ├─ poll_until_complete()                        │
│    │   └─ get_image()                                  │
│    │                                                     │
│    └─ OpenAIImageClient (云端后端)                     │
│        └─ generate_and_save()                          │
│                                                          │
│  Step 4: 结果汇总                                        │
│    输出: 所有图片 + 元数据                               │
└─────────────────────────────────────────────────────────┘
    ↓
生成的图片 + 完整元数据
```

## 核心组件

### 1. DesignSkillsEngine

**位置:** `agentflow/skills/builtin/design_skills/engine.py`

**职责:**
- 协调整个 4 步流程
- 继承自 `PipelineEngine`
- 管理 3 个 Agent 的顺序执行

**关键代码:**
```python
self._stage_configs = self._parse_stages([
    {"name": "intent_analyzer", "agent": ...},
    {"name": "prompt_planner", "agent": ...},
    {"name": "workflow_executor", "agent": ...},
])
```

### 2. WorkflowExecutorAgent

**位置:** `agentflow/skills/builtin/design_skills/agents/workflow_executor_agent.py`

**职责:**
- 执行图片生成
- 自动检测可用后端（ComfyUI 或 OpenAI）
- 处理批量生成和错误恢复

**后端选择逻辑:**
```python
async def _detect_backend(self) -> str:
    # 1. 优先尝试 ComfyUI (本地)
    if await self._comfyui.health_check():
        return "comfyui"
    
    # 2. 回退到 OpenAI (云端)
    if await self._openai.health_check():
        return "openai"
    
    # 3. 两者都不可用，抛出错误
    raise RuntimeError("No backend available")
```

### 3. ComfyUIClient

**位置:** `agentflow/skills/builtin/design_skills/tools/comfyui_client.py`

**职责:**
- 与本地 ComfyUI 服务器通信
- 构建 SDXL txt2img 工作流
- 管理生成队列和轮询

**关键方法:**

#### `build_workflow_payload(style, spec)`
构建 ComfyUI API 工作流 JSON：
```python
workflow = {
    "prompt": {
        "1": {"class_type": "CheckpointLoaderSimple", ...},  # 加载模型
        "2": {"class_type": "CLIPTextEncode", ...},          # 正向提示词
        "3": {"class_type": "CLIPTextEncode", ...},          # 负向提示词
        "4": {"class_type": "EmptyLatentImage", ...},        # 潜在空间
        "5": {"class_type": "KSampler", ...},                # 采样器
        "6": {"class_type": "VAEDecode", ...},               # 解码
        "7": {"class_type": "SaveImage", ...},               # 保存
    }
}
```

#### `queue_prompt(workflow)`
提交工作流到 ComfyUI 队列：
```python
response = await self._http_client.post("/prompt", json=workflow)
prompt_id = response.json()["prompt_id"]
```

#### `poll_until_complete(prompt_id)`
轮询直到生成完成：
```python
while (time.monotonic() - start) < max_wait:
    response = await self._http_client.get(f"/history/{prompt_id}")
    if prompt_id in response.json():
        return response.json()[prompt_id]
    await asyncio.sleep(poll_interval)
```

#### `get_image(filename)`
下载生成的图片：
```python
response = await self._http_client.get("/view", params={
    "filename": filename,
    "subfolder": subfolder,
    "type": "output"
})
return response.content
```

## 数据流

### 输入数据流

```python
# 用户输入
{
    "brief": "一个黑色的蓝牙音箱，科技风格，工作室拍摄",
    "num_images": 8
}

# ↓ Step 1: IntentAnalyzerAgent

{
    "category": "PRODUCT_PHOTOGRAPHY",
    "subject": "bluetooth speaker",
    "key_features": ["black", "tech style", "studio shot"],
    "style_direction": ["minimal", "dark", "tech"],
    "image_roles": ["HERO", "FEATURE", "DETAIL", "LIFESTYLE", ...]
}

# ↓ Step 2: PromptPlannerAgent

{
    "global_style": {
        "color_palette": ["black", "blue"],
        "lighting": "dramatic",
        "camera_angle": "front view",
        "mood": "professional",
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
            "seed": 42,
            "steps": 20,
            "cfg_scale": 7.0
        },
        # ... 更多图片
    ]
}

# ↓ Step 3: WorkflowExecutorAgent

# 对每张图片:
# 1. 构建 ComfyUI 工作流
workflow = {
    "prompt": {
        "1": {"class_type": "CheckpointLoaderSimple", "inputs": {"ckpt_name": "sd_xl_base_1.0.safetensors"}},
        "2": {"class_type": "CLIPTextEncode", "inputs": {"text": "bluetooth speaker, center frame, hero shot, ...", "clip": ["1", 1]}},
        "3": {"class_type": "CLIPTextEncode", "inputs": {"text": "blurry, low quality, ...", "clip": ["1", 1]}},
        "4": {"class_type": "EmptyLatentImage", "inputs": {"width": 1024, "height": 1024, "batch_size": 1}},
        "5": {"class_type": "KSampler", "inputs": {"seed": 42, "steps": 20, "cfg": 7.0, ...}},
        "6": {"class_type": "VAEDecode", "inputs": {"samples": ["5", 0], "vae": ["1", 2]}},
        "7": {"class_type": "SaveImage", "inputs": {"images": ["6", 0], "filename_prefix": "hero_001"}}
    }
}

# 2. 提交到 ComfyUI
POST http://localhost:8188/prompt
→ {"prompt_id": "abc123-def456"}

# 3. 轮询直到完成
GET http://localhost:8188/history/abc123-def456
→ {"abc123-def456": {"outputs": {"7": {"images": [{"filename": "hero_001_00001_.png"}]}}}}

# 4. 下载图片
GET http://localhost:8188/view?filename=hero_001_00001_.png&type=output
→ <PNG binary data>

# ↓ Step 4: 结果汇总

{
    "images": [
        {
            "image_id": "hero_001",
            "role": "HERO",
            "file_path": "/tmp/design_output/hero_001_00001_.png",
            "prompt_used": "bluetooth speaker, center frame, hero shot, ...",
            "seed_used": 42,
            "generation_time_seconds": 15.3
        },
        # ... 更多图片
    ],
    "output_directory": "/tmp/design_output",
    "total_generation_time_seconds": 120.5,
    "errors": []
}
```

## 本地 vs 云端

### 本地 ComfyUI（推荐）

**优点:**
- ✅ 使用本地 GPU，速度快
- ✅ 无 API 费用
- ✅ 完全控制生成参数
- ✅ 支持自定义模型

**要求:**
- ComfyUI 服务器运行在 `http://localhost:8188`
- SDXL Base 1.0 模型已安装

**检查:**
```bash
curl -sf http://localhost:8188/system_stats
```

### OpenAI 云端（回退）

**优点:**
- ✅ 无需本地 GPU
- ✅ 无需安装 ComfyUI

**缺点:**
- ❌ 产生 API 费用
- ❌ 参数控制有限
- ❌ 依赖网络连接

**要求:**
- 设置 `OPENAI_API_KEY` 环境变量

**检查:**
```bash
echo $OPENAI_API_KEY
```

## 测试

### 测试 Python API 是否使用本地 ComfyUI

```bash
python examples/test_python_api.py
```

这个脚本会：
1. ✅ 检查 ComfyUI 连接
2. ✅ 检测使用的后端（ComfyUI 或 OpenAI）
3. ✅ 执行完整的 4 步流程
4. ✅ 验证生成的图片

### 预期输出

```
╔════════════════════════════════════════════════════════════╗
║     测试 Python API 是否调用本地 ComfyUI                   ║
╚════════════════════════════════════════════════════════════╝

============================================================
Step 0: 检查 ComfyUI 连接
============================================================

✓ ComfyUI 服务器运行中
  URL: http://localhost:8188

============================================================
检查 WorkflowExecutorAgent 后端选择
============================================================

选择的后端: comfyui

✓ 使用本地 ComfyUI
  - 优点: 本地 GPU 加速，无 API 费用
  - 需要: ComfyUI 服务器运行中

============================================================
测试完整的 4 步流程
============================================================

输入参数:
{
  "brief": "一个黑色的蓝牙音箱，科技风格，工作室拍摄",
  "num_images": 2,
  "output_directory": "/tmp/design_output_test"
}

开始执行 4 步流程...

============================================================
✓ 执行成功!
============================================================

生成的图片:
  1. HERO
     路径: /tmp/design_output_test/hero_001_00001_.png
     提示词: bluetooth speaker, center frame, hero shot, studio backdrop...
     种子: 42

  2. FEATURE
     路径: /tmp/design_output_test/feature_001_00001_.png
     提示词: bluetooth speaker, waterproof feature, water splash...
     种子: 42

总共生成: 2 张图片
输出目录: /tmp/design_output_test

验证文件:
  ✓ hero_001_00001_.png (1234.5 KB)
  ✓ feature_001_00001_.png (1456.7 KB)

============================================================
✓ 测试完成!

结论:
  ✓ Python API 正确调用本地 ComfyUI
  ✓ 完整的 4 步流程执行成功
  ✓ 图片已保存到本地
============================================================
```

## 故障排除

### ComfyUI 连接失败

**症状:**
```
✗ ComfyUI 服务器未响应
  URL: http://localhost:8188
```

**解决:**
```bash
# 检查 ComfyUI 是否运行
curl -sf http://localhost:8188/system_stats

# 如果未运行，启动 ComfyUI
cd /path/to/ComfyUI
python main.py
```

### 模型未找到

**症状:**
```
HTTPStatusError: 500 Internal Server Error
```

**解决:**
```bash
# 检查可用模型
curl -sf http://localhost:8188/object_info | grep -o '"[^"]*\.safetensors"'

# 确保 sd_xl_base_1.0.safetensors 在列表中
# 如果没有，下载并放到 ComfyUI/models/checkpoints/
```

### 使用了 OpenAI 而不是 ComfyUI

**症状:**
```
选择的后端: openai

⚠ 使用 OpenAI 云端 API
```

**原因:**
- ComfyUI 服务器未运行
- ComfyUI URL 配置错误

**解决:**
```bash
# 1. 启动 ComfyUI
cd /path/to/ComfyUI
python main.py

# 2. 或设置正确的 URL
export COMFYUI_URL=http://192.168.1.100:8188
```

## 总结

Python API (`DesignSkillsEngine`) **确实调用本地 ComfyUI** 进行图片生成：

1. ✅ `WorkflowExecutorAgent` 优先检测 ComfyUI
2. ✅ `ComfyUIClient` 通过 HTTP API 与本地 ComfyUI 通信
3. ✅ 使用标准的 SDXL txt2img 工作流
4. ✅ 只有在 ComfyUI 不可用时才回退到 OpenAI

运行 `python examples/test_python_api.py` 可以验证整个流程。

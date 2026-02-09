# 如何运行测试

## 在 WSL Ubuntu 环境下运行

### 方法1: 直接运行测试脚本

```bash
# 1. 进入 WSL Ubuntu
wsl -d Ubuntu

# 2. 切换到项目目录
cd /home/liush/projects/serverlessAIAgents

# 3. 激活 conda 环境
conda activate agentflow

# 4. 运行测试
python examples/test_python_api.py
```

### 方法2: 一行命令

```bash
wsl -d Ubuntu bash -c "cd /home/liush/projects/serverlessAIAgents && python3 examples/test_python_api.py"
```

### 方法3: 使用 pytest 运行现有的 E2E 测试

```bash
# 进入 WSL
wsl -d Ubuntu

# 切换目录
cd /home/liush/projects/serverlessAIAgents

# 激活环境
conda activate agentflow

# 运行 E2E 测试（需要 ComfyUI 运行）
pytest --no-cov -m e2e tests/e2e/test_design_skills_e2e.py -v

# 或运行单个测试
pytest --no-cov tests/e2e/test_design_skills_e2e.py::TestFullPipeline::test_brief_to_images -v
```

## 预期结果

### 如果 ComfyUI 运行中

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

### 如果 ComfyUI 未运行

```
============================================================
Step 0: 检查 ComfyUI 连接
============================================================

✗ ComfyUI 服务器未响应
  URL: http://localhost:8188

请确保 ComfyUI 服务器正在运行:
  curl -sf http://localhost:8188/system_stats

============================================================
⚠ ComfyUI 未运行，无法继续测试
============================================================
```

## 启动 ComfyUI

如果 ComfyUI 未运行，需要先启动：

```bash
# 在另一个终端
cd /path/to/ComfyUI
python main.py

# 或使用 GPU
python main.py --gpu-only
```

## 验证 ComfyUI 状态

```bash
# 检查 ComfyUI 是否运行
curl -sf http://localhost:8188/system_stats

# 检查 SDXL 模型
curl -sf http://localhost:8188/object_info | grep sd_xl_base_1.0
```

## 故障排除

### 问题1: ModuleNotFoundError

```
ModuleNotFoundError: No module named 'agentflow'
```

**解决:**
```bash
# 确保在正确的 conda 环境
conda activate agentflow

# 安装 agentflow
pip install -e .
```

### 问题2: ComfyUI 连接失败

```
✗ ComfyUI 服务器未响应
```

**解决:**
```bash
# 检查 ComfyUI 是否运行
ps aux | grep ComfyUI

# 启动 ComfyUI
cd /path/to/ComfyUI
python main.py
```

### 问题3: 使用了 OpenAI 而不是 ComfyUI

```
选择的后端: openai
```

**原因:**
- ComfyUI 未运行
- ComfyUI URL 配置错误

**解决:**
```bash
# 启动 ComfyUI
cd /path/to/ComfyUI
python main.py

# 或设置正确的 URL
export COMFYUI_URL=http://localhost:8188
```

## 快速验证命令

```bash
# 一键检查所有前提条件
cd /home/liush/projects/serverlessAIAgents
python -c "
import asyncio
from agentflow.skills.builtin.design_skills.tools.comfyui_client import ComfyUIClient

async def check():
    client = ComfyUIClient()
    ok = await client.health_check()
    await client.close()
    print('✓ ComfyUI OK' if ok else '✗ ComfyUI 不可用')

asyncio.run(check())
"
```

#!/bin/bash
# Design Skills 快速测试脚本

echo "╔════════════════════════════════════════════════════════════╗"
echo "║         Design Skills 快速测试                             ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# 颜色定义
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 1. 检查 ComfyUI
echo "1. 检查 ComfyUI 服务器..."
if curl -sf ${COMFYUI_URL:-http://localhost:8188}/system_stats > /dev/null 2>&1; then
    echo -e "   ${GREEN}✓${NC} ComfyUI 服务器运行中"
else
    echo -e "   ${RED}✗${NC} ComfyUI 服务器未运行"
    echo ""
    echo "   请先启动 ComfyUI 服务器"
    exit 1
fi

# 2. 检查 SDXL 模型
echo "2. 检查 SDXL 模型..."
if curl -sf ${COMFYUI_URL:-http://localhost:8188}/object_info 2>/dev/null | grep -q "sd_xl_base_1.0.safetensors"; then
    echo -e "   ${GREEN}✓${NC} SDXL 模型已安装"
else
    echo -e "   ${RED}✗${NC} SDXL 模型未找到"
    echo ""
    echo "   请安装 sd_xl_base_1.0.safetensors 到 ComfyUI 的 models/checkpoints/ 目录"
    exit 1
fi

echo ""
echo "════════════════════════════════════════════════════════════"
echo "开始生成测试图片..."
echo "════════════════════════════════════════════════════════════"
echo ""

# 3. 生成图片
echo "提示词: bluetooth speaker, black, studio lighting, product photography"
echo "尺寸: 1024x1024"
echo "种子: 42"
echo ""

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

# 检查结果
if [ $? -eq 0 ]; then
    echo ""
    echo "════════════════════════════════════════════════════════════"
    echo -e "${GREEN}✓ 生成成功!${NC}"
    echo "════════════════════════════════════════════════════════════"
    echo ""
    echo "生成的图片保存在: /tmp/design_output/"
    echo ""
    echo "查看图片:"
    echo "  ls -lh /tmp/design_output/"
else
    echo ""
    echo "════════════════════════════════════════════════════════════"
    echo -e "${RED}✗ 生成失败${NC}"
    echo "════════════════════════════════════════════════════════════"
    exit 1
fi


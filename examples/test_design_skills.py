#!/usr/bin/env python3
"""Design Skills 测试脚本.

演示如何使用 design_skills 生成设计图。

使用方法:
    # 方法1: 使用 Python API
    python examples/test_design_skills.py

    # 方法2: 使用 Standalone Script
    echo '{"prompt": "bluetooth speaker, black, studio lighting", "width": 1024, "height": 1024}' | python agentflow/skills/builtin/design_skills/scripts/generate_images.py
"""

import asyncio
import json
import sys
from pathlib import Path


async def test_python_api() -> None:
    """测试 Python API 方式."""
    print("=" * 60)
    print("方法1: 使用 Python API (DesignSkillsEngine)")
    print("=" * 60)
    print()

    try:
        from agentflow.skills.builtin.design_skills import DesignSkillsEngine

        engine = DesignSkillsEngine()

        # 测试输入
        input_data = {
            "brief": "一个黑色的蓝牙音箱，科技风格，工作室拍摄",
            "num_images": 3,
            "target_platform": "amazon",
            "output_directory": "/tmp/design_output",
        }

        print(f"输入: {json.dumps(input_data, ensure_ascii=False, indent=2)}")
        print()
        print("正在生成图片...")
        print()

        result = await engine.run(input_data)

        print("✓ 生成成功!")
        print()
        print(f"结果: {json.dumps(result, ensure_ascii=False, indent=2)}")

    except ImportError as e:
        print(f"✗ 导入失败: {e}")
        print()
        print("提示: 请确保已安装 agentflow:")
        print("  pip install -e .")
        sys.exit(1)
    except Exception as e:
        print(f"✗ 执行失败: {e}")
        print()
        print("可能的原因:")
        print("  1. ComfyUI 服务器未运行")
        print("  2. SDXL 模型未安装")
        print()
        print("解决方法:")
        print("  # 检查 ComfyUI 状态")
        print("  curl -sf http://localhost:8188/system_stats")
        print()
        print("  # 检查 SDXL 模型")
        print("  curl -sf http://localhost:8188/object_info | grep sd_xl_base_1.0")
        sys.exit(1)


def test_standalone_script() -> None:
    """测试 Standalone Script 方式."""
    print()
    print("=" * 60)
    print("方法2: 使用 Standalone Script")
    print("=" * 60)
    print()

    script_path = Path("agentflow/skills/builtin/design_skills/scripts/generate_images.py")

    if not script_path.exists():
        print(f"✗ 脚本不存在: {script_path}")
        sys.exit(1)

    print("使用方法:")
    print()
    print("# 生成单张图片")
    print("echo '{")
    print('  "prompt": "bluetooth speaker, black, studio lighting, product photography",')
    print('  "negative_prompt": "blurry, low quality, text, watermark",')
    print('  "width": 1024,')
    print('  "height": 1024,')
    print('  "seed": 42,')
    print('  "steps": 20,')
    print('  "cfg_scale": 7.0,')
    print('  "model": "sd_xl_base_1.0.safetensors",')
    print('  "output_dir": "/tmp/design_output"')
    print(f"}}' | python {script_path}")
    print()
    print("输出格式 (JSON):")
    print("{")
    print('  "success": true,')
    print('  "image_path": "/tmp/design_output/img_abc123.png",')
    print('  "prompt_id": "comfyui-prompt-id",')
    print('  "seed": 42')
    print("}")


def check_prerequisites() -> bool:
    """检查前提条件."""
    print("=" * 60)
    print("前提条件检查")
    print("=" * 60)
    print()

    import subprocess

    # 检查 ComfyUI
    print("1. 检查 ComfyUI 服务器...")
    try:
        result = subprocess.run(
            ["curl", "-sf", "http://localhost:8188/system_stats"],
            capture_output=True,
            timeout=5,
        )
        if result.returncode == 0:
            print("   ✓ ComfyUI 服务器运行中")
        else:
            print("   ✗ ComfyUI 服务器未运行")
            print()
            print("   请启动 ComfyUI 服务器:")
            print("   (参考 ComfyUI 文档)")
            return False
    except (subprocess.TimeoutExpired, FileNotFoundError):
        print("   ✗ 无法连接到 ComfyUI")
        return False

    # 检查 SDXL 模型
    print("2. 检查 SDXL 模型...")
    try:
        result = subprocess.run(
            ["curl", "-sf", "http://localhost:8188/object_info"],
            capture_output=True,
            timeout=5,
        )
        if result.returncode == 0 and b"sd_xl_base_1.0.safetensors" in result.stdout:
            print("   ✓ SDXL 模型已安装")
        else:
            print("   ✗ SDXL 模型未找到")
            print()
            print("   请安装 sd_xl_base_1.0.safetensors 到 ComfyUI 的 models/checkpoints/ 目录")
            return False
    except (subprocess.TimeoutExpired, FileNotFoundError):
        print("   ✗ 无法检查模型")
        return False

    # 检查 httpx
    print("3. 检查 Python 依赖...")
    try:
        import httpx  # noqa: F401

        print("   ✓ httpx 已安装")
    except ImportError:
        print("   ✗ httpx 未安装")
        print()
        print("   请安装依赖:")
        print("   pip install httpx")
        return False

    print()
    print("✓ 所有前提条件满足!")
    print()
    return True


async def main() -> None:
    """主函数."""
    print()
    print("╔════════════════════════════════════════════════════════════╗")
    print("║         Design Skills 测试脚本                             ║")
    print("╚════════════════════════════════════════════════════════════╝")
    print()

    # 检查前提条件
    if not check_prerequisites():
        sys.exit(1)

    # 测试 Python API
    await test_python_api()

    # 显示 Standalone Script 使用方法
    test_standalone_script()

    print()
    print("=" * 60)
    print("测试完成!")
    print("=" * 60)
    print()
    print("生成的图片保存在: /tmp/design_output/")
    print()


if __name__ == "__main__":
    asyncio.run(main())


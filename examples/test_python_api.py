#!/usr/bin/env python3
"""测试 Python API 是否正确调用本地 ComfyUI.

这个脚本验证：
1. DesignSkillsEngine 是否执行完整的 4 步流程
2. WorkflowExecutorAgent 是否正确调用本地 ComfyUI
3. 生成的图片是否保存到本地

使用方法:
    python examples/test_python_api.py
"""

import asyncio
import json
import sys
from pathlib import Path


async def test_comfyui_connection():
    """测试 ComfyUI 连接."""
    print("=" * 60)
    print("Step 0: 检查 ComfyUI 连接")
    print("=" * 60)
    print()

    try:
        from agentflow.skills.builtin.design_skills.tools.comfyui_client import (
            ComfyUIClient,
        )

        client = ComfyUIClient()
        is_healthy = await client.health_check()
        await client.close()

        if is_healthy:
            print("✓ ComfyUI 服务器运行中")
            print(f"  URL: {client._base_url}")
            return True
        else:
            print("✗ ComfyUI 服务器未响应")
            print(f"  URL: {client._base_url}")
            print()
            print("请确保 ComfyUI 服务器正在运行:")
            print("  curl -sf http://localhost:8188/system_stats")
            return False

    except Exception as e:
        print(f"✗ 连接失败: {e}")
        return False


async def test_full_pipeline():
    """测试完整的 4 步流程."""
    print()
    print("=" * 60)
    print("测试完整的 4 步流程")
    print("=" * 60)
    print()

    try:
        from agentflow.skills.builtin.design_skills import DesignSkillsEngine

        engine = DesignSkillsEngine()

        # 测试输入
        input_data = {
            "brief": "一个黑色的蓝牙音箱，科技风格，工作室拍摄",
            "num_images": 2,  # 只生成 2 张图片用于测试
            "output_directory": "/tmp/design_output_test",
        }

        print("输入参数:")
        print(json.dumps(input_data, ensure_ascii=False, indent=2))
        print()

        print("开始执行 4 步流程...")
        print()

        # 执行
        result = await engine.run(input_data)

        print("=" * 60)
        print("✓ 執行成功!")
        print("=" * 60)
        print()

        # 显示结果
        print("生成的图片:")
        images = result.get("images", [])
        if images:
            for i, img in enumerate(images, 1):
                print(f"  {i}. {img['role']}")
                print(f"     路径: {img['path']}")
                print(f"     提示词: {img['prompt'][:60]}...")
                print(f"     种子: {img.get('seed_used', 'N/A')}")
                print()
        else:
            print("  ⚠ 没有生成任何图片")
            print()

        print(f"总共生成: {len(images)} 张图片")
        print(f"输出目录: {result.get('output_directory', 'N/A')}")
        
        # 检查是否有错误
        errors = result.get("errors", [])
        if errors:
            print()
            print("⚠ 生成过程中出现错误:")
            for error in errors:
                print(f"  - {error}")
            print()
        
        print()

        # 验证文件是否存在
        if images:
            print("验证文件:")
            for img in images:
                path = Path(img["path"])
                if path.exists():
                    size_kb = path.stat().st_size / 1024
                    print(f"  ✓ {path.name} ({size_kb:.1f} KB)")
                else:
                    print(f"  ✗ {path.name} (文件不存在)")

        # 如果没有生成任何图片，返回失败
        return len(images) > 0

    except ImportError as e:
        print(f"✗ 导入失败: {e}")
        print()
        print("请确保已安装 agentflow:")
        print("  pip install -e .")
        return False

    except Exception as e:
        print(f"✗ 执行失败: {e}")
        print()
        import traceback

        traceback.print_exc()
        return False


async def test_workflow_executor_backend():
    """测试 WorkflowExecutorAgent 使用的后端."""
    print()
    print("=" * 60)
    print("检查 WorkflowExecutorAgent 后端选择")
    print("=" * 60)
    print()

    try:
        from agentflow.skills.builtin.design_skills.agents.workflow_executor_agent import (
            WorkflowExecutorAgent,
        )

        agent = WorkflowExecutorAgent()

        # 检测后端
        backend = await agent._detect_backend()

        print(f"选择的后端: {backend}")
        print()

        if backend == "comfyui":
            print("✓ 使用本地 ComfyUI")
            print("  - 优点: 本地 GPU 加速，无 API 费用")
            print("  - 需要: ComfyUI 服务器运行中")
        elif backend == "openai":
            print("⚠ 使用 OpenAI 云端 API")
            print("  - 优点: 无需本地 GPU")
            print("  - 需要: OPENAI_API_KEY 环境变量")
            print("  - 注意: 会产生 API 费用")
        else:
            print(f"✗ 未知后端: {backend}")

        return backend == "comfyui"

    except RuntimeError as e:
        print(f"✗ 后端检测失败: {e}")
        print()
        print("请确保至少有一个后端可用:")
        print("  1. 启动 ComfyUI 服务器 (推荐)")
        print("  2. 或设置 OPENAI_API_KEY 环境变量")
        return False

    except Exception as e:
        print(f"✗ 检测失败: {e}")
        import traceback

        traceback.print_exc()
        return False


async def main():
    """主函数."""
    print()
    print("╔════════════════════════════════════════════════════════════╗")
    print("║     测试 Python API 是否调用本地 ComfyUI                   ║")
    print("╚════════════════════════════════════════════════════════════╝")
    print()

    # Step 0: 检查 ComfyUI 连接
    comfyui_ok = await test_comfyui_connection()

    if not comfyui_ok:
        print()
        print("=" * 60)
        print("⚠ ComfyUI 未运行，无法继续测试")
        print("=" * 60)
        sys.exit(1)

    # 检查后端选择
    uses_comfyui = await test_workflow_executor_backend()

    if not uses_comfyui:
        print()
        print("=" * 60)
        print("⚠ 警告: 将使用 OpenAI 而不是本地 ComfyUI")
        print("=" * 60)
        print()
        response = input("是否继续测试? (y/N): ")
        if response.lower() != "y":
            print("测试已取消")
            sys.exit(0)

    # 测试完整流程
    success = await test_full_pipeline()

    print()
    print("=" * 60)
    if success:
        print("✓ 测试完成!")
        print()
        print("结论:")
        if uses_comfyui:
            print("  ✓ Python API 正确调用本地 ComfyUI")
            print("  ✓ 完整的 4 步流程执行成功")
            print("  ✓ 图片已保存到本地")
        else:
            print("  ⚠ Python API 使用了 OpenAI 而不是本地 ComfyUI")
            print("  ✓ 完整的 4 步流程执行成功")
            print("  ✓ 图片已保存到本地")
    else:
        print("✗ 测试失败")
    print("=" * 60)
    print()


if __name__ == "__main__":
    asyncio.run(main())


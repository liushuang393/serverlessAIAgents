# -*- coding: utf-8 -*-
"""内置沙盒工具.

将沙盒功能封装为 MCP Tool，Agent 配置后可用。
配置启用才加载，不配置不加载。

支持的 provider:
- microsandbox（默认，推荐）
- docker
- e2b

使用方式:
    在 Agent 中配置 sandbox_provider 即可启用:

    >>> @agent
    ... class MyAgent:
    ...     sandbox_provider = "microsandbox"  # 启用沙盒

注意：此模块为内部实现，不直接导出给用户。
"""

from __future__ import annotations

import logging
from typing import Any

from agentflow.sandbox.base import SandboxConfig, SandboxProvider

logger = logging.getLogger(__name__)

# 内部沙盒实例（按 provider 缓存）
_sandbox_instances: dict[str, SandboxProvider] = {}


def _get_sandbox(provider: str = "microsandbox") -> SandboxProvider:
    """获取沙盒实例（懒加载）.

    Args:
        provider: 沙盒类型 (microsandbox/docker/e2b)

    Returns:
        SandboxProvider 实例
    """
    if provider in _sandbox_instances:
        return _sandbox_instances[provider]

    config = SandboxConfig()

    if provider == "microsandbox":
        from agentflow.sandbox.microsandbox_provider import MicrosandboxProvider
        instance = MicrosandboxProvider(config)
    elif provider == "docker":
        from agentflow.sandbox.docker_provider import DockerProvider
        instance = DockerProvider(config)
    elif provider == "e2b":
        from agentflow.sandbox.e2b_provider import E2BProvider
        instance = E2BProvider(config)
    else:
        raise ValueError(f"Unknown sandbox provider: {provider}. Use: microsandbox/docker/e2b")

    _sandbox_instances[provider] = instance
    logger.info(f"Sandbox initialized: {provider}")
    return instance


async def execute_python_code(
    code: str,
    packages: list[str] | None = None,
    timeout: float = 60.0,
    *,
    _provider: str = "microsandbox",
) -> dict[str, Any]:
    """执行 Python 代码.

    Args:
        code: 要执行的 Python 代码
        packages: 需要安装的包列表
        timeout: 超时秒数
        _provider: 沙盒类型（内部参数）

    Returns:
        执行结果字典
    """
    sandbox = _get_sandbox(_provider)
    result = await sandbox.execute(
        code=code,
        packages=packages,
        timeout=timeout,
    )

    return {
        "success": result.success,
        "stdout": result.stdout,
        "stderr": result.stderr,
        "exit_code": result.exit_code,
        "duration_ms": result.duration_ms,
        "error": result.error,
    }


# MCP Tool 定义（OpenAI Function Calling 兼容格式）
EXECUTE_CODE_TOOL_DEFINITION = {
    "type": "function",
    "function": {
        "name": "execute_python",
        "description": "在安全沙盒中执行 Python 代码。用于数据分析、计算、文件处理等任务。",
        "parameters": {
            "type": "object",
            "properties": {
                "code": {
                    "type": "string",
                    "description": "要执行的 Python 代码",
                },
                "packages": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "需要安装的 pip 包列表（可选）",
                },
            },
            "required": ["code"],
        },
    },
}


def create_sandbox_tool(provider: str = "microsandbox") -> dict[str, Any]:
    """创建沙盒 Tool 实例.

    Args:
        provider: 沙盒类型

    Returns:
        Tool 信息字典（包含执行函数）
    """
    async def _execute(code: str, packages: list[str] | None = None) -> dict[str, Any]:
        return await execute_python_code(code, packages, _provider=provider)

    return {
        "name": "execute_python",
        "description": EXECUTE_CODE_TOOL_DEFINITION["function"]["description"],
        "func": _execute,
        "definition": EXECUTE_CODE_TOOL_DEFINITION,
    }


def get_tool_definition() -> dict[str, Any]:
    """获取 Tool 定义（OpenAI 格式）."""
    return EXECUTE_CODE_TOOL_DEFINITION


# -*- coding: utf-8 -*-
"""LLM Tool Calling 集成测试.

本模块使用真实LLM API测试各提供商对工具/函数调用的支持程度。
测试内容:
- OpenAI GPT-4o 的 function calling
- Anthropic Claude 的 tool use
- Google Gemini 的 function calling

运行方式:
    pytest tests/integration/test_llm_tool_calling.py -v -s

前提条件:
    需要设置以下环境变量:
    - OPENAI_API_KEY
    - ANTHROPIC_API_KEY
    - GEMINI_API_KEY (或 GOOGLE_API_KEY)
"""

import json
import os

import pytest

from agentflow.llm import LLMClient, LLMConfig, LLMMessage


# 标准工具定义（OpenAI格式）
SAMPLE_TOOLS = [
    {
        "type": "function",
        "function": {
            "name": "get_weather",
            "description": "获取指定城市的天气信息",
            "parameters": {
                "type": "object",
                "properties": {
                    "city": {
                        "type": "string",
                        "description": "城市名称，如'Tokyo'或'New York'",
                    },
                    "unit": {
                        "type": "string",
                        "description": "温度单位: 'celsius' 或 'fahrenheit'",
                        "enum": ["celsius", "fahrenheit"],
                    },
                },
                "required": ["city"],
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "search_products",
            "description": "在产品数据库中搜索产品",
            "parameters": {
                "type": "object",
                "properties": {
                    "query": {
                        "type": "string",
                        "description": "搜索关键词",
                    },
                    "max_results": {
                        "type": "integer",
                        "description": "最大返回结果数",
                    },
                },
                "required": ["query"],
            },
        },
    },
]


def has_openai_key() -> bool:
    """检查OpenAI API key是否可用."""
    return bool(os.environ.get("OPENAI_API_KEY"))


def has_anthropic_key() -> bool:
    """检查Anthropic API key是否可用."""
    return bool(os.environ.get("ANTHROPIC_API_KEY"))


def has_gemini_key() -> bool:
    """检查Gemini API key是否可用."""
    return bool(os.environ.get("GEMINI_API_KEY"))


def has_gemini_library() -> bool:
    """检查google-genai库是否已安装."""
    try:
        from google import genai  # noqa: F401
        return True
    except ImportError:
        return False


class TestOpenAIToolCalling:
    """OpenAI GPT-4o Tool Calling 测试."""

    @pytest.mark.skipif(not has_openai_key(), reason="OPENAI_API_KEY not set")
    @pytest.mark.asyncio
    async def test_single_tool_call(self):
        """测试单个工具调用识别."""
        config = LLMConfig(provider="openai", model="gpt-4o-mini")
        client = LLMClient(config)

        messages = [
            LLMMessage(role="user", content="东京今天天气怎么样？请使用celsius单位。"),
        ]

        response = await client.chat(messages, tools=SAMPLE_TOOLS)

        print(f"\n[OpenAI] Response: {response}")
        print(f"[OpenAI] Tool calls: {response.tool_calls}")

        # 验证LLM识别到需要调用工具
        assert response.has_tool_calls(), "OpenAI应该识别出需要调用get_weather工具"
        assert len(response.tool_calls) >= 1

        # 验证工具名称
        tool_call = response.tool_calls[0]
        assert tool_call.name == "get_weather", f"期望get_weather，实际: {tool_call.name}"

        # 验证参数
        args = tool_call.get_arguments_dict()
        assert "city" in args, "参数中应包含city"
        print(f"[OpenAI] Parsed arguments: {args}")

    @pytest.mark.skipif(not has_openai_key(), reason="OPENAI_API_KEY not set")
    @pytest.mark.asyncio
    async def test_tool_choice_selection(self):
        """测试LLM能正确选择工具."""
        config = LLMConfig(provider="openai", model="gpt-4o-mini")
        client = LLMClient(config)

        messages = [
            LLMMessage(role="user", content="帮我搜索价格低于100美元的笔记本电脑"),
        ]

        response = await client.chat(messages, tools=SAMPLE_TOOLS)

        print(f"\n[OpenAI] Response for product search: {response}")

        if response.has_tool_calls():
            tool_call = response.tool_calls[0]
            print(f"[OpenAI] Selected tool: {tool_call.name}")
            assert tool_call.name == "search_products", "应该选择search_products工具"


class TestAnthropicToolCalling:
    """Anthropic Claude Tool Calling 测试."""

    @pytest.mark.skipif(not has_anthropic_key(), reason="ANTHROPIC_API_KEY not set")
    @pytest.mark.asyncio
    async def test_single_tool_call(self):
        """测试单个工具调用识别."""
        config = LLMConfig(provider="anthropic", model="claude-sonnet-4-20250514")
        client = LLMClient(config)

        messages = [
            LLMMessage(role="user", content="What's the weather like in Paris? Use celsius."),
        ]

        response = await client.chat(messages, tools=SAMPLE_TOOLS)

        print(f"\n[Anthropic] Response: {response}")
        print(f"[Anthropic] Tool calls: {response.tool_calls}")

        assert response.has_tool_calls(), "Anthropic应该识别出需要调用get_weather工具"

        tool_call = response.tool_calls[0]
        assert tool_call.name == "get_weather"
        args = tool_call.get_arguments_dict()
        assert "city" in args
        print(f"[Anthropic] Parsed arguments: {args}")


class TestGeminiToolCalling:
    """Google Gemini Tool Calling 测试."""

    @pytest.mark.skipif(
        not has_gemini_key() or not has_gemini_library(),
        reason="GEMINI_API_KEY not set or google-generativeai not installed"
    )
    @pytest.mark.asyncio
    async def test_single_tool_call(self):
        """测试单个工具调用识别."""
        config = LLMConfig(provider="google", model="gemini-2.0-flash")
        client = LLMClient(config)

        messages = [
            LLMMessage(role="user", content="Tell me the weather in London please, in celsius."),
        ]

        response = await client.chat(messages, tools=SAMPLE_TOOLS)

        print(f"\n[Gemini] Response: {response}")
        print(f"[Gemini] Tool calls: {response.tool_calls}")

        assert response.has_tool_calls(), "Gemini应该识别出需要调用get_weather工具"

        tool_call = response.tool_calls[0]
        assert tool_call.name == "get_weather"
        args = tool_call.get_arguments_dict()
        assert "city" in args
        print(f"[Gemini] Parsed arguments: {args}")


class TestCrossProviderComparison:
    """跨提供商比较测试."""

    @pytest.mark.asyncio
    async def test_all_providers_recognize_tool(self):
        """测试所有可用的提供商都能识别工具调用."""
        results = {}

        # 相同的测试提示
        messages = [
            LLMMessage(role="user", content="What is the current weather in New York?"),
        ]

        # 测试OpenAI
        if has_openai_key():
            try:
                config = LLMConfig(provider="openai", model="gpt-4o-mini")
                client = LLMClient(config)
                response = await client.chat(messages, tools=SAMPLE_TOOLS)
                results["openai"] = {
                    "recognized": response.has_tool_calls(),
                    "tool_name": response.tool_calls[0].name if response.tool_calls else None,
                    "arguments": response.tool_calls[0].get_arguments_dict() if response.tool_calls else None,
                }
            except Exception as e:
                results["openai"] = {"error": str(e)}

        # 测试Anthropic
        if has_anthropic_key():
            try:
                config = LLMConfig(provider="anthropic", model="claude-sonnet-4-20250514")
                client = LLMClient(config)
                response = await client.chat(messages, tools=SAMPLE_TOOLS)
                results["anthropic"] = {
                    "recognized": response.has_tool_calls(),
                    "tool_name": response.tool_calls[0].name if response.tool_calls else None,
                    "arguments": response.tool_calls[0].get_arguments_dict() if response.tool_calls else None,
                }
            except Exception as e:
                results["anthropic"] = {"error": str(e)}

        # 测试Gemini（仅在库已安装时）
        if has_gemini_key() and has_gemini_library():
            try:
                config = LLMConfig(provider="google", model="gemini-2.0-flash")
                client = LLMClient(config)
                response = await client.chat(messages, tools=SAMPLE_TOOLS)
                results["gemini"] = {
                    "recognized": response.has_tool_calls(),
                    "tool_name": response.tool_calls[0].name if response.tool_calls else None,
                    "arguments": response.tool_calls[0].get_arguments_dict() if response.tool_calls else None,
                }
            except Exception as e:
                results["gemini"] = {"error": str(e)}

        # 打印比较结果
        print("\n" + "=" * 60)
        print("跨提供商工具识别比较结果:")
        print("=" * 60)
        for provider, result in results.items():
            print(f"\n{provider.upper()}:")
            if "error" in result:
                print(f"  错误: {result['error']}")
            else:
                print(f"  识别工具: {result['recognized']}")
                print(f"  工具名称: {result['tool_name']}")
                print(f"  参数: {json.dumps(result['arguments'], ensure_ascii=False)}")
        print("=" * 60)

        # 至少有一个提供商测试通过
        assert len(results) > 0, "至少需要一个可用的LLM提供商"

        # 检查所有测试的提供商都识别了工具
        for provider, result in results.items():
            if "error" not in result:
                assert result["recognized"], f"{provider}未能识别工具调用"
                assert result["tool_name"] == "get_weather", f"{provider}选择了错误的工具"


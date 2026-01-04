# -*- coding: utf-8 -*-
"""测试 @agent 装饰器的 Pydantic 支持.

验证:
- input_schema 自动验证输入
- output_schema 自动序列化输出
- 与非 Pydantic Agent 的兼容性
"""

from unittest.mock import patch

import pytest
from pydantic import BaseModel, ValidationError

from agentflow.agent_decorator import agent, AgentClient, _agent_registry


# ============================================================
# Test Schemas
# ============================================================


class QuestionInput(BaseModel):
    """测试输入模式."""

    question: str
    context: str | None = None


class AnswerOutput(BaseModel):
    """测试输出模式."""

    answer: str
    confidence: float = 0.5


# ============================================================
# Test Agents (定义在 fixture 中，避免全局副作用)
# ============================================================

def _create_simple_agent():
    """创建简单Agent."""
    @agent
    class SimpleAgent:
        """无类型的简单Agent."""
        system_prompt = "你是一个助手"

        async def process(self, input_data: dict) -> dict:
            return {"response": f"回答: {input_data.get('question', '')}"}

    return SimpleAgent


def _create_typed_agent():
    """创建带类型的Agent."""
    @agent
    class TypedAgent:
        """带Pydantic类型的Agent."""
        system_prompt = "你是一个带类型的助手"
        input_schema = QuestionInput
        output_schema = AnswerOutput

        async def process(self, input_data: QuestionInput) -> AnswerOutput:
            return AnswerOutput(
                answer=f"回答: {input_data.question}",
                confidence=0.9,
            )

    return TypedAgent


# ============================================================
# Tests
# ============================================================


class TestPydanticSupport:
    """测试 Pydantic 支持."""

    def setup_method(self):
        """每个测试前清理注册表."""
        _agent_registry.clear()
        # 重新创建并注册测试Agent
        _create_simple_agent()
        _create_typed_agent()

    def test_simple_agent_registered(self):
        """SimpleAgent 应该被注册."""
        assert "SimpleAgent" in _agent_registry

    def test_typed_agent_registered(self):
        """TypedAgent 应该被注册."""
        assert "TypedAgent" in _agent_registry

    def test_typed_agent_has_schemas(self):
        """TypedAgent 应该有输入/输出模式."""
        registered = _agent_registry["TypedAgent"]
        assert registered.input_schema is QuestionInput
        assert registered.output_schema is AnswerOutput
        assert registered.has_typed_schema is True

    def test_simple_agent_no_schemas(self):
        """SimpleAgent 不应该有输入/输出模式."""
        registered = _agent_registry["SimpleAgent"]
        assert registered.input_schema is None
        assert registered.output_schema is None
        assert registered.has_typed_schema is False

    @pytest.mark.asyncio
    @patch("agentflow.providers.get_llm", return_value=None)
    async def test_simple_agent_invoke(self, mock_llm):
        """测试无类型Agent的调用."""
        client = AgentClient.get("SimpleAgent")
        result = await client.invoke({"question": "你好"})
        assert "response" in result
        assert "你好" in result["response"]

    @pytest.mark.asyncio
    @patch("agentflow.providers.get_llm", return_value=None)
    async def test_typed_agent_invoke(self, mock_llm):
        """测试带类型Agent的调用."""
        client = AgentClient.get("TypedAgent")
        result = await client.invoke({"question": "你好"})
        # 输出应该被序列化为字典
        assert isinstance(result, dict)
        assert "answer" in result
        assert "confidence" in result
        assert abs(result["confidence"] - 0.9) < 0.01

    @pytest.mark.asyncio
    @patch("agentflow.providers.get_llm", return_value=None)
    async def test_typed_agent_validation_success(self, mock_llm):
        """测试输入验证成功."""
        client = AgentClient.get("TypedAgent")
        result = await client.invoke({"question": "有效问题", "context": "上下文"})
        assert "answer" in result

    @pytest.mark.asyncio
    async def test_typed_agent_validation_failure(self):
        """测试输入验证失败（缺少必填字段）."""
        # 验证发生在 invoke 之前，不需要 mock LLM
        registered = _agent_registry["TypedAgent"]
        with pytest.raises(ValidationError):
            registered.validate_input({"context": "只有上下文没有问题"})

    @pytest.mark.asyncio
    async def test_typed_agent_validation_wrong_type(self):
        """测试输入验证失败（类型错误）."""
        registered = _agent_registry["TypedAgent"]
        # Pydantic 会尝试转换，123 会变成 "123"
        # 但 None 或者完全缺失 question 会失败
        with pytest.raises(ValidationError):
            registered.validate_input({})  # 完全缺失 question


if __name__ == "__main__":
    pytest.main([__file__, "-v"])


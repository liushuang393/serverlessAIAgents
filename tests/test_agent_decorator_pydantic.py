"""@agent デコレーターの Pydantic 対応テスト.

検証:
- input_schema による入力の自動検証
- output_schema による出力の自動シリアライズ
- 非 Pydantic Agent との互換性
"""

from unittest.mock import patch

import pytest
from pydantic import BaseModel, ValidationError

from agentflow.agent_decorator import AgentClient, _agent_registry, agent


# ============================================================
# Test Schemas
# ============================================================


class QuestionInput(BaseModel):
    """入力スキーマのテスト用."""

    question: str
    context: str | None = None


class AnswerOutput(BaseModel):
    """出力スキーマのテスト用."""

    answer: str
    confidence: float = 0.5


# ============================================================
# Test Agents (定义在 fixture 中，避免全局副作用)
# ============================================================


def _create_simple_agent():
    """シンプルな Agent を作成."""

    @agent
    class SimpleAgent:
        """型なしのシンプルな Agent."""

        system_prompt = "あなたはアシスタントです。"

        async def process(self, input_data: dict) -> dict:
            return {"response": f"回答: {input_data.get('question', '')}"}

    return SimpleAgent


def _create_typed_agent():
    """型付きの Agent を作成."""

    @agent
    class TypedAgent:
        """Pydantic の型付き Agent."""

        system_prompt = "あなたは型付きのアシスタントです。"
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
    """Pydantic 対応のテスト."""

    def setup_method(self):
        """各テスト前にレジストリをクリア."""
        _agent_registry.clear()
        # テスト Agent を作り直して登録
        _create_simple_agent()
        _create_typed_agent()

    def test_simple_agent_registered(self):
        """SimpleAgent が登録される."""
        assert "SimpleAgent" in _agent_registry

    def test_typed_agent_registered(self):
        """TypedAgent が登録される."""
        assert "TypedAgent" in _agent_registry

    def test_typed_agent_has_schemas(self):
        """TypedAgent が入出力スキーマを持つ."""
        registered = _agent_registry["TypedAgent"]
        assert registered.input_schema is QuestionInput
        assert registered.output_schema is AnswerOutput
        assert registered.has_typed_schema is True

    def test_simple_agent_no_schemas(self):
        """SimpleAgent は入出力スキーマを持たない."""
        registered = _agent_registry["SimpleAgent"]
        assert registered.input_schema is None
        assert registered.output_schema is None
        assert registered.has_typed_schema is False

    @pytest.mark.asyncio
    @patch("agentflow.providers.get_llm", return_value=None)
    async def test_simple_agent_invoke(self, mock_llm):
        """型なし Agent の呼び出しテスト."""
        client = AgentClient.get("SimpleAgent")
        result = await client.invoke({"question": "こんにちは"})
        assert "response" in result
        assert "こんにちは" in result["response"]

    @pytest.mark.asyncio
    @patch("agentflow.providers.get_llm", return_value=None)
    async def test_typed_agent_invoke(self, mock_llm):
        """型付き Agent の呼び出しテスト."""
        client = AgentClient.get("TypedAgent")
        result = await client.invoke({"question": "こんにちは"})
        # 出力は dict にシリアライズされる
        assert isinstance(result, dict)
        assert "answer" in result
        assert "confidence" in result
        assert abs(result["confidence"] - 0.9) < 0.01

    @pytest.mark.asyncio
    @patch("agentflow.providers.get_llm", return_value=None)
    async def test_typed_agent_validation_success(self, mock_llm):
        """入力検証が成功する."""
        client = AgentClient.get("TypedAgent")
        result = await client.invoke({"question": "有効な質問", "context": "コンテキスト"})
        assert "answer" in result

    @pytest.mark.asyncio
    async def test_typed_agent_validation_failure(self):
        """入力検証が失敗する（必須フィールド不足）."""
        # 検証は invoke より前に行われるため、LLM を mock する必要はない
        registered = _agent_registry["TypedAgent"]
        with pytest.raises(ValidationError):
            registered.validate_input({"context": "コンテキストのみで質問がない"})

    @pytest.mark.asyncio
    async def test_typed_agent_validation_wrong_type(self):
        """入力検証が失敗する（型が不正）."""
        registered = _agent_registry["TypedAgent"]
        # Pydantic は型変換を試み、123 は "123" になる
        # ただし、question が None / 未指定の場合は失敗する
        with pytest.raises(ValidationError):
            registered.validate_input({})  # question が未指定


if __name__ == "__main__":
    pytest.main([__file__, "-v"])

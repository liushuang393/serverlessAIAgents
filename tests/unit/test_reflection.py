"""Reflection Pattern のテスト."""

import pytest

from agentflow.core.agent_block import AgentBlock
from agentflow.patterns.reflection import (
    ImproverAgent,
    ReflectionLoop,
    ReflectionWorkflow,
    ReflectorAgent,
)


class MockLLMClient:
    """モック LLM クライアント."""

    def __init__(self, responses: list[str] | None = None) -> None:
        """初期化."""
        self.responses = responses or []
        self.call_count = 0

    async def generate(self, prompt: str) -> str:
        """生成."""
        if self.call_count < len(self.responses):
            response = self.responses[self.call_count]
            self.call_count += 1
            return response
        return "デフォルトレスポンス"


class MockGeneratorAgent(AgentBlock):
    """モック Generator Agent."""

    def __init__(self, output: str = "初期生成結果") -> None:
        """初期化."""
        super().__init__()
        self.output = output

    async def run(self, input_data: dict) -> dict:
        """実行."""
        return {"output": self.output}


# ========================================
# ReflectorAgent Tests
# ========================================


@pytest.mark.asyncio
async def test_reflector_with_llm_acceptable():
    """ReflectorAgent: LLM 使用、合格判定."""
    llm = MockLLMClient(responses=["判定: Yes\nスコア: 85\nフィードバック: 良好"])
    reflector = ReflectorAgent(
        llm_client=llm,
        evaluation_criteria={"quality": "品質が高いか"},
        acceptance_threshold=70.0,
    )

    result = await reflector.run(
        {
            "output": "これは良い出力です",
            "task": "記事を書く",
        }
    )

    assert result["is_acceptable"] is True
    assert result["score"] == 70.0  # Threshold
    assert "output" in result
    assert result["output"] == "これは良い出力です"


@pytest.mark.asyncio
async def test_reflector_with_llm_not_acceptable():
    """ReflectorAgent: LLM 使用、不合格判定."""
    llm = MockLLMClient(responses=["判定: No\nスコア: 50\nフィードバック: 改善が必要"])
    reflector = ReflectorAgent(
        llm_client=llm,
        acceptance_threshold=70.0,
    )

    result = await reflector.run(
        {
            "output": "短い出力",
            "task": "詳しく書く",
        }
    )

    assert result["is_acceptable"] is False
    assert result["score"] == 60.0  # Threshold - 10


@pytest.mark.asyncio
async def test_reflector_without_llm():
    """ReflectorAgent: LLM なし、Fallback."""
    reflector = ReflectorAgent(llm_client=None)

    result = await reflector.run(
        {
            "output": "これは十分な長さの出力です",
        }
    )

    assert "is_acceptable" in result
    assert "score" in result
    assert result["score"] == 50.0


@pytest.mark.asyncio
async def test_reflector_empty_output():
    """ReflectorAgent: 空の出力."""
    reflector = ReflectorAgent(llm_client=None)

    result = await reflector.run(
        {
            "output": "",
        }
    )

    assert result["is_acceptable"] is False


# ========================================
# ImproverAgent Tests
# ========================================


@pytest.mark.asyncio
async def test_improver_with_llm():
    """ImproverAgent: LLM 使用."""
    llm = MockLLMClient(responses=["改善された出力です"])
    improver = ImproverAgent(llm_client=llm)

    result = await improver.run(
        {
            "output": "元の出力",
            "feedback": "もっと詳しく",
            "suggestions": ["具体例を追加"],
            "task": "記事を書く",
        }
    )

    assert result["improved_output"] == "改善された出力です"
    assert result["task"] == "記事を書く"


@pytest.mark.asyncio
async def test_improver_without_llm():
    """ImproverAgent: LLM なし、Fallback."""
    improver = ImproverAgent(llm_client=None)

    result = await improver.run(
        {
            "output": "元の出力",
            "feedback": "改善してください",
            "suggestions": [],
        }
    )

    assert "元の出力" in result["improved_output"]
    assert "LLM が設定されていません" in result["improved_output"]


# ========================================
# ReflectionLoop Tests
# ========================================


@pytest.mark.asyncio
async def test_reflection_loop_acceptable_first_try():
    """ReflectionLoop: 1回目で合格."""
    generator = MockGeneratorAgent(output="良い出力")
    llm = MockLLMClient(responses=["判定: Yes\nスコア: 90"])
    reflector = ReflectorAgent(llm_client=llm, acceptance_threshold=70.0)
    improver = ImproverAgent(llm_client=llm)

    loop = ReflectionLoop(
        generator=generator,
        reflector=reflector,
        improver=improver,
        max_iterations=3,
    )

    result = await loop.execute("タスク")

    assert result["iterations"] == 1
    assert result["final_output"] == "良い出力"
    assert len(result["history"]) == 1
    assert result["history"][0]["is_acceptable"] is True


@pytest.mark.asyncio
async def test_reflection_loop_improve_until_acceptable():
    """ReflectionLoop: 改善を繰り返して合格."""
    generator = MockGeneratorAgent(output="初期出力")
    llm = MockLLMClient(
        responses=[
            "判定: No\nスコア: 50",  # 1回目: 不合格
            "改善された出力1",  # 改善1
            "判定: No\nスコア: 60",  # 2回目: 不合格
            "改善された出力2",  # 改善2
            "判定: Yes\nスコア: 80",  # 3回目: 合格
        ]
    )
    reflector = ReflectorAgent(llm_client=llm, acceptance_threshold=70.0)
    improver = ImproverAgent(llm_client=llm)

    loop = ReflectionLoop(
        generator=generator,
        reflector=reflector,
        improver=improver,
        max_iterations=3,
    )

    result = await loop.execute("タスク")

    assert result["iterations"] == 3
    assert result["final_output"] == "改善された出力2"
    assert len(result["history"]) == 3
    assert result["history"][2]["is_acceptable"] is True


@pytest.mark.asyncio
async def test_reflection_loop_max_iterations():
    """ReflectionLoop: 最大反復回数に達する."""
    generator = MockGeneratorAgent(output="初期出力")
    llm = MockLLMClient(
        responses=[
            "判定: No\nスコア: 50",  # 1回目
            "改善1",
            "判定: No\nスコア: 55",  # 2回目
            "改善2",
            "判定: No\nスコア: 60",  # 3回目（最後）
        ]
    )
    reflector = ReflectorAgent(llm_client=llm, acceptance_threshold=70.0)
    improver = ImproverAgent(llm_client=llm)

    loop = ReflectionLoop(
        generator=generator,
        reflector=reflector,
        improver=improver,
        max_iterations=3,
    )

    result = await loop.execute("タスク")

    assert result["iterations"] == 3
    assert len(result["history"]) == 3
    assert result["history"][-1]["is_acceptable"] is False  # 最後も不合格


# ========================================
# ReflectionWorkflow Tests
# ========================================


def test_reflection_workflow_create():
    """ReflectionWorkflow: 作成."""
    generator = MockGeneratorAgent()
    llm = MockLLMClient()

    workflow = ReflectionWorkflow.create(
        workflow_id="test-reflection",
        generator=generator,
        llm_client=llm,
        evaluation_criteria={"quality": "品質"},
        max_iterations=5,
        acceptance_threshold=80.0,
    )

    assert workflow.workflow_id == "test-reflection"
    assert workflow.name == "Reflection Workflow"
    assert "5回" in workflow.description
    assert len(workflow.nodes) == 2
    assert len(workflow.edges) == 1
    assert workflow.config["max_iterations"] == 5
    assert abs(workflow.config["acceptance_threshold"] - 80.0) < 0.01


def test_reflection_workflow_default_params():
    """ReflectionWorkflow: デフォルトパラメータ."""
    generator = MockGeneratorAgent()
    llm = MockLLMClient()

    workflow = ReflectionWorkflow.create(
        workflow_id="test",
        generator=generator,
        llm_client=llm,
    )

    assert workflow.config["max_iterations"] == 3
    assert abs(workflow.config["acceptance_threshold"] - 70.0) < 0.01


# ========================================
# Integration Tests
# ========================================


@pytest.mark.asyncio
async def test_full_reflection_pipeline():
    """統合テスト: 完全な Reflection パイプライン."""
    generator = MockGeneratorAgent(output="初期コンテンツ")
    llm = MockLLMClient(
        responses=[
            "判定: No\nスコア: 60\nフィードバック: 詳細が不足",
            "初期コンテンツ + 詳細追加",
            "判定: Yes\nスコア: 85\nフィードバック: 良好",
        ]
    )

    reflector = ReflectorAgent(
        llm_client=llm,
        evaluation_criteria={
            "completeness": "完全性",
            "clarity": "明確性",
        },
        acceptance_threshold=70.0,
    )

    improver = ImproverAgent(llm_client=llm)

    loop = ReflectionLoop(
        generator=generator,
        reflector=reflector,
        improver=improver,
        max_iterations=3,
    )

    result = await loop.execute("詳しい記事を書く")

    # 検証
    assert result["iterations"] == 2
    assert "詳細追加" in result["final_output"]
    assert result["final_score"] >= 70.0
    assert len(result["history"]) == 2

    # 履歴検証
    assert result["history"][0]["is_acceptable"] is False
    assert result["history"][1]["is_acceptable"] is True

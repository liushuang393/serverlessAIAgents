# -*- coding: utf-8 -*-
"""Engine Pattern テスト.

4 種類の Engine Pattern をテスト:
- SimpleEngine
- GateEngine
- PipelineEngine
- RAGEngine
"""

import pytest

from agentflow.engines import (
    BaseEngine,
    EngineConfig,
    GateEngine,
    PipelineEngine,
    RAGEngine,
    SimpleEngine,
)


# ============================================================
# Mock Agents
# ============================================================


class MockAgent:
    """シンプルなモック Agent."""

    name = "MockAgent"

    async def run(self, inputs: dict) -> dict:
        return {"answer": f"Mock response for: {inputs.get('question', 'N/A')}"}


class MockGateAgent:
    """Mock Gate Agent."""

    name = "MockGate"

    async def run(self, inputs: dict) -> dict:
        question = inputs.get("question", "")
        # "reject" を含む場合は拒否
        passed = "reject" not in question.lower()
        return {"passed": passed, "reason": "OK" if passed else "Rejected by gate"}


class MockReviewAgent:
    """Mock Review Agent."""

    name = "MockReview"
    call_count = 0

    async def run(self, inputs: dict) -> dict:
        MockReviewAgent.call_count += 1
        # 1 回目は REVISE、2 回目は PASS
        if MockReviewAgent.call_count == 1:
            return {"verdict": "REVISE", "reason": "Need improvement"}
        return {"verdict": "PASS", "reason": "Approved"}


# ============================================================
# Tests
# ============================================================


class TestSimpleEngine:
    """SimpleEngine のテスト."""

    @pytest.mark.asyncio
    async def test_simple_run(self):
        """基本実行のテスト."""
        engine = SimpleEngine(agent=MockAgent)
        result = await engine.run({"question": "Hello"})

        assert "answer" in result
        assert "Hello" in result["answer"]

    @pytest.mark.asyncio
    async def test_simple_stream(self):
        """ストリーミング実行のテスト."""
        engine = SimpleEngine(agent=MockAgent)
        events = []

        async for event in engine.run_stream({"question": "Test"}):
            events.append(event)

        # flow_start, node_start, node_complete, result, flow_complete が含まれる
        event_types = [e.get("type") or e.get("event_type") for e in events]
        assert "result" in event_types


class TestGateEngine:
    """GateEngine のテスト."""

    @pytest.mark.asyncio
    async def test_gate_pass(self):
        """Gate の通過テスト."""
        engine = GateEngine(
            gate_agent=MockGateAgent,
            main_agent=MockAgent,
            gate_check=lambda r: r.get("passed", False),
        )
        result = await engine.run({"question": "Normal question"})

        assert result["status"] == "success"
        assert "result" in result

    @pytest.mark.asyncio
    async def test_gate_reject(self):
        """Gate の拒否テスト."""
        engine = GateEngine(
            gate_agent=MockGateAgent,
            main_agent=MockAgent,
            gate_check=lambda r: r.get("passed", False),
        )
        result = await engine.run({"question": "Please reject this"})

        assert result["status"] == "rejected"
        assert "reason" in result


class TestPipelineEngine:
    """PipelineEngine のテスト."""

    @pytest.mark.asyncio
    async def test_pipeline_simple(self):
        """シンプルな Pipeline のテスト."""
        engine = PipelineEngine(
            stages=[
                {"name": "process", "agent": MockAgent},
            ]
        )
        result = await engine.run({"question": "Test"})

        assert result["status"] == "success"
        assert "results" in result


class TestRAGEngine:
    """RAGEngine のテスト."""

    @pytest.mark.asyncio
    async def test_rag_with_mock_retriever(self):
        """モック Retriever を使った RAG のテスト."""
        mock_docs = [
            {"content": "Document 1", "score": 0.9},
            {"content": "Document 2", "score": 0.8},
        ]

        engine = RAGEngine(
            agent=MockAgent,
            retriever=lambda q: mock_docs,
        )
        result = await engine.run({"question": "What is AI?"})

        assert "answer" in result
        assert "sources" in result
        assert len(result["sources"]) == 2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])

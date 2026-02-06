# Apps 现代化改善计划

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 将 `apps/` 下的 6 个应用升级到框架最新标准，达到平台化生产就绪状态

**Architecture:** 基于 8 层清洁架构，全面采用 ResilientAgent + Pydantic I/O、松耦合 Provider、标准 Engine、AG-UI 标准事件。每个应用需达到 80% 测试覆盖率。

**Tech Stack:** Python 3.13+, Pydantic, pytest, ResilientAgent, PipelineEngine, AG-UI events

---

## Phase 1: 参照应用完善 (decision_governance_engine)

> 参照应用是框架的门面，必须 100% 合规

### Task 1.1: 为 DaoAgent 添加单元测试

**Files:**
- Create: `apps/decision_governance_engine/tests/unit/test_dao_agent.py`
- Reference: `apps/decision_governance_engine/agents/dao_agent.py`

**Step 1: Write the failing test**

```python
"""Unit tests for DaoAgent."""
import pytest
from unittest.mock import AsyncMock, patch

from apps.decision_governance_engine.agents.dao_agent import DaoAgent
from apps.decision_governance_engine.schemas.agent_io import DaoInput, DaoOutput


@pytest.fixture
def dao_agent():
    """Create DaoAgent instance for testing."""
    return DaoAgent()


@pytest.fixture
def sample_input():
    """Create sample DaoInput."""
    return DaoInput(
        decision_context="Should we expand to APAC market?",
        clarification_result={
            "clarified_question": "Market expansion decision for APAC region",
            "stakeholders": ["executive team", "regional managers"],
        },
    )


class TestDaoAgent:
    """Test cases for DaoAgent."""

    @pytest.mark.asyncio
    async def test_process_returns_dao_output(
        self, dao_agent: DaoAgent, sample_input: DaoInput
    ):
        """Test that process returns valid DaoOutput."""
        with patch.object(dao_agent, "_call_llm", new_callable=AsyncMock) as mock_llm:
            mock_llm.return_value = {
                "dao_analysis": "Strategic alignment analysis...",
                "principles": ["market_fit", "risk_tolerance"],
                "confidence": 0.85,
            }

            result = await dao_agent.process(sample_input)

            assert isinstance(result, DaoOutput)
            assert result.confidence >= 0.0
            assert result.confidence <= 1.0

    @pytest.mark.asyncio
    async def test_parse_input_validates_schema(self, dao_agent: DaoAgent):
        """Test that _parse_input validates against schema."""
        raw_input = {
            "decision_context": "Test context",
            "clarification_result": {"clarified_question": "Test"},
        }

        parsed = dao_agent._parse_input(raw_input)

        assert isinstance(parsed, DaoInput)
        assert parsed.decision_context == "Test context"
```

**Step 2: Run test to verify it fails**

Run: `pytest apps/decision_governance_engine/tests/unit/test_dao_agent.py -v`
Expected: FAIL (tests directory or module not found)

**Step 3: Create tests directory structure**

```bash
mkdir -p apps/decision_governance_engine/tests/unit
touch apps/decision_governance_engine/tests/__init__.py
touch apps/decision_governance_engine/tests/unit/__init__.py
```

**Step 4: Run test to verify it passes**

Run: `pytest apps/decision_governance_engine/tests/unit/test_dao_agent.py -v`
Expected: PASS

**Step 5: Commit**

```bash
git add apps/decision_governance_engine/tests/
git commit -m "test(dge): add unit tests for DaoAgent"
```

---

### Task 1.2: 为其余 Agent 添加单元测试

**Files:**
- Create: `apps/decision_governance_engine/tests/unit/test_cognitive_gate_agent.py`
- Create: `apps/decision_governance_engine/tests/unit/test_fa_agent.py`
- Create: `apps/decision_governance_engine/tests/unit/test_shu_agent.py`
- Create: `apps/decision_governance_engine/tests/unit/test_qi_agent.py`
- Create: `apps/decision_governance_engine/tests/unit/test_review_agent.py`

**Step 1: Write the failing tests**

为每个 Agent 创建类似 Task 1.1 的测试文件，测试：
- `process()` 返回正确的 Output 类型
- `_parse_input()` 正确验证 schema
- Edge cases (空输入、异常处理)

**Step 2: Run tests to verify they fail**

Run: `pytest apps/decision_governance_engine/tests/unit/ -v`
Expected: Some FAIL

**Step 3: Fix any issues in tests or implementation**

根据测试结果修复问题

**Step 4: Run tests to verify they pass**

Run: `pytest apps/decision_governance_engine/tests/unit/ -v --cov=apps/decision_governance_engine/agents`
Expected: PASS, coverage > 80%

**Step 5: Commit**

```bash
git add apps/decision_governance_engine/tests/
git commit -m "test(dge): add unit tests for all agents"
```

---

### Task 1.3: 为 DecisionEngine 添加集成测试

**Files:**
- Create: `apps/decision_governance_engine/tests/integration/test_engine.py`
- Reference: `apps/decision_governance_engine/engine.py`

**Step 1: Write the failing test**

```python
"""Integration tests for DecisionEngine."""
import pytest
from unittest.mock import AsyncMock, patch

from apps.decision_governance_engine.engine import DecisionEngine


@pytest.fixture
def engine():
    """Create DecisionEngine instance."""
    return DecisionEngine()


class TestDecisionEngine:
    """Integration tests for DecisionEngine."""

    @pytest.mark.asyncio
    async def test_run_stream_emits_standard_events(self, engine: DecisionEngine):
        """Test that run_stream emits AG-UI standard events."""
        with patch("agentflow.providers.get_llm") as mock_get_llm:
            mock_llm = AsyncMock()
            mock_llm.chat.return_value = {"content": "mock response"}
            mock_get_llm.return_value = mock_llm

            events = []
            async for event in engine.run_stream({"question": "Test question"}):
                events.append(event)

            # Verify standard event types
            event_types = [e.get("event_type") or e.get("type") for e in events]
            assert "flow.start" in event_types
            assert "flow.complete" in event_types or "flow.error" in event_types

    @pytest.mark.asyncio
    async def test_stages_execute_in_order(self, engine: DecisionEngine):
        """Test that pipeline stages execute in correct order."""
        with patch("agentflow.providers.get_llm") as mock_get_llm:
            mock_llm = AsyncMock()
            mock_get_llm.return_value = mock_llm

            await engine._initialize()

            stage_names = [s.name for s in engine._stage_configs]
            expected_order = [
                "cognitive_gate", "gatekeeper", "clarification",
                "dao", "fa", "shu", "qi", "review"
            ]
            assert stage_names == expected_order
```

**Step 2: Run test to verify it fails**

Run: `pytest apps/decision_governance_engine/tests/integration/test_engine.py -v`
Expected: FAIL

**Step 3: Create integration test directory**

```bash
mkdir -p apps/decision_governance_engine/tests/integration
touch apps/decision_governance_engine/tests/integration/__init__.py
```

**Step 4: Run test to verify it passes**

Run: `pytest apps/decision_governance_engine/tests/integration/ -v`
Expected: PASS

**Step 5: Commit**

```bash
git add apps/decision_governance_engine/tests/integration/
git commit -m "test(dge): add integration tests for DecisionEngine"
```

---

### Task 1.4: 实现 AG-UI 标准事件

**Files:**
- Modify: `apps/decision_governance_engine/engine.py`
- Reference: `agentflow/protocols/agui_events.py`

**Step 1: Write the failing test**

在 `test_engine.py` 添加：

```python
@pytest.mark.asyncio
async def test_progress_events_have_required_fields(self, engine: DecisionEngine):
    """Test that progress events have current, total, percentage fields."""
    with patch("agentflow.providers.get_llm") as mock_get_llm:
        mock_llm = AsyncMock()
        mock_get_llm.return_value = mock_llm

        progress_events = []
        async for event in engine.run_stream({"question": "Test"}):
            if event.get("event_type") == "progress":
                progress_events.append(event)

        for event in progress_events:
            assert "current" in event
            assert "total" in event
            assert "percentage" in event
```

**Step 2: Run test to verify it fails**

Run: `pytest apps/decision_governance_engine/tests/integration/test_engine.py::TestDecisionEngine::test_progress_events_have_required_fields -v`
Expected: FAIL

**Step 3: Modify engine.py to emit standard events**

```python
from agentflow.protocols.agui_events import (
    FlowStartEvent,
    FlowCompleteEvent,
    NodeStartEvent,
    NodeCompleteEvent,
    ProgressEvent,
    AGUIEventType,
)
import time

# In _execute_stream method:
async def _execute_stream(self, inputs: dict) -> AsyncIterator[dict]:
    total_stages = len(self._stage_configs)

    for i, stage in enumerate(self._stage_configs):
        # Emit node start
        yield NodeStartEvent(
            event_type=AGUIEventType.NODE_START,
            timestamp=time.time(),
            flow_id=self._flow_id,
            node_id=stage.name,
        ).to_dict()

        # Execute stage...
        result = await self._execute_stage(stage, inputs)

        # Emit progress
        yield ProgressEvent(
            event_type=AGUIEventType.PROGRESS,
            timestamp=time.time(),
            flow_id=self._flow_id,
            current=i + 1,
            total=total_stages,
            percentage=(i + 1) / total_stages * 100,
        ).to_dict()

        # Emit node complete
        yield NodeCompleteEvent(
            event_type=AGUIEventType.NODE_COMPLETE,
            timestamp=time.time(),
            flow_id=self._flow_id,
            node_id=stage.name,
            result=result,
        ).to_dict()
```

**Step 4: Run test to verify it passes**

Run: `pytest apps/decision_governance_engine/tests/integration/test_engine.py -v`
Expected: PASS

**Step 5: Commit**

```bash
git add apps/decision_governance_engine/engine.py
git add apps/decision_governance_engine/tests/
git commit -m "feat(dge): implement AG-UI standard events"
```

---

## Phase 2: 高优先级应用修复 (code_migration_assistant)

> 从旧 @agent 模式迁移到 ResilientAgent

### Task 2.1: 将 TransformAgent 迁移到 ResilientAgent

**Files:**
- Modify: `apps/code_migration_assistant/agents/transform_agent.py`
- Create: `apps/code_migration_assistant/schemas/transform_io.py`

**Step 1: Write the failing test**

```python
# apps/code_migration_assistant/tests/unit/test_transform_agent.py
import pytest
from apps.code_migration_assistant.agents.transform_agent import TransformAgent
from apps.code_migration_assistant.schemas.transform_io import (
    TransformInput,
    TransformOutput,
)


class TestTransformAgent:
    @pytest.mark.asyncio
    async def test_agent_inherits_resilient_agent(self):
        """Test that TransformAgent inherits from ResilientAgent."""
        from agentflow import ResilientAgent

        agent = TransformAgent()
        assert isinstance(agent, ResilientAgent)

    @pytest.mark.asyncio
    async def test_process_returns_typed_output(self):
        """Test that process returns TransformOutput."""
        agent = TransformAgent()
        input_data = TransformInput(
            source_code="MOVE A TO B",
            source_language="cobol",
            target_language="java",
        )

        # Mock LLM call
        with patch.object(agent, "_call_llm", new_callable=AsyncMock) as mock:
            mock.return_value = {"transformed_code": "b = a;"}
            result = await agent.process(input_data)

        assert isinstance(result, TransformOutput)
```

**Step 2: Run test to verify it fails**

Run: `pytest apps/code_migration_assistant/tests/unit/test_transform_agent.py -v`
Expected: FAIL (not ResilientAgent subclass)

**Step 3: Create I/O schemas**

```python
# apps/code_migration_assistant/schemas/transform_io.py
from pydantic import BaseModel, Field


class TransformInput(BaseModel):
    """Input for TransformAgent."""

    source_code: str = Field(..., description="Source code to transform")
    source_language: str = Field(..., description="Source language (cobol, pl1, etc)")
    target_language: str = Field(..., description="Target language (java, python, etc)")
    context: dict | None = Field(default=None, description="Additional context")


class TransformOutput(BaseModel):
    """Output from TransformAgent."""

    transformed_code: str = Field(..., description="Transformed code")
    confidence: float = Field(..., ge=0.0, le=1.0, description="Transformation confidence")
    warnings: list[str] = Field(default_factory=list, description="Transformation warnings")
```

**Step 4: Migrate TransformAgent**

```python
# apps/code_migration_assistant/agents/transform_agent.py
from agentflow import ResilientAgent
from agentflow.providers import get_llm
from apps.code_migration_assistant.schemas.transform_io import (
    TransformInput,
    TransformOutput,
)


class TransformAgent(ResilientAgent[TransformInput, TransformOutput]):
    """Agent for transforming code between languages."""

    name = "transform_agent"
    description = "Transforms source code from one language to another"

    input_schema = TransformInput
    output_schema = TransformOutput

    def __init__(self):
        super().__init__()
        self._llm = get_llm()  # 松耦合

    async def process(self, input_data: TransformInput) -> TransformOutput:
        """Transform source code."""
        response = await self._call_llm(
            messages=[
                {"role": "system", "content": self._build_system_prompt()},
                {"role": "user", "content": self._build_user_prompt(input_data)},
            ]
        )

        return self._parse_response(response)

    def _build_system_prompt(self) -> str:
        return "You are a code transformation specialist..."

    def _build_user_prompt(self, input_data: TransformInput) -> str:
        return f"""Transform the following {input_data.source_language} code to {input_data.target_language}:

```{input_data.source_language}
{input_data.source_code}
```"""

    def _parse_response(self, response: dict) -> TransformOutput:
        # Parse LLM response into TransformOutput
        return TransformOutput(
            transformed_code=response.get("transformed_code", ""),
            confidence=response.get("confidence", 0.8),
            warnings=response.get("warnings", []),
        )
```

**Step 5: Run test to verify it passes**

Run: `pytest apps/code_migration_assistant/tests/unit/test_transform_agent.py -v`
Expected: PASS

**Step 6: Commit**

```bash
git add apps/code_migration_assistant/
git commit -m "refactor(cma): migrate TransformAgent to ResilientAgent"
```

---

### Task 2.2: 将其余 Agent 迁移到 ResilientAgent

**Files:**
- Modify: `apps/code_migration_assistant/agents/checker_agent.py`
- Modify: `apps/code_migration_assistant/agents/fixer_agent.py`
- Modify: `apps/code_migration_assistant/agents/coordinator_agent.py`
- Create: `apps/code_migration_assistant/schemas/checker_io.py`
- Create: `apps/code_migration_assistant/schemas/fixer_io.py`
- Create: `apps/code_migration_assistant/schemas/coordinator_io.py`

重复 Task 2.1 的模式，为每个 Agent:
1. 创建 Pydantic I/O schemas
2. 继承 ResilientAgent[Input, Output]
3. 使用 get_llm() 获取 LLM
4. 实现 process() 方法
5. 添加单元测试

**Step 5: Commit**

```bash
git add apps/code_migration_assistant/
git commit -m "refactor(cma): migrate all agents to ResilientAgent pattern"
```

---

### Task 2.3: 修复 Engine 的 AG-UI 事件

**Files:**
- Modify: `apps/code_migration_assistant/engine.py`

与 Task 1.4 类似，将自定义 dict 事件替换为标准事件类。

---

## Phase 3: 修复 faq_system

### Task 3.1: 修复 main.py 的 import 错误

**Files:**
- Modify: `apps/faq_system/main.py`

**Step 1: Identify the broken import**

```python
# 当前（错误）
from agentflow.agents import FAQAgent, SalesAgent  # ❌ 不存在

# 修复后
from apps.faq_system.backend.agents.enhanced_faq_agent import EnhancedFAQAgent
```

**Step 2: Update to use local agents**

**Step 3: Add get_llm() usage**

**Step 4: Add unit tests**

---

### Task 3.2: 将 EnhancedFAQAgent 迁移到 ResilientAgent

与 Task 2.1 类似模式。

---

## Phase 4: 标准化 market_trend_monitor

### Task 4.1: 将 WorkflowEngine 迁移到 PipelineEngine

**Files:**
- Modify: `apps/market_trend_monitor/backend/workflow_engine.py`

**Step 1: Write the failing test**

```python
@pytest.mark.asyncio
async def test_workflow_engine_is_pipeline_engine():
    from apps.market_trend_monitor.backend.workflow_engine import WorkflowEngine
    from agentflow.engines import PipelineEngine

    engine = WorkflowEngine()
    assert isinstance(engine, PipelineEngine)
```

**Step 2: Refactor to inherit PipelineEngine**

---

## Phase 5: 添加 Engine 到无 Engine 应用

### Task 5.1: 为 messaging_hub 添加 SimpleEngine

### Task 5.2: 为 platform 添加更多测试

---

## 验收标准

| 应用 | 目标分数 | 关键指标 |
|------|---------|---------|
| decision_governance_engine | 100/115 | 测试覆盖 80%+ |
| code_migration_assistant | 90/115 | ResilientAgent 100% |
| market_trend_monitor | 95/115 | PipelineEngine |
| faq_system | 80/115 | import 修复 |
| platform | 85/115 | 测试覆盖 80%+ |
| messaging_hub | 70/115 | SimpleEngine 引入 |

**全局目标:**
- AG-UI 标准事件: 100% 合规
- ResilientAgent 使用率: 90%+
- 松耦合 (get_llm()): 100%
- 测试覆盖率: 80%+

---

## 执行顺序建议

1. **Phase 1** (参照应用) - 2 天
2. **Phase 2** (code_migration_assistant) - 2 天
3. **Phase 3** (faq_system) - 1 天
4. **Phase 4** (market_trend_monitor) - 1 天
5. **Phase 5** (其余) - 1 天

总计: 约 7 个工作日

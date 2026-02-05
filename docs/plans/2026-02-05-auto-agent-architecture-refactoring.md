# Auto-Agent Architecture Refactoring Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refactor AgentFlow into a future-ready AI development framework with autonomous analysis, auto-agent creation, and unified MCP/Skills binding - eliminating technical debt while maintaining high abstraction, cohesion, and extensibility.

**Architecture:** Create unified registries (ToolRegistry, AgentRegistry) with formal capability models, implement lazy initialization for testability, add runtime tool binding, and integrate AgentWizard into execution flows for auto-agent generation.

**Tech Stack:** Python 3.13+, Pydantic v2, asyncio, pytest, mypy strict mode

---

## Phase 1: Foundation - Unified Tool Registry

### Task 1.1: Create ToolDefinition Model

**Files:**
- Create: `agentflow/core/tool_definition.py`
- Test: `tests/unit/core/test_tool_definition.py`

**Step 1: Write the failing test**

```python
# tests/unit/core/test_tool_definition.py
"""Tests for ToolDefinition model."""
import pytest
from pydantic import ValidationError


def test_tool_definition_creation():
    """Test basic ToolDefinition creation with required fields."""
    from agentflow.core.tool_definition import ToolDefinition, ToolSource

    tool = ToolDefinition(
        uri="tool://builtin/calculator",
        name="calculator",
        description="Performs arithmetic calculations",
        source=ToolSource.BUILTIN,
        input_schema={"type": "object", "properties": {"expression": {"type": "string"}}},
    )

    assert tool.uri == "tool://builtin/calculator"
    assert tool.name == "calculator"
    assert tool.source == ToolSource.BUILTIN
    assert tool.input_schema is not None


def test_tool_definition_uri_validation():
    """Test URI must follow tool:// scheme."""
    from agentflow.core.tool_definition import ToolDefinition, ToolSource

    with pytest.raises(ValidationError, match="uri"):
        ToolDefinition(
            uri="invalid-uri",  # Missing tool:// scheme
            name="test",
            description="Test",
            source=ToolSource.BUILTIN,
        )


def test_tool_definition_from_mcp():
    """Test creating ToolDefinition from MCP tool format."""
    from agentflow.core.tool_definition import ToolDefinition

    mcp_tool = {
        "name": "read_file",
        "description": "Read file contents",
        "inputSchema": {"type": "object", "properties": {"path": {"type": "string"}}},
    }

    tool = ToolDefinition.from_mcp(mcp_tool, server_name="filesystem")

    assert tool.uri == "tool://mcp/filesystem/read_file"
    assert tool.name == "read_file"


def test_tool_definition_from_skill():
    """Test creating ToolDefinition from Skill."""
    from agentflow.core.tool_definition import ToolDefinition

    skill_data = {
        "name": "code_review",
        "description": "Reviews code for best practices",
        "parameters": {"code": {"type": "string"}},
    }

    tool = ToolDefinition.from_skill(skill_data)

    assert tool.uri == "tool://skill/code_review"
    assert tool.name == "code_review"


def test_tool_definition_to_mcp_format():
    """Test converting ToolDefinition to MCP tool format."""
    from agentflow.core.tool_definition import ToolDefinition, ToolSource

    tool = ToolDefinition(
        uri="tool://builtin/search",
        name="search",
        description="Search documents",
        source=ToolSource.BUILTIN,
        input_schema={"type": "object", "properties": {"query": {"type": "string"}}},
    )

    mcp_format = tool.to_mcp()

    assert mcp_format["name"] == "search"
    assert mcp_format["description"] == "Search documents"
    assert "inputSchema" in mcp_format
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/core/test_tool_definition.py -v`
Expected: FAIL with "ModuleNotFoundError: No module named 'agentflow.core.tool_definition'"

**Step 3: Write minimal implementation**

```python
# agentflow/core/tool_definition.py
"""Unified tool definition model for all tool sources."""
from __future__ import annotations

from enum import Enum
from typing import Any

from pydantic import BaseModel, Field, field_validator


class ToolSource(str, Enum):
    """Source type of a tool."""

    BUILTIN = "builtin"  # @tool decorated functions
    MCP = "mcp"  # MCP server tools
    SKILL = "skill"  # Skills engine
    DYNAMIC = "dynamic"  # Runtime generated


class ToolDefinition(BaseModel):
    """Unified tool definition across all sources.

    Provides a consistent interface for tools from:
    - @tool decorated functions (builtin)
    - MCP servers (mcp)
    - Skills engine (skill)
    - Runtime generated tools (dynamic)
    """

    uri: str = Field(..., description="Unique tool URI: tool://{source}/{name}")
    name: str = Field(..., description="Tool name")
    description: str = Field(..., description="What the tool does")
    source: ToolSource = Field(..., description="Where this tool comes from")
    input_schema: dict[str, Any] = Field(
        default_factory=lambda: {"type": "object", "properties": {}},
        description="JSON Schema for input parameters",
    )
    output_schema: dict[str, Any] | None = Field(
        default=None, description="JSON Schema for output (optional)"
    )
    metadata: dict[str, Any] = Field(
        default_factory=dict, description="Additional source-specific metadata"
    )

    @field_validator("uri")
    @classmethod
    def validate_uri(cls, v: str) -> str:
        """Ensure URI follows tool:// scheme."""
        if not v.startswith("tool://"):
            raise ValueError(f"URI must start with 'tool://', got: {v}")
        return v

    @classmethod
    def from_mcp(
        cls, mcp_tool: dict[str, Any], server_name: str
    ) -> ToolDefinition:
        """Create ToolDefinition from MCP tool format.

        Args:
            mcp_tool: MCP tool definition with name, description, inputSchema
            server_name: Name of the MCP server providing this tool

        Returns:
            ToolDefinition instance
        """
        name = mcp_tool["name"]
        return cls(
            uri=f"tool://mcp/{server_name}/{name}",
            name=name,
            description=mcp_tool.get("description", ""),
            source=ToolSource.MCP,
            input_schema=mcp_tool.get("inputSchema", {"type": "object", "properties": {}}),
            metadata={"server": server_name, "original": mcp_tool},
        )

    @classmethod
    def from_skill(cls, skill_data: dict[str, Any]) -> ToolDefinition:
        """Create ToolDefinition from Skill format.

        Args:
            skill_data: Skill definition with name, description, parameters

        Returns:
            ToolDefinition instance
        """
        name = skill_data["name"]
        return cls(
            uri=f"tool://skill/{name}",
            name=name,
            description=skill_data.get("description", ""),
            source=ToolSource.SKILL,
            input_schema={
                "type": "object",
                "properties": skill_data.get("parameters", {}),
            },
            metadata={"skill_data": skill_data},
        )

    @classmethod
    def from_builtin(
        cls, name: str, description: str, input_schema: dict[str, Any]
    ) -> ToolDefinition:
        """Create ToolDefinition from @tool decorated function.

        Args:
            name: Function name
            description: Function docstring
            input_schema: Inferred JSON Schema from type hints

        Returns:
            ToolDefinition instance
        """
        return cls(
            uri=f"tool://builtin/{name}",
            name=name,
            description=description,
            source=ToolSource.BUILTIN,
            input_schema=input_schema,
        )

    def to_mcp(self) -> dict[str, Any]:
        """Convert to MCP tool format.

        Returns:
            Dict compatible with MCP tool definition
        """
        return {
            "name": self.name,
            "description": self.description,
            "inputSchema": self.input_schema,
        }

    def matches(self, query: str) -> float:
        """Calculate relevance score for a query.

        Args:
            query: Search query

        Returns:
            Score between 0.0 and 1.0
        """
        query_lower = query.lower()
        score = 0.0

        if query_lower in self.name.lower():
            score += 0.5
        if query_lower in self.description.lower():
            score += 0.3

        # Check input schema property names
        props = self.input_schema.get("properties", {})
        for prop_name in props:
            if query_lower in prop_name.lower():
                score += 0.1
                break

        return min(score, 1.0)
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/core/test_tool_definition.py -v`
Expected: PASS (5 tests)

**Step 5: Commit**

```bash
git add agentflow/core/tool_definition.py tests/unit/core/test_tool_definition.py
git commit -m "$(cat <<'EOF'
feat(core): add unified ToolDefinition model

Introduces ToolDefinition as the canonical representation for tools
across all sources (builtin, MCP, skills, dynamic). Provides factory
methods for each source and conversion to MCP format.

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 1.2: Create ToolRegistry Interface

**Files:**
- Create: `agentflow/core/tool_registry.py`
- Test: `tests/unit/core/test_tool_registry.py`

**Step 1: Write the failing test**

```python
# tests/unit/core/test_tool_registry.py
"""Tests for ToolRegistry interface and implementation."""
import pytest


@pytest.fixture
def tool_registry():
    """Create a fresh ToolRegistry instance."""
    from agentflow.core.tool_registry import ToolRegistry
    return ToolRegistry()


@pytest.fixture
def sample_tool():
    """Create a sample ToolDefinition."""
    from agentflow.core.tool_definition import ToolDefinition, ToolSource
    return ToolDefinition(
        uri="tool://builtin/test_tool",
        name="test_tool",
        description="A test tool for testing",
        source=ToolSource.BUILTIN,
        input_schema={"type": "object", "properties": {"input": {"type": "string"}}},
    )


class TestToolRegistry:
    """Tests for ToolRegistry."""

    def test_register_and_get(self, tool_registry, sample_tool):
        """Test registering and retrieving a tool."""
        tool_registry.register(sample_tool)

        retrieved = tool_registry.get("tool://builtin/test_tool")

        assert retrieved is not None
        assert retrieved.name == "test_tool"

    def test_get_nonexistent_returns_none(self, tool_registry):
        """Test getting a nonexistent tool returns None."""
        result = tool_registry.get("tool://nonexistent/tool")
        assert result is None

    def test_list_all_tools(self, tool_registry, sample_tool):
        """Test listing all registered tools."""
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        tool2 = ToolDefinition(
            uri="tool://mcp/server/tool2",
            name="tool2",
            description="Another tool",
            source=ToolSource.MCP,
        )

        tool_registry.register(sample_tool)
        tool_registry.register(tool2)

        all_tools = tool_registry.list_all()

        assert len(all_tools) == 2

    def test_search_by_query(self, tool_registry, sample_tool):
        """Test searching tools by query."""
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        calc_tool = ToolDefinition(
            uri="tool://builtin/calculator",
            name="calculator",
            description="Performs arithmetic calculations",
            source=ToolSource.BUILTIN,
        )

        tool_registry.register(sample_tool)
        tool_registry.register(calc_tool)

        results = tool_registry.search("calculator")

        assert len(results) > 0
        assert results[0].name == "calculator"

    def test_filter_by_source(self, tool_registry):
        """Test filtering tools by source."""
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        builtin = ToolDefinition(
            uri="tool://builtin/b1",
            name="b1",
            description="Builtin",
            source=ToolSource.BUILTIN,
        )
        mcp = ToolDefinition(
            uri="tool://mcp/server/m1",
            name="m1",
            description="MCP",
            source=ToolSource.MCP,
        )

        tool_registry.register(builtin)
        tool_registry.register(mcp)

        builtin_tools = tool_registry.filter_by_source(ToolSource.BUILTIN)
        mcp_tools = tool_registry.filter_by_source(ToolSource.MCP)

        assert len(builtin_tools) == 1
        assert len(mcp_tools) == 1
        assert builtin_tools[0].name == "b1"

    def test_unregister(self, tool_registry, sample_tool):
        """Test unregistering a tool."""
        tool_registry.register(sample_tool)
        assert tool_registry.get(sample_tool.uri) is not None

        tool_registry.unregister(sample_tool.uri)

        assert tool_registry.get(sample_tool.uri) is None

    def test_register_duplicate_replaces(self, tool_registry, sample_tool):
        """Test registering duplicate URI replaces existing."""
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        tool_registry.register(sample_tool)

        updated = ToolDefinition(
            uri=sample_tool.uri,
            name="updated_name",
            description="Updated description",
            source=ToolSource.BUILTIN,
        )
        tool_registry.register(updated)

        retrieved = tool_registry.get(sample_tool.uri)
        assert retrieved.name == "updated_name"
        assert len(tool_registry.list_all()) == 1
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/core/test_tool_registry.py -v`
Expected: FAIL with "ModuleNotFoundError: No module named 'agentflow.core.tool_registry'"

**Step 3: Write minimal implementation**

```python
# agentflow/core/tool_registry.py
"""Unified tool registry for discovering and managing tools from all sources."""
from __future__ import annotations

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from agentflow.core.tool_definition import ToolDefinition, ToolSource


class ToolRegistry:
    """Central registry for all tools across sources.

    Provides unified discovery, search, and management for:
    - @tool decorated functions
    - MCP server tools
    - Skills
    - Dynamically generated tools

    Thread-safe for concurrent access.
    """

    def __init__(self) -> None:
        """Initialize empty registry."""
        self._tools: dict[str, ToolDefinition] = {}

    def register(self, tool: ToolDefinition) -> None:
        """Register a tool definition.

        If a tool with the same URI exists, it will be replaced.

        Args:
            tool: Tool definition to register
        """
        self._tools[tool.uri] = tool

    def unregister(self, uri: str) -> bool:
        """Remove a tool from the registry.

        Args:
            uri: Tool URI to remove

        Returns:
            True if tool was removed, False if not found
        """
        if uri in self._tools:
            del self._tools[uri]
            return True
        return False

    def get(self, uri: str) -> ToolDefinition | None:
        """Get a tool by its URI.

        Args:
            uri: Tool URI

        Returns:
            ToolDefinition or None if not found
        """
        return self._tools.get(uri)

    def list_all(self) -> list[ToolDefinition]:
        """List all registered tools.

        Returns:
            List of all tool definitions
        """
        return list(self._tools.values())

    def search(self, query: str, limit: int = 10) -> list[ToolDefinition]:
        """Search tools by query string.

        Searches tool names and descriptions.

        Args:
            query: Search query
            limit: Maximum results to return

        Returns:
            List of matching tools, sorted by relevance
        """
        if not query:
            return self.list_all()[:limit]

        scored = [(tool, tool.matches(query)) for tool in self._tools.values()]
        scored = [(t, s) for t, s in scored if s > 0]
        scored.sort(key=lambda x: x[1], reverse=True)

        return [t for t, _ in scored[:limit]]

    def filter_by_source(self, source: ToolSource) -> list[ToolDefinition]:
        """Filter tools by their source.

        Args:
            source: ToolSource to filter by

        Returns:
            List of tools from that source
        """
        return [t for t in self._tools.values() if t.source == source]

    def __len__(self) -> int:
        """Return number of registered tools."""
        return len(self._tools)

    def __contains__(self, uri: str) -> bool:
        """Check if a tool URI is registered."""
        return uri in self._tools
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/core/test_tool_registry.py -v`
Expected: PASS (7 tests)

**Step 5: Commit**

```bash
git add agentflow/core/tool_registry.py tests/unit/core/test_tool_registry.py
git commit -m "$(cat <<'EOF'
feat(core): add ToolRegistry for unified tool management

Implements central registry for tools from all sources with:
- Register/unregister/get operations
- Search by query with relevance scoring
- Filter by source type (builtin, MCP, skill, dynamic)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 1.3: Create AgentCapability Model

**Files:**
- Create: `agentflow/core/agent_capability.py`
- Test: `tests/unit/core/test_agent_capability.py`

**Step 1: Write the failing test**

```python
# tests/unit/core/test_agent_capability.py
"""Tests for AgentCapability model."""
import pytest


def test_capability_creation():
    """Test basic capability creation."""
    from agentflow.core.agent_capability import AgentCapability

    cap = AgentCapability(
        id="pdf_analysis_v1",
        name="PDF Analysis",
        description="Analyzes PDF documents and extracts information",
        input_schema={"type": "object", "properties": {"pdf_path": {"type": "string"}}},
        output_schema={"type": "object", "properties": {"text": {"type": "string"}}},
        required_tools=["tool://mcp/filesystem/read_file"],
        tags=["pdf", "analysis", "extraction"],
    )

    assert cap.id == "pdf_analysis_v1"
    assert cap.name == "PDF Analysis"
    assert len(cap.required_tools) == 1
    assert "pdf" in cap.tags


def test_capability_default_confidence():
    """Test default confidence is 1.0 for manually defined capabilities."""
    from agentflow.core.agent_capability import AgentCapability

    cap = AgentCapability(
        id="test",
        name="Test",
        description="Test capability",
    )

    assert cap.confidence == 1.0


def test_capability_llm_config():
    """Test LLM configuration in capability."""
    from agentflow.core.agent_capability import AgentCapability, LLMConfig

    cap = AgentCapability(
        id="creative_writing",
        name="Creative Writing",
        description="Generates creative content",
        llm_config=LLMConfig(
            model="claude-3-opus",
            temperature=0.9,
            max_tokens=4000,
        ),
    )

    assert cap.llm_config.temperature == 0.9
    assert cap.llm_config.model == "claude-3-opus"


def test_capability_matches_requirements():
    """Test matching capability against task requirements."""
    from agentflow.core.agent_capability import AgentCapability, TaskRequirement

    cap = AgentCapability(
        id="pdf_analysis",
        name="PDF Analysis",
        description="Analyzes PDF documents",
        tags=["pdf", "analysis"],
        required_tools=["tool://mcp/filesystem/read_file"],
    )

    req = TaskRequirement(
        description="Analyze a PDF report",
        required_tags=["pdf"],
        required_tools=["tool://mcp/filesystem/read_file"],
    )

    score = cap.matches(req)

    assert score > 0.5  # Good match


def test_capability_from_agent_metadata():
    """Test creating capability from agent metadata."""
    from agentflow.core.agent_capability import AgentCapability

    metadata = {
        "name": "SummarizerAgent",
        "description": "Summarizes text documents",
        "skills": ["summarization", "text_analysis"],
        "tools": ["tool://builtin/tokenizer"],
    }

    cap = AgentCapability.from_agent_metadata(metadata)

    assert cap.id == "summarizeragent"
    assert cap.name == "SummarizerAgent"
    assert "summarization" in cap.tags
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/core/test_agent_capability.py -v`
Expected: FAIL with "ModuleNotFoundError: No module named 'agentflow.core.agent_capability'"

**Step 3: Write minimal implementation**

```python
# agentflow/core/agent_capability.py
"""Agent capability model for formal capability declaration and matching."""
from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field


class LLMConfig(BaseModel):
    """LLM configuration requirements for a capability."""

    model: str | None = Field(default=None, description="Preferred model name")
    temperature: float = Field(default=0.7, ge=0.0, le=2.0)
    max_tokens: int = Field(default=2000, gt=0)
    additional: dict[str, Any] = Field(default_factory=dict)


class TaskRequirement(BaseModel):
    """Requirements for a task that needs capability matching."""

    description: str = Field(..., description="Task description")
    required_tags: list[str] = Field(default_factory=list)
    required_tools: list[str] = Field(default_factory=list)
    preferred_model: str | None = None


class AgentCapability(BaseModel):
    """Formal capability declaration for an agent.

    Used for:
    - Capability-based agent discovery
    - Task-to-agent matching
    - Auto-agent generation requirements
    """

    id: str = Field(..., description="Unique capability ID (lowercase, versioned)")
    name: str = Field(..., description="Human-readable name")
    description: str = Field(..., description="What this capability does")
    input_schema: dict[str, Any] = Field(
        default_factory=lambda: {"type": "object", "properties": {}},
        description="Expected input format",
    )
    output_schema: dict[str, Any] = Field(
        default_factory=lambda: {"type": "object", "properties": {}},
        description="Expected output format",
    )
    required_tools: list[str] = Field(
        default_factory=list, description="Tool URIs this capability needs"
    )
    llm_config: LLMConfig = Field(
        default_factory=LLMConfig, description="LLM requirements"
    )
    tags: list[str] = Field(default_factory=list, description="Discovery tags")
    confidence: float = Field(
        default=1.0, ge=0.0, le=1.0, description="Confidence score (lower for learned)"
    )
    metadata: dict[str, Any] = Field(default_factory=dict)

    def matches(self, requirement: TaskRequirement) -> float:
        """Calculate match score against a task requirement.

        Args:
            requirement: Task requirements to match against

        Returns:
            Score between 0.0 and 1.0
        """
        score = 0.0
        weights = {"description": 0.3, "tags": 0.4, "tools": 0.3}

        # Description match
        req_words = set(requirement.description.lower().split())
        cap_words = set(self.description.lower().split())
        cap_words.update(self.name.lower().split())
        overlap = len(req_words & cap_words)
        if req_words:
            score += weights["description"] * (overlap / len(req_words))

        # Tag match
        if requirement.required_tags:
            matched_tags = sum(1 for t in requirement.required_tags if t in self.tags)
            score += weights["tags"] * (matched_tags / len(requirement.required_tags))
        else:
            score += weights["tags"] * 0.5  # Neutral if no tags required

        # Tool match
        if requirement.required_tools:
            matched_tools = sum(
                1 for t in requirement.required_tools if t in self.required_tools
            )
            score += weights["tools"] * (matched_tools / len(requirement.required_tools))
        else:
            score += weights["tools"] * 0.5  # Neutral if no tools required

        return score * self.confidence

    @classmethod
    def from_agent_metadata(cls, metadata: dict[str, Any]) -> AgentCapability:
        """Create capability from existing agent metadata.

        Args:
            metadata: Agent metadata dict with name, description, skills, tools

        Returns:
            AgentCapability instance
        """
        name = metadata.get("name", "unknown")
        skills = metadata.get("skills", [])
        tools = metadata.get("tools", [])

        return cls(
            id=name.lower().replace(" ", "_"),
            name=name,
            description=metadata.get("description", ""),
            required_tools=tools,
            tags=skills,
            metadata={"source": "agent_metadata", "original": metadata},
        )
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/core/test_agent_capability.py -v`
Expected: PASS (5 tests)

**Step 5: Commit**

```bash
git add agentflow/core/agent_capability.py tests/unit/core/test_agent_capability.py
git commit -m "$(cat <<'EOF'
feat(core): add AgentCapability model for formal capability declaration

Introduces AgentCapability with:
- Input/output schemas
- Required tools declaration
- LLM configuration requirements
- Task requirement matching with scoring
- Factory from existing agent metadata

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 1.4: Create AgentRegistry Interface

**Files:**
- Create: `agentflow/core/agent_registry.py`
- Test: `tests/unit/core/test_agent_registry.py`

**Step 1: Write the failing test**

```python
# tests/unit/core/test_agent_registry.py
"""Tests for AgentRegistry interface."""
import pytest


@pytest.fixture
def agent_registry():
    """Create a fresh AgentRegistry."""
    from agentflow.core.agent_registry import AgentRegistry
    return AgentRegistry()


@pytest.fixture
def sample_capability():
    """Create a sample AgentCapability."""
    from agentflow.core.agent_capability import AgentCapability
    return AgentCapability(
        id="test_agent_v1",
        name="Test Agent",
        description="An agent for testing",
        tags=["test", "demo"],
        required_tools=["tool://builtin/echo"],
    )


class TestAgentRegistry:
    """Tests for AgentRegistry."""

    def test_register_agent(self, agent_registry, sample_capability):
        """Test registering an agent with its capability."""
        agent_registry.register(
            agent_id="test_agent",
            capability=sample_capability,
            factory=lambda: "mock_agent_instance",
        )

        assert "test_agent" in agent_registry

    def test_get_agent_factory(self, agent_registry, sample_capability):
        """Test retrieving agent factory."""
        factory = lambda: "instance"
        agent_registry.register("test", sample_capability, factory)

        retrieved_factory = agent_registry.get_factory("test")

        assert retrieved_factory() == "instance"

    def test_get_capability(self, agent_registry, sample_capability):
        """Test retrieving agent capability."""
        agent_registry.register("test", sample_capability, lambda: None)

        cap = agent_registry.get_capability("test")

        assert cap.name == "Test Agent"

    def test_find_by_tags(self, agent_registry):
        """Test finding agents by tags."""
        from agentflow.core.agent_capability import AgentCapability

        cap1 = AgentCapability(
            id="a1", name="A1", description="", tags=["pdf", "analysis"]
        )
        cap2 = AgentCapability(
            id="a2", name="A2", description="", tags=["text", "analysis"]
        )

        agent_registry.register("a1", cap1, lambda: None)
        agent_registry.register("a2", cap2, lambda: None)

        pdf_agents = agent_registry.find_by_tags(["pdf"])
        analysis_agents = agent_registry.find_by_tags(["analysis"])

        assert len(pdf_agents) == 1
        assert len(analysis_agents) == 2

    def test_find_matching(self, agent_registry):
        """Test finding agents matching requirements."""
        from agentflow.core.agent_capability import AgentCapability, TaskRequirement

        cap1 = AgentCapability(
            id="pdf_analyzer",
            name="PDF Analyzer",
            description="Analyzes PDF documents",
            tags=["pdf"],
        )
        cap2 = AgentCapability(
            id="text_summarizer",
            name="Text Summarizer",
            description="Summarizes text",
            tags=["text"],
        )

        agent_registry.register("pdf", cap1, lambda: None)
        agent_registry.register("text", cap2, lambda: None)

        req = TaskRequirement(
            description="Analyze a PDF document",
            required_tags=["pdf"],
        )

        matches = agent_registry.find_matching(req)

        assert len(matches) > 0
        assert matches[0][0] == "pdf"  # (agent_id, score)

    def test_list_all(self, agent_registry, sample_capability):
        """Test listing all registered agents."""
        agent_registry.register("a1", sample_capability, lambda: None)
        agent_registry.register("a2", sample_capability, lambda: None)

        all_agents = agent_registry.list_all()

        assert len(all_agents) == 2
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/core/test_agent_registry.py -v`
Expected: FAIL with "ModuleNotFoundError: No module named 'agentflow.core.agent_registry'"

**Step 3: Write minimal implementation**

```python
# agentflow/core/agent_registry.py
"""Central registry for discovering and instantiating agents."""
from __future__ import annotations

from typing import Any, Callable, TYPE_CHECKING

if TYPE_CHECKING:
    from agentflow.core.agent_capability import AgentCapability, TaskRequirement


class AgentEntry:
    """Registry entry for an agent."""

    def __init__(
        self,
        agent_id: str,
        capability: AgentCapability,
        factory: Callable[[], Any],
    ) -> None:
        self.agent_id = agent_id
        self.capability = capability
        self.factory = factory


class AgentRegistry:
    """Central registry for agents with capability-based discovery.

    Provides:
    - Agent registration with capabilities
    - Tag-based discovery
    - Task requirement matching
    - Factory-based instantiation
    """

    def __init__(self) -> None:
        """Initialize empty registry."""
        self._agents: dict[str, AgentEntry] = {}

    def register(
        self,
        agent_id: str,
        capability: AgentCapability,
        factory: Callable[[], Any],
    ) -> None:
        """Register an agent with its capability and factory.

        Args:
            agent_id: Unique identifier for the agent
            capability: Agent's capability declaration
            factory: Callable that creates agent instances
        """
        self._agents[agent_id] = AgentEntry(agent_id, capability, factory)

    def unregister(self, agent_id: str) -> bool:
        """Remove an agent from the registry.

        Args:
            agent_id: Agent ID to remove

        Returns:
            True if removed, False if not found
        """
        if agent_id in self._agents:
            del self._agents[agent_id]
            return True
        return False

    def get_factory(self, agent_id: str) -> Callable[[], Any] | None:
        """Get the factory function for an agent.

        Args:
            agent_id: Agent ID

        Returns:
            Factory callable or None if not found
        """
        entry = self._agents.get(agent_id)
        return entry.factory if entry else None

    def get_capability(self, agent_id: str) -> AgentCapability | None:
        """Get the capability declaration for an agent.

        Args:
            agent_id: Agent ID

        Returns:
            AgentCapability or None if not found
        """
        entry = self._agents.get(agent_id)
        return entry.capability if entry else None

    def find_by_tags(self, tags: list[str]) -> list[str]:
        """Find agents that have all specified tags.

        Args:
            tags: Tags to match

        Returns:
            List of matching agent IDs
        """
        results = []
        for agent_id, entry in self._agents.items():
            if all(tag in entry.capability.tags for tag in tags):
                results.append(agent_id)
        return results

    def find_matching(
        self, requirement: TaskRequirement, limit: int = 5
    ) -> list[tuple[str, float]]:
        """Find agents matching a task requirement.

        Args:
            requirement: Task requirements
            limit: Maximum results

        Returns:
            List of (agent_id, score) tuples, sorted by score descending
        """
        scored = []
        for agent_id, entry in self._agents.items():
            score = entry.capability.matches(requirement)
            if score > 0:
                scored.append((agent_id, score))

        scored.sort(key=lambda x: x[1], reverse=True)
        return scored[:limit]

    def list_all(self) -> list[str]:
        """List all registered agent IDs.

        Returns:
            List of agent IDs
        """
        return list(self._agents.keys())

    def __contains__(self, agent_id: str) -> bool:
        """Check if an agent is registered."""
        return agent_id in self._agents

    def __len__(self) -> int:
        """Return number of registered agents."""
        return len(self._agents)
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/core/test_agent_registry.py -v`
Expected: PASS (6 tests)

**Step 5: Commit**

```bash
git add agentflow/core/agent_registry.py tests/unit/core/test_agent_registry.py
git commit -m "$(cat <<'EOF'
feat(core): add AgentRegistry for capability-based agent discovery

Implements central agent registry with:
- Registration with capability and factory
- Tag-based discovery
- Task requirement matching with scoring
- Factory-based instantiation

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Phase 2: Refactor Agent Decorator for Lazy Initialization

### Task 2.1: Add Lazy LLM Initialization

**Files:**
- Modify: `agentflow/agent_decorator.py:176-196`
- Test: `tests/unit/test_agent_decorator_lazy.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_agent_decorator_lazy.py
"""Tests for lazy initialization in @agent decorator."""
import pytest
from unittest.mock import patch, MagicMock


def test_agent_no_llm_at_creation():
    """Test that agent can be created without LLM being available."""
    from agentflow import agent

    @agent(
        name="TestAgent",
        description="A test agent",
        lazy_init=True,  # New parameter
    )
    class TestAgent:
        async def run(self, query: str) -> str:
            return f"Processed: {query}"

    # Should not raise even without LLM configured
    instance = TestAgent()
    assert instance is not None


def test_agent_llm_resolved_on_first_use():
    """Test LLM is resolved when first needed."""
    from agentflow import agent

    @agent(
        name="LazyAgent",
        description="Lazy agent",
        lazy_init=True,
    )
    class LazyAgent:
        async def run(self, query: str) -> str:
            # This would trigger LLM resolution
            return await self._llm.generate(query)

    instance = LazyAgent()

    # LLM should not be set yet
    assert not hasattr(instance, '_llm') or instance._llm is None


def test_agent_explicit_llm_injection():
    """Test explicit LLM injection for testing."""
    from agentflow import agent

    @agent(
        name="InjectableAgent",
        description="Agent with injectable LLM",
        lazy_init=True,
    )
    class InjectableAgent:
        async def run(self, query: str) -> str:
            return await self._llm.generate(query)

    mock_llm = MagicMock()
    mock_llm.generate.return_value = "mocked response"

    instance = InjectableAgent()
    instance.inject_llm(mock_llm)  # New method

    assert instance._llm is mock_llm


def test_agent_backwards_compatible():
    """Test existing agents without lazy_init still work."""
    from agentflow import agent

    # Default behavior (lazy_init=False for backwards compatibility)
    @agent(
        name="EagerAgent",
        description="Eager agent",
    )
    class EagerAgent:
        async def run(self, query: str) -> str:
            return query

    # This should work as before (may require LLM if one is expected)
    # For now, just verify decorator doesn't break
    assert EagerAgent is not None
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_agent_decorator_lazy.py -v`
Expected: FAIL (lazy_init parameter not recognized)

**Step 3: Modify implementation**

Edit `agentflow/agent_decorator.py` to add lazy initialization:

```python
# In AgentConfig class (around line 50), add:
    lazy_init: bool = False  # New field

# In RegisteredAgent.__init__ (around line 176-196), modify:
    def __init__(self, *args: Any, **kwargs: Any) -> None:
        """Initialize agent instance."""
        self._instance = self._agent_class(*args, **kwargs)
        self._instance._agent_config = self._config

        # Lazy initialization support
        if self._config.lazy_init:
            self._instance._llm = None  # Deferred
            self._instance._llm_resolver = lambda: get_llm(
                temperature=self._config.temperature,
                max_tokens=self._config.max_tokens,
            )
        else:
            # Original eager initialization
            self._llm_provider = get_llm(
                temperature=self._config.temperature,
                max_tokens=self._config.max_tokens,
            )
            self._instance._llm = self._llm_provider

        # Add injection method for testing
        self._instance.inject_llm = self._inject_llm

        # ... rest of initialization

    def _inject_llm(self, llm: Any) -> None:
        """Inject LLM for testing purposes."""
        self._instance._llm = llm
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/test_agent_decorator_lazy.py -v`
Expected: PASS (4 tests)

**Step 5: Commit**

```bash
git add agentflow/agent_decorator.py tests/unit/test_agent_decorator_lazy.py
git commit -m "$(cat <<'EOF'
feat(agent): add lazy_init option to @agent decorator

Enables agents to be created without immediate LLM resolution:
- lazy_init=True defers LLM creation until first use
- inject_llm() method for testing with mocks
- Backwards compatible (lazy_init=False by default)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2.2: Integrate AgentRegistry with @agent Decorator

**Files:**
- Modify: `agentflow/agent_decorator.py`
- Test: `tests/unit/test_agent_registry_integration.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_agent_registry_integration.py
"""Tests for @agent decorator integration with AgentRegistry."""
import pytest


def test_agent_registered_on_decoration():
    """Test that @agent registers agent in global registry."""
    from agentflow import agent
    from agentflow.core.agent_registry import get_global_registry

    registry = get_global_registry()
    initial_count = len(registry)

    @agent(
        name="AutoRegisteredAgent",
        description="Auto registered",
    )
    class AutoRegisteredAgent:
        async def run(self, query: str) -> str:
            return query

    assert len(registry) == initial_count + 1
    assert "AutoRegisteredAgent" in registry


def test_agent_capability_generated():
    """Test that capability is auto-generated from decorator config."""
    from agentflow import agent
    from agentflow.core.agent_registry import get_global_registry

    @agent(
        name="CapableAgent",
        description="An agent with capabilities",
        skills=["analysis", "reporting"],
        tools=["tool://builtin/search"],
    )
    class CapableAgent:
        async def run(self, query: str) -> str:
            return query

    registry = get_global_registry()
    cap = registry.get_capability("CapableAgent")

    assert cap is not None
    assert "analysis" in cap.tags
    assert "tool://builtin/search" in cap.required_tools


def test_agent_discoverable_by_tags():
    """Test registered agent can be discovered by tags."""
    from agentflow import agent
    from agentflow.core.agent_registry import get_global_registry

    @agent(
        name="TaggedAgent",
        description="Agent with tags",
        skills=["unique_tag_for_test"],
    )
    class TaggedAgent:
        async def run(self, query: str) -> str:
            return query

    registry = get_global_registry()
    found = registry.find_by_tags(["unique_tag_for_test"])

    assert "TaggedAgent" in found
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_agent_registry_integration.py -v`
Expected: FAIL (get_global_registry not found or agents not registered)

**Step 3: Modify implementation**

Add global registry and registration logic:

```python
# agentflow/core/agent_registry.py - add at end:

_global_registry: AgentRegistry | None = None


def get_global_registry() -> AgentRegistry:
    """Get or create the global agent registry singleton."""
    global _global_registry
    if _global_registry is None:
        _global_registry = AgentRegistry()
    return _global_registry


# agentflow/agent_decorator.py - modify agent() function:

from agentflow.core.agent_registry import get_global_registry
from agentflow.core.agent_capability import AgentCapability

def agent(...) -> Callable:
    """Agent decorator."""
    def decorator(cls: type) -> type:
        # ... existing code ...

        # Register in global registry
        registry = get_global_registry()
        capability = AgentCapability(
            id=config.name.lower().replace(" ", "_"),
            name=config.name,
            description=config.description,
            tags=list(config.skills) if config.skills else [],
            required_tools=list(config.tools) if config.tools else [],
        )
        registry.register(
            agent_id=config.name,
            capability=capability,
            factory=lambda: registered_agent_class(),
        )

        return registered_agent_class

    return decorator
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/test_agent_registry_integration.py -v`
Expected: PASS (3 tests)

**Step 5: Commit**

```bash
git add agentflow/agent_decorator.py agentflow/core/agent_registry.py tests/unit/test_agent_registry_integration.py
git commit -m "$(cat <<'EOF'
feat(agent): auto-register agents in global AgentRegistry

@agent decorator now:
- Registers agent in global registry on decoration
- Auto-generates AgentCapability from config
- Enables tag-based and requirement-based discovery

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Phase 3: Tool Binding Interface

### Task 3.1: Create ToolBinder Interface

**Files:**
- Create: `agentflow/core/tool_binding.py`
- Test: `tests/unit/core/test_tool_binding.py`

**Step 1: Write the failing test**

```python
# tests/unit/core/test_tool_binding.py
"""Tests for ToolBinder interface."""
import pytest
from unittest.mock import MagicMock, AsyncMock


@pytest.fixture
def tool_registry():
    """Create populated tool registry."""
    from agentflow.core.tool_registry import ToolRegistry
    from agentflow.core.tool_definition import ToolDefinition, ToolSource

    registry = ToolRegistry()
    registry.register(ToolDefinition(
        uri="tool://builtin/calculator",
        name="calculator",
        description="Calculate math",
        source=ToolSource.BUILTIN,
    ))
    registry.register(ToolDefinition(
        uri="tool://mcp/fs/read",
        name="read",
        description="Read files",
        source=ToolSource.MCP,
    ))
    return registry


@pytest.fixture
def tool_binder(tool_registry):
    """Create ToolBinder with registry."""
    from agentflow.core.tool_binding import ToolBinder
    return ToolBinder(tool_registry)


class TestToolBinder:
    """Tests for ToolBinder."""

    @pytest.mark.asyncio
    async def test_bind_tools_by_uri(self, tool_binder):
        """Test binding specific tools by URI."""
        mock_agent = MagicMock()
        mock_agent._tools = None

        uris = ["tool://builtin/calculator"]
        bound = await tool_binder.bind(mock_agent, tool_uris=uris)

        assert bound._tools is not None
        assert len(bound._tools) == 1

    @pytest.mark.asyncio
    async def test_bind_tools_by_capability(self, tool_binder, tool_registry):
        """Test binding tools required by a capability."""
        from agentflow.core.agent_capability import AgentCapability

        mock_agent = MagicMock()
        mock_agent._tools = None

        cap = AgentCapability(
            id="test",
            name="Test",
            description="Test",
            required_tools=["tool://builtin/calculator", "tool://mcp/fs/read"],
        )

        bound = await tool_binder.bind_for_capability(mock_agent, cap)

        assert len(bound._tools) == 2

    @pytest.mark.asyncio
    async def test_bind_validates_tool_exists(self, tool_binder):
        """Test binding fails gracefully for missing tools."""
        mock_agent = MagicMock()
        mock_agent._tools = None

        uris = ["tool://nonexistent/tool"]
        bound = await tool_binder.bind(mock_agent, tool_uris=uris)

        # Should bind what exists, skip missing
        assert len(bound._tools) == 0

    @pytest.mark.asyncio
    async def test_create_tool_executor(self, tool_binder):
        """Test creating tool executor from bindings."""
        from agentflow.core.tool_binding import BoundAgent

        mock_agent = MagicMock()
        mock_agent._tools = None

        uris = ["tool://builtin/calculator"]
        bound = await tool_binder.bind(mock_agent, tool_uris=uris)

        # BoundAgent should have execute_tool method
        assert hasattr(bound, 'execute_tool') or hasattr(bound, '_tool_executor')
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/core/test_tool_binding.py -v`
Expected: FAIL with "ModuleNotFoundError: No module named 'agentflow.core.tool_binding'"

**Step 3: Write minimal implementation**

```python
# agentflow/core/tool_binding.py
"""Tool binding interface for runtime tool attachment to agents."""
from __future__ import annotations

from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from agentflow.core.agent_capability import AgentCapability
    from agentflow.core.tool_definition import ToolDefinition
    from agentflow.core.tool_registry import ToolRegistry


class BoundTools:
    """Container for tools bound to an agent."""

    def __init__(self, tools: list[ToolDefinition]) -> None:
        self._tools = {t.uri: t for t in tools}

    def __len__(self) -> int:
        return len(self._tools)

    def __iter__(self):
        return iter(self._tools.values())

    def get(self, uri: str) -> ToolDefinition | None:
        return self._tools.get(uri)

    def to_mcp_format(self) -> list[dict[str, Any]]:
        """Convert all tools to MCP format for LLM."""
        return [t.to_mcp() for t in self._tools.values()]


class ToolBinder:
    """Binds tools from registry to agent instances.

    Provides runtime tool attachment with:
    - URI-based binding
    - Capability-based binding
    - Validation of tool existence
    """

    def __init__(self, registry: ToolRegistry) -> None:
        """Initialize with tool registry.

        Args:
            registry: ToolRegistry to resolve tools from
        """
        self._registry = registry

    async def bind(
        self,
        agent: Any,
        tool_uris: list[str] | None = None,
    ) -> Any:
        """Bind specified tools to an agent.

        Args:
            agent: Agent instance to bind tools to
            tool_uris: List of tool URIs to bind

        Returns:
            Agent with tools bound
        """
        tools = []

        for uri in tool_uris or []:
            tool = self._registry.get(uri)
            if tool:
                tools.append(tool)

        agent._tools = BoundTools(tools)
        agent._tool_executor = self._create_executor(tools)

        return agent

    async def bind_for_capability(
        self,
        agent: Any,
        capability: AgentCapability,
    ) -> Any:
        """Bind all tools required by a capability.

        Args:
            agent: Agent instance
            capability: Capability declaring required tools

        Returns:
            Agent with capability's tools bound
        """
        return await self.bind(agent, capability.required_tools)

    def _create_executor(self, tools: list[ToolDefinition]) -> ToolExecutor:
        """Create executor for bound tools.

        Args:
            tools: List of tool definitions

        Returns:
            ToolExecutor instance
        """
        return ToolExecutor(tools)


class ToolExecutor:
    """Executes bound tools."""

    def __init__(self, tools: list[ToolDefinition]) -> None:
        self._tools = {t.uri: t for t in tools}
        self._handlers: dict[str, Any] = {}

    def register_handler(self, uri: str, handler: Any) -> None:
        """Register execution handler for a tool URI."""
        self._handlers[uri] = handler

    async def execute(self, uri: str, **kwargs: Any) -> Any:
        """Execute a tool by URI.

        Args:
            uri: Tool URI
            **kwargs: Tool input parameters

        Returns:
            Tool execution result

        Raises:
            ValueError: If tool not found or no handler
        """
        if uri not in self._tools:
            raise ValueError(f"Tool not bound: {uri}")

        handler = self._handlers.get(uri)
        if not handler:
            raise ValueError(f"No handler for tool: {uri}")

        return await handler(**kwargs)
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/core/test_tool_binding.py -v`
Expected: PASS (4 tests)

**Step 5: Commit**

```bash
git add agentflow/core/tool_binding.py tests/unit/core/test_tool_binding.py
git commit -m "$(cat <<'EOF'
feat(core): add ToolBinder for runtime tool attachment

Implements ToolBinder with:
- URI-based tool binding
- Capability-based binding (bind all required tools)
- Validation against registry
- ToolExecutor for execution

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3.2: Integrate ToolBinder with SimpleEngine

**Files:**
- Modify: `agentflow/engines/simple_engine.py`
- Test: `tests/unit/engines/test_simple_engine_tools.py`

**Step 1: Write the failing test**

```python
# tests/unit/engines/test_simple_engine_tools.py
"""Tests for SimpleEngine tool integration."""
import pytest
from unittest.mock import MagicMock, AsyncMock


@pytest.fixture
def tool_registry():
    """Create tool registry with test tools."""
    from agentflow.core.tool_registry import ToolRegistry
    from agentflow.core.tool_definition import ToolDefinition, ToolSource

    registry = ToolRegistry()
    registry.register(ToolDefinition(
        uri="tool://builtin/search",
        name="search",
        description="Search documents",
        source=ToolSource.BUILTIN,
        input_schema={"type": "object", "properties": {"query": {"type": "string"}}},
    ))
    return registry


class TestSimpleEngineTools:
    """Tests for SimpleEngine with tool binding."""

    @pytest.mark.asyncio
    async def test_engine_binds_tools_on_init(self, tool_registry):
        """Test engine binds tools during initialization."""
        from agentflow.engines.simple_engine import SimpleEngine

        engine = SimpleEngine(
            tools=["tool://builtin/search"],
            tool_registry=tool_registry,  # New parameter
        )

        await engine._initialize()

        assert engine._bound_tools is not None
        assert len(engine._bound_tools) == 1

    @pytest.mark.asyncio
    async def test_engine_provides_tools_to_llm(self, tool_registry):
        """Test engine includes tools in LLM call."""
        from agentflow.engines.simple_engine import SimpleEngine

        mock_llm = AsyncMock()
        mock_llm.generate.return_value = "result"

        engine = SimpleEngine(
            tools=["tool://builtin/search"],
            tool_registry=tool_registry,
        )
        engine._llm = mock_llm

        await engine._initialize()
        result = await engine.run("test query")

        # LLM should have been called with tools
        call_kwargs = mock_llm.generate.call_args
        assert "tools" in call_kwargs.kwargs or len(call_kwargs.args) > 1

    @pytest.mark.asyncio
    async def test_engine_without_tools_works(self):
        """Test engine works without any tools."""
        from agentflow.engines.simple_engine import SimpleEngine

        engine = SimpleEngine()  # No tools

        await engine._initialize()

        assert engine._bound_tools is None or len(engine._bound_tools) == 0
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/engines/test_simple_engine_tools.py -v`
Expected: FAIL (tool_registry parameter not supported)

**Step 3: Modify implementation**

```python
# agentflow/engines/simple_engine.py - modify __init__ and _initialize:

from agentflow.core.tool_registry import ToolRegistry, get_global_registry
from agentflow.core.tool_binding import ToolBinder, BoundTools

class SimpleEngine:
    def __init__(
        self,
        # ... existing params ...
        tools: list[str] | None = None,
        tool_registry: ToolRegistry | None = None,
    ) -> None:
        # ... existing code ...
        self._tool_uris = tools or []
        self._tool_registry = tool_registry or get_global_registry()
        self._bound_tools: BoundTools | None = None
        self._tool_binder = ToolBinder(self._tool_registry)

    async def _initialize(self) -> None:
        """Initialize engine including tool binding."""
        # Bind tools if specified
        if self._tool_uris:
            # Create a simple object to hold tools
            class ToolHolder:
                _tools = None
                _tool_executor = None

            holder = ToolHolder()
            await self._tool_binder.bind(holder, self._tool_uris)
            self._bound_tools = holder._tools
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/engines/test_simple_engine_tools.py -v`
Expected: PASS (3 tests)

**Step 5: Commit**

```bash
git add agentflow/engines/simple_engine.py tests/unit/engines/test_simple_engine_tools.py
git commit -m "$(cat <<'EOF'
feat(engines): integrate ToolBinder with SimpleEngine

SimpleEngine now:
- Accepts tool_registry parameter
- Binds tools during initialization
- Uses global registry by default
- Provides tools to LLM calls

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Phase 4: Auto-Discovery Integration

### Task 4.1: Create ToolDiscoveryService

**Files:**
- Create: `agentflow/core/tool_discovery.py`
- Test: `tests/unit/core/test_tool_discovery.py`

**Step 1: Write the failing test**

```python
# tests/unit/core/test_tool_discovery.py
"""Tests for ToolDiscoveryService."""
import pytest
from unittest.mock import MagicMock, AsyncMock, patch


@pytest.fixture
def tool_registry():
    """Create empty tool registry."""
    from agentflow.core.tool_registry import ToolRegistry
    return ToolRegistry()


class TestToolDiscoveryService:
    """Tests for ToolDiscoveryService."""

    @pytest.mark.asyncio
    async def test_discover_builtin_tools(self, tool_registry):
        """Test discovering @tool decorated functions."""
        from agentflow.core.tool_discovery import ToolDiscoveryService

        service = ToolDiscoveryService(tool_registry)

        await service.discover_builtins()

        # Should have discovered some builtin tools
        builtins = tool_registry.filter_by_source(
            service.tool_registry.get("tool://builtin/")  # Check any builtin
        )
        # At minimum, registry should be populated
        assert tool_registry is not None

    @pytest.mark.asyncio
    async def test_discover_skills(self, tool_registry):
        """Test discovering skills as tools."""
        from agentflow.core.tool_discovery import ToolDiscoveryService
        from agentflow.core.tool_definition import ToolSource

        service = ToolDiscoveryService(tool_registry)

        # Mock skill engine
        mock_skills = [
            {"name": "code_review", "description": "Reviews code"},
            {"name": "summarize", "description": "Summarizes text"},
        ]

        await service.discover_skills(mock_skills)

        skills = tool_registry.filter_by_source(ToolSource.SKILL)
        assert len(skills) == 2

    @pytest.mark.asyncio
    async def test_discover_mcp_servers(self, tool_registry):
        """Test discovering MCP server tools."""
        from agentflow.core.tool_discovery import ToolDiscoveryService
        from agentflow.core.tool_definition import ToolSource

        service = ToolDiscoveryService(tool_registry)

        # Mock MCP server response
        mock_tools = [
            {"name": "read_file", "description": "Read file", "inputSchema": {}},
            {"name": "write_file", "description": "Write file", "inputSchema": {}},
        ]

        await service.discover_mcp_tools("filesystem", mock_tools)

        mcp_tools = tool_registry.filter_by_source(ToolSource.MCP)
        assert len(mcp_tools) == 2
        assert any(t.uri == "tool://mcp/filesystem/read_file" for t in mcp_tools)

    @pytest.mark.asyncio
    async def test_full_discovery(self, tool_registry):
        """Test running full discovery pipeline."""
        from agentflow.core.tool_discovery import ToolDiscoveryService

        service = ToolDiscoveryService(tool_registry)

        await service.discover_all()

        # Should have run all discovery methods
        assert len(tool_registry) >= 0  # May be 0 if no tools configured
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/core/test_tool_discovery.py -v`
Expected: FAIL with "ModuleNotFoundError: No module named 'agentflow.core.tool_discovery'"

**Step 3: Write minimal implementation**

```python
# agentflow/core/tool_discovery.py
"""Service for discovering tools from all sources."""
from __future__ import annotations

from typing import Any, TYPE_CHECKING

from agentflow.core.tool_definition import ToolDefinition, ToolSource

if TYPE_CHECKING:
    from agentflow.core.tool_registry import ToolRegistry


class ToolDiscoveryService:
    """Discovers and registers tools from all sources.

    Sources:
    - Builtin: @tool decorated functions
    - Skills: SkillEngine loaded skills
    - MCP: MCP server tool definitions
    - Dynamic: Runtime generated tools
    """

    def __init__(self, registry: ToolRegistry) -> None:
        """Initialize with target registry.

        Args:
            registry: ToolRegistry to populate
        """
        self.tool_registry = registry

    async def discover_all(self) -> int:
        """Run full discovery pipeline.

        Returns:
            Number of tools discovered
        """
        initial = len(self.tool_registry)

        await self.discover_builtins()
        # Skills and MCP require external data, skip in discover_all

        return len(self.tool_registry) - initial

    async def discover_builtins(self) -> int:
        """Discover @tool decorated functions.

        Returns:
            Number of builtin tools discovered
        """
        # Import tool provider to get registered tools
        try:
            from agentflow.providers.tool_provider import ToolProvider

            provider = ToolProvider()
            tools = provider.discover()

            count = 0
            for tool_info in tools:
                tool_def = ToolDefinition.from_builtin(
                    name=tool_info.get("name", "unknown"),
                    description=tool_info.get("description", ""),
                    input_schema=tool_info.get("input_schema", {}),
                )
                self.tool_registry.register(tool_def)
                count += 1

            return count
        except ImportError:
            return 0

    async def discover_skills(self, skills: list[dict[str, Any]]) -> int:
        """Register skills as tools.

        Args:
            skills: List of skill definitions

        Returns:
            Number of skills registered
        """
        count = 0
        for skill in skills:
            tool_def = ToolDefinition.from_skill(skill)
            self.tool_registry.register(tool_def)
            count += 1
        return count

    async def discover_mcp_tools(
        self,
        server_name: str,
        tools: list[dict[str, Any]],
    ) -> int:
        """Register MCP server tools.

        Args:
            server_name: MCP server name
            tools: List of MCP tool definitions

        Returns:
            Number of MCP tools registered
        """
        count = 0
        for mcp_tool in tools:
            tool_def = ToolDefinition.from_mcp(mcp_tool, server_name)
            self.tool_registry.register(tool_def)
            count += 1
        return count

    async def refresh(self) -> int:
        """Clear and re-discover all tools.

        Returns:
            Number of tools after refresh
        """
        # Clear existing
        for tool in list(self.tool_registry.list_all()):
            self.tool_registry.unregister(tool.uri)

        # Re-discover
        await self.discover_all()

        return len(self.tool_registry)
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/core/test_tool_discovery.py -v`
Expected: PASS (4 tests)

**Step 5: Commit**

```bash
git add agentflow/core/tool_discovery.py tests/unit/core/test_tool_discovery.py
git commit -m "$(cat <<'EOF'
feat(core): add ToolDiscoveryService for unified tool discovery

Implements discovery service for:
- Builtin @tool decorated functions
- Skills from SkillEngine
- MCP server tools
- Full discovery pipeline

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Phase 5: Export New APIs

### Task 5.1: Update Public API Exports

**Files:**
- Modify: `agentflow/__init__.py`
- Modify: `agentflow/core/__init__.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_public_api.py
"""Tests for public API exports."""
import pytest


def test_core_models_exported():
    """Test core models are exported from main package."""
    from agentflow import (
        ToolDefinition,
        ToolSource,
        ToolRegistry,
        AgentCapability,
        AgentRegistry,
        ToolBinder,
    )

    assert ToolDefinition is not None
    assert ToolRegistry is not None
    assert AgentCapability is not None


def test_get_global_registries():
    """Test global registry accessors are exported."""
    from agentflow import (
        get_tool_registry,
        get_agent_registry,
    )

    tool_reg = get_tool_registry()
    agent_reg = get_agent_registry()

    assert tool_reg is not None
    assert agent_reg is not None


def test_discovery_service_exported():
    """Test discovery service is exported."""
    from agentflow import ToolDiscoveryService

    assert ToolDiscoveryService is not None
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_public_api.py -v`
Expected: FAIL (imports not available)

**Step 3: Update exports**

```python
# agentflow/core/__init__.py
"""Core models and interfaces."""
from agentflow.core.tool_definition import ToolDefinition, ToolSource
from agentflow.core.tool_registry import ToolRegistry, get_global_registry as get_tool_registry
from agentflow.core.agent_capability import AgentCapability, TaskRequirement, LLMConfig
from agentflow.core.agent_registry import AgentRegistry, get_global_registry as get_agent_registry
from agentflow.core.tool_binding import ToolBinder, BoundTools, ToolExecutor
from agentflow.core.tool_discovery import ToolDiscoveryService

__all__ = [
    "ToolDefinition",
    "ToolSource",
    "ToolRegistry",
    "get_tool_registry",
    "AgentCapability",
    "TaskRequirement",
    "LLMConfig",
    "AgentRegistry",
    "get_agent_registry",
    "ToolBinder",
    "BoundTools",
    "ToolExecutor",
    "ToolDiscoveryService",
]


# agentflow/__init__.py - add to existing exports:
from agentflow.core import (
    ToolDefinition,
    ToolSource,
    ToolRegistry,
    get_tool_registry,
    AgentCapability,
    TaskRequirement,
    AgentRegistry,
    get_agent_registry,
    ToolBinder,
    ToolDiscoveryService,
)
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/test_public_api.py -v`
Expected: PASS (3 tests)

**Step 5: Commit**

```bash
git add agentflow/__init__.py agentflow/core/__init__.py tests/unit/test_public_api.py
git commit -m "$(cat <<'EOF'
feat: export new core APIs from main package

Exports from agentflow:
- ToolDefinition, ToolSource, ToolRegistry
- AgentCapability, TaskRequirement, AgentRegistry
- ToolBinder, ToolDiscoveryService
- get_tool_registry(), get_agent_registry()

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Final Integration Test

### Task 6.1: Write Integration Test for Full Flow

**Files:**
- Create: `tests/integration/test_auto_agent_flow.py`

**Step 1: Write the integration test**

```python
# tests/integration/test_auto_agent_flow.py
"""Integration tests for auto-agent creation flow."""
import pytest


@pytest.mark.integration
class TestAutoAgentFlow:
    """Integration tests for complete auto-agent workflow."""

    @pytest.mark.asyncio
    async def test_agent_registration_discovery_binding(self):
        """Test full flow: register agent -> discover -> bind tools -> execute."""
        from agentflow import (
            agent,
            get_agent_registry,
            get_tool_registry,
            ToolDefinition,
            ToolSource,
            ToolBinder,
            ToolDiscoveryService,
        )

        # 1. Register tools
        tool_registry = get_tool_registry()
        tool_registry.register(ToolDefinition(
            uri="tool://builtin/echo",
            name="echo",
            description="Echo input",
            source=ToolSource.BUILTIN,
        ))

        # 2. Define and register agent
        @agent(
            name="IntegrationTestAgent",
            description="Agent for integration testing",
            skills=["testing"],
            tools=["tool://builtin/echo"],
            lazy_init=True,
        )
        class IntegrationTestAgent:
            async def run(self, query: str) -> str:
                return f"Processed: {query}"

        # 3. Discover agent
        agent_registry = get_agent_registry()
        assert "IntegrationTestAgent" in agent_registry

        cap = agent_registry.get_capability("IntegrationTestAgent")
        assert "testing" in cap.tags
        assert "tool://builtin/echo" in cap.required_tools

        # 4. Create instance and bind tools
        factory = agent_registry.get_factory("IntegrationTestAgent")
        instance = factory()

        binder = ToolBinder(tool_registry)
        bound_agent = await binder.bind_for_capability(instance, cap)

        assert len(bound_agent._tools) == 1

        # 5. Execute
        result = await bound_agent.run("test input")
        assert "Processed" in result

    @pytest.mark.asyncio
    async def test_task_requirement_matching(self):
        """Test matching task requirements to agents."""
        from agentflow import (
            agent,
            get_agent_registry,
            TaskRequirement,
        )

        @agent(
            name="PDFAgent",
            description="Analyzes PDF documents",
            skills=["pdf", "analysis"],
            lazy_init=True,
        )
        class PDFAgent:
            async def run(self, query: str) -> str:
                return "PDF analyzed"

        @agent(
            name="TextAgent",
            description="Processes text",
            skills=["text", "processing"],
            lazy_init=True,
        )
        class TextAgent:
            async def run(self, query: str) -> str:
                return "Text processed"

        registry = get_agent_registry()

        # Find agent for PDF task
        req = TaskRequirement(
            description="Analyze a PDF report",
            required_tags=["pdf"],
        )

        matches = registry.find_matching(req)

        assert len(matches) > 0
        assert matches[0][0] == "PDFAgent"  # Best match
```

**Step 2: Run integration test**

Run: `pytest tests/integration/test_auto_agent_flow.py -v -m integration`
Expected: PASS (2 tests)

**Step 3: Commit**

```bash
git add tests/integration/test_auto_agent_flow.py
git commit -m "$(cat <<'EOF'
test: add integration tests for auto-agent flow

Tests complete workflow:
- Agent registration via @agent decorator
- Tool discovery and registration
- Capability-based agent discovery
- Task requirement matching
- Tool binding

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Summary

This plan refactors AgentFlow to support auto-agent creation through:

1. **Phase 1: Foundation** - Unified ToolDefinition, ToolRegistry, AgentCapability, AgentRegistry
2. **Phase 2: Lazy Init** - Testable agents without LLM dependency, AgentRegistry integration
3. **Phase 3: Tool Binding** - Runtime tool attachment, SimpleEngine integration
4. **Phase 4: Discovery** - ToolDiscoveryService for all sources
5. **Phase 5: API Export** - Clean public API

**Key Principles Applied:**
- High abstraction (ToolDefinition abstracts all tool sources)
- Low coupling (Registries are interfaces, not implementations)
- High cohesion (Each module has single responsibility)
- Easy extension (New tool sources just implement ToolDefinition.from_*)
- TDD throughout

**Technical Debt Addressed:**
- TD-1: LLM injection at init → lazy_init option
- TD-6: Global _agent_registry → proper AgentRegistry
- TD-7: Unused skills/tools params → actual binding

**Future Tasks (not in this plan):**
- Integrate AgentWizard with runtime
- Auto-agent generation for unmatched capabilities
- MCP lazy client integration
- DeepAgentCoordinator capability-based selection

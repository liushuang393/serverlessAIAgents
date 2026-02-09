# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

AgentFlow is a lightweight AI Agent development framework with MCP/A2A/AG-UI/A2UI protocol support. It provides a unified interface for 4 protocols and follows an 8-layer clean architecture.

**Version**: 1.14.1 | **Python**: >=3.10 (3.13+ for AG-UI protocol) | **Coverage minimum**: 80%

## Common Commands

```bash
# Installation
pip install -e ".[dev]"           # Development mode with all deps
cd studio && npm install          # Frontend dependencies

# Code Quality
make format              # Auto-format Python + JS/TS (ruff + prettier)
make lint                # Lint checks (ruff + eslint)
make type-check          # Type checking (mypy + tsc)
make check-all           # All quality checks combined

# Testing
make test                # Run all tests (pytest)
make test-cov            # Tests with coverage report (80% minimum)
pytest tests/unit/test_file.py::test_function  # Run single test
pytest -k "test_name"    # Run tests matching pattern

# Development Servers
make dev-backend         # FastAPI backend (port 8000)
make dev-frontend        # Vite dev server for studio/

# Pre-commit
make install-hooks       # Install pre-commit hooks
make pre-commit          # Run pre-commit on all files
```

## Architecture

### 8-Layer Architecture (Top to Bottom)

1. **Application Layer** (`apps/`): Business applications (6 apps: decision_governance_engine, code_migration_assistant, faq_system, market_trend_monitor, messaging_hub, platform)
2. **UI Layer** (`studio/`, `agentflow/studio/`): Studio UI (React/Vite/Tailwind), A2UI, AG-UI
3. **Flow Layer** (`agentflow/flow/`): Three development methods - @agent, create_flow, AgentCoordinator. Includes parallel flows, rollback flows, conditional flows, sandbox nodes.
4. **Agent Layer** (`agentflow/agents/`, `agentflow/core/agent_block.py`): AgentBlock base class, @agent decorator
5. **Tools Layer** (`agentflow/tools/`, `agentflow/skills/`): @tool methods, MCP tools, Skills Engine
6. **Provider Layer** (`agentflow/providers/`): Unified access - get_llm(), get_db(), get_vectordb()
7. **Protocol Layer** (`agentflow/protocols/`): MCP, A2A, AG-UI, A2UI implementations
8. **Infrastructure Layer**: External services (LLMs, databases, vector DBs, cache)

**Key Principle**: Upper layers depend on lower layers only. Use interfaces for cross-layer communication.

### Core Patterns

**Loose Coupling**: Agents don't know concrete provider implementations. Use:
```python
from agentflow import get_llm, get_db, get_vectordb, get_cache
llm = get_llm()  # Auto-detects from env vars (OpenAI, Anthropic, Google, etc.)
```

**Agent Development** (3 methods, simplest to most complex):
1. `@agent` decorator - Zero config, recommended for simple agents
2. `create_flow` - Declarative chain API for coordination
3. `AgentCoordinator` - Full control for complex patterns

**Engine Patterns** (`agentflow/engines/`):
- SimpleEngine: Single agent Q&A
- GateEngine: Gate + Agent flow
- PipelineEngine: Multi-stage with review
- RAGEngine: Knowledge base augmented
- PEVEngine: Plan-Execute-Verify (from `agentflow/pev/`)

Custom engines extend `BaseEngine` and implement `_initialize()` and `_execute()`.

### Key Entry Points

- `agentflow/__init__.py` - Public API exports
- `agentflow/cli/main.py` - CLI entry point (`agentflow` command)
- `agentflow/studio/server.py` - FastAPI backend
- `agentflow/core/agent_block.py` - Base agent class
- `agentflow/agent_decorator.py` - @agent decorator
- `agentflow/decorators.py` - @tool decorator

### Additional Modules

- **Context Engineering** (`agentflow/context/`): Token budget management, tool relevance selection, RAG retrieval gating, conversation compression, key notes persistence. Unified via `ContextEngineer`.
- **PEV** (`agentflow/pev/`): HierarchicalPlanner, MonitoredExecutor, ResultVerifier
- **Channels** (`agentflow/channels/`): Multi-platform message integration
- **HITL** (`agentflow/hitl/`): Human-in-the-loop approval and checkpointing
- **Code Intelligence** (`agentflow/code_intelligence/`): AST parsing, code transformation, migration
- **World Model** (`agentflow/world_model/`): Causal modeling, constraint solving
- **Task OS** (`agentflow/task/`): Task lifecycle management

## Code Standards

### Python Requirements
- **Python >=3.10** (pyproject.toml), **3.13+ for AG-UI protocol**
- Ruff and mypy target `py313`
- **100% type annotations** required
- **All I/O must be async** - No blocking I/O allowed
- **Line length**: 100 characters (Ruff)
- **Docstring style**: Google format

### Quality Gates
- Ruff format + lint must pass
- MyPy strict mode must pass
- Test coverage >= 80%

### Naming Conventions
- Modules: `snake_case` (agent_flow.py)
- Classes: `PascalCase` (AgentFlow)
- Functions/variables: `snake_case` (create_agent)
- Constants: `UPPER_SNAKE_CASE` (MAX_RETRIES)
- Private: `_single_underscore` (_internal_state)

### Prohibited
- `Any` type without justification
- `type: ignore` without comment
- Hardcoded secrets/config/paths
- `print()` debugging (use structlog)
- Mutable default arguments
- Synchronous I/O operations

## Testing

Tests are in `tests/` with markers: `unit`, `integration`, `e2e`, `slow`. `asyncio_mode = "auto"` is enabled — no need for `@pytest.mark.asyncio` on async tests.

```bash
pytest tests/unit/                    # Unit tests only
pytest -m "not slow"                  # Skip slow tests
pytest --cov=agentflow -v            # With coverage
```

Coverage omits `tests/`, `__init__.py`, and `cli/` directories. Coverage is configured with branch coverage enabled.

## Environment Variables

LLM providers (priority order): `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GOOGLE_API_KEY`
Databases: `DATABASE_URL`, `SUPABASE_URL`, `SUPABASE_KEY`
Vector DBs: `PINECONE_API_KEY`, `QDRANT_URL`
Cache: `REDIS_URL`

See `.env.example` for full list.

## AG-UI Event Format Standards

When emitting events in engines or flows, ALWAYS use the standard event classes from `agentflow/protocols/agui_events.py`:

- `FlowStartEvent`, `FlowCompleteEvent`, `FlowErrorEvent` - Flow control
- `NodeStartEvent`, `NodeCompleteEvent`, `NodeErrorEvent` - Node control
- `ProgressEvent` - Progress (`current`, `total`, `percentage` fields required)
- `LogEvent` - Thinking logs (`level`, `message`, `source` fields required)

Never create custom event dicts like `{"type": "xxx", "data": {...}}`. Always use the event classes and call `.to_dict()`.

```python
from agentflow.protocols.agui_events import ProgressEvent, AGUIEventType
event = ProgressEvent(
    event_type=AGUIEventType.PROGRESS,
    timestamp=time.time(),
    flow_id=self._flow_id,
    current=i+1, total=len(stages),
    percentage=(i+1)/len(stages)*100
)
yield event.to_dict()
```

## Context Engineering

Manage context as an "attention budget" to maximize LLM efficiency:

| Component | Budget | Class |
|-----------|--------|-------|
| System prompt | ≤500 tokens | `TokenBudgetManager` |
| Tool exposure | Top 5-7 | `ToolRelevanceSelector` |
| RAG retrieval | On-demand | `RetrievalGate` |
| Conversation | Compress every 10 turns | `TurnBasedCompressor` |
| Key info | Persistent notes | `KeyNotesStore` |
| Sub-agent results | Final output only | `ResultSummarizer` |

Unified interface: `ContextEngineer` from `agentflow/context/context_engineer.py`. All components importable from `agentflow`.

## Reference Applications

`apps/` contains 6 complete applications demonstrating the framework:
- `decision_governance_engine/` - Multi-agent decision support (PipelineEngine, agent coordination) — primary reference app
- `code_migration_assistant/` - Code migration with AST analysis
- `market_trend_monitor/` - Market analysis with frontend
- `faq_system/` - FAQ Q&A system
- `messaging_hub/` - Multi-channel messaging
- `platform/` - Platform application

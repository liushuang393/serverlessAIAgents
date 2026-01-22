# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

AgentFlow is a lightweight AI Agent development framework with MCP/A2A/AG-UI/A2UI protocol support. It provides a unified interface for 4 protocols and follows an 8-layer clean architecture.

**Key Statistics**: 434 tests passing, 92.46% coverage, Python 3.13+ required

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

1. **Application Layer** (`apps/`): Business applications (decision_governance_engine, etc.)
2. **UI Layer** (`studio/`, `agentflow/studio/`): Studio UI, A2UI, AG-UI
3. **Flow Layer** (`agentflow/flow/`): Three development methods - @agent, create_flow, AgentCoordinator
4. **Agent Layer** (`agentflow/agents/`, `agentflow/core/agent_block.py`): AgentBlock base class, @agent decorator
5. **Tools Layer** (`agentflow/tools/`, `agentflow/skills/`): @tool methods, MCP tools, Skills Engine
6. **Provider Layer** (`agentflow/providers/`): Unified access - get_llm(), get_db(), get_vectordb()
7. **Protocol Layer** (`agentflow/protocols/`): MCP, A2A, AG-UI, A2UI implementations
8. **Infrastructure Layer**: External services (LLMs, databases, vector DBs, cache)

**Key Principle**: Upper layers depend on lower layers only. Use interfaces for cross-layer communication.

### Core Patterns

**Loose Coupling (松耦合)**: Agents don't know concrete provider implementations. Use:
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

### Key Entry Points

- `agentflow/__init__.py` - Public API exports
- `agentflow/cli/main.py` - CLI entry point (`agentflow` command)
- `agentflow/studio/server.py` - FastAPI backend
- `agentflow/core/agent_block.py` - Base agent class
- `agentflow/agent_decorator.py` - @agent decorator
- `agentflow/decorators.py` - @tool decorator

## Code Standards

### Python Requirements
- **Python 3.13+** (required for AG-UI protocol)
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

Tests are in `tests/` with markers: `unit`, `integration`, `e2e`, `slow`

```bash
pytest tests/unit/                    # Unit tests only
pytest -m "not slow"                  # Skip slow tests
pytest --cov=agentflow -v            # With coverage
```

## Environment Variables

LLM providers (priority order): `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GOOGLE_API_KEY`
Databases: `DATABASE_URL`, `SUPABASE_URL`, `SUPABASE_KEY`
Vector DBs: `PINECONE_API_KEY`, `QDRANT_URL`
Cache: `REDIS_URL`

See `.env.example` for full list.

## Reference Application

`apps/decision_governance_engine/` is a complete multi-agent decision support system demonstrating PipelineEngine, agent coordination, and the 8-layer architecture.

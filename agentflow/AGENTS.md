# agentflow/

## Overview
- Primary Python package for the framework; most other top-level directories are consumers (apps/tests/studio).

## Structure
```
agentflow/
├── __init__.py          # public API surface (re-exports)
├── core/                # base agent + errors + shared types
├── engines/             # Engine Pattern entrypoints (run/run_stream)
├── flow/                # Flow DSL used by PipelineEngine
├── protocols/           # MCP/A2A/AG-UI/A2UI/UCP
├── providers/           # get_llm/get_db/... facade
├── services/            # unified backend surface for API/CLI/Studio
├── studio/              # Studio backend (FastAPI)
├── codegen/             # workflow -> code generators
├── deploy/              # deploy targets + config templates
└── skills/              # built-in skills + skill engine
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Public/stable API | `agentflow/__init__.py` | Canonical exports + backward-compat notes.
| Base agent patterns | `agentflow/core/agent_block.py`, `agentflow/core/resilient_agent.py` | `AgentBlock`, `ResilientAgent`.
| Engine patterns | `agentflow/engines/` | `SimpleEngine`, `GateEngine`, `PipelineEngine`, `RAGEngine`.
| Flow DSL | `agentflow/flow/builder.py`, `agentflow/flow/nodes.py` | `create_flow()` and node model.
| Protocol layer | `agentflow/protocols/` | Event models + adapters.
| Studio backend | `agentflow/studio/api.py`, `agentflow/studio/routes/` | FastAPI app factory + routers.
| Codegen | `agentflow/codegen/generator.py` | `CodeGenerator.generate(...)`.
| Deploy | `agentflow/deploy/__init__.py`, `agentflow/deploy/targets/` | Target implementations.

## Conventions
- Treat `agentflow/__init__.py` as the contract: update it intentionally when promoting APIs.

## Anti-Patterns
- Adding new “public” APIs only in submodules without re-exporting or documenting them in `agentflow/__init__.py`.

## Notes
- `agentflow/__init__.py` auto-loads `.env` by searching CWD and parents; running commands from different directories changes env discovery.
- This repo mixes orchestration APIs: Engine Pattern (preferred for apps), Flow DSL (lower-level), plus legacy `AgentFlowEngine` kept for compatibility.

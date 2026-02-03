# agentflow/services/

## Overview
- Service layer used by API/CLI/Studio as the unified backend surface.

## Structure
```
agentflow/services/
├── base.py             # service base + event model
├── agent_service.py    # agent execution
├── workflow_service.py # workflow execution
└── (domain services)   # rag/text2sql/chart/etc
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Service base + events | `agentflow/services/base.py` | Service event model; progress callbacks.
| Agent execution service | `agentflow/services/agent_service.py` | Execute agents, emit progress.
| Workflow execution service | `agentflow/services/workflow_service.py` | Runs workflows and streams events.
| RAG / Text2SQL services | `agentflow/services/rag_service.py`, `agentflow/services/text2sql_service.py` | Used by demo apps (e.g., FAQ).

## Notes
- Prefer services for UI-facing integrations; keep apps thin when possible.

## Anti-Patterns
- Duplicating service logic inside apps when it already exists in `agentflow/services/`.

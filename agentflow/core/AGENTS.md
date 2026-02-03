# agentflow/core/

## Overview
- Base agent abstractions, shared schemas/types, and the error model.

## Structure
```
agentflow/core/
├── agent_block.py        # AgentBlock base
├── resilient_agent.py    # ResilientAgent family
├── exceptions.py         # framework error taxonomy
├── types.py              # shared types
└── engine.py             # legacy engine (compat)
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Implement a new agent | `agentflow/core/agent_block.py` | `AgentBlock` async lifecycle (`initialize/run/cleanup`).
| Reliability patterns | `agentflow/core/resilient_agent.py` | `ResilientAgent` and decision-agent variants.
| Error taxonomy | `agentflow/core/exceptions.py` | Framework exceptions used across layers.
| Shared types | `agentflow/core/types.py` | `AgentMetadata`, `WorkflowConfig`, etc.
| Legacy engine | `agentflow/core/engine.py` | Kept for backward compatibility; prefer `agentflow/engines/`.

## Conventions
- Agents are async-first; prefer explicit input/output schemas (Pydantic) for safety.
- Keep exceptions in `agentflow/core/exceptions.py` and reuse them across layers.

## Anti-Patterns
- Raising generic `Exception` in framework code when a specific `AgentFlowError` subclass fits.
- Putting app-specific business logic into core (keep it reusable).

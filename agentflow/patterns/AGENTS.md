# agentflow/patterns/

## Overview
- Higher-level orchestration patterns (adaptive coordinator, deep agent, pipelines, progress emitters).

## Structure
```
agentflow/patterns/
├── adaptive_coordinator.py
├── deep_agent/
└── progress_emitter.py
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Adaptive coordinator | `agentflow/patterns/adaptive_coordinator.py` | Capability-aware delegation.
| Deep agent pattern | `agentflow/patterns/deep_agent/` | Planning/execution loops.
| Progress emitting | `agentflow/patterns/progress_emitter.py` | AG-UI event shaping.
| Agent pipeline | `agentflow/patterns/agent_pipeline.py` | Pipeline composition and review.

## Anti-Patterns
- Mixing protocol serialization into pattern logic; keep protocol-specific output in `agentflow/protocols/`.

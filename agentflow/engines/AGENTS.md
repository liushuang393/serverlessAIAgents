# agentflow/engines/

## Overview
- Engine Pattern: unified entrypoint for running agents/workflows (`run()` / `run_stream()`).

## Structure
```
agentflow/engines/
├── base.py            # BaseEngine + streaming/event emission
├── simple_engine.py   # single agent
├── gate_engine.py     # gate + main
├── pipeline_engine.py # multi-stage flow builder
└── rag_engine.py      # retrieval + agent
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Engine lifecycle and streaming | `agentflow/engines/base.py` | Emits AG-UI events for streaming runs.
| Single agent execution | `agentflow/engines/simple_engine.py` | Minimal wrapper around a single agent.
| Gate + main agent | `agentflow/engines/gate_engine.py` | Pre-check then main execution.
| Multi-stage orchestration | `agentflow/engines/pipeline_engine.py` | Builds a `Flow` internally (Gate/Review/Parallel).
| Retrieval augmented execution | `agentflow/engines/rag_engine.py` | Injects retrieved context before agent run.

## Anti-Patterns
- Do not emit custom UI event dicts; use `agentflow/protocols/agui_events.py` models and `.to_dict()`.
- Do not bypass `PipelineEngine` flow construction if you rely on streaming; it expects an internal `Flow`.

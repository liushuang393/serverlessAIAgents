# agentflow/flow/

## Overview
- Declarative Flow DSL used to compose multi-step executions (gate/then/parallel/review).

## Structure
```
agentflow/flow/
├── builder.py    # create_flow() builder API
├── nodes.py      # node model + execution
└── progress.py   # progress/event shaping
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Flow builder API | `agentflow/flow/builder.py` | `create_flow()` entrypoint.
| Nodes and execution model | `agentflow/flow/nodes.py` | Node types: Agent/Gate/Parallel/Review.
| Progress/events | `agentflow/flow/progress.py` | Bridges flow progress to AG-UI shapes.

## Notes
- `PipelineEngine` composes a Flow; changes here affect multi-stage engine behavior.

## Anti-Patterns
- Emitting AG-UI-ish dicts directly from Flow code; prefer `agentflow/protocols/agui_events.py` models.

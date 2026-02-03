# agentflow/templates/

## Overview
- Project/app templates (scenarios) used by code generation and project scaffolding.

## Structure
```
agentflow/templates/
└── scenarios/  # template packs (fullstack-app, chatbot, etc.)
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Scenario templates | `agentflow/templates/scenarios/` | fullstack-app, chatbot, data-pipeline, invoice-processor.
| Template README | `agentflow/templates/scenarios/fullstack-app/README.md` | Template usage + structure.

## Notes
- Template Python files are excluded from Ruff/MyPy in `pyproject.toml`.

## Anti-Patterns
- Treating scenario templates as production code: they are excluded from lint/type checks by design.

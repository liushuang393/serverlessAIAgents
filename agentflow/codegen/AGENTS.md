# agentflow/codegen/

## Overview
- Workflow -> code generator. Output types: backend, frontend, fullstack.

## Structure
```
agentflow/codegen/
├── generator.py  # CodeGenerator
└── builders/     # backend/frontend/fullstack
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Main entrypoint | `agentflow/codegen/generator.py` | `CodeGenerator.generate(...)`.
| Builders | `agentflow/codegen/builders/` | Backend/Frontend/Fullstack build implementations.
| Backend builder | `agentflow/codegen/builders/backend.py` | Generates FastAPI backend files.
| Frontend builder | `agentflow/codegen/builders/frontend.py` | Generates React/Vite frontend files.
| Fullstack builder | `agentflow/codegen/builders/fullstack.py` | Composes backend + frontend outputs.

## Notes
- Codegen templates are mostly Python-string generators in builders (not only static files).

## Anti-Patterns
- Adding a new output type without updating `CodeGenerator` routing and its returned commands/entrypoint.

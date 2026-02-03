# apps/code_migration_assistant/

## Overview
- COBOL -> Java migration assistant demo; emphasizes MCP-tool-style decomposition.

## Structure
```
apps/code_migration_assistant/
├── orchestrator.py
├── mcp_client.py
├── mcp_tools/
└── adapters/
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| API server | `apps/code_migration_assistant/api.py` | FastAPI entry.
| CLI | `apps/code_migration_assistant/cli.py` | CLI runner.
| Orchestrator | `apps/code_migration_assistant/orchestrator.py` | Coordinates tool/agent steps.
| MCP tools | `apps/code_migration_assistant/mcp_tools/` | Tool implementations.
| Adapters/parsers | `apps/code_migration_assistant/adapters/`, `apps/code_migration_assistant/parsers/` | Language-specific pieces.
| Tests | `apps/code_migration_assistant/tests/` | App tests.

## Notes
- This app is documentation-heavy; start with `apps/code_migration_assistant/README.md`.

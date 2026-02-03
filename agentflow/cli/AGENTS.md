# agentflow/cli/

## Overview
- Click-based CLI: `agentflow` command (defined in `pyproject.toml`).

## Structure
```
agentflow/cli/
├── main.py        # click group + wiring
└── commands/      # subcommands
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| CLI entrypoint | `agentflow/cli/main.py` | Defines `cli` group and subcommands.
| Subcommands | `agentflow/cli/commands/` | init/create/run/template/skills/etc.

## Notes
- `agentflow run` supports both agent.yaml-based agents and `@agent` decorator agents.

## Anti-Patterns
- Adding new commands without wiring them into the top-level `cli` group.

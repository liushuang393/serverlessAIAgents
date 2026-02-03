# agentflow/skills/

## Overview
- Skills system: built-in skills + runtime skill resolution ("learn/evolve" mechanics).

## Structure
```
agentflow/skills/
├── __init__.py          # entrypoints
├── builtin/             # built-in skills (often SKILL.md)
└── (skill engine files) # resolution/execution
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Skill system entrypoints | `agentflow/skills/__init__.py` | Skill registry and high-level API.
| Built-in skills catalog | `agentflow/skills/builtin/` | Each skill typically has a `SKILL.md`.
| Market trend analysis skill | `agentflow/skills/builtin/market-trend-analysis/` | Includes deterministic scripts + references.

## Notes
- Built-in skills documentation lives next to code as `SKILL.md` (good onboarding surface).

## Anti-Patterns
- Treating built-in skills as ad-hoc scripts; prefer a consistent SKILL.md + implementation layout.

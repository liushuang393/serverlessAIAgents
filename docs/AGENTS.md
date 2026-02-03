# docs/

## Overview
- Developer-facing documentation and design notes.

## Structure
```
docs/
├── index.md
├── architecture.md
├── protocols.md
├── engines.md
└── design/  # deeper proposals
```

## Where To Look
| Need | Location | Notes |
|------|----------|-------|
| Entry index | `docs/index.md` | Top-level documentation hub.
| Architecture overview | `docs/architecture.md` | 8-layer architecture and system map.
| Engines | `docs/engines.md` | Engine Pattern guide.
| Protocols | `docs/protocols.md` | Protocol stack guide (MCP/A2A/AG-UI/A2UI/WebSocket).
| CLI guide | `docs/guide-cli.md` | CLI usage patterns.
| Studio guide | `docs/guide-studio-ui.md` | Studio usage and dev setup.
| Context engineering | `docs/context-engineering.md` | Context budgets + RLM.
| Design notes | `docs/design/` | Deeper architecture proposals and design docs.

## Notes
- Prefer linking to these docs from AGENTS.md rather than duplicating rules.

## Anti-Patterns
- Duplicating large doc sections inside AGENTS.md; keep AGENTS.md as pointers and repo-map.

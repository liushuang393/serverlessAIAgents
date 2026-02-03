# code-rules/

## Overview
- Coding rules system used to keep implementations consistent and reviewable.

## Structure
```
code-rules/
├── README.md
├── CLAUDE.md
├── global/
├── languages/
└── project/
```

## Where To Look
| Need | Location | Notes |
|------|----------|-------|
| Entry hub | `code-rules/README.md` | Explains the rules hierarchy and how to apply.
| Index file | `code-rules/CLAUDE.md` | Links global/language/project rule docs.
| Naming conventions | `code-rules/global/naming-guidelines.md` | Canonical naming rules.
| Repo structure rules | `code-rules/project/repo-structure.md` | How dirs map to architecture.

## Notes
- This repo also has a separate `CLAUDE.md` at root with project-specific conventions.

## Anti-Patterns
- Ignoring the rules index and reinventing standards per feature.

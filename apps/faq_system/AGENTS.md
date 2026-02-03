# apps/faq_system/

## Overview
- FAQ demo application; designed as a thin app layer (most logic in `agentflow/`).

## Structure
```
apps/faq_system/
├── main.py
├── main_enhanced.py
└── (additional versions)
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| App API entrypoints | `apps/faq_system/main.py`, `apps/faq_system/main_enhanced.py` | Multiple versions/ports.
| Framework agent | `agentflow/agents/faq_agent.py` | Where the real agent logic lives.
| Framework services | `agentflow/services/` | RAG/Text2SQL/Chart/etc.

## Anti-Patterns
- Do not add custom agents under `apps/faq_system/backend/agents/` (keep app thin; follow README guidance).

## Notes
- Framework-owned FAQ logic: `agentflow/agents/faq_agent.py` and `agentflow/services/`.

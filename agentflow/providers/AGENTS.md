# agentflow/providers/

## Overview
- "Loose-coupling" provider facade for LLM/DB/VectorDB/Embeddings.
- The public API is re-exported from `agentflow/__init__.py` (e.g., `get_llm()`).

## Structure
```
agentflow/providers/
├── __init__.py          # get_* and reset_* entrypoints
├── tool_provider.py     # @tool decorator
└── (provider impl files) # LLM/DB/VectorDB/Embeddings
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Provider entrypoints | `agentflow/providers/__init__.py` | `get_*` and `reset_*` functions.
| Tool decorator | `agentflow/providers/tool_provider.py` | `@tool` integration.

## Notes
- Provider selection is environment-driven (keys like `OPENAI_API_KEY`, `DATABASE_URL`, etc.).

## Anti-Patterns
- Letting app code instantiate concrete providers directly; prefer `get_llm()`/`get_db()` etc.

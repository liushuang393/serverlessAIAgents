# agentflow/memory/

## Overview
- Memory and knowledge-store integrations (in-memory + optional memvid-backed store).

## Structure
```
agentflow/memory/
├── knowledge/        # knowledge store implementations
├── embeddings/       # embedding adapters
└── vector_store.py   # vector store facade
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Knowledge store API | `agentflow/memory/knowledge/` | `get_knowledge_store`, memvid integration.
| Vector store/search | `agentflow/memory/vector_store.py`, `agentflow/memory/vector_search.py` | Retrieval primitives.

## Notes
- memvid support is optional (`pyproject.toml` extra `memvid`).

## Anti-Patterns
- Assuming memvid is installed; use capability checks (`is_memvid_available`) and fallbacks.

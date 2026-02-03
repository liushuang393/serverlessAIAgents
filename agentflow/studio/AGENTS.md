# agentflow/studio/

## Overview
- Studio backend (FastAPI): agent/workflow management, preview, publish, knowledge, marketplace.

## Structure
```
agentflow/studio/
├── server.py        # Uvicorn runner
├── api.py           # FastAPI app factory
├── models.py        # shared pydantic models
└── routes/          # feature routers
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Server entrypoint | `agentflow/studio/server.py` | CLI entry; runs Uvicorn.
| App factory | `agentflow/studio/api.py` | `create_app()` wires routers + CORS.
| Routes | `agentflow/studio/routes/` | Feature routers (agents/workflows/preview/publish/etc.).
| Shared request/response models | `agentflow/studio/models.py` | Pydantic models for API.

## Integration Notes
- Frontend (Vite/React) lives in `studio/` and proxies `/api` and `/ws` to backend port 8000.

## Commands
```bash
make dev-backend
cd studio && npm run dev
```

## Anti-Patterns
- Adding routes directly in `api.py` instead of a dedicated router module under `agentflow/studio/routes/`.

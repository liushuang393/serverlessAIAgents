# apps/decision_governance_engine/

## Overview
- Enterprise decision support demo built on `PipelineEngine`.

## Structure
```
apps/decision_governance_engine/
├── engine.py
├── api.py
├── main.py
├── agents/
└── frontend/
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Engine entrypoint | `apps/decision_governance_engine/engine.py` | `DecisionEngine` (PipelineEngine subclass).
| API server | `apps/decision_governance_engine/api.py` | FastAPI app; routes under `routers/`.
| CLI entry | `apps/decision_governance_engine/main.py` | CLI runner.
| Agents | `apps/decision_governance_engine/agents/` | Dao/Fa/Shu/Qi + Gate/Review.
| Frontend | `apps/decision_governance_engine/frontend/` | React/TS UI.

## Run (Typical)
```bash
# backend (example ports vary; see README)
python -m uvicorn apps.decision_governance_engine.api:app --host 0.0.0.0 --port 8001 --reload

# frontend
cd apps/decision_governance_engine/frontend && npm install && npm run dev
```

## Notes
- Ports vary across README/examples; treat `api.py` as authoritative.

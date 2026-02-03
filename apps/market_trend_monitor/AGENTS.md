# apps/market_trend_monitor/

## Overview
- Market trend monitoring demo: collector/analyzer/reporter/notifier agents.

## Structure
```
apps/market_trend_monitor/
└── backend/
    ├── api/
    ├── agents/
    └── workflow.py
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| API server | `apps/market_trend_monitor/backend/api/main.py` | FastAPI entry.
| Agents | `apps/market_trend_monitor/backend/agents/` | Collector/Analyzer/Reporter/Notifier.
| Workflow wiring | `apps/market_trend_monitor/backend/workflow.py` | Orchestration glue.
| App tests | `apps/market_trend_monitor/tests/` | App-level tests.

## Run (Typical)
```bash
python -m apps.market_trend_monitor.backend.api.main
```

## Notes
- README describes a frontend as planned/"not implemented"; treat this app as backend-first.

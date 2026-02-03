# agentflow/sdk/

## Overview
- SDK surface for integrating with Studio/API from other contexts; includes a small frontend SDK.

## Structure
```
agentflow/sdk/
├── api/       # python SDK pieces
└── frontend/  # frontend SDK docs
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Frontend SDK docs | `agentflow/sdk/frontend/README.md` | How frontend pieces integrate.
| SDK API package | `agentflow/sdk/api/` | Python SDK entrypoints.

## Notes
- Treat this as the integration layer; avoid coupling app code directly to internal Studio APIs.

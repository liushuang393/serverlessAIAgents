# studio/

## Overview
- React/Vite frontend for AgentFlow Studio.

## Structure
```
studio/
├── vite.config.ts
├── package.json
└── src/  # React app
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Dev scripts and dependencies | `studio/package.json` | `dev`, `build`, `lint`, `type-check`.
| Vite dev server proxy | `studio/vite.config.ts` | Proxies `/api` + `/ws` to backend port 8000.
| App entry | `studio/src/main.tsx` | React entrypoint.
| Main UI components | `studio/src/components/` | Canvas/Sidebar/PropertiesPanel/etc.
| State management | `studio/src/stores/workflowStore.ts` | Zustand store.

## Notes
- Backend API is `agentflow/studio/` (FastAPI). Frontend calls `/api/*` and `/ws/*` via proxy in dev.

## Commands
```bash
cd studio && npm install
cd studio && npm run dev
cd studio && npm run build
```

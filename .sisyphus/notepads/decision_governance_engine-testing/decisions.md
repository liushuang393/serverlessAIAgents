## 2026-02-03 Decisions

- E2E tests will target the Decision Governance Engine SPA at http://localhost:5174.
- To make tests deterministic and independent of LLM/API keys, we will mock:
  - fetch() for /api/auth/*, /api/config/*, /api/knowledge/*, /api/decision/history*, /api/report/*
  - EventSource for /api/decision/stream
- We will still optionally keep a thin “smoke” test that only checks the login page renders with no console errors.

## 2026-02-03 Decisions

- E2E tests will target the Decision Governance Engine SPA at http://localhost:5174.
- To make tests deterministic and independent of LLM/API keys, we will mock:
  - fetch() for /api/auth/_, /api/config/_, /api/knowledge/_, /api/decision/history_, /api/report/\*
  - EventSource for /api/decision/stream
- We will still optionally keep a thin “smoke” test that only checks the login page renders with no console errors.

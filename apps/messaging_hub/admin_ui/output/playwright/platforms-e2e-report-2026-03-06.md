# Messaging Hub Platforms E2E Test Report

- Date: 2026-03-06
- Scope: `http://localhost:3001/platforms`
- Test file: `apps/messaging_hub/admin_ui/tests/e2e/platforms-auth.spec.ts`
- Command:
  - `MH_E2E_PORT=3001 MH_E2E_BASE_URL=http://localhost:3001 npm run test:e2e -- tests/e2e/platforms-auth.spec.ts`
- Result: **PASS** (1/1)

## Coverage Definition (Business Network)

Business network events on `/platforms` are defined as below:

1. `GET /api/health`
2. `GET /api/platforms`
3. `POST /api/platforms/{platform}/connect`
4. `POST /api/platforms/{platform}/credentials`

Coverage rule in test:

- Every event above must be called at least once.
- Every response must be 2xx.
- `requestfailed` for API requests must be zero.
- 4xx/5xx API responses must be zero.

Coverage result:

- Covered endpoints: **4/4 (100%)**
- API failed requests: **0**
- API non-2xx responses: **0**

## UI Flow Assertions

Validated actions/results:

1. Platforms page heading and managed platform message render correctly.
2. Credential-missing platform (`Slack`) blocks `接続テスト` locally and expands credential form.
3. No network connect request is sent for credential-missing platform.
4. Connected/Unconnected ordering and card content render correctly.
5. Auth/docs links for Telegram card are present and correct.
6. `接続テスト` action succeeds and success notice appears for configured platform.
7. Empty credential save is blocked with expected validation message.
8. Reveal toggle changes credential field type (`password` ↔ `text`).
9. Credential save succeeds, payload carries token, input is cleared after save.
10. Manual `更新` triggers refetch without network errors.

## Test Run Output (Summary)

- `1 passed (2.4s)`

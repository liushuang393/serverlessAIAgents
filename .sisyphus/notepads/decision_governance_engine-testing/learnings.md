# Decision Governance Engine - E2E Testing Infrastructure Findings

## Current E2E Testing Status

### ❌ No Local E2E Infrastructure Found
- **Playwright Config**: No `playwright.config.*` files exist in the project
- **E2E Test Directory**: No `tests/e2e/` directory exists  
- **Root package.json**: Only has `@playwright/test` dependency but no configuration or scripts

### ✅ Reusable CI/CD E2E Workflow Available
**File**: `.github/workflows/reusable-e2e-test.yml`

**Features**:
- Configurable working directory, test directory, and base URL
- Supports multiple browsers (chromium default)
- Automatic browser installation with dependencies
- Test result and screenshot artifact uploads
- Environment variable support for test credentials

**Usage Pattern**:
```yaml
uses: ./.github/workflows/reusable-e2e-test.yml
with:
  working-directory: "apps/decision_governance_engine"
  test-directory: "tests/e2e"  
  base-url: "http://localhost:8001"
  start-command: "npm run dev"
  browser: "chromium"
```

### ✅ Existing Unit Tests (Frontend)
**Location**: `apps/decision_governance_engine/frontend/src/__tests__/`

**Files**:
- `api/client.test.ts` - API client logic testing (Vitest)
- `store/useDecisionStore.test.ts` - State management testing (Vitest)

**Testing Framework**: Vitest with React Testing Library

### ✅ CI/CD Integration Ready
**Decision Governance Engine CI**: `.github/workflows/ci.yml`
- Uses reusable workflows for Python testing
- No E2E step currently included (can be added)

## Missing E2E Infrastructure for Decision Governance Engine

### 1. Playwright Configuration
**Needed**: `apps/decision_governance_engine/frontend/playwright.config.ts`
```typescript
import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: './tests/e2e',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  reporter: 'html',
  use: {
    baseURL: 'http://localhost:8001',
    trace: 'on-first-retry',
  },
  projects: [
    { name: 'chromium', use: { ...devices['Desktop Chrome'] } },
    { name: 'firefox', use: { ...devices['Desktop Firefox'] } },
    { name: 'webkit', use: { ...devices['Desktop Safari'] } },
  ],
  webServer: {
    command: 'npm run dev',
    port: 5173,
    reuseExistingServer: !process.env.CI,
  },
});
```

### 2. Package.json Scripts
**Needed in**: `apps/decision_governance_engine/frontend/package.json`
```json
{
  "scripts": {
    "test:e2e": "playwright test",
    "test:e2e:ui": "playwright test --ui",
    "test:e2e:report": "playwright show-report",
    "test:e2e:install": "playwright install --with-deps"
  }
}
```

### 3. Test Directory Structure
**Needed**: `apps/decision_governance_engine/frontend/tests/e2e/`
```
tests/e2e/
├── fixtures/
│   ├── decision-requests.json
│   └── mock-responses.json
├── basic-decision-flow.spec.ts
├── decision-submission.spec.ts
├── report-generation.spec.ts
└── error-handling.spec.ts
```

## Recommended Commands for E2E Testing

### Development Commands
```bash
# Install Playwright browsers
cd apps/decision_governance_engine/frontend
npm run test:e2e:install

# Run E2E tests
npm run test:e2e

# Run with UI mode (interactive)
npm run test:e2e:ui

# View HTML report
npm run test:e2e:report
```

### CI/CD Integration
**Add to**: `.github/workflows/ci.yml`
```yaml
e2e:
  name: E2E Tests
  needs: ci
  uses: ./.github/workflows/reusable-e2e-test.yml
  with:
    working-directory: apps/decision_governance_engine/frontend
    base-url: "http://localhost:8001"
    start-command: "npm run dev"
```

### Test Execution Commands
```bash
# Run specific browser
npx playwright test --project=chromium

# Run specific test file
npx playwright test tests/e2e/basic-decision-flow.spec.ts

# Run with trace generation
npx playwright test --trace on

# Run in headed mode (debug)
npx playwright test --headed
```

## Business Coverage Test Scenarios (To Implement)

### 1. Complete Decision Flow E2E
- User lands on input page
- Fills decision question with valid constraints
- Submits form and navigates to processing
- Monitors real-time progress via SSE/WebSocket
- Receives completed report
- Downloads PDF report
- Views individual agent outputs

### 2. Input Validation Tests
- Question length validation (10-2000 chars)
- Constraint field validation
- Error message display and recovery
- Form submission edge cases

### 3. Error Handling & Recovery
- API failure scenarios (500, 429)
- Connection timeout handling
- Progress update failures
- Report generation failures

### 4. UI Stability Tests
- Responsive design across devices
- Component state persistence
- Real-time progress updates
- Browser navigation interruptions

### 5. Integration Scenarios
- Backend API integration
- WebSocket connection stability
- SSE event handling
- Cross-browser compatibility

## Next Steps

1. **Set up local E2E infrastructure** (playwright config + test directory)
2. **Create basic decision flow test** as foundation
3. **Add E2E step to CI/CD pipeline** using reusable workflow
4. **Expand test coverage** for business scenarios
5. **Integrate with existing unit test suite** (Vitest + Playwright)

## 2026-02-03 追加メモ
- ログインフォームのラベルは input と紐づいていないため、E2Eでは `getByPlaceholder("ユーザー名を入力")` / `getByPlaceholder("パスワードを入力")` が安定。
- 認証モックは `/api/auth/me` と `/api/auth/login` を Playwright route で返し、ローカルストレージは `addInitScript` で事前クリアが有効。
- Playwright の `webServer` で `apps/decision_governance_engine/frontend` の Vite を起動し、`baseURL` と `url` を `http://127.0.0.1:5174` に統一すると E2E が自己起動で安定。
- Windows 側の UI に接続する場合は `E2E_BASE_URL=http://<windows-host-ip>:5174` を設定し、`npx playwright install chromium` 後に `npx playwright test` を実行する。
- `installMockEventSource` は `page.addInitScript` で `EventSource` を差し替え、`window.__mockEventSource` 経由で URL 記録と message/event/error/close を発火できるようにしている。
- ProcessingPage のE2Eは `connection.established` → `flow.start` → `progress` → `node.complete` → `flow.complete`（`result` に最小レポート）でボタン表示まで進められた。

import { defineConfig, devices } from "@playwright/test";

const baseURL = process.env.E2E_BASE_URL ?? "http://127.0.0.1:5174";
const shouldStartServer = process.env.E2E_START_SERVER === "1";

export default defineConfig({
  testDir: "apps/decision_governance_engine/frontend/tests/e2e",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 1 : 0,
  workers: process.env.CI ? 1 : undefined,
  reporter: "list",
  use: {
    baseURL,
    trace: "on-first-retry",
    viewport: { width: 1280, height: 720 },
  },
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
  ],
  webServer: shouldStartServer
    ? {
        cwd: "apps/decision_governance_engine/frontend",
        command: "npm run dev -- --host 127.0.0.1 --port 5174",
        url: "http://127.0.0.1:5174",
        reuseExistingServer: !process.env.CI,
        timeout: 120_000,
      }
    : undefined,
});

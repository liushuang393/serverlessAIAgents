import { defineConfig, devices } from "playwright/test";
import { fileURLToPath } from "node:url";
import path from "node:path";

const port = Number(process.env.MH_E2E_PORT ?? 3181);
const baseURL = process.env.MH_E2E_BASE_URL ?? `http://127.0.0.1:${port}`;
const configDir = path.dirname(fileURLToPath(import.meta.url));

export default defineConfig({
  testDir: "./tests/e2e",
  fullyParallel: true,
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
  webServer: {
    cwd: configDir,
    command: `npm run dev -- --host 127.0.0.1 --port ${port}`,
    url: baseURL,
    reuseExistingServer: !process.env.CI,
    timeout: 120000,
  },
});

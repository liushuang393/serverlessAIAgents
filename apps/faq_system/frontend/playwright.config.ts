import { defineConfig } from "@playwright/test";
import path from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const repoRoot = path.resolve(__dirname, "../../..");
const apiPort = 8005;
const frontendPort = 3004;
const playwrightDbPath = path.resolve(
  repoRoot,
  "apps/faq_system/data/faq_system_playwright.db",
);
const playwrightDbUrl = `sqlite+aiosqlite:///${playwrightDbPath}`;

export default defineConfig({
  testDir: "../tests",
  testMatch: "**/*.spec.ts",
  use: {
    baseURL: `http://localhost:${frontendPort}`,
    screenshot: "only-on-failure",
    video: "retain-on-failure",
  },
  webServer: [
    {
      command: [
        "conda run -n agentflow env",
        `FAQ_PORT=${apiPort}`,
        `FAQ_DATABASE_URL=${playwrightDbUrl}`,
        "FAQ_DB_AUTO_CREATE=true",
        "FAQ_AUTH_PROVIDER=local_db",
        "FAQ_AUTH_DEV_MODE=true",
        "PYTHONUNBUFFERED=1",
        "python -m apps.faq_system.main",
      ].join(" "),
      port: apiPort,
      reuseExistingServer: !process.env.CI,
      cwd: repoRoot,
      timeout: 120000,
    },
    {
      command: `npm run dev -- --port ${frontendPort}`,
      port: frontendPort,
      reuseExistingServer: !process.env.CI,
      cwd: "apps/faq_system/frontend",
      timeout: 120000,
    },
  ],
});

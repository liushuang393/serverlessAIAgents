import { defineConfig } from "@playwright/test";
import { GEO_E2E_BASE_URL } from "./playwright.runtime";

export default defineConfig({
  testDir: "./src",
  testMatch: "**/*.e2e.spec.ts",
  globalSetup: "./playwright.global-setup.ts",
  use: {
    baseURL: GEO_E2E_BASE_URL,
    screenshot: "only-on-failure",
    video: "retain-on-failure",
  },
  reporter: [
    ["html", { outputFolder: "playwright-report" }],
    ["json", { outputFile: "playwright-report/results.json" }],
    ["list"],
  ],
});

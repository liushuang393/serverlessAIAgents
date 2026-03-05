import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: '../tests',
  testMatch: '**/*.spec.ts',
  use: {
    baseURL: 'http://localhost:3004',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },
  webServer: {
    command: 'npm run dev -- --port 3004',
    port: 3004,
    reuseExistingServer: !process.env.CI,
    cwd: 'apps/faq_system/frontend',
  },
});

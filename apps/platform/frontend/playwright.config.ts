import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: '../../../tests/apps/platform',
  testMatch: '**/*.spec.ts',
  use: {
    baseURL: 'http://localhost:3000',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },
  webServer: {
    command: 'npm run dev',
    port: 3000,
    reuseExistingServer: !process.env.CI,
    cwd: 'apps/platform/frontend',
  },
});

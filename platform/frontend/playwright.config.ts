import { defineConfig } from '@playwright/test';
import fs from 'node:fs';
import path from 'node:path';

const appConfig = JSON.parse(
  fs.readFileSync(path.resolve(__dirname, '../app_config.json'), 'utf-8'),
);
const frontendPort = appConfig.ports?.frontend ?? 3200;
const frontendBaseUrl = `http://localhost:${frontendPort}`;

export default defineConfig({
  testDir: '../../../tests/apps/platform',
  testMatch: '**/*.spec.ts',
  use: {
    baseURL: frontendBaseUrl,
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },
  webServer: {
    command: 'npm run dev',
    port: frontendPort,
    reuseExistingServer: !process.env.CI,
    cwd: 'apps/platform/frontend',
  },
});

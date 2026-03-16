import fs from 'node:fs';
import path from 'node:path';

import react from '@vitejs/plugin-react';
import { defineConfig } from 'vitest/config';

const appConfig = JSON.parse(
  fs.readFileSync(path.resolve(__dirname, '../app_config.json'), 'utf-8'),
);
const backendPort = appConfig.ports?.api ?? 8100;
const frontendPort = appConfig.ports?.frontend ?? 3100;
const frontendHost = appConfig.runtime?.hosts?.frontend ?? '0.0.0.0';

export default defineConfig({
  plugins: [react()],
  server: {
    host: frontendHost,
    port: frontendPort,
    proxy: {
      '/api': `http://localhost:${backendPort}`,
      '/geo': `http://localhost:${backendPort}`,
    },
  },
  test: {
    environment: 'jsdom',
    setupFiles: './src/test/setup.ts',
    include: ['src/**/*.{test,spec}.{ts,tsx}'],
    exclude: ['src/**/*.e2e.spec.ts', 'node_modules/**'],
  },
});

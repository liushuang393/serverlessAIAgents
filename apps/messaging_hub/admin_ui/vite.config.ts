import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import fs from 'node:fs';
import path from 'node:path';

const appConfigPath = path.resolve(__dirname, '../app_config.json');
let appConfig: { ports?: { api?: number; frontend?: number } } = {};
if (fs.existsSync(appConfigPath)) {
  try {
    appConfig = JSON.parse(fs.readFileSync(appConfigPath, 'utf-8')) as { ports?: { api?: number; frontend?: number } };
  } catch {
    appConfig = {};
  }
}

const apiPort = appConfig.ports?.api ?? 8000;
const frontendPort = appConfig.ports?.frontend ?? 3000;
const targetOrigin = `http://localhost:${apiPort}`;

export default defineConfig({
  plugins: [react()],
  server: {
    port: frontendPort,
    proxy: {
      '/api': {
        target: targetOrigin,
        changeOrigin: true,
      },
      '/ws': {
        target: targetOrigin.replace('http://', 'ws://'),
        ws: true,
      },
    },
  },
  build: {
    outDir: 'dist',
    sourcemap: true,
  },
});

import fs from 'node:fs';
import react from '@vitejs/plugin-react';
import { defineConfig, loadEnv } from 'vite';
import path from 'path';

type AppConfig = {
  api_host?: string;
  api_port?: number;
  frontend_port?: number;
  ports?: {
    api?: number;
    frontend?: number;
  };
  runtime?: {
    urls?: {
      backend?: string;
      frontend?: string;
    };
  };
};

function parsePort(value: string | number | undefined, fallback: number): number {
  if (typeof value === 'number' && Number.isInteger(value) && value > 0) {
    return value;
  }

  if (typeof value === 'string' && value.trim().length > 0) {
    const parsed = Number.parseInt(value, 10);
    if (Number.isInteger(parsed) && parsed > 0) {
      return parsed;
    }
  }

  return fallback;
}

function normalizeBaseURL(baseURL: string): string {
  return baseURL.endsWith('/') ? baseURL.slice(0, -1) : baseURL;
}

function readAppConfig(): AppConfig {
  const configPath = path.resolve(__dirname, '../app_config.json');

  try {
    const raw = fs.readFileSync(configPath, 'utf-8');
    const parsed = JSON.parse(raw) as AppConfig;
    return parsed;
  } catch {
    return {};
  }
}

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => {
  const appConfig = readAppConfig();
  const env = loadEnv(mode, path.resolve(__dirname, '..'), '');

  const runtimeBackend = appConfig.runtime?.urls?.backend;
  const backendFromRuntime = typeof runtimeBackend === 'string' ? normalizeBaseURL(runtimeBackend) : '';
  const runtimeBackendHost = (() => {
    if (!backendFromRuntime) return '';
    try {
      return new URL(backendFromRuntime).hostname;
    } catch {
      return '';
    }
  })();

  const apiHost = env.MARKET_TREND_MONITOR_API_HOST || appConfig.api_host || runtimeBackendHost || 'localhost';
  const apiPort = parsePort(
    env.MARKET_TREND_MONITOR_API_PORT,
    parsePort(appConfig.ports?.api, parsePort(appConfig.api_port, 8002))
  );
  const frontendPort = parsePort(
    env.MARKET_TREND_MONITOR_FRONTEND_PORT,
    parsePort(appConfig.ports?.frontend, parsePort(appConfig.frontend_port, 3002))
  );

  const apiProxyHost = apiHost === '0.0.0.0' ? 'localhost' : apiHost;
  const backendOrigin = normalizeBaseURL(
    env.MARKET_TREND_MONITOR_API_ORIGIN || backendFromRuntime || `http://${apiProxyHost}:${apiPort}`
  );
  const apiBaseURL = normalizeBaseURL(
    env.VITE_API_BASE_URL || `${backendOrigin}/api`
  );

  return {
    envDir: path.resolve(__dirname, '..'),
    define: {
      __MARKET_TREND_MONITOR_API_BASE_URL__: JSON.stringify(apiBaseURL),
    },
    plugins: [react()],
    resolve: {
      alias: {
        '@': path.resolve(__dirname, './src'),
      },
    },
    server: {
      port: frontendPort,
      proxy: {
        '/api': {
          target: backendOrigin,
          changeOrigin: true,
        },
      },
    },
    test: {
      globals: true,
      environment: 'jsdom',
      setupFiles: './src/test/setup.ts',
      coverage: {
        provider: 'v8',
        reporter: ['text', 'json', 'html'],
        exclude: [
          'node_modules/',
          'src/test/',
          '**/*.d.ts',
          '**/*.config.*',
          '**/mockData',
          'dist/',
        ],
      },
    },
  };
});

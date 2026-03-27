/**
 * Vite 設定.
 *
 * 注意事項:
 * - server.proxy は開発環境（vite dev）でのみ有効
 * - 本番環境（vite build）では静的ファイルが生成され、
 *   Nginx リバースプロキシ経由で API に接続する
 * - 本番デプロイ: docker-compose up --build
 */

/// <reference types="vitest" />
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import fs from "node:fs";
import path from "path";

type AppConfig = {
  ports?: {
    api?: number;
    frontend?: number;
  };
  runtime?: {
    hosts?: {
      backend?: string | null;
      frontend?: string | null;
    };
  };
};

function parsePort(value: string | number | undefined): number | undefined {
  if (typeof value === "number" && Number.isInteger(value) && value > 0) {
    return value;
  }
  if (typeof value === "string" && value.trim().length > 0) {
    const parsed = Number.parseInt(value, 10);
    if (Number.isInteger(parsed) && parsed > 0) {
      return parsed;
    }
  }
  return undefined;
}

function requirePort(
  name: string,
  envValue: string | undefined,
  manifestValue: number | undefined,
): number {
  const resolved = parsePort(envValue ?? manifestValue);
  if (resolved !== undefined) {
    return resolved;
  }
  throw new Error(`${name} が app_config.json または env にありません。`);
}

function requireHost(
  name: string,
  envValue: string | undefined,
  manifestValue: string | null | undefined,
): string {
  const resolved = envValue?.trim() || manifestValue?.trim();
  if (resolved) {
    return resolved;
  }
  throw new Error(`${name} が app_config.json または env にありません。`);
}

function normalizeProxyHost(host: string): string {
  if (host === "0.0.0.0" || host === "::") {
    return "localhost";
  }
  return host;
}

const appConfigPath = path.resolve(__dirname, "../app_config.json");
let appConfig: AppConfig = {};
if (fs.existsSync(appConfigPath)) {
  try {
    appConfig = JSON.parse(
      fs.readFileSync(appConfigPath, "utf-8"),
    ) as AppConfig;
  } catch {
    appConfig = {};
  }
}
const apiPort = requirePort(
  "DGE_PORT",
  process.env.DGE_PORT,
  appConfig.ports?.api,
);
const frontendPort = requirePort(
  "FRONTEND_PORT",
  process.env.FRONTEND_PORT,
  appConfig.ports?.frontend,
);
const frontendHost = requireHost(
  "FRONTEND_HOST",
  process.env.FRONTEND_HOST,
  appConfig.runtime?.hosts?.frontend,
);
const backendHost = normalizeProxyHost(
  requireHost(
    "DGE_HOST",
    process.env.DGE_HOST,
    appConfig.runtime?.hosts?.backend,
  ),
);

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
      "@bizcore/i18n": path.resolve(__dirname, "./src/i18n/base"),
    },
  },
  server: {
    port: frontendPort,
    host: frontendHost,
    proxy: {
      "/api": {
        target: process.env.PROXY_TARGET || `http://${backendHost}:${apiPort}`,
        changeOrigin: true,
        ws: false,
        timeout: 0,
        proxyTimeout: 0,
        configure: (proxy) => {
          proxy.on("proxyReq", (proxyReq, req) => {
            if (req.url?.includes("/stream")) {
              proxyReq.setHeader("Accept", "text/event-stream");
              proxyReq.setHeader("Cache-Control", "no-cache");
              proxyReq.setHeader("Connection", "keep-alive");
            }
          });
          proxy.on("proxyRes", (proxyRes, req) => {
            if (req.url?.includes("/stream")) {
              proxyRes.headers["Cache-Control"] = "no-cache";
              proxyRes.headers["X-Accel-Buffering"] = "no";
            }
          });
          proxy.on("error", (err, req, res) => {
            void req;
            void res;
            console.error("[Vite Proxy Error]", err.message);
          });
        },
      },
    },
  },
  test: {
    globals: true,
    environment: "jsdom",
    setupFiles: ["./src/__tests__/setup.ts"],
    include: ["src/**/*.{test,spec}.{ts,tsx}"],
    coverage: {
      reporter: ["text", "json", "html"],
      include: ["src/**/*.{ts,tsx}"],
      exclude: ["src/**/*.d.ts", "src/__tests__/**"],
    },
  },
});

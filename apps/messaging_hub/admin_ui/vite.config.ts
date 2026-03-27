import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import fs from "node:fs";
import path from "node:path";

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

function parsePort(
  value: string | number | undefined,
  fallback: number,
): number {
  if (typeof value === "number" && Number.isInteger(value) && value > 0) {
    return value;
  }
  if (typeof value === "string" && value.trim().length > 0) {
    const parsed = Number.parseInt(value, 10);
    if (Number.isInteger(parsed) && parsed > 0) {
      return parsed;
    }
  }
  return fallback;
}

function normalizeProxyHost(host: string | null | undefined): string {
  if (!host || host === "0.0.0.0" || host === "::") {
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

const apiPort = parsePort(
  process.env.MSGHUB_PORT,
  appConfig.ports?.api ?? 8004,
);
const frontendPort = parsePort(
  process.env.MSGHUB_FRONTEND_PORT,
  appConfig.ports?.frontend ?? 3001,
);
const backendHost = normalizeProxyHost(
  process.env.MSGHUB_HOST ?? appConfig.runtime?.hosts?.backend,
);
const frontendHost =
  process.env.MSGHUB_FRONTEND_HOST ??
  appConfig.runtime?.hosts?.frontend ??
  "0.0.0.0";
const targetOrigin = `http://${backendHost}:${apiPort}`;

export default defineConfig({
  plugins: [react()],
  server: {
    port: frontendPort,
    host: frontendHost,
    proxy: {
      "/api": {
        target: targetOrigin,
        changeOrigin: true,
      },
      "/ws": {
        target: targetOrigin.replace("http://", "ws://"),
        ws: true,
      },
    },
  },
  build: {
    outDir: "dist",
    sourcemap: true,
  },
});

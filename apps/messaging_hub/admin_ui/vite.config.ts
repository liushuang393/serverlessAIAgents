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

const apiPort = requirePort(
  "MSGHUB_PORT",
  process.env.MSGHUB_PORT,
  appConfig.ports?.api,
);
const frontendPort = requirePort(
  "MSGHUB_FRONTEND_PORT",
  process.env.MSGHUB_FRONTEND_PORT,
  appConfig.ports?.frontend,
);
const backendHost = normalizeProxyHost(
  requireHost(
    "MSGHUB_HOST",
    process.env.MSGHUB_HOST,
    appConfig.runtime?.hosts?.backend,
  ),
);
const frontendHost = requireHost(
  "MSGHUB_FRONTEND_HOST",
  process.env.MSGHUB_FRONTEND_HOST,
  appConfig.runtime?.hosts?.frontend,
);
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

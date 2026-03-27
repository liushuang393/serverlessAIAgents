import fs from "node:fs";
import path from "node:path";

import react from "@vitejs/plugin-react";
import { defineConfig } from "vitest/config";

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

const appConfig = JSON.parse(
  fs.readFileSync(path.resolve(__dirname, "../app_config.json"), "utf-8"),
) as AppConfig;
const backendPort = requirePort(
  "GEO_PLATFORM_PORT",
  process.env.GEO_PLATFORM_PORT,
  appConfig.ports?.api,
);
const frontendPort = requirePort(
  "GEO_PLATFORM_FRONTEND_PORT",
  process.env.GEO_PLATFORM_FRONTEND_PORT,
  appConfig.ports?.frontend,
);
const frontendHost = requireHost(
  "GEO_PLATFORM_FRONTEND_HOST",
  process.env.GEO_PLATFORM_FRONTEND_HOST,
  appConfig.runtime?.hosts?.frontend,
);
const backendHost = normalizeProxyHost(
  requireHost(
    "GEO_PLATFORM_HOST",
    process.env.GEO_PLATFORM_HOST,
    appConfig.runtime?.hosts?.backend,
  ),
);

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      "@bizcore/i18n": path.resolve(__dirname, "./src/i18n/base"),
    },
  },
  server: {
    host: frontendHost,
    port: frontendPort,
    proxy: {
      "/api": `http://${backendHost}:${backendPort}`,
      "/geo": `http://${backendHost}:${backendPort}`,
    },
  },
  test: {
    environment: "jsdom",
    setupFiles: "./src/test/setup.ts",
    include: ["src/**/*.{test,spec}.{ts,tsx}"],
    exclude: ["src/**/*.e2e.spec.ts", "node_modules/**"],
  },
});

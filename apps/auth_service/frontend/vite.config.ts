import react from "@vitejs/plugin-react";
import fs from "node:fs";
import path from "node:path";
import { defineConfig } from "vite";

const appConfig = JSON.parse(
  fs.readFileSync(path.resolve(__dirname, "../app_config.json"), "utf-8"),
);
const backendUrl = appConfig.runtime?.urls?.backend ?? `http://localhost:${appConfig.ports?.api ?? 8010}`;
const frontendPort = appConfig.ports?.frontend ?? 3010;
const frontendHost = appConfig.runtime?.hosts?.frontend ?? "0.0.0.0";

export default defineConfig({
  plugins: [react()],
  server: {
    port: frontendPort,
    host: frontendHost,
    proxy: {
      "/auth": {
        target: backendUrl,
        changeOrigin: true,
      },
    },
  },
  build: {
    outDir: "dist",
    sourcemap: false,
  },
});

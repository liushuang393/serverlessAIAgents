/**
 * AgentFlow Platform Frontend - Vite 設定.
 *
 * 注意事項:
 * - server.proxy は開発環境（vite dev）でのみ有効
 * - 本番環境では静的ファイルが生成され、API は同一オリジンまたはリバースプロキシ経由
 */

/// <reference types="vitest" />
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import path from 'path';
import fs from 'fs';

/**
 * app_config.json からバックエンド API ポートを読み取る.
 *
 * ポートの定義元は app_config.json の ports.api（単一定義元）。
 * 環境変数 VITE_API_PORT で上書きも可能。
 */
const appConfig = JSON.parse(
  fs.readFileSync(path.resolve(__dirname, '../app_config.json'), 'utf-8'),
);
const API_PORT = process.env.VITE_API_PORT ?? String(appConfig.ports?.api ?? 8000);

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src'),
      // agentflow フレームワーク i18n 基底実装へのエイリアス
      '@agentflow/i18n': path.resolve(__dirname, '../../../agentflow/i18n/frontend'),
    },
  },
  server: {
    port: appConfig.ports?.frontend ?? 3000,
    proxy: {
      '/api': {
        target: `http://localhost:${API_PORT}`,
        changeOrigin: true,
      },
    },
  },
  test: {
    globals: true,
    environment: 'jsdom',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/',
        '**/*.d.ts',
        '**/*.config.*',
        'dist/',
      ],
    },
  },
});


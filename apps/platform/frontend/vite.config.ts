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

/**
 * バックエンド API ポート.
 *
 * Platform サーバーのデフォルトは 8000。
 * 環境変数 VITE_API_PORT で上書き可能。
 */
const API_PORT = process.env.VITE_API_PORT ?? '8000';

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src'),
    },
  },
  server: {
    port: 3000,
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


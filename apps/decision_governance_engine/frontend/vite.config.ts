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
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import path from 'path';

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src'),
    },
  },
  // -------------------------------------------------------------------------
  // 開発サーバー設定（vite dev 時のみ有効）
  // 本番環境では Nginx が /api/ を backend:8000 にプロキシ
  // -------------------------------------------------------------------------
  server: {
    port: 5174,
    host: '0.0.0.0',  // WSL2 から Windows ブラウザにアクセス可能にするため
    proxy: {
      '/api': {
        // PROXY_TARGET: 容器内では Docker 内部ネットワーク名を使用
        // ※ VITE_ プレフィックスを使うとブラウザ側に注入されてしまうため、
        //    proxy target 専用に PROXY_TARGET を使用する
        target: process.env.PROXY_TARGET || 'http://localhost:8001',
        changeOrigin: true,
        // SSE ストリーミング対応 - 重要な設定
        ws: false,  // WebSocket を無効化（SSE と競合防止）
        timeout: 0,  // タイムアウト無効化（長時間接続対応）
        proxyTimeout: 0,  // プロキシタイムアウト無効化
        configure: (proxy) => {
          // SSE 接続用の設定
          proxy.on('proxyReq', (proxyReq, req) => {
            if (req.url?.includes('/stream')) {
              proxyReq.setHeader('Accept', 'text/event-stream');
              proxyReq.setHeader('Cache-Control', 'no-cache');
              proxyReq.setHeader('Connection', 'keep-alive');
            }
          });
          // レスポンスヘッダーの追加
          proxy.on('proxyRes', (proxyRes, req) => {
            if (req.url?.includes('/stream')) {
              proxyRes.headers['Cache-Control'] = 'no-cache';
              proxyRes.headers['X-Accel-Buffering'] = 'no';
            }
          });
          // エラーハンドリング
          proxy.on('error', (err, req, res) => {
            console.error('[Vite Proxy Error]', err.message);
          });
        },
      },
    },
  },
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['./src/__tests__/setup.ts'],
    include: ['src/**/*.{test,spec}.{ts,tsx}'],
    coverage: {
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.{ts,tsx}'],
      exclude: ['src/**/*.d.ts', 'src/__tests__/**'],
    },
  },
});


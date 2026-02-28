import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'
import path from 'path'
import fs from 'fs'

interface AppConfig {
  ports?: {
    api?: number
    frontend?: number
  }
  runtime?: {
    urls?: {
      backend?: string
    }
  }
}

// app_config.json からポート設定を読み込む
const appConfigPath = path.resolve(__dirname, '../app_config.json')
let appConfig: AppConfig = {}
try {
  appConfig = JSON.parse(fs.readFileSync(appConfigPath, 'utf-8')) as AppConfig
} catch (e) {
  console.warn('Failed to load app_config.json', e)
}

// 環境変数または app_config.json から設定を取得
const apiPort = process.env.VITE_API_PORT || appConfig.ports?.api || 8005
const frontendPort = process.env.VITE_FRONTEND_PORT || appConfig.ports?.frontend || 3004
const apiHost = process.env.VITE_API_HOST || 'localhost'
const apiProxyTimeoutMs = Number(process.env.VITE_API_PROXY_TIMEOUT_MS || 10 * 60 * 1000)

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react(), tailwindcss()],
  define: {
    __APP_CONFIG__: JSON.stringify({
      apiPort,
      frontendPort,
      // 本番環境では環境変数 VITE_API_URL を優先、未指定なら相対パス or 構築時の設定を使用
      backendUrl: process.env.VITE_API_URL || appConfig.runtime?.urls?.backend || ""
    })
  },
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src'),
      // agentflow フレームワーク i18n 基底実装へのエイリアス
      '@agentflow/i18n': path.resolve(__dirname, '../../../agentflow/i18n/frontend'),
      // agentflow 配下の TSX からも、このアプリの React を確実に解決する
      react: path.resolve(__dirname, './node_modules/react'),
      'react/jsx-runtime': path.resolve(__dirname, './node_modules/react/jsx-runtime.js'),
      'react/jsx-dev-runtime': path.resolve(__dirname, './node_modules/react/jsx-dev-runtime.js'),
      'react-dom': path.resolve(__dirname, './node_modules/react-dom'),
    },
  },
  server: {
    port: Number(frontendPort),
    host: true, // 外部ホストからのアクセスを許可
    proxy: {
      '/api': {
        target: `http://${apiHost}:${apiPort}`,
        changeOrigin: true,
        secure: false,
        // /api/chat/stream は長時間 SSE 接続になるため、短い timeout だと
        // フロント側だけ "network error" になりやすい。
        timeout: apiProxyTimeoutMs,
        proxyTimeout: apiProxyTimeoutMs,
        configure: (proxy) => {
          proxy.on('error', (_err, req, _res) => {
            const url = req.url ?? ''
            if (url.includes('/api/auth')) {
              console.error(
                '[vite proxy] API 接続エラー（ログイン等）: バックエンドが起動しているか確認してください。',
              )
              console.error(
                `  起動例: python -m apps.faq_system.main --reload  (port ${apiPort})`,
              )
            }
          })
        },
      },
      '/ws': {
        target: `ws://${apiHost}:${apiPort}`,
        ws: true,
      },
    },
  },
})

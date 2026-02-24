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
        timeout: 30000,
        configure: (proxy) => {
          proxy.on('error', (err, req, res) => {
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

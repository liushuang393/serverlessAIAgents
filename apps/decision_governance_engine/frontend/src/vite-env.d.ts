/// <reference types="vite/client" />

/**
 * Vite 環境変数の型定義.
 *
 * 目的: import.meta.env の型安全性を確保
 */
interface ImportMetaEnv {
  /** API サーバーURL */
  readonly VITE_API_URL: string;
  /** 他の Vite 環境変数を追加可能 */
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}


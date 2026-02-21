/**
 * Enterprise FAQ Studio - i18n プロバイダー設定。
 *
 * 実装はフレームワーク (@agentflow/i18n) に委譲。
 * このファイルはアプリ固有のロケールファイル読み込みのみ担当する。
 * 対応ロケール: ja / en / zh
 */
import type { ReactNode } from 'react';
import {
  I18nProvider as BaseProvider,
  detectBrowserLocale,
} from '@agentflow/i18n';
import type { Locale, Translations } from '@agentflow/i18n';

/**
 * このアプリのロケール JSON ローダー。
 * Vite 動的 import はこのファイルからの相対パスで解決されるため、アプリ側に残す。
 * @param locale - 読み込むロケール識別子
 */
async function loadLocale(locale: Locale): Promise<Translations> {
  const mod = await import(`./locales/${locale}.json`);
  return mod.default as Translations;
}

/**
 * FAQ Studio i18n コンテキストプロバイダー。
 * main.tsx の最外層（BrowserRouter の外側）に配置する。
 * @param children - 子コンポーネント
 */
export function I18nProvider({ children }: { readonly children: ReactNode }) {
  return (
    <BaseProvider loader={loadLocale} defaultLocale={detectBrowserLocale()}>
      {children}
    </BaseProvider>
  );
}

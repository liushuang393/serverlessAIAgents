/**
 * AgentFlow i18n フロントエンド基底実装のエントリーポイント。
 *
 * 各 App は vite.config.ts に '@agentflow/i18n' エイリアスを設定し、
 * このモジュールから必要なものを import する。
 * App 側で再実装せず、ここからの import のみで完結させること。
 *
 * App 側の使い方:
 *   import { I18nProvider as BaseProvider, useI18n, detectBrowserLocale } from '@agentflow/i18n';
 *   import type { Locale, I18nContextValue, Translations } from '@agentflow/i18n';
 */

export { I18nProvider, useI18n, detectBrowserLocale } from './I18nProvider';
export type { I18nContextValue, Locale, LocaleLoader, Translations } from './I18nProvider';
export { LocaleSwitcher } from './LocaleSwitcher';


/**
 * Decision Governance Engine i18n エントリーポイント。
 *
 * main.tsx での使用例:
 *   import { I18nProvider } from './i18n';
 *   <I18nProvider><App /></I18nProvider>
 *
 * コンポーネントでの使用例:
 *   import { useI18n } from './i18n';
 *   const { t } = useI18n();
 *   <h1>{t('login.title')}</h1>
 */
export { I18nProvider } from './I18nProvider';
export { useI18n, LocaleSwitcher } from '@agentflow/i18n';
export type { I18nContextValue, Locale } from '@agentflow/i18n';


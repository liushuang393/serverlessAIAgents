/**
 * AgentFlow i18n フロントエンド基底実装。
 *
 * 外部ライブラリ不要・純 React 実装。
 * 各 App は @agentflow/i18n エイリアスを通じてこのモジュールを import し、
 * ロケール JSON ローダー（loader prop）を渡すだけで使用できる。
 * 実装ロジックはここに集約する。App 側は設定・呼び出しのみ担当。
 *
 * App 側の使い方:
 *   1. vite.config.ts に '@agentflow/i18n' エイリアスを設定する。
 *   2. App の I18nProvider.tsx で loader のみ定義してフレームワーク Provider をラップする。
 *   3. main.tsx で <I18nProvider> を最外層に配置する。
 *   4. コンポーネント内で const { t, locale, setLocale } = useI18n() を呼ぶ。
 */

import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useState,
  type ReactNode,
} from 'react';

// ------------------------------------------------------------------ 型定義

/** ロケール識別子 */
export type Locale = 'ja' | 'en' | 'zh';

/** JSON 翻訳辞書（ネスト構造を許容） */
type TranslationNode = string | { [key: string]: TranslationNode };
/** JSON 翻訳辞書（App の loadLocale 関数の戻り値型として使用） */
export type Translations = { [key: string]: TranslationNode };

/** Context が提供する値 */
export interface I18nContextValue {
  /** 現在のロケール */
  locale: Locale;
  /** ロケール切り替え */
  setLocale: (locale: Locale) => void;
  /**
   * 翻訳テキストを返す。
   * @param key  ドット区切りキー（例: "common.save"）
   * @param params  {placeholder} 置換パラメータ
   */
  t: (key: string, params?: Record<string, string>) => string;
}

// ------------------------------------------------------------------ Context

const I18nContext = createContext<I18nContextValue | null>(null);

// ------------------------------------------------------------------ ブラウザロケール検出

/**
 * ブラウザ言語設定から対応ロケールを検出して返す。
 * 各 App の I18nProvider で defaultLocale として使用する。
 * 対応外言語は日本語（ja）にフォールバックする。
 */
export function detectBrowserLocale(): Locale {
  const lang = navigator.language.split('-')[0];
  if (lang === 'en') return 'en';
  if (lang === 'zh') return 'zh';
  return 'ja';
}

// ------------------------------------------------------------------ ロケール解決

/**
 * ドット区切りキーで翻訳辞書を走査し、末端の文字列を返す。
 * 見つからない場合はキー文字列をそのまま返す。
 */
function resolve(
  translations: Translations,
  key: string,
  params: Record<string, string> | undefined,
): string {
  const parts = key.split('.');
  let node: TranslationNode = translations;
  for (const part of parts) {
    if (typeof node === 'object' && part in node) {
      node = node[part];
    } else {
      node = key; // フォールバック
      break;
    }
  }
  const text = typeof node === 'string' ? node : key;
  if (!params) return text;
  return text.replace(/\{(\w+)\}/g, (_match: string, k: string) => params[k] ?? `{${k}}`);
}

// ------------------------------------------------------------------ Provider

/** ロケール JSON を動的インポートするローダー関数の型 */
export type LocaleLoader = (locale: Locale) => Promise<Translations>;

interface I18nProviderProps {
  /** デフォルトロケール（省略時: "ja"） */
  defaultLocale?: Locale;
  /**
   * ロケール JSON を返す非同期ローダー。
   * 例: (locale) => import(`./locales/${locale}.json`).then(m => m.default)
   */
  loader: LocaleLoader;
  children: ReactNode;
}

/**
 * i18n コンテキストプロバイダー。
 * main.tsx の最外層に配置する。
 */
export function I18nProvider({
  defaultLocale = 'ja',
  loader,
  children,
}: I18nProviderProps) {
  const [locale, setLocaleState] = useState<Locale>(defaultLocale);
  const [translations, setTranslations] = useState<Translations>({});

  useEffect(() => {
    loader(locale)
      .then((data) => setTranslations(data))
      .catch(() => setTranslations({}));
  }, [locale, loader]);

  const setLocale = useCallback((next: Locale) => {
    setLocaleState(next);
  }, []);

  const t = useCallback(
    (key: string, params?: Record<string, string>): string =>
      resolve(translations, key, params),
    [translations],
  );

  return (
    <I18nContext.Provider value={{ locale, setLocale, t }}>
      {children}
    </I18nContext.Provider>
  );
}

// ------------------------------------------------------------------ Hook

/**
 * i18n フックで翻訳機能を取得する。
 * I18nProvider の子孫コンポーネントで使用すること。
 */
export function useI18n(): I18nContextValue {
  const ctx = useContext(I18nContext);
  if (ctx === null) {
    throw new Error('useI18n は <I18nProvider> 内でのみ使用可能です');
  }
  return ctx;
}

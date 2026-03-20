import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useState,
  type ReactNode,
} from 'react';

export type Locale = 'ja' | 'en' | 'zh';

type TranslationNode = string | { [key: string]: TranslationNode };
export type Translations = { [key: string]: TranslationNode };

export interface I18nContextValue {
  locale: Locale;
  setLocale: (locale: Locale) => void;
  t: (key: string, params?: Record<string, string>) => string;
}

const I18nContext = createContext<I18nContextValue | null>(null);
const HTML_LANG_BY_LOCALE: Record<Locale, string> = {
  ja: 'ja',
  en: 'en',
  zh: 'zh-CN',
};

export function detectBrowserLocale(): Locale {
  if (typeof navigator === 'undefined') {
    return 'ja';
  }
  const lang = navigator.language.split('-')[0];
  if (lang === 'en') return 'en';
  if (lang === 'zh') return 'zh';
  return 'ja';
}

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
      node = key;
      break;
    }
  }
  const text = typeof node === 'string' ? node : key;
  if (params === undefined) return text;
  return text.replace(/\{(\w+)\}/g, (_match: string, token: string) => params[token] ?? `{${token}}`);
}

export type LocaleLoader = (locale: Locale) => Promise<Translations>;

interface I18nProviderProps {
  defaultLocale?: Locale;
  loader: LocaleLoader;
  children: ReactNode;
}

export function I18nProvider({ defaultLocale = 'ja', loader, children }: I18nProviderProps) {
  const [locale, setLocaleState] = useState<Locale>(defaultLocale);
  const [translations, setTranslations] = useState<Translations>({});

  useEffect(() => {
    loader(locale)
      .then((data) => setTranslations(data))
      .catch(() => setTranslations({}));
  }, [locale, loader]);

  useEffect(() => {
    if (typeof document !== 'undefined') {
      document.documentElement.lang = HTML_LANG_BY_LOCALE[locale];
    }
  }, [locale]);

  const setLocale = useCallback((next: Locale) => {
    setLocaleState(next);
  }, []);

  const t = useCallback(
    (key: string, params?: Record<string, string>): string => resolve(translations, key, params),
    [translations],
  );

  return <I18nContext.Provider value={{ locale, setLocale, t }}>{children}</I18nContext.Provider>;
}

export function useI18n(): I18nContextValue {
  const ctx = useContext(I18nContext);
  if (ctx === null) {
    throw new Error('useI18n must be used within <I18nProvider>');
  }
  return ctx;
}

const LOCALE_OPTIONS: { value: Locale; label: string }[] = [
  { value: 'ja', label: '日本語' },
  { value: 'en', label: 'English' },
  { value: 'zh', label: '中文' },
];

interface LocaleSwitcherProps {
  readonly className?: string;
  readonly ariaLabel?: string;
  readonly testId?: string;
}

export function LocaleSwitcher({ className, ariaLabel = 'Select language', testId }: LocaleSwitcherProps) {
  const { locale, setLocale } = useI18n();

  return (
    <select
      value={locale}
      onChange={(event) => {
        setLocale(event.target.value as Locale);
      }}
      className={className}
      aria-label={ariaLabel}
      data-testid={testId}
    >
      {LOCALE_OPTIONS.map((option) => (
        <option key={option.value} value={option.value}>
          {option.label}
        </option>
      ))}
    </select>
  );
}

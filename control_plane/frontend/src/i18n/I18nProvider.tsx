import {
  createContext,
  type ReactNode,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";

import en from "./locales/en.json";
import ja from "./locales/ja.json";
import zh from "./locales/zh.json";

export type Locale = "en" | "ja" | "zh";
export type Translations = Record<string, unknown>;

export interface I18nContextValue {
  locale: Locale;
  setLocale: (locale: Locale) => void;
  t: (key: string, vars?: Record<string, string | number>) => string;
}

const STORAGE_KEY = "bizcore-platform-locale";
const TRANSLATIONS: Record<Locale, Translations> = { en, ja, zh };

const I18nContext = createContext<I18nContextValue | null>(null);

const isLocale = (value: string | null): value is Locale =>
  value === "en" || value === "ja" || value === "zh";

const detectBrowserLocale = (): Locale => {
  if (typeof window === "undefined") {
    return "ja";
  }

  const stored = window.localStorage.getItem(STORAGE_KEY);
  if (isLocale(stored)) {
    return stored;
  }

  const language = window.navigator.language.toLowerCase();
  if (language.startsWith("zh")) {
    return "zh";
  }
  if (language.startsWith("en")) {
    return "en";
  }
  return "ja";
};

const resolveMessage = (
  translations: Translations,
  key: string,
): string | null => {
  const parts = key.split(".");
  let current: unknown = translations;
  for (const part of parts) {
    if (typeof current !== "object" || current === null || !(part in current)) {
      return null;
    }
    current = (current as Record<string, unknown>)[part];
  }
  return typeof current === "string" ? current : null;
};

const interpolate = (
  message: string,
  vars?: Record<string, string | number>,
): string => {
  if (vars === undefined) {
    return message;
  }
  return Object.entries(vars).reduce(
    (result, [name, value]) => result.split(`{${name}}`).join(String(value)),
    message,
  );
};

export function I18nProvider({ children }: { readonly children: ReactNode }) {
  const [locale, setLocale] = useState<Locale>(detectBrowserLocale);

  useEffect(() => {
    if (typeof window !== "undefined") {
      window.localStorage.setItem(STORAGE_KEY, locale);
    }
  }, [locale]);

  const value = useMemo<I18nContextValue>(
    () => ({
      locale,
      setLocale,
      t: (key: string, vars?: Record<string, string | number>) => {
        const localized =
          resolveMessage(TRANSLATIONS[locale], key) ??
          resolveMessage(TRANSLATIONS.ja, key) ??
          key;
        return interpolate(localized, vars);
      },
    }),
    [locale],
  );

  return <I18nContext.Provider value={value}>{children}</I18nContext.Provider>;
}

export function useI18n(): I18nContextValue {
  const value = useContext(I18nContext);
  if (value === null) {
    throw new Error("I18nProvider が必要です。");
  }
  return value;
}

export function LocaleSwitcher({ className }: { readonly className?: string }) {
  const { locale, setLocale } = useI18n();

  return (
    <select
      value={locale}
      onChange={(event) => setLocale(event.target.value as Locale)}
      className={className}
      aria-label="Locale"
    >
      <option value="ja">日本語</option>
      <option value="en">English</option>
      <option value="zh">中文</option>
    </select>
  );
}

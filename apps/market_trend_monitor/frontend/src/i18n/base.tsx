import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useState,
  type ReactNode,
} from "react";

export type Locale = "ja" | "en" | "zh";

type TranslationNode = string | { [key: string]: TranslationNode };
export type Translations = { [key: string]: TranslationNode };

export interface I18nContextValue {
  locale: Locale;
  setLocale: (locale: Locale) => void;
  t: (key: string, params?: Record<string, string>) => string;
}

const I18nContext = createContext<I18nContextValue | null>(null);

export function detectBrowserLocale(): Locale {
  const lang = navigator.language.split("-")[0];
  if (lang === "en") return "en";
  if (lang === "zh") return "zh";
  return "ja";
}

function resolve(
  translations: Translations,
  key: string,
  params: Record<string, string> | undefined,
): string {
  const parts = key.split(".");
  let node: TranslationNode = translations;
  for (const part of parts) {
    if (typeof node === "object" && part in node) {
      node = node[part];
    } else {
      node = key;
      break;
    }
  }
  const text = typeof node === "string" ? node : key;
  if (!params) return text;
  return text.replace(
    /\{(\w+)\}/g,
    (_match: string, k: string) => params[k] ?? `{${k}}`,
  );
}

export type LocaleLoader = (locale: Locale) => Promise<Translations>;

interface I18nProviderProps {
  defaultLocale?: Locale;
  loader: LocaleLoader;
  children: ReactNode;
}

export function I18nProvider({
  defaultLocale = "ja",
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

export function useI18n(): I18nContextValue {
  const ctx = useContext(I18nContext);
  if (ctx === null) {
    throw new Error("useI18n must be used within <I18nProvider>");
  }
  return ctx;
}

const LOCALE_OPTIONS: { value: Locale; label: string }[] = [
  { value: "ja", label: "🇯🇵 日本語" },
  { value: "en", label: "🇺🇸 English" },
  { value: "zh", label: "🇨🇳 中文" },
];

interface LocaleSwitcherProps {
  readonly className?: string;
}

export function LocaleSwitcher({ className }: LocaleSwitcherProps) {
  const { locale, setLocale } = useI18n();

  return (
    <select
      value={locale}
      onChange={(e) => {
        setLocale(e.target.value as Locale);
      }}
      className={className}
      aria-label="Select language"
    >
      {LOCALE_OPTIONS.map((opt) => (
        <option key={opt.value} value={opt.value}>
          {opt.label}
        </option>
      ))}
    </select>
  );
}

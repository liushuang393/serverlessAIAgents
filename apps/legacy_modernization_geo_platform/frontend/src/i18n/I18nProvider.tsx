import type { ReactNode } from "react";
import { I18nProvider as BaseProvider } from "@bizcore/i18n";
import type { Locale, Translations } from "@bizcore/i18n";

async function loadLocale(locale: Locale): Promise<Translations> {
  const mod = await import(`./locales/${locale}.json`);
  return mod.default as Translations;
}

export function I18nProvider({ children }: { readonly children: ReactNode }) {
  return (
    <BaseProvider loader={loadLocale} defaultLocale="ja">
      {children}
    </BaseProvider>
  );
}

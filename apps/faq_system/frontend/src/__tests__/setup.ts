/**
 * Vitest テストセットアップ.
 *
 * @testing-library/jest-dom のカスタムマッチャーを登録する。
 * i18n モックを全テストに自動適用する。
 */
import "@testing-library/jest-dom";
import { vi } from "vitest";

// ---------------------------------------------------------------------------
// i18n モック: テスト環境では t() がキーをそのまま返す
// ---------------------------------------------------------------------------
vi.mock("@bizcore/i18n", () => ({
  useI18n: () => ({
    t: (key: string) => key,
    locale: "ja" as const,
    setLocale: vi.fn(),
  }),
  I18nProvider: ({ children }: { children: React.ReactNode }) => children,
  detectBrowserLocale: () => "ja" as const,
  LocaleSwitcher: () => null,
}));

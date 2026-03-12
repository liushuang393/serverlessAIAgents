/**
 * LocaleSwitcher - 言語切り替えドロップダウン（フレームワーク共通部品）.
 *
 * 目的: useI18n() の setLocale を呼び出す汎用セレクトボックス。
 * 各 App のレイアウト（サイドバー・ヘッダー等）に配置して使用する。
 * スタイルは className prop で上書き可能。
 *
 * 使用例:
 *   import { LocaleSwitcher } from '@agentflow/i18n';
 *   <LocaleSwitcher className="my-select-class" />
 */

import { useI18n } from './I18nProvider';
import type { Locale } from './I18nProvider';

/** 言語オプション定義 */
const LOCALE_OPTIONS: { value: Locale; label: string }[] = [
  { value: 'ja', label: '🇯🇵 日本語' },
  { value: 'en', label: '🇺🇸 English' },
  { value: 'zh', label: '🇨🇳 中文' },
];

interface LocaleSwitcherProps {
  /**
   * select 要素に適用する CSS クラス名。
   * 未指定時はデフォルトのシンプルスタイルを使用。
   */
  readonly className?: string;
}

/**
 * 言語切り替えセレクトボックス。
 * I18nProvider の内側で使用すること。
 */
export function LocaleSwitcher({ className }: LocaleSwitcherProps) {
  const { locale, setLocale } = useI18n();

  return (
    <select
      value={locale}
      onChange={(e) => { setLocale(e.target.value as Locale); }}
      className={className}
      aria-label="言語を選択 / Select language"
    >
      {LOCALE_OPTIONS.map((opt) => (
        <option key={opt.value} value={opt.value}>
          {opt.label}
        </option>
      ))}
    </select>
  );
}


/**
 * 設定モーダルコンポーネント.
 *
 * 目的: 言語切替、ユーザープロフィール表示、システム情報表示
 * チャットパネル右上の歯車アイコンから開くモーダルダイアログ。
 */

import { useEffect, useCallback } from "react";
import { X, Globe, User, Server, Shield } from "lucide-react";
import { LocaleSwitcher, useI18n } from "../../i18n";
import { useAuthStore } from "../../stores/authStore";

/** 設定モーダルの props 型定義 */
interface SettingsModalProps {
  /** モーダルが開いているか */
  isOpen: boolean;
  /** モーダルを閉じるコールバック */
  onClose: () => void;
}

/** 設定モーダル */
export const SettingsModal = ({ isOpen, onClose }: SettingsModalProps) => {
  const { t } = useI18n();
  const { user } = useAuthStore();

  /** Escape キーでモーダルを閉じる */
  const handleKeyDown = useCallback(
    (e: KeyboardEvent) => {
      if (e.key === "Escape") onClose();
    },
    [onClose],
  );

  useEffect(() => {
    if (isOpen) {
      document.addEventListener("keydown", handleKeyDown);
      return () => document.removeEventListener("keydown", handleKeyDown);
    }
  }, [isOpen, handleKeyDown]);

  return (
    <div
      className={`fixed inset-0 z-50 flex items-center justify-center transition-all duration-300 ${isOpen ? "opacity-100 visible" : "opacity-0 invisible pointer-events-none"
        }`}
    >
      {/* オーバーレイ背景 */}
      <button
        type="button"
        tabIndex={-1}
        className={`absolute inset-0 w-full h-full bg-black/60 backdrop-blur-sm border-none cursor-default transition-opacity duration-300 ${isOpen ? "opacity-100" : "opacity-0"
          }`}
        onClick={onClose}
        aria-label={t("common.close")}
      />
      {/* モーダル本体 */}
      <div
        className={`relative z-10 w-full max-w-2xl max-h-[85vh] overflow-y-auto custom-scrollbar bg-black rounded-2xl border border-white/10 shadow-2xl mx-4 transition-all duration-300 transform ${isOpen ? "scale-100 translate-y-0" : "scale-95 translate-y-8"
          }`}
      >
        <div className="p-8 space-y-6">
          {/* ヘッダー */}
          <div className="flex items-center justify-between">
            <h1 className="text-xl font-bold text-white">
              {t("settings.title")}
            </h1>
            <button
              onClick={onClose}
              className="p-2 rounded-xl hover:bg-white/5 text-[var(--text-muted)] hover:text-white transition-all"
              aria-label={t("common.close")}
            >
              <X size={20} />
            </button>
          </div>

          {/* 言語設定セクション */}
          <section className="bg-white/[0.02] rounded-2xl p-8 border border-white/5 space-y-4">
            <div className="flex items-center gap-3">
              <Globe size={18} className="text-[var(--primary)]" />
              <h2 className="text-base font-semibold text-white">
                {t("settings.language")}
              </h2>
            </div>
            <p className="text-sm text-[var(--text-muted)]">
              {t("settings.language_desc")}
            </p>
            <LocaleSwitcher className="w-full bg-[var(--bg-main)] border border-white/10 rounded-xl px-4 py-3 text-sm text-white cursor-pointer focus:outline-none focus:border-[var(--primary)]/50 transition-colors" />
          </section>

          {/* ユーザー情報セクション */}
          <section className="bg-white/[0.02] rounded-2xl p-8 border border-white/5 space-y-4">
            <div className="flex items-center gap-3">
              <User size={18} className="text-[var(--primary)]" />
              <h2 className="text-base font-semibold text-white">
                {t("settings.profile")}
              </h2>
            </div>
            <div className="grid grid-cols-2 gap-4">
              <InfoRow
                label={t("settings.username")}
                value={user?.username ?? "-"}
              />
              <InfoRow
                label={t("settings.display_name")}
                value={user?.display_name ?? "-"}
              />
              <InfoRow
                label={t("settings.department")}
                value={user?.department ?? "-"}
              />
              <InfoRow label={t("settings.role")} value={user?.role ?? "-"} />
            </div>
          </section>

          {/* システム情報セクション */}
          <section className="bg-white/[0.02] rounded-2xl p-8 border border-white/5 space-y-4">
            <div className="flex items-center gap-3">
              <Server size={18} className="text-[var(--primary)]" />
              <h2 className="text-base font-semibold text-white">
                {t("settings.system")}
              </h2>
            </div>
            <div className="grid grid-cols-2 gap-4">
              <InfoRow label={t("settings.version")} value="2.0.0" />
              <InfoRow
                label={t("settings.app_name")}
                value="FAQ Intelligence"
              />
            </div>
          </section>

          {/* セキュリティセクション */}
          <section className="bg-white/[0.02] rounded-2xl p-8 border border-white/5 space-y-4">
            <div className="flex items-center gap-3">
              <Shield size={18} className="text-[var(--primary)]" />
              <h2 className="text-base font-semibold text-white">
                {t("settings.security")}
              </h2>
            </div>
            <p className="text-sm text-[var(--text-muted)]">
              {t("settings.security_desc")}
            </p>
          </section>
        </div>
      </div>
    </div>
  );
};

/** 情報表示行コンポーネント */
function InfoRow({ label, value }: Readonly<{ label: string; value: string }>) {
  return (
    <div className="p-4 bg-white/[0.02] rounded-xl border border-white/5 shadow-sm">
      <div className="text-[10px] font-semibold text-[var(--text-muted)] uppercase tracking-wider mb-1">
        {label}
      </div>
      <div className="text-sm text-white font-medium truncate">{value}</div>
    </div>
  );
}

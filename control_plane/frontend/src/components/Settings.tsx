/**
 * Settings - 設定画面.
 *
 * 実運用で使う設定アクションのみを提供。
 */

import { useState } from "react";
import { Link } from "react-router-dom";
import { useAppStore } from "@/store/useAppStore";
import { useI18n } from "../i18n";

export function Settings() {
  const { t } = useI18n();
  const { refresh, error, clearError } = useAppStore();
  const [refreshing, setRefreshing] = useState(false);
  const [message, setMessage] = useState<string | null>(null);
  const quickLinks = [
    { to: "/llm-management", label: t("stg.hub_llm") },
    { to: "/rag", label: t("stg.hub_rag") },
    { to: "/mcp", label: t("stg.hub_mcp") },
  ];

  const handleRefresh = async () => {
    setRefreshing(true);
    setMessage(null);
    try {
      await refresh();
      setMessage(t("stg.refresh_success"));
    } finally {
      setRefreshing(false);
    }
  };

  return (
    <div className="p-6 max-w-3xl mx-auto space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-slate-100">{t("stg.title")}</h1>
        <p className="text-sm text-slate-500 mt-1">{t("stg.subtitle")}</p>
      </div>

      {/* エラー */}
      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button
            onClick={clearError}
            className="text-red-400 hover:text-red-300 text-xs"
          >
            ✕
          </button>
        </div>
      )}

      {/* 成功メッセージ */}
      {message && (
        <div className="bg-emerald-500/10 border border-emerald-500/20 rounded-lg p-4 text-emerald-400 text-sm">
          {message}
        </div>
      )}

      {/* アクション */}
      <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
        <h2 className="text-sm font-semibold text-slate-200">
          {t("stg.actions")}
        </h2>
        <div className="space-y-3">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-slate-300">
                {t("stg.refresh_registry")}
              </p>
              <p className="text-xs text-slate-500">{t("stg.refresh_desc")}</p>
            </div>
            <button
              onClick={handleRefresh}
              disabled={refreshing}
              className="px-4 py-2 bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 disabled:cursor-not-allowed text-white text-sm font-medium rounded-lg transition-colors"
            >
              {refreshing ? t("stg.scanning") : t("stg.refresh_btn")}
            </button>
          </div>
        </div>
      </div>

      {/* 実務設定ハブ */}
      <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
        <h2 className="text-sm font-semibold text-slate-200">
          {t("stg.config_hubs")}
        </h2>
        <p className="text-xs text-slate-500">{t("stg.config_hubs_desc")}</p>
        <div className="grid grid-cols-1 sm:grid-cols-3 gap-2">
          {quickLinks.map((item) => (
            <Link
              key={item.to}
              to={item.to}
              className="px-3 py-2 text-center text-sm rounded-lg border border-slate-700 bg-slate-900 hover:bg-slate-800 text-slate-200 transition-colors"
            >
              {item.label}
            </Link>
          ))}
        </div>
      </div>
    </div>
  );
}

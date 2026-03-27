/**
 * 知識ベース管理レイアウト.
 *
 * タブベースのレイアウト。ダッシュボード / コレクション / ドキュメント / RAG設定。
 */

import React, { useEffect } from "react";
import { useDecisionStore } from "../../store/useDecisionStore";
import { useKnowledgeStore, type KBTab } from "../../store/useKnowledgeStore";
import { useI18n } from "../../i18n";
import { KnowledgeDashboard } from "./KnowledgeDashboard";
import { KnowledgeDocManager } from "./KnowledgeDocManager";
import { KnowledgeRAGConfig } from "./KnowledgeRAGConfig";

const TABS: { key: KBTab; labelKey: string }[] = [
  { key: "dashboard", labelKey: "kb.tab_dashboard" },
  { key: "collections", labelKey: "kb.tab_collections" },
  { key: "documents", labelKey: "kb.tab_documents" },
  { key: "config", labelKey: "kb.tab_config" },
];

export const KnowledgeLayout: React.FC = () => {
  const { t } = useI18n();
  const { setPage } = useDecisionStore();
  const { activeTab, setActiveTab, error, setError, fetchCollections } =
    useKnowledgeStore();

  useEffect(() => {
    fetchCollections();
  }, [fetchCollections]);

  const renderTab = () => {
    switch (activeTab) {
      case "dashboard":
        return <KnowledgeDashboard />;
      case "collections":
        return <KnowledgeDashboard showCollectionForm />;
      case "documents":
        return <KnowledgeDocManager />;
      case "config":
        return <KnowledgeRAGConfig />;
      default:
        return <KnowledgeDashboard />;
    }
  };

  return (
    <div className="min-h-screen bg-[#0a0a0f] text-white">
      {/* ヘッダー */}
      <header className="border-b border-white/5 px-6 py-4">
        <div className="max-w-5xl mx-auto flex items-center justify-between">
          <div>
            <h1 className="font-semibold text-lg">{t("kb.title")}</h1>
            <p className="text-xs text-slate-500">{t("kb.subtitle")}</p>
          </div>
          <button
            onClick={() => setPage("input")}
            className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm transition-colors"
          >
            {t("common.back")}
          </button>
        </div>
      </header>

      {/* タブナビゲーション */}
      <div className="border-b border-white/5">
        <div className="max-w-5xl mx-auto px-6 flex gap-1">
          {TABS.map((tab) => (
            <button
              key={tab.key}
              onClick={() => setActiveTab(tab.key)}
              className={`px-4 py-3 text-sm font-medium transition-colors relative ${
                activeTab === tab.key
                  ? "text-indigo-400"
                  : "text-slate-500 hover:text-slate-300"
              }`}
            >
              {t(tab.labelKey)}
              {activeTab === tab.key && (
                <span className="absolute bottom-0 left-0 right-0 h-0.5 bg-indigo-500 rounded-t" />
              )}
            </button>
          ))}
        </div>
      </div>

      {/* エラー表示 */}
      {error && (
        <div className="max-w-5xl mx-auto px-6 mt-4">
          <div className="bg-red-500/10 border border-red-500/20 rounded-xl p-3 text-red-400 text-sm flex justify-between">
            <span>{error}</span>
            <button
              onClick={() => setError(null)}
              className="text-slate-500 hover:text-white ml-2"
            >
              &times;
            </button>
          </div>
        </div>
      )}

      {/* コンテンツ */}
      <main className="max-w-5xl mx-auto px-6 py-6">{renderTab()}</main>
    </div>
  );
};

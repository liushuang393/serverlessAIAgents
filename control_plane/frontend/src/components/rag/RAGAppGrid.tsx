/**
 * RAG アプリグリッド.
 *
 * 各アプリの RAG 設定サマリーをカード形式で一覧表示。
 * 読み取り専用。クリックで詳細モーダルを開く。
 */
import { useI18n } from "../../i18n";

import type { AppRAGConfig } from "@/types";

interface Props {
  configs: AppRAGConfig[];
  onSelect: (appName: string) => void;
}

function HealthDot({ enabled }: { enabled: boolean }) {
  return (
    <span
      className={`inline-block h-2 w-2 rounded-full ${
        enabled ? "bg-emerald-400" : "bg-slate-500"
      }`}
    />
  );
}

/** アプリ RAG カード */
function AppCard({
  config,
  onClick,
}: {
  config: AppRAGConfig;
  onClick: () => void;
}) {
  const { t } = useI18n();
  const r = config.rag;

  return (
    <button
      type="button"
      onClick={onClick}
      className="w-full text-left rounded-xl border border-slate-700/50 bg-slate-800/40 p-4 hover:bg-slate-800/70 hover:border-indigo-500/30 transition-all focus:outline-none focus:ring-1 focus:ring-indigo-500"
    >
      {/* ヘッダー */}
      <div className="flex items-center gap-3 mb-3">
        <span className="text-xl">{config.icon || "📱"}</span>
        <div className="flex-1 min-w-0">
          <p className="text-sm font-medium text-slate-200 truncate">
            {config.display_name}
          </p>
          <p className="text-xs text-slate-500">{config.app_name}</p>
        </div>
        <HealthDot enabled={r.enabled} />
      </div>

      {/* RAG 情報 */}
      {r.enabled ? (
        <div className="space-y-1.5 text-xs text-slate-400">
          <div className="flex justify-between">
            <span>{t("rag_dashboard.retrieval")}</span>
            <span className="text-slate-300">{r.retrieval_method}</span>
          </div>
          <div className="flex justify-between">
            <span>{t("rag_dashboard.chunk")}</span>
            <span className="text-slate-300">
              {r.chunk_strategy} / {r.chunk_size}
            </span>
          </div>
          <div className="flex justify-between">
            <span>{t("rag_dashboard.vector")}</span>
            <span className="text-slate-300">{r.vector_provider || "-"}</span>
          </div>
          {r.reranker && (
            <div className="flex justify-between">
              <span>{t("rag_dashboard.reranker")}</span>
              <span className="text-slate-300">{r.reranker}</span>
            </div>
          )}
          <div className="flex justify-between">
            <span>Top-K</span>
            <span className="text-slate-300">{r.top_k}</span>
          </div>
        </div>
      ) : (
        <p className="text-xs text-slate-500 italic">
          {t("rag_dashboard.rag_disabled")}
        </p>
      )}

      {/* データソース数 */}
      {r.enabled && r.data_sources.length > 0 && (
        <div className="mt-3 pt-2 border-t border-slate-700/50">
          <p className="text-xs text-slate-500">
            {r.data_sources.length} {t("rag_dashboard.data_sources")}
          </p>
        </div>
      )}
    </button>
  );
}

/** RAG アプリグリッド */
export function RAGAppGrid({ configs, onSelect }: Props) {
  const { t } = useI18n();

  if (configs.length === 0) {
    return (
      <div className="text-center py-12 text-slate-500">
        {t("rag_dashboard.no_apps")}
      </div>
    );
  }

  return (
    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
      {configs.map((cfg) => (
        <AppCard
          key={cfg.app_name}
          config={cfg}
          onClick={() => onSelect(cfg.app_name)}
        />
      ))}
    </div>
  );
}

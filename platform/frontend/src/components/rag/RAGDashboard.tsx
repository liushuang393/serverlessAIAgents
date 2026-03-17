/**
 * RAG 読み取り専用ダッシュボード.
 *
 * Platform の RAG モニタリング画面。
 * 全アプリの RAG 設定を読み取り専用で表示し、
 * 編集操作は各アプリの管理 UI へ誘導する。
 */
import { useCallback, useEffect, useState } from 'react';
import { useI18n } from '../../i18n';

import { fetchAppRAGConfigs } from '@/api/client';
import { useAppStore } from '@/store/useAppStore';
import type { AppRAGConfig } from '@/types';

import { RAGAppDetailModal } from './RAGAppDetailModal';
import { RAGAppGrid } from './RAGAppGrid';
import { RAGStatsRow } from './RAGStatsRow';

/** RAG 読み取り専用ダッシュボード */
export function RAGDashboard() {
  const { t } = useI18n();
  const { ragOverview, loading, error, loadRAGOverview, clearError } =
    useAppStore();

  const [configs, setConfigs] = useState<AppRAGConfig[]>([]);
  const [configLoading, setConfigLoading] = useState(false);
  const [selectedApp, setSelectedApp] = useState<string | null>(null);

  // 初期読み込み
  useEffect(() => {
    loadRAGOverview();
  }, [loadRAGOverview]);

  const loadConfigs = useCallback(async () => {
    setConfigLoading(true);
    try {
      const resp = await fetchAppRAGConfigs();
      setConfigs(resp.apps);
    } catch {
      // ragOverview の error に任せる
    } finally {
      setConfigLoading(false);
    }
  }, []);

  useEffect(() => {
    void loadConfigs();
  }, [loadConfigs]);

  const selectedConfig = configs.find((c) => c.app_name === selectedApp);

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-slate-100">
            {t('rag_dashboard.title')}
          </h1>
          <p className="text-sm text-slate-500 mt-1">
            {t('rag_dashboard.subtitle')}
          </p>
        </div>
        <button
          onClick={() => {
            loadRAGOverview();
            void loadConfigs();
          }}
          disabled={loading || configLoading}
          className="px-3 py-1.5 rounded-md bg-slate-800 hover:bg-slate-700 text-slate-300 text-xs disabled:opacity-50 transition-colors"
        >
          {t('rag_dashboard.reload')}
        </button>
      </div>

      {/* エラー表示 */}
      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button
            onClick={clearError}
            className="text-red-400 hover:text-red-300 text-xs"
          >
            &times;
          </button>
        </div>
      )}

      {/* ローディング */}
      {(loading || configLoading) && !ragOverview && (
        <div className="flex items-center justify-center py-12 text-slate-400">
          <span className="animate-spin mr-2">&#9696;</span>
          {t('rag_dashboard.loading')}
        </div>
      )}

      {/* 統計行 */}
      {ragOverview && (
        <RAGStatsRow
          stats={ragOverview.stats}
          appCount={configs.filter((c) => c.rag.enabled).length}
        />
      )}

      {/* アプリグリッド */}
      <section>
        <h2 className="text-sm font-semibold text-slate-300 mb-3">
          {t('rag_dashboard.app_configs')}
        </h2>
        <RAGAppGrid configs={configs} onSelect={setSelectedApp} />
      </section>

      {/* 利用可能な戦略・リランカー */}
      {ragOverview && (
        <section className="grid grid-cols-1 lg:grid-cols-2 gap-4">
          {/* チャンキング戦略 */}
          <div className="rounded-xl bg-slate-800/40 border border-slate-700/50 p-4">
            <h3 className="text-xs font-semibold text-slate-400 uppercase mb-3">
              {t('rag_dashboard.available_strategies')}
            </h3>
            <div className="space-y-1.5">
              {ragOverview.chunk_strategies.map((s) => (
                <div key={s.name} className="flex justify-between text-xs">
                  <span className="text-slate-300">{s.label}</span>
                  <span className="text-slate-500 font-mono">{s.name}</span>
                </div>
              ))}
            </div>
          </div>

          {/* リランカー */}
          <div className="rounded-xl bg-slate-800/40 border border-slate-700/50 p-4">
            <h3 className="text-xs font-semibold text-slate-400 uppercase mb-3">
              {t('rag_dashboard.available_rerankers')}
            </h3>
            <div className="space-y-1.5">
              {ragOverview.rerankers.map((r) => (
                <div key={r.name} className="flex justify-between text-xs">
                  <span className="text-slate-300">{r.label}</span>
                  <span className="text-slate-500 font-mono">{r.name}</span>
                </div>
              ))}
            </div>
          </div>
        </section>
      )}

      {/* 詳細モーダル */}
      {selectedConfig && (
        <RAGAppDetailModal
          config={selectedConfig}
          onClose={() => setSelectedApp(null)}
        />
      )}
    </div>
  );
}

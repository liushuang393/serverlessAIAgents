/**
 * ナレッジベース ダッシュボード.
 *
 * コレクション総数・ドキュメント数・最終インジェスト等の
 * サマリーカードとクイックアクションを表示する。
 */
import { useEffect } from 'react';
import { Database, FileText, Clock, Search } from 'lucide-react';
import { useI18n } from '../../i18n';
import { useRAGStore } from '../../stores/ragStore';

function StatCard({
  icon: Icon,
  label,
  value,
  color,
}: {
  icon: typeof Database;
  label: string;
  value: string | number;
  color: string;
}) {
  return (
    <div className="rounded-xl bg-white/[0.03] border border-white/5 p-5 flex flex-col gap-2">
      <Icon size={20} className={color} />
      <p className="text-2xl font-bold text-white">{value}</p>
      <p className="text-xs text-[var(--text-muted)]">{label}</p>
    </div>
  );
}

export function KBDashboard() {
  const { t } = useI18n();
  const {
    collections,
    collectionsLoading,
    fetchCollections,
    ingestRuns,
    fetchIngestRuns,
    setActiveTab,
  } = useRAGStore();

  useEffect(() => {
    void fetchCollections();
    void fetchIngestRuns();
  }, [fetchCollections, fetchIngestRuns]);

  const totalDocs = collections.reduce((sum, c) => sum + c.document_count, 0);
  const lastIngest = ingestRuns[0];

  return (
    <div className="space-y-6">
      {/* 統計カード */}
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
        <StatCard
          icon={Database}
          label={t('rag.stat_collections')}
          value={collectionsLoading ? '...' : collections.length}
          color="text-indigo-400"
        />
        <StatCard
          icon={FileText}
          label={t('rag.stat_documents')}
          value={collectionsLoading ? '...' : totalDocs}
          color="text-emerald-400"
        />
        <StatCard
          icon={Clock}
          label={t('rag.stat_last_ingest')}
          value={
            lastIngest?.started_at
              ? new Date(lastIngest.started_at).toLocaleDateString('ja-JP')
              : '-'
          }
          color="text-amber-400"
        />
        <StatCard
          icon={Search}
          label={t('rag.stat_active_collections')}
          value={collectionsLoading ? '...' : collections.filter((c) => c.document_count > 0).length}
          color="text-cyan-400"
        />
      </div>

      {/* コレクション一覧 */}
      <div>
        <div className="flex items-center justify-between mb-3">
          <h2 className="text-sm font-semibold text-white">{t('rag.collections')}</h2>
          <button
            onClick={() => setActiveTab('collections')}
            className="text-xs text-[var(--primary)] hover:underline"
          >
            {t('rag.view_all')}
          </button>
        </div>

        {collections.length === 0 && !collectionsLoading && (
          <div className="text-center py-12 text-[var(--text-muted)] text-sm">
            {t('rag.no_collections')}
          </div>
        )}

        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3">
          {collections.slice(0, 6).map((col) => (
            <button
              key={col.collection_name}
              type="button"
              onClick={() => {
                useRAGStore.getState().selectCollection(col.collection_name);
                setActiveTab('documents');
              }}
              className="text-left rounded-xl bg-white/[0.03] border border-white/5 p-4 hover:bg-white/[0.06] hover:border-[var(--primary)]/20 transition-all"
            >
              <div className="flex items-center gap-2 mb-2">
                <Database size={14} className="text-indigo-400" />
                <span className="text-sm font-medium text-white truncate">
                  {col.display_name || col.collection_name}
                </span>
              </div>
              <div className="flex items-center justify-between text-xs text-[var(--text-muted)]">
                <span>{col.document_count} docs</span>
                <span className="font-mono">{col.chunk_strategy}</span>
              </div>
            </button>
          ))}
        </div>
      </div>

      {/* 直近のインジェスト */}
      {ingestRuns.length > 0 && (
        <div>
          <div className="flex items-center justify-between mb-3">
            <h2 className="text-sm font-semibold text-white">{t('rag.recent_ingests')}</h2>
            <button
              onClick={() => setActiveTab('ingest')}
              className="text-xs text-[var(--primary)] hover:underline"
            >
              {t('rag.view_all')}
            </button>
          </div>
          <div className="space-y-1.5">
            {ingestRuns.slice(0, 3).map((run) => (
              <div
                key={run.id}
                className="flex items-center justify-between p-3 rounded-lg bg-white/[0.02] border border-white/5 text-xs"
              >
                <div className="flex items-center gap-2">
                  <span
                    className={`px-2 py-0.5 rounded-full ${
                      run.status === 'completed'
                        ? 'text-emerald-400 bg-emerald-500/10'
                        : run.status === 'failed'
                          ? 'text-rose-400 bg-rose-500/10'
                          : 'text-blue-400 bg-blue-500/10'
                    }`}
                  >
                    {run.status}
                  </span>
                  <span className="text-[var(--text-muted)] font-mono">{run.id.slice(0, 8)}</span>
                </div>
                <span className="text-[var(--text-muted)]">
                  {run.started_at ? new Date(run.started_at).toLocaleString('ja-JP') : '-'}
                </span>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

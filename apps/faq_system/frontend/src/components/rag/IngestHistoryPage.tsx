/**
 * インジェスト履歴ページ.
 *
 * RAG データソースのインジェスト実行履歴を表示する。
 */
import { useEffect } from 'react';
import { Play, RefreshCw, Clock, CheckCircle, AlertCircle, Loader2 } from 'lucide-react';
import { useI18n } from '../../i18n';
import { useRAGStore } from '../../stores/ragStore';

const STATUS_MAP: Record<string, { icon: typeof CheckCircle; color: string; bg: string }> = {
  completed: { icon: CheckCircle, color: 'text-emerald-400', bg: 'bg-emerald-500/10' },
  failed: { icon: AlertCircle, color: 'text-rose-400', bg: 'bg-rose-500/10' },
  running: { icon: Loader2, color: 'text-blue-400', bg: 'bg-blue-500/10' },
  queued: { icon: Clock, color: 'text-amber-400', bg: 'bg-amber-500/10' },
};

export function IngestHistoryPage() {
  const { t } = useI18n();
  const { ingestRuns, ingestLoading, fetchIngestRuns, triggerIngest } = useRAGStore();

  useEffect(() => {
    void fetchIngestRuns();
  }, [fetchIngestRuns]);

  return (
    <div className="space-y-4">
      {/* ヘッダー */}
      <div className="flex items-center justify-between">
        <h2 className="text-sm font-semibold text-white">{t('rag.ingest_history')}</h2>
        <div className="flex gap-2">
          <button
            onClick={() => void fetchIngestRuns()}
            disabled={ingestLoading}
            className="flex items-center gap-1.5 px-3 py-1.5 rounded-lg text-xs text-[var(--text-muted)] hover:bg-white/5 transition disabled:opacity-50"
          >
            <RefreshCw size={12} className={ingestLoading ? 'animate-spin' : ''} />
            {t('rag.refresh')}
          </button>
          <button
            onClick={() => void triggerIngest()}
            disabled={ingestLoading}
            className="flex items-center gap-1.5 px-3 py-1.5 rounded-lg bg-[var(--primary)]/10 text-[var(--primary)] text-xs font-medium border border-[var(--primary)]/20 hover:bg-[var(--primary)]/20 transition disabled:opacity-50"
          >
            <Play size={12} />
            {t('rag.trigger_ingest')}
          </button>
        </div>
      </div>

      {/* 履歴リスト */}
      {ingestRuns.length === 0 && !ingestLoading && (
        <div className="text-center py-12 text-[var(--text-muted)] text-sm">
          {t('rag.no_ingest_runs')}
        </div>
      )}

      <div data-testid="ingest-history-list" className="space-y-1.5">
        {ingestRuns.map((run) => {
          const s = STATUS_MAP[run.status] ?? STATUS_MAP.queued;
          const StatusIcon = s.icon;
          return (
            <div
              key={run.id}
              className="flex items-center justify-between p-3.5 rounded-xl bg-white/[0.02] border border-white/5"
            >
              <div className="flex items-center gap-3">
                <span className={`flex items-center gap-1.5 px-2 py-0.5 rounded-full text-xs ${s.color} ${s.bg}`}>
                  <StatusIcon size={12} className={run.status === 'running' ? 'animate-spin' : ''} />
                  {run.status}
                </span>
                <span className="text-xs text-[var(--text-muted)] font-mono">{run.id.slice(0, 12)}</span>
                <span className="text-xs text-[var(--text-muted)]">{run.trigger_mode}</span>
              </div>
              <div className="flex items-center gap-4 text-xs text-[var(--text-muted)]">
                {run.finished_at && run.started_at && (
                  <span>
                    {((new Date(run.finished_at).getTime() - new Date(run.started_at).getTime()) / 1000).toFixed(1)}s
                  </span>
                )}
                <span>{run.started_at ? new Date(run.started_at).toLocaleString('ja-JP') : '-'}</span>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}

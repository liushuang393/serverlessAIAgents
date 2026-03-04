/**
 * RAG インジェスト履歴.
 *
 * インジェスト実行のタイムラインを表示。
 */
import React from 'react';
import type { IngestRunSummary } from '../types/rag';

export interface RAGIngestHistoryProps {
  runs: IngestRunSummary[];
  onViewDetail?: (runId: string) => void;
  loading?: boolean;
}

const STATUS_COLORS: Record<string, string> = {
  queued: 'text-slate-400 bg-slate-500/10',
  running: 'text-blue-400 bg-blue-500/10',
  completed: 'text-emerald-400 bg-emerald-500/10',
  failed: 'text-rose-400 bg-rose-500/10',
};

const STATUS_LABELS: Record<string, string> = {
  queued: '待機中',
  running: '実行中',
  completed: '完了',
  failed: '失敗',
};

/** 期間のフォーマット */
function formatDuration(ms: number): string {
  if (ms < 1000) return `${ms}ms`;
  if (ms < 60_000) return `${(ms / 1000).toFixed(1)}s`;
  return `${(ms / 60_000).toFixed(1)}m`;
}

/** インジェスト履歴タイムライン */
export const RAGIngestHistory: React.FC<RAGIngestHistoryProps> = ({
  runs,
  onViewDetail,
  loading = false,
}) => {
  if (loading) {
    return (
      <div className="flex items-center justify-center py-8 text-slate-400">
        <span className="animate-spin mr-2">&#9696;</span>
        読み込み中...
      </div>
    );
  }

  if (runs.length === 0) {
    return (
      <div className="text-center py-8 text-slate-500">
        インジェスト履歴がありません
      </div>
    );
  }

  return (
    <div className="space-y-2">
      {runs.map((run) => (
        <div
          key={run.run_id}
          onClick={() => onViewDetail?.(run.run_id)}
          className={`flex items-center justify-between p-3 rounded-lg bg-slate-800/40 border border-slate-700/50 transition-colors ${
            onViewDetail ? 'cursor-pointer hover:bg-slate-800/60' : ''
          }`}
        >
          <div className="flex items-center gap-3">
            <span
              className={`text-xs px-2 py-0.5 rounded-full ${
                STATUS_COLORS[run.status] ?? ''
              }`}
            >
              {STATUS_LABELS[run.status] ?? run.status}
            </span>
            <span className="text-xs font-mono text-slate-500">
              {run.run_id.slice(0, 8)}
            </span>
            {run.dry_run && (
              <span className="text-xs px-1.5 py-0.5 rounded bg-amber-500/10 text-amber-400">
                dry-run
              </span>
            )}
          </div>
          <div className="flex items-center gap-4 text-xs text-slate-500">
            <span>{formatDuration(run.duration_ms)}</span>
            {run.started_at && (
              <span>{new Date(run.started_at).toLocaleString('ja-JP')}</span>
            )}
          </div>
        </div>
      ))}
    </div>
  );
};

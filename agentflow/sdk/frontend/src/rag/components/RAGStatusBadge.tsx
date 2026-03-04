/**
 * RAG ステータスバッジ.
 *
 * コレクション／ドキュメントの状態を色付きバッジで表示。
 */
import React from 'react';

export type RAGHealthStatus = 'healthy' | 'warning' | 'error' | 'inactive' | 'indexing';

export interface RAGStatusBadgeProps {
  status: RAGHealthStatus;
  size?: 'sm' | 'md';
}

const STATUS_CONFIG: Record<RAGHealthStatus, { bg: string; dot: string; label: string }> = {
  healthy: { bg: 'bg-emerald-500/10', dot: 'bg-emerald-400', label: '正常' },
  warning: { bg: 'bg-amber-500/10', dot: 'bg-amber-400', label: '警告' },
  error: { bg: 'bg-rose-500/10', dot: 'bg-rose-400', label: 'エラー' },
  inactive: { bg: 'bg-slate-500/10', dot: 'bg-slate-400', label: '無効' },
  indexing: { bg: 'bg-blue-500/10', dot: 'bg-blue-400', label: 'インデックス中' },
};

/** ヘルスステータスバッジ */
export const RAGStatusBadge: React.FC<RAGStatusBadgeProps> = ({
  status,
  size = 'sm',
}) => {
  const cfg = STATUS_CONFIG[status] ?? STATUS_CONFIG.inactive;
  const px = size === 'sm' ? 'px-2 py-0.5 text-xs' : 'px-3 py-1 text-sm';

  return (
    <span className={`inline-flex items-center gap-1.5 rounded-full ${cfg.bg} ${px}`}>
      <span className={`h-1.5 w-1.5 rounded-full ${cfg.dot}`} />
      <span className="text-slate-200">{cfg.label}</span>
    </span>
  );
};

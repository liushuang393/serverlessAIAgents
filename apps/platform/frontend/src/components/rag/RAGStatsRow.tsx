/**
 * RAG 統計行.
 *
 * Platform ダッシュボードで RAG 全体の統計を表示する。
 * 読み取り専用。
 */
import { useI18n } from '../../i18n';

import type { RAGStatsResponse } from '@/types';

interface Props {
  stats: RAGStatsResponse;
  appCount: number;
}

interface StatItemProps {
  icon: string;
  label: string;
  value: number | string;
  color: string;
}

function StatItem({ icon, label, value, color }: StatItemProps) {
  return (
    <div className="rounded-xl bg-slate-800/60 border border-slate-700/50 p-4 flex flex-col gap-2">
      <span className={`text-xl ${color}`}>{icon}</span>
      <p className="text-2xl font-bold text-slate-100">{value}</p>
      <p className="text-xs text-slate-400">{label}</p>
    </div>
  );
}

/** RAG 統計サマリー行 */
export function RAGStatsRow({ stats, appCount }: Props) {
  const { t } = useI18n();

  return (
    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
      <StatItem
        icon="📦"
        label={t('rag_dashboard.apps_with_rag')}
        value={appCount}
        color="text-amber-400"
      />
      <StatItem
        icon="📄"
        label={t('rag_dashboard.chunk_strategies')}
        value={stats.total_strategies}
        color="text-indigo-400"
      />
      <StatItem
        icon="🔀"
        label={t('rag_dashboard.rerankers')}
        value={stats.total_rerankers}
        color="text-cyan-400"
      />
      <StatItem
        icon="🔍"
        label={t('rag_dashboard.retrieval_methods')}
        value={stats.total_apps_using_rag}
        color="text-emerald-400"
      />
    </div>
  );
}

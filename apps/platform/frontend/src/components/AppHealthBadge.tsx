/**
 * AppHealthBadge - App ヘルスステータスバッジ.
 *
 * ステータスに応じて色とラベルを変える。
 */

import type { AppStatus } from '@/types';

/** ステータス別の表示設定 */
const STATUS_CONFIG: Record<
  AppStatus,
  { label: string; dotClass: string; bgClass: string; textClass: string }
> = {
  healthy: {
    label: 'Healthy',
    dotClass: 'bg-emerald-400',
    bgClass: 'bg-emerald-500/10 border-emerald-500/20',
    textClass: 'text-emerald-400',
  },
  unhealthy: {
    label: 'Unhealthy',
    dotClass: 'bg-red-400',
    bgClass: 'bg-red-500/10 border-red-500/20',
    textClass: 'text-red-400',
  },
  stopped: {
    label: 'Stopped',
    dotClass: 'bg-slate-500',
    bgClass: 'bg-slate-500/10 border-slate-500/20',
    textClass: 'text-slate-400',
  },
  unknown: {
    label: 'Unknown',
    dotClass: 'bg-amber-400',
    bgClass: 'bg-amber-500/10 border-amber-500/20',
    textClass: 'text-amber-400',
  },
};

interface Props {
  status: AppStatus;
}

export function AppHealthBadge({ status }: Props) {
  const cfg = STATUS_CONFIG[status] ?? STATUS_CONFIG.unknown;

  return (
    <span
      className={`inline-flex items-center gap-1.5 px-2.5 py-0.5 rounded-full text-xs font-medium border ${cfg.bgClass} ${cfg.textClass}`}
    >
      <span className={`w-1.5 h-1.5 rounded-full ${cfg.dotClass}`} />
      {cfg.label}
    </span>
  );
}


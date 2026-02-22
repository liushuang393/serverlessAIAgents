/**
 * AppHealthBadge - App ヘルスステータスバッジ.
 *
 * ステータスに応じて色とラベルを変える。
 */

import type { AppStatus } from '@/types';
import { useI18n } from '../i18n';

/** ステータス別のスタイル定義（ラベルは i18n で解決） */
const STATUS_STYLE: Record<
  AppStatus,
  { dotClass: string; bgClass: string; textClass: string }
> = {
  healthy: {
    dotClass: 'bg-emerald-400',
    bgClass: 'bg-emerald-500/10 border-emerald-500/20',
    textClass: 'text-emerald-400',
  },
  unhealthy: {
    dotClass: 'bg-red-400',
    bgClass: 'bg-red-500/10 border-red-500/20',
    textClass: 'text-red-400',
  },
  stopped: {
    dotClass: 'bg-slate-500',
    bgClass: 'bg-slate-500/10 border-slate-500/20',
    textClass: 'text-slate-400',
  },
  unknown: {
    dotClass: 'bg-amber-400',
    bgClass: 'bg-amber-500/10 border-amber-500/20',
    textClass: 'text-amber-400',
  },
};

interface Props {
  status: AppStatus;
}

export function AppHealthBadge({ status }: Props) {
  const { t } = useI18n();
  const style = STATUS_STYLE[status] ?? STATUS_STYLE.unknown;
  const label = t(`health.${status}`) || status;

  return (
    <span
      className={`inline-flex items-center gap-1.5 px-2.5 py-0.5 rounded-full text-xs font-medium border ${style.bgClass} ${style.textClass}`}
    >
      <span className={`w-1.5 h-1.5 rounded-full ${style.dotClass}`} />
      {label}
    </span>
  );
}


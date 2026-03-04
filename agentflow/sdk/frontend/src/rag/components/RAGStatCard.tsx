/**
 * RAG 統計カード.
 *
 * ダッシュボード上で主要指標を表示する。
 */
import React from 'react';

export interface RAGStatCardProps {
  /** アイコン（絵文字 or React ノード） */
  icon: React.ReactNode;
  /** ラベル */
  label: string;
  /** 値 */
  value: string | number;
  /** カラーテーマ（Tailwind クラス名） */
  color?: string;
  /** トレンド情報 */
  trend?: { direction: 'up' | 'down' | 'flat'; value: string };
}

/** 統計表示カード */
export const RAGStatCard: React.FC<RAGStatCardProps> = ({
  icon,
  label,
  value,
  color = 'text-indigo-400',
  trend,
}) => (
  <div className="rounded-xl bg-slate-800/60 border border-slate-700/50 p-4 flex flex-col gap-2">
    <div className="flex items-center justify-between">
      <span className={`text-xl ${color}`}>{icon}</span>
      {trend && (
        <span
          className={`text-xs font-medium ${
            trend.direction === 'up'
              ? 'text-emerald-400'
              : trend.direction === 'down'
                ? 'text-rose-400'
                : 'text-slate-400'
          }`}
        >
          {trend.direction === 'up' ? '+' : trend.direction === 'down' ? '-' : ''}
          {trend.value}
        </span>
      )}
    </div>
    <p className="text-2xl font-bold text-slate-100">{value}</p>
    <p className="text-xs text-slate-400">{label}</p>
  </div>
);

/**
 * Dashboard - 統計ダッシュボード.
 *
 * App 総数・Agent 総数・ステータス概要を表示。
 */

import { useEffect } from 'react';
import { Link } from 'react-router-dom';
import { useAppStore } from '@/store/useAppStore';
import { AppHealthBadge } from './AppHealthBadge';
import { useI18n } from '../i18n';

export function Dashboard() {
  const { t } = useI18n();
  const { apps, totalApps, summary, loading, error, loadApps, loadSummary } =
    useAppStore();

  useEffect(() => {
    loadApps();
    loadSummary();
  }, [loadApps, loadSummary]);

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div>
        <h1 className="text-2xl font-bold text-slate-100">{t('dash.title')}</h1>
        <p className="text-sm text-slate-500 mt-1">
          {t('dash.subtitle')}
        </p>
      </div>

      {/* エラー表示 */}
      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 text-red-400 text-sm">
          {error}
        </div>
      )}

      {/* ローディング */}
      {loading && (
        <div className="flex justify-center py-12">
          <div className="w-10 h-10 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin" />
        </div>
      )}

      {/* 統計カード */}
      {!loading && (
        <>
          <div className="grid grid-cols-1 sm:grid-cols-3 gap-4">
            <StatCard
              icon="📦"
              label={t('dash.total_apps')}
              value={totalApps}
              color="indigo"
            />
            <StatCard
              icon="🤖"
              label={t('dash.total_agents')}
              value={summary?.total_agents ?? 0}
              color="emerald"
            />
            <StatCard
              icon="🔌"
              label={t('dash.with_api')}
              value={apps.filter((a) => a.ports.api !== null).length}
              color="amber"
            />
          </div>

          {/* App 一覧テーブル */}
          <div className="bg-slate-900/50 border border-slate-800 rounded-xl overflow-hidden">
            <div className="px-5 py-3.5 border-b border-slate-800 flex items-center justify-between">
              <h2 className="text-sm font-semibold text-slate-200">
                {t('dash.registered_apps')}
              </h2>
              <Link
                to="/apps"
                className="text-xs text-indigo-400 hover:text-indigo-300 transition-colors"
              >
                {t('dash.view_all')}
              </Link>
            </div>
            <div className="divide-y divide-slate-800/50">
              {apps.map((app) => {
                const backendUrl = app.ports.api ? `http://localhost:${app.ports.api}` : app.urls?.backend;
                const frontendUrl = app.ports.frontend ? `http://localhost:${app.ports.frontend}` : app.urls?.frontend;
                return (
                  <Link
                    key={app.name}
                    to={`/apps/${app.name}`}
                    className="flex items-center gap-4 px-5 py-3 hover:bg-slate-800/30 transition-colors"
                  >
                  <span className="text-2xl">{app.icon}</span>
                  <div className="flex-1 min-w-0">
                    <p className="text-sm font-medium text-slate-200 truncate">
                      {app.display_name}
                    </p>
                    <p className="text-xs text-slate-500">{app.name}</p>
                    <div className="mt-1 space-y-0.5">
                      {backendUrl && (
                        <p className="text-[11px] text-slate-500 font-mono truncate">
                          API: {backendUrl}
                        </p>
                      )}
                      {frontendUrl && (
                        <p className="text-[11px] text-slate-500 font-mono truncate">
                          FE: {frontendUrl}
                        </p>
                      )}
                      {app.urls?.database && (
                        <p className="text-[11px] text-slate-500 font-mono truncate">
                          DB: {app.urls.database}
                        </p>
                      )}
                    </div>
                  </div>
                  <span className="text-xs text-slate-500">
                    {t('dash.agents_count').replace(/\{count\}/g, String(app.agent_count))}
                  </span>
                  <AppHealthBadge status={app.status} />
                  </Link>
                );
              })}
              {apps.length === 0 && (
                <p className="px-5 py-8 text-center text-sm text-slate-500">
                  {t('dash.no_apps')}
                </p>
              )}
            </div>
          </div>
        </>
      )}
    </div>
  );
}

/** 統計カード */
function StatCard({
  icon,
  label,
  value,
  color,
}: {
  icon: string;
  label: string;
  value: number;
  color: 'indigo' | 'emerald' | 'amber';
}) {
  const borderColor = {
    indigo: 'border-indigo-500/20',
    emerald: 'border-emerald-500/20',
    amber: 'border-amber-500/20',
  }[color];
  const valueColor = {
    indigo: 'text-indigo-400',
    emerald: 'text-emerald-400',
    amber: 'text-amber-400',
  }[color];

  return (
    <div
      className={`bg-slate-900/50 border ${borderColor} rounded-xl p-5`}
    >
      <div className="flex items-center gap-3 mb-2">
        <span className="text-xl">{icon}</span>
        <span className="text-xs text-slate-500 uppercase tracking-wider">
          {label}
        </span>
      </div>
      <p className={`text-3xl font-bold ${valueColor}`}>{value}</p>
    </div>
  );
}

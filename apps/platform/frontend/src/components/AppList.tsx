/**
 * AppList - App 一覧（カードグリッド表示）.
 *
 * 全登録 App をカード形式で表示。再スキャンボタン付き。
 */

import { useEffect, useMemo, useState } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { fetchPortConflicts, rebalancePorts } from '@/api/client';
import { useAppStore } from '@/store/useAppStore';
import type { AppStatus, PortConflictReport } from '@/types';
import { AppHealthBadge } from './AppHealthBadge';
import { AppCreateModal } from './AppCreateModal';
import { useI18n } from '../i18n';

export function AppList() {
  const { t } = useI18n();
  const navigate = useNavigate();
  const { apps, totalApps, loading, error, loadApps, refresh, clearError } =
    useAppStore();
  const [refreshing, setRefreshing] = useState(false);
  const [rebalancing, setRebalancing] = useState(false);
  const [createOpen, setCreateOpen] = useState(false);
  const [conflicts, setConflicts] = useState<PortConflictReport | null>(null);
  const [keyword, setKeyword] = useState('');
  const [statusFilter, setStatusFilter] = useState<'all' | AppStatus>('all');
  const [sortKey, setSortKey] = useState<'name' | 'api' | 'frontend'>('name');

  useEffect(() => {
    loadApps();
    fetchPortConflicts()
      .then((report) => setConflicts(report))
      .catch(() => {
        setConflicts(null);
      });
  }, [loadApps]);

  /** 再スキャン */
  const handleRefresh = async () => {
    setRefreshing(true);
    try {
      await refresh();
      const report = await fetchPortConflicts();
      setConflicts(report);
    } finally {
      setRefreshing(false);
    }
  };

  const handleRebalance = async () => {
    setRebalancing(true);
    try {
      await rebalancePorts(false);
      await refresh();
      const report = await fetchPortConflicts();
      setConflicts(report);
    } finally {
      setRebalancing(false);
    }
  };

  const statusCounts = useMemo(() => {
    const counts: Record<'healthy' | 'unhealthy' | 'unknown' | 'stopped', number> = {
      healthy: 0,
      unhealthy: 0,
      unknown: 0,
      stopped: 0,
    };
    for (const app of apps) {
      counts[app.status] += 1;
    }
    return counts;
  }, [apps]);

  const filteredApps = useMemo(() => {
    const lowerKeyword = keyword.trim().toLowerCase();
    const matches = apps.filter((app) => {
      if (statusFilter !== 'all' && app.status !== statusFilter) {
        return false;
      }
      if (!lowerKeyword) {
        return true;
      }
      const haystack = [
        app.name,
        app.display_name,
        app.description ?? '',
        app.tags.join(' '),
        app.urls?.backend ?? '',
        app.urls?.frontend ?? '',
      ]
        .join(' ')
        .toLowerCase();
      return haystack.includes(lowerKeyword);
    });

    return [...matches].sort((a, b) => {
      if (sortKey === 'api') {
        return (a.ports.api ?? Number.MAX_SAFE_INTEGER) - (b.ports.api ?? Number.MAX_SAFE_INTEGER);
      }
      if (sortKey === 'frontend') {
        return (a.ports.frontend ?? Number.MAX_SAFE_INTEGER) - (b.ports.frontend ?? Number.MAX_SAFE_INTEGER);
      }
      return a.display_name.localeCompare(b.display_name, 'ja');
    });
  }, [apps, keyword, statusFilter, sortKey]);

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-slate-100">{t('app_list.title')}</h1>
          <p className="text-sm text-slate-500 mt-1">
            {t('app_list.registered_count').replaceAll('{count}', String(totalApps))}
          </p>
        </div>
        <div className="flex items-center gap-2">
          <button
            onClick={() => setCreateOpen(true)}
            className="px-4 py-2 bg-emerald-600 hover:bg-emerald-700 text-white text-sm font-medium rounded-lg transition-colors"
          >
            {t('app_list.add_app')}
          </button>
          <button
            onClick={handleRefresh}
            disabled={refreshing || loading}
            className="px-4 py-2 bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 disabled:cursor-not-allowed text-white text-sm font-medium rounded-lg transition-colors flex items-center gap-2"
          >
            {refreshing ? (
              <span className="w-4 h-4 border-2 border-white/30 border-t-white rounded-full animate-spin" />
            ) : (
              <span>🔄</span>
            )}
            {t('app_list.refresh')}
          </button>
        </div>
      </div>

      <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-4 space-y-3">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
          <input
            value={keyword}
            onChange={(e) => setKeyword(e.target.value)}
            placeholder={t('app_list.search_placeholder')}
            className="input"
          />
          <select
            value={statusFilter}
            onChange={(e) => setStatusFilter(e.target.value as 'all' | AppStatus)}
            className="input"
          >
            <option value="all">{t('app_list.all_status')}</option>
            <option value="healthy">{t('app_list.healthy')}</option>
            <option value="unhealthy">{t('app_list.unhealthy')}</option>
            <option value="unknown">{t('app_list.unknown')}</option>
            <option value="stopped">{t('app_list.stopped')}</option>
          </select>
          <select
            value={sortKey}
            onChange={(e) => setSortKey(e.target.value as 'name' | 'api' | 'frontend')}
            className="input"
          >
            <option value="name">{t('app_list.sort_name')}</option>
            <option value="api">{t('app_list.sort_api')}</option>
            <option value="frontend">{t('app_list.sort_fe')}</option>
          </select>
        </div>
        <div className="flex flex-wrap gap-2 text-xs text-slate-400">
          <span className="px-2 py-1 rounded-md bg-slate-800/70">{t('app_list.healthy_count').replaceAll('{count}', String(statusCounts.healthy))}</span>
          <span className="px-2 py-1 rounded-md bg-slate-800/70">{t('app_list.unhealthy_count').replaceAll('{count}', String(statusCounts.unhealthy))}</span>
          <span className="px-2 py-1 rounded-md bg-slate-800/70">{t('app_list.unknown_count').replaceAll('{count}', String(statusCounts.unknown))}</span>
          <span className="px-2 py-1 rounded-md bg-slate-800/70">{t('app_list.stopped_count').replaceAll('{count}', String(statusCounts.stopped))}</span>
          <span className="px-2 py-1 rounded-md bg-indigo-500/10 text-indigo-300">
            {t('app_list.filtered_count').replaceAll('{count}', String(filteredApps.length))}
          </span>
        </div>
      </div>

      {/* エラー */}
      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button
            onClick={clearError}
            className="text-red-400 hover:text-red-300 text-xs"
          >
            ✕
          </button>
        </div>
      )}

      {/* ポート重複警告 */}
      {conflicts?.has_conflicts && (
        <div className="bg-amber-500/10 border border-amber-500/30 rounded-lg p-4">
          <div className="flex items-center justify-between gap-3 mb-2">
            <p className="text-amber-300 text-sm font-medium">
              {t('app_list.port_conflict')}（{conflicts.conflicts.length}）
            </p>
            <button
              onClick={handleRebalance}
              disabled={rebalancing}
              className="px-3 py-1 rounded-md bg-amber-500/20 hover:bg-amber-500/30 disabled:opacity-50 text-amber-200 text-xs"
            >
              {rebalancing ? t('app_list.resolving') : t('app_list.auto_resolve')}
            </button>
          </div>
          <div className="space-y-1">
            {conflicts.conflicts.slice(0, 4).map((item) => (
              <p key={`${item.port_type}-${item.port}`} className="text-xs text-amber-200/80">
                {item.port_type.toUpperCase()}:{item.port} → {item.apps.join(', ')}
              </p>
            ))}
          </div>
        </div>
      )}

      {/* ローディング */}
      {loading && !refreshing && (
        <div className="flex justify-center py-12">
          <div className="w-10 h-10 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin" />
        </div>
      )}

      {/* App カードグリッド */}
      {!loading && (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {filteredApps.map((app) => {
            const backendUrl = app.ports.api ? `http://localhost:${app.ports.api}` : (app.urls?.backend ?? null);
            const frontendUrl = app.ports.frontend ? `http://localhost:${app.ports.frontend}` : (app.urls?.frontend ?? null);
            return (
              <Link
                key={app.name}
                to={`/apps/${app.name}`}
                className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 hover:border-indigo-500/40 hover:bg-slate-900/80 transition-all group"
              >
              <div className="flex items-start justify-between mb-2">
                <span className="text-3xl">{app.icon}</span>
                <AppHealthBadge status={app.status} />
              </div>
              <h3 className="text-base font-semibold text-slate-200 group-hover:text-indigo-400 transition-colors">
                {app.display_name}
              </h3>
              <p className="text-xs text-slate-500 mt-0.5">{app.name}</p>
              {app.description && (
                <p className="text-xs text-slate-400 mt-2 line-clamp-2">{app.description}</p>
              )}

              <div className="flex flex-wrap items-center gap-2 text-xs text-slate-400 mt-3">
                <span>v{app.version}</span>
                <span className="px-2 py-0.5 bg-slate-800/80 rounded-md">API:{app.ports.api ?? '-'}</span>
                <span className="px-2 py-0.5 bg-slate-800/80 rounded-md">FE:{app.ports.frontend ?? '-'}</span>
                <span className="px-2 py-0.5 bg-slate-800/80 rounded-md">DB:{app.ports.db ?? '-'}</span>
                <span className="px-2 py-0.5 bg-slate-800/80 rounded-md">🤖 {app.agent_count}</span>
              </div>

                <div className="mt-3 space-y-1">
                  {backendUrl && (
                    <p className="text-[11px] text-slate-500 font-mono truncate">BE: {backendUrl}</p>
                  )}
                  {frontendUrl && (
                    <p className="text-[11px] text-slate-500 font-mono truncate">FE: {frontendUrl}</p>
                  )}
                </div>
              {app.tags.length > 0 && (
                <div className="flex flex-wrap gap-1.5 mt-3">
                  {app.tags.slice(0, 4).map((tag) => (
                    <span
                      key={tag}
                      className="px-2 py-0.5 bg-slate-800/80 text-slate-400 text-[10px] rounded-full"
                    >
                      {tag}
                    </span>
                  ))}
                  {app.tags.length > 4 && (
                    <span className="px-2 py-0.5 text-slate-500 text-[10px]">
                      +{app.tags.length - 4}
                    </span>
                  )}
                </div>
              )}
              </Link>
            );
          })}
        </div>
      )}

      {/* 空状態 */}
      {!loading && filteredApps.length === 0 && (
        <div className="text-center py-16">
          <p className="text-4xl mb-4">📭</p>
          <p className="text-slate-400">
            {apps.length === 0 ? t('app_list.no_apps') : t('app_list.no_match')}
          </p>
          <button
            onClick={handleRefresh}
            className="mt-4 text-sm text-indigo-400 hover:text-indigo-300"
          >
            {t('app_list.scan_apps')}
          </button>
        </div>
      )}

      <AppCreateModal
        open={createOpen}
        onClose={() => setCreateOpen(false)}
        onCreated={(created) => {
          refresh();
          fetchPortConflicts().then((report) => setConflicts(report)).catch(() => {});
          navigate(`/apps/${created.app_name}`);
        }}
      />
    </div>
  );
}

/**
 * AppList - App 一覧（カードグリッド表示）.
 *
 * 全登録 App をカード形式で表示。再スキャンボタン付き。
 */

import { useEffect, useMemo, useState } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { fetchPortConflicts, localStartApp, rebalancePorts } from '@/api/client';
import { useAppStore } from '@/store/useAppStore';
import type { AppStatus, PortConflictReport } from '@/types';
import { AppHealthBadge } from './AppHealthBadge';
import { AppCreateModal } from './AppCreateModal';
import { CategoryNav, type CategoryId } from './CategoryNav';
import { useI18n } from '../i18n';

const getAppCategory = (app: any): CategoryId => {
  // 1. Explicit override by name for known apps
  const MANUAL_MAP: Record<string, CategoryId> = {
    faq_system: 'core',
    market_trend_monitor: 'core',
    code_migration_assistant: 'studio',
    design_skills_engine: 'studio',
    decision_governance_engine: 'governance',
    auth_service: 'ops',
    messaging_hub: 'ops',
    orchestration_guardian: 'ops',
    platform: 'ops',
  };
  if (MANUAL_MAP[app.name]) return MANUAL_MAP[app.name];

  // 2. Map by business_base from backend
  const BASE_MAP: Record<string, CategoryId> = {
    knowledge: 'core',
    reasoning: 'core',
    interaction: 'core',
    media: 'studio',
    governance: 'governance',
    platform: 'ops',
    operations: 'ops',
    integration: 'ops',
    custom: 'daily',
  };
  if (app.business_base && BASE_MAP[app.business_base]) {
    return BASE_MAP[app.business_base];
  }

  // 3. Fallback to daily (日常作業) as requested by user
  return 'daily';
};

type SharedServiceAction = 'start';

const SHARED_SERVICE_META: Record<string, { usage: string; usedBy: string }> = {
  auth_service: {
    usage: 'Shared auth core for plugins and platform contracts (JWT/OAuth/MFA).',
    usedBy: 'FAQ System, platform contracts.auth, plugin runtime guards',
  },
  design_skills_engine: {
    usage: 'Shared generation engine used by design/image plugins and workflows.',
    usedBy: 'Design-related plugins, app-level generation pipelines',
  },
};

const isSharedServiceApp = (appName: string): boolean =>
  Object.prototype.hasOwnProperty.call(SHARED_SERVICE_META, appName);

export function AppList() {
  const { t } = useI18n();
  const navigate = useNavigate();
  const { apps, totalApps, loading, error, loadApps, refresh, clearError } =
    useAppStore();
  const [refreshing, setRefreshing] = useState(false);
  const [rebalancing, setRebalancing] = useState(false);
  const [createOpen, setCreateOpen] = useState(false);
  const [conflicts, setConflicts] = useState<PortConflictReport | null>(null);
  const [serviceActionLoading, setServiceActionLoading] = useState<Record<string, SharedServiceAction | null>>({});
  const [serviceActionMessage, setServiceActionMessage] = useState<string | null>(null);
  const [serviceActionError, setServiceActionError] = useState<string | null>(null);
  const [keyword, setKeyword] = useState('');
  const [statusFilter, setStatusFilter] = useState<'all' | AppStatus>('all');
  const [sortKey, setSortKey] = useState<'name' | 'api' | 'frontend'>('name');
  const [activeCategory, setActiveCategory] = useState<CategoryId>('all');
  const [pinnedApps, setPinnedApps] = useState<string[]>(() => {
    const saved = localStorage.getItem('agentflow_pinned_apps');
    return saved ? JSON.parse(saved) : [];
  });

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

  const handleSharedServiceAction = async (
    appName: string,
    action: SharedServiceAction,
    e: React.MouseEvent,
  ) => {
    e.preventDefault();
    e.stopPropagation();
    setServiceActionMessage(null);
    setServiceActionError(null);
    setServiceActionLoading((prev) => ({ ...prev, [appName]: action }));
    try {
      const result = await localStartApp(appName);
      if (!result.success) {
        throw new Error(result.error || result.stderr || 'start failed');
      }
      setServiceActionMessage(`${appName} service started`);
      await refresh();
    } catch (err: unknown) {
      const message = err instanceof Error ? err.message : String(err);
      setServiceActionError(`${appName} start failed: ${message}`);
    } finally {
      setServiceActionLoading((prev) => ({ ...prev, [appName]: null }));
    }
  };

  const togglePin = (name: string, e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    const next = pinnedApps.includes(name)
      ? pinnedApps.filter((n) => n !== name)
      : [...pinnedApps, name];
    setPinnedApps(next);
    localStorage.setItem('agentflow_pinned_apps', JSON.stringify(next));
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
      if (activeCategory !== 'all') {
        const cat = getAppCategory(app);
        if (cat !== activeCategory) return false;
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
      // Pinned apps come first
      const aPinned = pinnedApps.includes(a.name);
      const bPinned = pinnedApps.includes(b.name);
      if (aPinned && !bPinned) return -1;
      if (!aPinned && bPinned) return 1;

      if (sortKey === 'api') {
        return (a.ports.api ?? Number.MAX_SAFE_INTEGER) - (b.ports.api ?? Number.MAX_SAFE_INTEGER);
      }
      if (sortKey === 'frontend') {
        return (a.ports.frontend ?? Number.MAX_SAFE_INTEGER) - (b.ports.frontend ?? Number.MAX_SAFE_INTEGER);
      }
      return a.display_name.localeCompare(b.display_name, 'ja');
    });
  }, [apps, keyword, statusFilter, sortKey, activeCategory, pinnedApps]);

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-slate-100">{t('app_list.title')}</h1>
          <p className="text-sm text-slate-500 mt-1">
            {t('app_list.registered_count').replace(/{count}/g, String(totalApps))}
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

      <CategoryNav activeCategory={activeCategory} onSelect={setActiveCategory} />

      <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-4 space-y-3">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
          <div className="relative">
            <input
              value={keyword}
              onChange={(e) => setKeyword(e.target.value)}
              placeholder={t('app_list.search_placeholder')}
              className="input pl-9"
            />
            <span className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-500">🔍</span>
          </div>
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
          <span className="px-2 py-1 rounded-md bg-slate-800/70 border border-slate-700/50">
            🟢 {t('app_list.healthy_count').replace(/{count}/g, String(statusCounts.healthy))}
          </span>
          <span className="px-2 py-1 rounded-md bg-slate-800/70 border border-slate-700/50">
            🔴 {t('app_list.unhealthy_count').replace(/{count}/g, String(statusCounts.unhealthy))}
          </span>
          <span className="px-2 py-1 rounded-md bg-slate-800/70 border border-slate-700/50">
            ⚪ {t('app_list.unknown_count').replace(/{count}/g, String(statusCounts.unknown))}
          </span>
          <span className="px-2 py-1 rounded-md bg-slate-800/70 border border-slate-700/50">
            ⏹️ {t('app_list.stopped_count').replace(/{count}/g, String(statusCounts.stopped))}
          </span>
          <span className="px-2 py-1 rounded-md bg-slate-800/70 border border-slate-700/50">
            📒 {t('app_list.cat_daily')}
          </span>
          <span className="px-2 py-1 rounded-md bg-indigo-500/10 text-indigo-300 border border-indigo-500/20">
            ✨ {t('app_list.filtered_count').replace(/{count}/g, String(filteredApps.length))}
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

      {serviceActionError && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-300 text-sm">{serviceActionError}</span>
          <button
            onClick={() => setServiceActionError(null)}
            className="text-red-400 hover:text-red-300 text-xs"
          >
            ✕
          </button>
        </div>
      )}

      {serviceActionMessage && (
        <div className="bg-emerald-500/10 border border-emerald-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-emerald-300 text-sm">{serviceActionMessage}</span>
          <button
            onClick={() => setServiceActionMessage(null)}
            className="text-emerald-400 hover:text-emerald-300 text-xs"
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
            const isPinned = pinnedApps.includes(app.name);
            const sharedMeta = SHARED_SERVICE_META[app.name];
            const isSharedService = isSharedServiceApp(app.name);
            const actionLoading = serviceActionLoading[app.name];

            const cardBody = (
              <>
                {/* Decoration background */}
                <div className="absolute -right-4 -top-4 w-24 h-24 bg-indigo-500/5 rounded-full blur-2xl group-hover:bg-indigo-500/10 transition-colors" />

                <div className="flex items-start justify-between relative z-10">
                  <div className="flex items-center gap-3">
                    <span className="text-4xl filter drop-shadow-md group-hover:scale-110 transition-transform duration-300">
                      {app.icon}
                    </span>
                    <div>
                      <h3 className="text-lg font-bold text-slate-100 group-hover:text-indigo-400 transition-colors leading-tight">
                        {app.display_name}
                      </h3>
                      <p className="text-[10px] text-slate-500 font-mono mt-0.5 uppercase tracking-wider">
                        {app.name}
                      </p>
                    </div>
                  </div>
                  <div className="flex flex-col items-end gap-2">
                    <AppHealthBadge status={app.status} />
                    <button
                      onClick={(e) => togglePin(app.name, e)}
                      className={`text-lg transition-all duration-300 hover:scale-125 ${isPinned ? 'grayscale-0 opacity-100' : 'grayscale opacity-30 hover:opacity-100 hover:grayscale-0'
                        }`}
                      title={isPinned ? 'Unpin' : 'Pin to top'}
                    >
                      📌
                    </button>
                  </div>
                </div>

                {app.description && (
                  <p className="text-xs text-slate-400 mt-4 line-clamp-2 leading-relaxed min-h-[2.5rem]">
                    {app.description}
                  </p>
                )}

                {sharedMeta && (
                  <div className="mt-4 rounded-xl border border-cyan-500/20 bg-cyan-500/5 px-3 py-2.5 space-y-1.5">
                    <p className="text-[10px] uppercase tracking-wider text-cyan-300 font-semibold">
                      Shared Service (No End-User Detail View)
                    </p>
                    <p className="text-[11px] text-slate-200">{sharedMeta.usage}</p>
                    <p className="text-[11px] text-slate-400">Used by: {sharedMeta.usedBy}</p>
                  </div>
                )}

                <div className="flex flex-wrap items-center gap-1.5 text-[10px] text-slate-500 mt-4">
                  <span className="px-2 py-0.5 bg-slate-800/80 rounded-full border border-slate-700/50">v{app.version}</span>
                  {app.ports.api && (
                    <span className="px-2 py-0.5 bg-indigo-500/10 text-indigo-300/80 rounded-full border border-indigo-500/20">
                      API:{app.ports.api}
                    </span>
                  )}
                  {app.ports.frontend && (
                    <span className="px-2 py-0.5 bg-emerald-500/10 text-emerald-300/80 rounded-full border border-emerald-500/20">
                      FE:{app.ports.frontend}
                    </span>
                  )}
                  <span className="px-2 py-0.5 bg-slate-800/80 rounded-full border border-slate-700/50">🤖 {app.agent_count}</span>
                </div>

                <div className="mt-4 pt-4 border-t border-slate-800/60 flex flex-wrap gap-1.5">
                  {app.tags.length > 0 ? (
                    app.tags.slice(0, 3).map((tag) => (
                      <span
                        key={tag}
                        className="px-2 py-0.5 bg-slate-800/40 text-slate-500 text-[9px] rounded-md border border-slate-800/50"
                      >
                        {tag}
                      </span>
                    ))
                  ) : (
                    <span className="text-[9px] text-slate-600 italic">no tags</span>
                  )}
                  {app.tags.length > 3 && (
                    <span className="text-[9px] text-slate-600">+{app.tags.length - 3}</span>
                  )}
                </div>

                {isSharedService && (
                  <div className="mt-3 flex items-center gap-2">
                    <button
                      onClick={(e) => {
                        void handleSharedServiceAction(app.name, 'start', e);
                      }}
                      disabled={actionLoading !== null}
                      className="px-3 py-1.5 bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 text-white text-xs rounded-lg transition-colors"
                    >
                      {actionLoading === 'start' ? 'Starting...' : 'Start Service'}
                    </button>
                  </div>
                )}
              </>
            );

            if (isSharedService) {
              return (
                <div
                  key={app.name}
                  className="group relative bg-slate-900/40 border border-cyan-700/40 rounded-2xl p-5 hover:border-cyan-500/50 hover:bg-slate-900/80 transition-all duration-300 overflow-hidden"
                >
                  {cardBody}
                </div>
              );
            }

            return (
              <Link
                key={app.name}
                to={`/apps/${app.name}`}
                className="group relative bg-slate-900/40 border border-slate-800 rounded-2xl p-5 hover:border-indigo-500/50 hover:bg-slate-900/80 transition-all duration-300 overflow-hidden"
              >
                {cardBody}
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
          fetchPortConflicts().then((report) => setConflicts(report)).catch(() => { });
          navigate(`/apps/${created.app_name}`);
        }}
      />
    </div>
  );
}

/**
 * AppList - App 一覧（カードグリッド表示）.
 *
 * 全登録 App をカード形式で表示。再スキャンボタン付き。
 */

import { useEffect, useState } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { fetchPortConflicts, rebalancePorts } from '@/api/client';
import { useAppStore } from '@/store/useAppStore';
import type { PortConflictReport } from '@/types';
import { AppHealthBadge } from './AppHealthBadge';
import { AppCreateModal } from './AppCreateModal';

export function AppList() {
  const navigate = useNavigate();
  const { apps, totalApps, loading, error, loadApps, refresh, clearError } =
    useAppStore();
  const [refreshing, setRefreshing] = useState(false);
  const [rebalancing, setRebalancing] = useState(false);
  const [createOpen, setCreateOpen] = useState(false);
  const [conflicts, setConflicts] = useState<PortConflictReport | null>(null);

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

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-slate-100">Apps</h1>
          <p className="text-sm text-slate-500 mt-1">
            {totalApps} apps registered
          </p>
        </div>
        <div className="flex items-center gap-2">
          <button
            onClick={() => setCreateOpen(true)}
            className="px-4 py-2 bg-emerald-600 hover:bg-emerald-700 text-white text-sm font-medium rounded-lg transition-colors"
          >
            APPを追加
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
            Refresh
          </button>
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
              ポート重複を検出しました（{conflicts.conflicts.length}件）
            </p>
            <button
              onClick={handleRebalance}
              disabled={rebalancing}
              className="px-3 py-1 rounded-md bg-amber-500/20 hover:bg-amber-500/30 disabled:opacity-50 text-amber-200 text-xs"
            >
              {rebalancing ? '整理中...' : '自動整理を適用'}
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
          {apps.map((app) => (
            <Link
              key={app.name}
              to={`/apps/${app.name}`}
              className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 hover:border-indigo-500/30 hover:bg-slate-900/80 transition-all group"
            >
              <div className="flex items-start justify-between mb-3">
                <span className="text-3xl">{app.icon}</span>
                <AppHealthBadge status={app.status} />
              </div>
              <h3 className="text-base font-semibold text-slate-200 group-hover:text-indigo-400 transition-colors">
                {app.display_name}
              </h3>
              <p className="text-xs text-slate-500 mt-0.5 mb-3">{app.name}</p>
              <div className="flex items-center gap-3 text-xs text-slate-400">
                <span>v{app.version}</span>
                <span className="text-slate-700">|</span>
                <span>🤖 {app.agent_count} agents</span>
                {app.ports.api && (
                  <>
                    <span className="text-slate-700">|</span>
                    <span>API:{app.ports.api}</span>
                  </>
                )}
                <span className="text-slate-700">|</span>
                <span>🖥 {app.ports.frontend ?? '-'}</span>
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
          ))}
        </div>
      )}

      {/* 空状態 */}
      {!loading && apps.length === 0 && (
        <div className="text-center py-16">
          <p className="text-4xl mb-4">📭</p>
          <p className="text-slate-400">No apps registered yet</p>
          <button
            onClick={handleRefresh}
            className="mt-4 text-sm text-indigo-400 hover:text-indigo-300"
          >
            Scan for apps →
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

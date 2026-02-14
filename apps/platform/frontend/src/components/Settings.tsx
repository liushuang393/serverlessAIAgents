/**
 * Settings - 設定画面.
 *
 * Platform バージョン情報と再スキャンアクション。
 */

import { useState } from 'react';
import { useAppStore } from '@/store/useAppStore';

export function Settings() {
  const { refresh, totalApps, error, clearError } = useAppStore();
  const [refreshing, setRefreshing] = useState(false);
  const [message, setMessage] = useState<string | null>(null);

  const handleRefresh = async () => {
    setRefreshing(true);
    setMessage(null);
    try {
      await refresh();
      setMessage('App 一覧を再スキャンしました');
    } finally {
      setRefreshing(false);
    }
  };

  return (
    <div className="p-6 max-w-3xl mx-auto space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-slate-100">Settings</h1>
        <p className="text-sm text-slate-500 mt-1">
          Platform の設定と管理
        </p>
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

      {/* 成功メッセージ */}
      {message && (
        <div className="bg-emerald-500/10 border border-emerald-500/20 rounded-lg p-4 text-emerald-400 text-sm">
          {message}
        </div>
      )}

      {/* Platform 情報 */}
      <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
        <h2 className="text-sm font-semibold text-slate-200">Platform Info</h2>
        <div className="grid grid-cols-2 gap-4 text-sm">
          <div>
            <p className="text-[10px] text-slate-500 uppercase tracking-wider mb-0.5">
              Version
            </p>
            <p className="text-slate-300">2.0.0</p>
          </div>
          <div>
            <p className="text-[10px] text-slate-500 uppercase tracking-wider mb-0.5">
              Registered Apps
            </p>
            <p className="text-slate-300">{totalApps}</p>
          </div>
          <div>
            <p className="text-[10px] text-slate-500 uppercase tracking-wider mb-0.5">
              Backend
            </p>
            <p className="text-slate-300">FastAPI (port 8000)</p>
          </div>
          <div>
            <p className="text-[10px] text-slate-500 uppercase tracking-wider mb-0.5">
              Frontend
            </p>
            <p className="text-slate-300">React + Tailwind (port 3000)</p>
          </div>
        </div>
      </div>

      {/* アクション */}
      <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
        <h2 className="text-sm font-semibold text-slate-200">Actions</h2>
        <div className="space-y-3">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-slate-300">Refresh App Registry</p>
              <p className="text-xs text-slate-500">
                apps/*/app_config.json を再スキャンして App 一覧を更新
              </p>
            </div>
            <button
              onClick={handleRefresh}
              disabled={refreshing}
              className="px-4 py-2 bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 disabled:cursor-not-allowed text-white text-sm font-medium rounded-lg transition-colors"
            >
              {refreshing ? 'Scanning...' : 'Refresh'}
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}


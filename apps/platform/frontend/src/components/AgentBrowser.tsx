/**
 * AgentBrowser - 全 App 横断 Agent 一覧.
 *
 * Capability フィルタ付きで Agent を表示。
 */

import { useEffect, useState } from 'react';
import { useAppStore } from '@/store/useAppStore';

export function AgentBrowser() {
  const {
    agents,
    capabilities,
    loading,
    error,
    loadAgents,
    searchAgentsByCapability,
    clearError,
  } = useAppStore();

  const [selectedCap, setSelectedCap] = useState<string>('');

  useEffect(() => {
    loadAgents();
  }, [loadAgents]);

  /** Capability フィルタ適用 */
  const handleFilter = (cap: string) => {
    setSelectedCap(cap);
    if (cap === '') {
      loadAgents();
    } else {
      searchAgentsByCapability(cap);
    }
  };

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div>
        <h1 className="text-2xl font-bold text-slate-100">Agent Browser</h1>
        <p className="text-sm text-slate-500 mt-1">
          全 App 横断の Agent 一覧（{agents.length} agents）
        </p>
      </div>

      {/* エラー */}
      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button onClick={clearError} className="text-red-400 hover:text-red-300 text-xs">✕</button>
        </div>
      )}

      {/* Capability フィルタ */}
      {capabilities.length > 0 && (
        <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-4">
          <p className="text-xs text-slate-500 uppercase tracking-wider mb-3">
            Filter by Capability
          </p>
          <div className="flex flex-wrap gap-2">
            <button
              onClick={() => handleFilter('')}
              className={`px-3 py-1.5 text-xs rounded-full transition-colors ${
                selectedCap === ''
                  ? 'bg-indigo-600 text-white'
                  : 'bg-slate-800 text-slate-400 hover:bg-slate-700'
              }`}
            >
              All
            </button>
            {capabilities.map((cap) => (
              <button
                key={cap.id}
                onClick={() => handleFilter(cap.id)}
                className={`px-3 py-1.5 text-xs rounded-full transition-colors ${
                  selectedCap === cap.id
                    ? 'bg-indigo-600 text-white'
                    : 'bg-slate-800 text-slate-400 hover:bg-slate-700'
                }`}
              >
                {cap.label}
                <span className="ml-1.5 opacity-60">({cap.count})</span>
              </button>
            ))}
          </div>
        </div>
      )}

      {/* ローディング */}
      {loading && (
        <div className="flex justify-center py-12">
          <div className="w-10 h-10 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin" />
        </div>
      )}

      {/* Agent 一覧テーブル */}
      {!loading && agents.length > 0 && (
        <div className="bg-slate-900/50 border border-slate-800 rounded-xl overflow-hidden">
          <div className="px-5 py-3.5 border-b border-slate-800">
            <h2 className="text-sm font-semibold text-slate-200">Agents</h2>
          </div>
          <div className="divide-y divide-slate-800/50">
            {agents.map((agent, idx) => (
              <div key={`${agent.app_name}-${agent.name}-${idx}`} className="px-5 py-3.5 hover:bg-slate-800/30 transition-colors">
                <div className="flex items-center gap-4">
                  <span className="text-2xl">{agent.app_icon}</span>
                  <div className="flex-1 min-w-0">
                    <p className="text-sm font-medium text-slate-200">{agent.name}</p>
                    <p className="text-xs text-slate-500">{agent.app_display_name}</p>
                    {agent.module && (
                      <p className="text-[10px] text-slate-600 font-mono mt-0.5">{agent.module}</p>
                    )}
                  </div>
                  <div className="flex flex-wrap gap-1.5 max-w-xs">
                    {agent.capabilities.map((cap) => (
                      <span
                        key={cap.id}
                        onClick={() => handleFilter(cap.id)}
                        className="px-2 py-0.5 bg-indigo-500/10 text-indigo-400 text-[10px] rounded-full cursor-pointer hover:bg-indigo-500/20 transition-colors"
                      >
                        {cap.label}
                      </span>
                    ))}
                  </div>
                </div>
                {agent.capabilities_legacy.length > 0 && (
                  <p className="text-[10px] text-slate-600 mt-2">
                    legacy: {agent.capabilities_legacy.join(', ')}
                  </p>
                )}
              </div>
            ))}
          </div>
        </div>
      )}

      {/* 空状態 */}
      {!loading && agents.length === 0 && (
        <div className="text-center py-16">
          <p className="text-4xl mb-4">🤖</p>
          <p className="text-slate-400">No agents found</p>
        </div>
      )}
    </div>
  );
}

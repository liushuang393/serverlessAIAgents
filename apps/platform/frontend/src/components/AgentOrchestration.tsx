/**
 * AgentOrchestration - Agent 編排（オーケストレーション）可視化.
 *
 * App 別のエンジンパターンと Agent 間のフロー構成を表示。
 * 各 App のオーケストレーション構成をビジュアルカードで表現する。
 */

import { useEffect, useState } from 'react';
import { fetchAgentsByApp, fetchAgentStats } from '@/api/client';
import type { AgentGroup, AgentStatsResponse } from '@/types';
import { useI18n } from '../i18n';

/** エンジンパターンのスタイル定義（ラベル・説明は i18n で解決） */
const ENGINE_PATTERN_STYLE: Record<string, { icon: string; color: string }> = {
  simple: {
    icon: '⚡',
    color: 'from-emerald-500/20 to-emerald-600/5 border-emerald-500/30',
  },
  flow: {
    icon: '🔀',
    color: 'from-blue-500/20 to-blue-600/5 border-blue-500/30',
  },
  pipeline: {
    icon: '🔗',
    color: 'from-amber-500/20 to-amber-600/5 border-amber-500/30',
  },
  coordinator: {
    icon: '🎯',
    color: 'from-purple-500/20 to-purple-600/5 border-purple-500/30',
  },
  deep_agent: {
    icon: '🧠',
    color: 'from-rose-500/20 to-rose-600/5 border-rose-500/30',
  },
  custom: {
    icon: '🛠️',
    color: 'from-slate-500/20 to-slate-600/5 border-slate-500/30',
  },
};

/** Agent 役割バッジの色 */
const ROLE_COLORS: Record<string, string> = {
  planner: 'bg-purple-500/15 text-purple-400 ring-purple-500/20',
  specialist: 'bg-emerald-500/15 text-emerald-400 ring-emerald-500/20',
  reactor: 'bg-blue-500/15 text-blue-400 ring-blue-500/20',
  gatekeeper: 'bg-red-500/15 text-red-400 ring-red-500/20',
  reviewer: 'bg-cyan-500/15 text-cyan-400 ring-cyan-500/20',
  executor: 'bg-orange-500/15 text-orange-400 ring-orange-500/20',
  router: 'bg-indigo-500/15 text-indigo-400 ring-indigo-500/20',
  reporter: 'bg-teal-500/15 text-teal-400 ring-teal-500/20',
  custom: 'bg-slate-500/15 text-slate-400 ring-slate-500/20',
};

export function AgentOrchestration() {
  const { t } = useI18n();
  const [groups, setGroups] = useState<AgentGroup[]>([]);
  const [stats, setStats] = useState<AgentStatsResponse | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [expandedApp, setExpandedApp] = useState<string | null>(null);
  const [filterEngine, setFilterEngine] = useState<string>('');

  useEffect(() => {
    const load = async () => {
      setLoading(true);
      setError(null);
      try {
        const [byApp, agentStats] = await Promise.all([
          fetchAgentsByApp(),
          fetchAgentStats(),
        ]);
        setGroups(byApp.groups);
        setStats(agentStats);
      } catch (err) {
        const message = err instanceof Error ? err.message : t('orch.error_load');
        setError(message);
      } finally {
        setLoading(false);
      }
    };
    void load();
  }, []);

  /** エンジンパターンの推定（Appグループ内の最初のAgentから取得） */
  const getEnginePattern = (group: AgentGroup): string => {
    const first = group.agents[0];
    return first?.app_engine_pattern ?? 'custom';
  };

  /** フィルタ適用後のグループ */
  const filteredGroups = filterEngine
    ? groups.filter((g) => getEnginePattern(g) === filterEngine)
    : groups;

  /** エンジンパターン統計 */
  const engineStats = groups.reduce<Record<string, number>>((acc, g) => {
    const pattern = getEnginePattern(g);
    acc[pattern] = (acc[pattern] ?? 0) + 1;
    return acc;
  }, {});

  return (
    <div className="p-6 max-w-7xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div>
        <h1 className="text-2xl font-bold text-slate-100">{t('orch.title')}</h1>
        <p className="text-sm text-slate-500 mt-1">
          {t('orch.subtitle')}
        </p>
      </div>

      {/* エラー */}
      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button onClick={() => setError(null)} className="text-red-400 hover:text-red-300 text-xs">✕</button>
        </div>
      )}

      {/* サマリーカード */}
      {stats && !loading && (
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
          <SummaryCard label={t('orch.total_apps')} value={stats.total_apps_with_agents} icon="📦" />
          <SummaryCard label={t('orch.total_agents')} value={stats.total_agents} icon="🤖" />
          <SummaryCard label={t('orch.capabilities')} value={stats.total_capabilities} icon="🎯" />
          <SummaryCard label={t('orch.engine_patterns')} value={Object.keys(engineStats).length} icon="⚙️" />
        </div>
      )}

      {/* エンジンパターン フィルタ */}
      {!loading && Object.keys(engineStats).length > 0 && (
        <EngineFilter
          engineStats={engineStats}
          selected={filterEngine}
          onSelect={setFilterEngine}
        />
      )}

      {/* ローディング */}
      {loading && (
        <div className="flex justify-center py-16">
          <div className="w-10 h-10 border-4 border-purple-500/30 border-t-purple-500 rounded-full animate-spin" />
        </div>
      )}

      {/* App グループ一覧 */}
      {!loading && filteredGroups.length === 0 && !error && (
        <div className="text-center py-16 text-slate-500 text-sm">
          <p className="text-4xl mb-3">🔍</p>
          {t('orch.no_agents')}
        </div>
      )}

      <div className="space-y-4">
        {filteredGroups.map((group) => {
          const pattern = getEnginePattern(group);
          const style = ENGINE_PATTERN_STYLE[pattern] ?? ENGINE_PATTERN_STYLE.custom;
          const isExpanded = expandedApp === group.app_name;

          return (
            <div
              key={group.app_name}
              className={`border rounded-xl bg-gradient-to-br ${style.color} transition-all`}
            >
              {/* App ヘッダー */}
              <button
                onClick={() => setExpandedApp(isExpanded ? null : group.app_name)}
                className="w-full flex items-center gap-4 p-4 text-left"
              >
                <span className="text-2xl">{group.icon || '📦'}</span>
                <div className="flex-1 min-w-0">
                  <div className="flex items-center gap-2">
                    <h3 className="text-sm font-bold text-slate-100 truncate">
                      {group.display_name}
                    </h3>
                    <span className="text-xs px-2 py-0.5 rounded-full bg-slate-800/60 text-slate-400">
                      {style.icon} {t(`orch.engine_${pattern}`) || pattern}
                    </span>
                  </div>
                  <p className="text-xs text-slate-500 mt-0.5">{t(`orch.engine_${pattern}_desc`) || ''}</p>
                </div>
                <div className="flex items-center gap-3">
                  <span className="text-xs text-slate-400">
                    {group.agents.length} agent{group.agents.length !== 1 ? 's' : ''}
                  </span>
                  <span className={`text-slate-500 transition-transform ${isExpanded ? 'rotate-180' : ''}`}>
                    ▾
                  </span>
                </div>
              </button>

              {/* Agent 一覧 (展開時) */}
              {isExpanded && (
                <div className="border-t border-slate-800/50 p-4 space-y-2">
                  {group.agents.map((agent) => {
                    const role = agent.agent_type ?? 'specialist';
                    const roleColor = ROLE_COLORS[role] ?? ROLE_COLORS.custom;
                    return (
                      <div
                        key={agent.name}
                        className="flex items-center gap-3 p-3 bg-slate-900/40 rounded-lg"
                      >
                        <span className="text-base">🤖</span>
                        <div className="flex-1 min-w-0">
                          <p className="text-sm font-medium text-slate-200 truncate">{agent.name}</p>
                          {agent.module && (
                            <p className="text-[10px] text-slate-600 font-mono truncate">{agent.module}</p>
                          )}
                        </div>
                        <span className={`text-[10px] px-2 py-0.5 rounded-full ring-1 ${roleColor}`}>
                          {role}
                        </span>
                        {agent.capabilities.length > 0 && (
                          <div className="flex gap-1 flex-wrap max-w-[200px]">
                            {agent.capabilities.slice(0, 3).map((cap) => (
                              <span key={cap.id} className="text-[10px] px-1.5 py-0.5 bg-slate-800 text-slate-400 rounded">
                                {cap.label}
                              </span>
                            ))}
                            {agent.capabilities.length > 3 && (
                              <span className="text-[10px] text-slate-600">+{agent.capabilities.length - 3}</span>
                            )}
                          </div>
                        )}
                      </div>
                    );
                  })}
                </div>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
}

/* ================================================================
 * サブコンポーネント
 * ================================================================ */

/** サマリーカード */
function SummaryCard({ label, value, icon }: { label: string; value: number; icon: string }) {
  return (
    <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-4 flex items-center gap-3">
      <span className="text-2xl">{icon}</span>
      <div>
        <p className="text-xl font-bold text-slate-100">{value}</p>
        <p className="text-[10px] text-slate-500 uppercase tracking-wider">{label}</p>
      </div>
    </div>
  );
}

/** エンジンパターン フィルタバー */
function EngineFilter({
  engineStats,
  selected,
  onSelect,
}: {
  engineStats: Record<string, number>;
  selected: string;
  onSelect: (v: string) => void;
}) {
  const { t } = useI18n();
  return (
    <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-4">
      <p className="text-xs text-slate-500 uppercase tracking-wider mb-3">{t('orch.engine_patterns')}</p>
      <div className="flex flex-wrap gap-2">
        <button
          onClick={() => onSelect('')}
          className={`text-xs px-3 py-1.5 rounded-full border transition-colors ${
            selected === ''
              ? 'bg-indigo-600/20 border-indigo-500/40 text-indigo-400'
              : 'border-slate-700 text-slate-400 hover:border-slate-600'
          }`}
        >
          {t('orch.all')}
        </button>
        {Object.entries(engineStats).map(([pattern, count]) => {
          const style = ENGINE_PATTERN_STYLE[pattern] ?? ENGINE_PATTERN_STYLE.custom;
          return (
            <button
              key={pattern}
              onClick={() => onSelect(selected === pattern ? '' : pattern)}
              className={`text-xs px-3 py-1.5 rounded-full border transition-colors ${
                selected === pattern
                  ? 'bg-indigo-600/20 border-indigo-500/40 text-indigo-400'
                  : 'border-slate-700 text-slate-400 hover:border-slate-600'
              }`}
            >
              {style.icon} {t(`orch.engine_${pattern}`) || pattern} ({count})
            </button>
          );
        })}
      </div>
    </div>
  );
}

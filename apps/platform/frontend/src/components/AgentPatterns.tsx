/**
 * AgentPatterns - Agent パターンカタログ.
 *
 * Planning / Loop / Human-in-the-Loop / ReAct 等の設計パターンを
 * 一覧表示し、各パターンに対応する実 Agent を表示する。
 */

import { useEffect, useState } from 'react';
import { fetchAgentsByPattern, fetchAgentsByBusinessBase } from '@/api/client';
import type {
  AgentPatternGroup,
  AgentBusinessBaseGroup,
  AggregatedAgent,
} from '@/types';

/* ================================================================
 * パターン定義（静的メタデータ）
 * ================================================================ */

interface PatternMeta {
  readonly label: string;
  readonly icon: string;
  readonly gradient: string;
  readonly description: string;
  readonly traits: readonly string[];
}

/** Agent パターンの表示メタ */
const PATTERN_CATALOG: Readonly<Record<string, PatternMeta>> = {
  specialist: {
    label: 'Specialist',
    icon: '🎯',
    gradient: 'from-emerald-500/20 to-emerald-600/5 border-emerald-500/30',
    description: '特定ドメインに特化。単一責務で高精度な処理を行う。',
    traits: ['Single Responsibility', 'Domain Expert', 'High Accuracy'],
  },
  coordinator: {
    label: 'Coordinator / Planning',
    icon: '🧠',
    gradient: 'from-purple-500/20 to-purple-600/5 border-purple-500/30',
    description: 'タスクを分析・分解し、子 Agent に動的委譲。計画→実行→振り返りのループ。',
    traits: ['Planning', 'Delegation', 'Recursive Reasoning'],
  },
  pipeline_stage: {
    label: 'Pipeline Stage',
    icon: '🔗',
    gradient: 'from-amber-500/20 to-amber-600/5 border-amber-500/30',
    description: '直列処理の1ステージ。前段出力→変換→次段入力の連鎖構造。',
    traits: ['Sequential', 'Transform', 'Chain'],
  },
  gatekeeper: {
    label: 'Gatekeeper / HITL',
    icon: '🛡️',
    gradient: 'from-red-500/20 to-red-600/5 border-red-500/30',
    description: 'ポリシー適合チェック・人間介入ゲート。承認フロー制御。',
    traits: ['Human-in-the-Loop', 'Policy Check', 'Approval Gate'],
  },
  reviewer: {
    label: 'Reviewer / Loop',
    icon: '🔄',
    gradient: 'from-cyan-500/20 to-cyan-600/5 border-cyan-500/30',
    description: '品質検証・フィードバックループ。基準未達なら再実行を指示。',
    traits: ['Quality Loop', 'Feedback', 'Iterative Refinement'],
  },
  analyzer: {
    label: 'Analyzer / ReAct',
    icon: '🔬',
    gradient: 'from-blue-500/20 to-blue-600/5 border-blue-500/30',
    description: '観察→推論→行動のReActサイクル。データ分析・診断に特化。',
    traits: ['ReAct Pattern', 'Observe-Think-Act', 'Diagnostic'],
  },
  executor: {
    label: 'Executor / Tool-Use',
    icon: '⚡',
    gradient: 'from-orange-500/20 to-orange-600/5 border-orange-500/30',
    description: '外部ツール・API呼び出しによる実行。変換・合成・修正操作。',
    traits: ['Tool Invocation', 'Side Effects', 'Action Execution'],
  },
  router: {
    label: 'Router / Dispatch',
    icon: '🔀',
    gradient: 'from-indigo-500/20 to-indigo-600/5 border-indigo-500/30',
    description: 'インテント判定＋適切な Agent へのルーティング。',
    traits: ['Intent Classification', 'Dynamic Routing', 'Dispatch'],
  },
  reporter: {
    label: 'Reporter / Summary',
    icon: '📊',
    gradient: 'from-teal-500/20 to-teal-600/5 border-teal-500/30',
    description: '結果集約・レポート生成。実行結果を整理して最終出力。',
    traits: ['Aggregation', 'Report Generation', 'Final Output'],
  },
  custom: {
    label: 'Custom',
    icon: '🛠️',
    gradient: 'from-slate-500/20 to-slate-600/5 border-slate-500/30',
    description: 'カスタムパターン。プロジェクト固有のロジック。',
    traits: ['Custom Logic', 'Project Specific'],
  },
};

/* ================================================================
 * ビュー切替タブ定義
 * ================================================================ */
type ViewTab = 'patterns' | 'business';

/* ================================================================
 * メインコンポーネント
 * ================================================================ */
export function AgentPatterns() {
  const [patternGroups, setPatternGroups] = useState<AgentPatternGroup[]>([]);
  const [businessGroups, setBusinessGroups] = useState<AgentBusinessBaseGroup[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [activeTab, setActiveTab] = useState<ViewTab>('patterns');
  const [expandedPattern, setExpandedPattern] = useState<string | null>(null);

  useEffect(() => {
    const load = async () => {
      setLoading(true);
      setError(null);
      try {
        const [byPattern, byBusiness] = await Promise.all([
          fetchAgentsByPattern(),
          fetchAgentsByBusinessBase(),
        ]);
        setPatternGroups(byPattern.groups);
        setBusinessGroups(byBusiness.groups);
      } catch (err) {
        const message = err instanceof Error ? err.message : 'パターン情報の取得に失敗';
        setError(message);
      } finally {
        setLoading(false);
      }
    };
    void load();
  }, []);

  /** パターン別の Agent 数マップ */
  const patternCounts = patternGroups.reduce<Record<string, number>>((acc, g) => {
    acc[g.pattern] = g.count;
    return acc;
  }, {});

  const totalAgents = patternGroups.reduce((s, g) => s + g.count, 0);

  return (
    <div className="p-6 max-w-7xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div>
        <h1 className="text-2xl font-bold text-slate-100">Agent Patterns</h1>
        <p className="text-sm text-slate-500 mt-1">
          設計パターンカタログ — {Object.keys(patternCounts).length} patterns / {totalAgents} agents
        </p>
      </div>

      {/* エラー */}
      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button onClick={() => setError(null)} className="text-red-400 hover:text-red-300 text-xs">✕</button>
        </div>
      )}

      {/* タブ切替 */}
      <div className="flex gap-2">
        {(['patterns', 'business'] as const).map((tab) => (
          <button
            key={tab}
            onClick={() => setActiveTab(tab)}
            className={`text-xs px-4 py-2 rounded-lg border transition-colors ${
              activeTab === tab
                ? 'bg-indigo-600/20 border-indigo-500/40 text-indigo-400 font-medium'
                : 'border-slate-700 text-slate-400 hover:border-slate-600'
            }`}
          >
            {tab === 'patterns' ? '🧩 Design Patterns' : '🏢 Business Base'}
          </button>
        ))}
      </div>

      {/* ローディング */}
      {loading && (
        <div className="flex justify-center py-16">
          <div className="w-10 h-10 border-4 border-purple-500/30 border-t-purple-500 rounded-full animate-spin" />
        </div>
      )}

      {/* パターンビュー */}
      {!loading && activeTab === 'patterns' && (
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {Object.entries(PATTERN_CATALOG).map(([key, meta]) => {
            const group = patternGroups.find((g) => g.pattern === key);
            const count = group?.count ?? 0;
            const isExpanded = expandedPattern === key;

            return (
              <div
                key={key}
                className={`border rounded-xl bg-gradient-to-br ${meta.gradient} transition-all`}
              >
                <button
                  onClick={() => setExpandedPattern(isExpanded ? null : key)}
                  className="w-full text-left p-4"
                >
                  <div className="flex items-start gap-3">
                    <span className="text-2xl mt-0.5">{meta.icon}</span>
                    <div className="flex-1 min-w-0">
                      <div className="flex items-center gap-2">
                        <h3 className="text-sm font-bold text-slate-100">{meta.label}</h3>
                        <span className="text-[10px] px-1.5 py-0.5 rounded-full bg-slate-800/60 text-slate-400">
                          {count} agent{count !== 1 ? 's' : ''}
                        </span>
                      </div>
                      <p className="text-xs text-slate-400 mt-1">{meta.description}</p>
                      <div className="flex flex-wrap gap-1.5 mt-2">
                        {meta.traits.map((t) => (
                          <span key={t} className="text-[10px] px-2 py-0.5 bg-slate-800/40 text-slate-500 rounded-full">
                            {t}
                          </span>
                        ))}
                      </div>
                    </div>
                  </div>
                </button>

                {/* 展開: 対応 Agent 一覧 */}
                {isExpanded && group && group.agents.length > 0 && (
                  <div className="border-t border-slate-800/50 p-3 space-y-1.5">
                    {group.agents.map((agent: AggregatedAgent) => (
                      <div key={agent.name} className="flex items-center gap-2 p-2 bg-slate-900/40 rounded-lg">
                        <span className="text-sm">🤖</span>
                        <div className="flex-1 min-w-0">
                          <p className="text-xs font-medium text-slate-200 truncate">{agent.name}</p>
                          <p className="text-[10px] text-slate-600 truncate">{agent.app_display_name}</p>
                        </div>
                        {agent.capabilities.slice(0, 2).map((cap) => (
                          <span key={cap.id} className="text-[10px] px-1.5 py-0.5 bg-slate-800 text-slate-400 rounded">
                            {cap.label}
                          </span>
                        ))}
                      </div>
                    ))}
                  </div>
                )}

                {isExpanded && (!group || group.agents.length === 0) && (
                  <div className="border-t border-slate-800/50 p-4 text-center text-xs text-slate-600">
                    対応 Agent なし
                  </div>
                )}
              </div>
            );
          })}
        </div>
      )}

      {/* 業務基盤ビュー */}
      {!loading && activeTab === 'business' && (
        <BusinessBaseView groups={businessGroups} />
      )}
    </div>
  );
}

/* ================================================================
 * サブコンポーネント: 業務基盤ビュー
 * ================================================================ */

const BUSINESS_ICONS: Readonly<Record<string, string>> = {
  ecommerce: '🛒',
  finance: '💰',
  healthcare: '🏥',
  education: '📚',
  governance: '🏛️',
  analytics: '📈',
  communication: '💬',
  development: '💻',
  infrastructure: '🔧',
};

function BusinessBaseView({ groups }: { readonly groups: readonly AgentBusinessBaseGroup[] }) {
  const [expanded, setExpanded] = useState<string | null>(null);

  if (groups.length === 0) {
    return (
      <div className="text-center py-16 text-slate-500 text-sm">
        <p className="text-4xl mb-3">🏢</p>
        業務基盤データなし
      </div>
    );
  }

  return (
    <div className="space-y-3">
      {groups.map((group) => {
        const icon = BUSINESS_ICONS[group.business_base] ?? '📦';
        const isOpen = expanded === group.business_base;
        return (
          <div key={group.business_base} className="border border-slate-800 rounded-xl bg-slate-900/30">
            <button
              onClick={() => setExpanded(isOpen ? null : group.business_base)}
              className="w-full flex items-center gap-3 p-4 text-left"
            >
              <span className="text-xl">{icon}</span>
              <div className="flex-1">
                <h3 className="text-sm font-bold text-slate-200 capitalize">{group.business_base}</h3>
              </div>
              <span className="text-xs text-slate-500">{group.count} agents</span>
              <span className={`text-slate-500 transition-transform ${isOpen ? 'rotate-180' : ''}`}>▾</span>
            </button>
            {isOpen && (
              <div className="border-t border-slate-800/50 p-3 space-y-1.5">
                {group.agents.map((agent: AggregatedAgent) => (
                  <div key={agent.name} className="flex items-center gap-2 p-2 bg-slate-900/40 rounded-lg">
                    <span className="text-sm">🤖</span>
                    <p className="text-xs font-medium text-slate-200 flex-1 truncate">{agent.name}</p>
                    <p className="text-[10px] text-slate-600">{agent.app_display_name}</p>
                  </div>
                ))}
              </div>
            )}
          </div>
        );
      })}
    </div>
  );
}


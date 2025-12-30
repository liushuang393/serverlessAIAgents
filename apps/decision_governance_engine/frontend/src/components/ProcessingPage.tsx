/**
 * 進捗画面コンポーネント.
 *
 * 目的: SSE ストリームでリアルタイム進捗を表示
 * API対接: GET /api/decision/stream (SSE)
 * 設計参考: design/decision-processing-ui.tsx
 */

import React, { useEffect } from 'react';
import { useDecisionStore } from '../store/useDecisionStore';
import { useDecisionStream, AgentProgress } from '../hooks/useDecisionStream';

/** Agent アイコン設定 */
const AGENT_ICONS: Record<string, string> = {
  gatekeeper: '🚪',
  dao: '🎯',
  fa: '🛤️',
  shu: '📋',
  qi: '🔧',
  review: '🔍',
};

/** Agent カード */
const AgentCard: React.FC<{ agent: AgentProgress; isReview?: boolean }> = ({ agent, isReview }) => {
  const statusColor = {
    waiting: 'border-slate-700',
    running: 'border-indigo-500 shadow-lg shadow-indigo-500/10',
    completed: 'border-emerald-500/20',
    failed: 'border-red-500',
  };

  const icon = AGENT_ICONS[agent.id] || '○';

  return (
    <div className={`bg-[#12121a] rounded-xl ${isReview ? 'border-2 border-dashed' : 'border'} ${statusColor[agent.status]} p-5 transition-all duration-500`}>
      <div className="flex items-center justify-between mb-3">
        <div className="flex items-center gap-3">
          <div className={`w-10 h-10 rounded-lg flex items-center justify-center text-xl ${
            agent.status === 'completed' ? 'bg-emerald-500/10' :
            agent.status === 'running' ? 'bg-indigo-500/10' : 'bg-slate-800'
          }`}>
            {agent.status === 'completed' ? '✓' : icon}
          </div>
          <div>
            <div className="font-medium">{agent.name} <span className="text-slate-500 font-normal">/ {agent.label}</span></div>
            <div className="text-xs text-slate-500">
              {agent.message || (agent.status === 'waiting' ? '待機中' : '')}
            </div>
          </div>
        </div>
        <span className={`text-sm ${
          agent.status === 'completed' ? 'text-emerald-400' :
          agent.status === 'running' ? 'text-indigo-400' : 'text-slate-600'
        }`}>
          {agent.status === 'completed' ? '完了' : 
           agent.status === 'running' ? `${agent.progress}%` : ''}
        </span>
      </div>

      {/* プログレスバー */}
      <div className="h-1.5 bg-slate-800 rounded-full overflow-hidden">
        <div
          className={`h-full transition-all duration-500 rounded-full ${
            agent.status === 'completed' ? 'bg-emerald-500' :
            agent.status === 'running' ? (isReview ? 'bg-amber-500' : 'bg-indigo-500') : 'bg-slate-700'
          }`}
          style={{ width: `${agent.progress}%` }}
        />
      </div>

      {/* 結果プレビュー（完了時） */}
      {agent.status === 'completed' && agent.result && (
        <div className="mt-3 pt-3 border-t border-white/5">
          {agent.id === 'dao' && agent.result.type && (
            <div className="flex items-center gap-4 text-sm">
              <span className="px-2 py-1 bg-indigo-500/10 text-indigo-400 rounded text-xs">
                {agent.result.type}
              </span>
              <span className="text-slate-400">{agent.result.essence}</span>
            </div>
          )}
          {agent.id === 'fa' && agent.result.paths && (
            <div className="text-sm text-slate-400">
              {agent.result.paths}つの戦略を評価 → <span className="text-emerald-400">{agent.result.recommended}を推奨</span>
            </div>
          )}
          {agent.id === 'shu' && agent.result.phases && (
            <div className="text-sm text-slate-400">
              {agent.result.phases}フェーズの実行計画を策定
            </div>
          )}
          {agent.id === 'qi' && agent.result.implementations && (
            <div className="text-sm text-slate-400">
              {agent.result.implementations}件の実装要素を特定
            </div>
          )}
          {agent.id === 'review' && agent.result.verdict && (
            <div className={`text-sm ${
              agent.result.verdict === 'PASS' ? 'text-emerald-400' : 'text-amber-400'
            }`}>
              判定: {agent.result.verdict}
            </div>
          )}
        </div>
      )}
    </div>
  );
};

export const ProcessingPage: React.FC = () => {
  const { question, constraints, setPage, setReport } = useDecisionStore();
  const {
    isConnected,
    isComplete,
    error,
    agents,
    report,
    startStream,
    stopStream,
  } = useDecisionStream();

  // 画面表示時に SSE ストリーム開始
  useEffect(() => {
    if (question) {
      const budget = constraints.budget ? parseFloat(constraints.budget) : undefined;
      const timeline = constraints.timeline ? parseInt(constraints.timeline, 10) : undefined;
      startStream(question, budget, timeline);
    }

    return () => {
      stopStream();
    };
  }, [question, constraints.budget, constraints.timeline, startStream, stopStream]);

  // 完了時にレポートを保存して遷移
  useEffect(() => {
    if (isComplete && report) {
      setReport(report);
      setTimeout(() => setPage('report'), 1500);
    }
  }, [isComplete, report, setReport, setPage]);

  const completedCount = agents.filter((a) => a.status === 'completed').length;
  const overallProgress = Math.round((completedCount / agents.length) * 100);

  return (
    <div className="min-h-screen bg-[#0a0a0f] text-white">
      {/* Header */}
      <header className="border-b border-white/5 px-6 py-4">
        <div className="max-w-6xl mx-auto flex items-center justify-between">
          <div className="flex items-center gap-3">
            <div className="w-10 h-10 rounded-xl bg-gradient-to-br from-indigo-500 to-violet-600 flex items-center justify-center">
              <span className="text-xl">⚡</span>
            </div>
            <span className="font-semibold">Decision Agent</span>
          </div>
          <div className="flex items-center gap-2">
            <span className={`w-2 h-2 rounded-full ${isConnected ? 'bg-emerald-400' : 'bg-slate-600'}`} />
            <span className="text-sm text-slate-400">{isConnected ? 'ストリーム接続中' : '未接続'}</span>
          </div>
        </div>
      </header>

      {/* Main */}
      <main className="max-w-4xl mx-auto px-6 py-12">
        {/* 全体進捗 */}
        <div className="text-center mb-12">
          <div className="text-6xl font-bold bg-gradient-to-r from-indigo-400 to-violet-400 bg-clip-text text-transparent">
            {overallProgress}%
          </div>
          <div className="text-slate-400 mt-2">
            {isComplete ? '✅ 分析完了' : '⏳ 分析処理中...'}
          </div>
        </div>

        {/* エラー表示 */}
        {error && (
          <div className="mb-8 bg-red-500/10 border border-red-500/30 rounded-xl p-4 text-red-400">
            🚨 {error}
          </div>
        )}

        {/* Core Agent カード（道・法・術・器） */}
        <div className="space-y-4 mb-8">
          {agents.slice(0, 4).map((agent) => (
            <AgentCard key={agent.id} agent={agent} />
          ))}
        </div>

        {/* Review Agent（検証 - 特別表示） */}
        {agents[4] && (
          <AgentCard agent={agents[4]} isReview />
        )}

        {/* 質問表示 */}
        <div className="mt-12 bg-[#12121a] rounded-xl p-4 border border-white/5">
          <div className="text-xs text-slate-500 mb-2">処理中の質問</div>
          <div className="text-slate-300">{question}</div>
        </div>
      </main>
    </div>
  );
};


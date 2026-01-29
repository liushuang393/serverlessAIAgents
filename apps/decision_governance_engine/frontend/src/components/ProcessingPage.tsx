/**
 * 進捗画面コンポーネント.
 *
 * 目的: SSE ストリームでリアルタイム進捗を表示
 * API対接: GET /api/decision/stream (SSE)
 * 設計参考: design/decision-processing-ui.tsx
 */

import React, { useEffect, useCallback, useState, useRef } from 'react';
import { useDecisionStore } from '../store/useDecisionStore';
import { useDecisionStream, AgentProgress, ThinkingLog } from '../hooks/useDecisionStream';

/** Agent アイコン設定（8 Agent対応） - Decision Governance Engine v1.1 */
const AGENT_ICONS: Record<string, string> = {
  cognitive_gate: '🧠',
  gatekeeper: '🚪',
  clarification: '🔬',
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
          {agent.id === 'cognitive_gate' && agent.result.evaluation_object ? (
            <div className="text-sm text-slate-400">
              評価対象: <span className="text-cyan-400">{String(agent.result.evaluation_object)}</span>
            </div>
          ) : null}
          {agent.id === 'gatekeeper' && (
            <div className="text-sm text-emerald-400">
              ✓ 質問を受理しました
            </div>
          )}
          {agent.id === 'clarification' && agent.result.confidence ? (
            <div className="text-sm text-slate-400">
              診断信頼度: <span className="text-cyan-400">{String(agent.result.confidence)}</span>
            </div>
          ) : null}
          {agent.id === 'dao' && agent.result.type ? (
            <div className="flex items-center gap-4 text-sm">
              <span className="px-2 py-1 bg-indigo-500/10 text-indigo-400 rounded text-xs">
                {String(agent.result.type)}
              </span>
              <span className="text-slate-400">{String(agent.result.essence)}</span>
            </div>
          ) : null}
          {agent.id === 'fa' && agent.result.paths ? (
            <div className="text-sm text-slate-400">
              {String(agent.result.paths)}つの戦略を評価 → <span className="text-emerald-400">{String(agent.result.recommended)}を推奨</span>
            </div>
          ) : null}
          {agent.id === 'shu' && agent.result.phases ? (
            <div className="text-sm text-slate-400">
              {String(agent.result.phases)}フェーズの実行計画を策定
            </div>
          ) : null}
          {agent.id === 'qi' && agent.result.implementations ? (
            <div className="text-sm text-slate-400">
              {Array.isArray(agent.result.implementations)
                ? agent.result.implementations.length
                : String(agent.result.implementations)}件の実装要素を特定
            </div>
          ) : null}
          {agent.id === 'review' && agent.result.verdict ? (
            <div className={`text-sm ${
              agent.result.verdict === 'PASS' ? 'text-emerald-400' : 'text-amber-400'
            }`}>
              判定: {String(agent.result.verdict)}
            </div>
          ) : null}
        </div>
      )}
    </div>
  );
};

/** 思考ログパネル */
const ThinkingLogPanel: React.FC<{ logs: ThinkingLog[]; isExpanded: boolean; onToggle: () => void }> = ({ logs, isExpanded, onToggle }) => {
  const logEndRef = React.useRef<HTMLDivElement>(null);

  React.useEffect(() => {
    if (isExpanded && logEndRef.current) {
      logEndRef.current.scrollIntoView({ behavior: 'smooth' });
    }
  }, [logs, isExpanded]);

  return (
    <div className="mt-6 bg-[#12121a] rounded-xl border border-white/5 overflow-hidden">
      <button
        onClick={onToggle}
        className="w-full px-4 py-3 flex items-center justify-between text-sm hover:bg-white/5 transition-colors"
      >
        <span className="text-slate-400 flex items-center gap-2">
          <span>💭</span> 思考ログ
          <span className="text-xs text-slate-600">({logs.length}件)</span>
        </span>
        <span className={`text-slate-500 transition-transform ${isExpanded ? 'rotate-180' : ''}`}>▼</span>
      </button>
      {isExpanded && (
        <div className="max-h-48 overflow-y-auto border-t border-white/5 p-3 space-y-2 text-xs font-mono">
          {logs.map((log, i) => (
            <div key={i} className="flex gap-2 text-slate-400">
              <span className="text-slate-600 shrink-0">
                {new Date(log.timestamp).toLocaleTimeString('ja-JP', { hour: '2-digit', minute: '2-digit', second: '2-digit' })}
              </span>
              <span className="text-indigo-400 shrink-0">[{log.agentName}]</span>
              <span className="text-slate-300 break-all">{log.content}</span>
            </div>
          ))}
          <div ref={logEndRef} />
        </div>
      )}
    </div>
  );
};

export const ProcessingPage: React.FC = () => {
  const { question, constraints, setPage, setReport, addToHistory } = useDecisionStore();
  const {
    isConnected,
    isComplete,
    error,
    agents,
    report,
    thinkingLogs,
    startStream,
    stopStream,
  } = useDecisionStream();

  const [isCancelling, setIsCancelling] = useState(false);
  const [isLogExpanded, setIsLogExpanded] = useState(false);

  /** キャンセル処理（入力内容は保持） */
  const handleCancel = useCallback(() => {
    setIsCancelling(true);
    stopStream();
    setTimeout(() => {
      // reset() は呼ばない - 入力内容を保持
      setPage('input');
      setIsCancelling(false);
    }, 300);
  }, [stopStream, setPage]);

  /** リトライ処理 */
  const handleRetry = useCallback(() => {
    if (question) {
      const budget = constraints.budget ? parseFloat(constraints.budget) : undefined;
      const timeline = constraints.timeline ? parseInt(constraints.timeline, 10) : undefined;
      startStream(question, budget, timeline);
    }
  }, [question, constraints.budget, constraints.timeline, startStream]);

  /** レポート画面へ遷移 */
  const handleViewReport = useCallback(() => {
    if (report) {
      setReport(report);
      setPage('report');
    }
  }, [report, setReport, setPage]);

  // 画面表示時に SSE ストリーム開始（question が設定されたら1回のみ）
  const hasStartedRef = useRef(false);
  // React Strict Mode 対応: 真のアンマウントかどうかを判定
  const isMountedRef = useRef(false);

  // ストリーム開始用 Effect（question 変化時に発火）
  useEffect(() => {
    isMountedRef.current = true;

    // question がある場合のみ startStream を呼ぶ（1回のみ）
    // Zustand persist の hydration 完了後に question が設定される
    if (question && !hasStartedRef.current) {
      hasStartedRef.current = true;
      const budget = constraints.budget ? parseFloat(constraints.budget) : undefined;
      const timeline = constraints.timeline ? parseInt(constraints.timeline, 10) : undefined;
      console.log('[ProcessingPage] startStream 開始', { question: question.slice(0, 50), budget, timeline });
      startStream(question, budget, timeline);
    } else if (!question) {
      console.log('[ProcessingPage] question が空 - hydration 待ち中');
    }

    // Cleanup: React Strict Mode 対応
    // Strict Mode では mount → unmount → mount の順で実行される
    // 真のアンマウント時のみ stopStream を呼ぶ
    return () => {
      // 少し遅延して、Strict Mode の再マウントかどうかを確認
      setTimeout(() => {
        if (!isMountedRef.current) {
          console.log('[ProcessingPage] 真のアンマウント - ストリーム停止');
          stopStream();
        }
      }, 100);
      isMountedRef.current = false;
      hasStartedRef.current = false;  // 再マウント時に再開できるようにリセット
    };
  }, [question, constraints.budget, constraints.timeline, startStream, stopStream]);

  // 完了時にレポートを保存（自動遷移しない - ボタンで遷移）
  useEffect(() => {
    if (isComplete && report) {
      setReport(report);
      // 履歴に追加
      addToHistory({
        question,
        reportId: report.report_id,
        status: 'completed',
      });
      // 自動遷移を削除 - ユーザーがボタンをクリックして遷移
    }
  }, [isComplete, report, question, setReport, addToHistory]);

  // エラー時に履歴に記録（自動遷移しない）
  useEffect(() => {
    if (error && !isComplete) {
      addToHistory({
        question,
        reportId: null,
        status: 'failed',
      });
      // エラー時も自動遷移しない - 画面に留まってエラーを表示
    }
  }, [error, isComplete, question, addToHistory]);

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

        {/* Agent カード（認知・門番・診断・道・法・術・器） */}
        <div className="space-y-4 mb-8">
          {agents.slice(0, 7).map((agent) => (
            <AgentCard key={agent.id} agent={agent} />
          ))}
        </div>

        {/* Review Agent（検証 - 特別表示） */}
        {agents[7] && (
          <AgentCard agent={agents[7]} isReview />
        )}

        {/* アクションボタン */}
        <div className="flex justify-center gap-4 mt-8">
          {isComplete && report ? (
            <button 
              onClick={handleViewReport}
              className="px-8 py-3 bg-gradient-to-r from-indigo-600 to-violet-600 hover:from-indigo-500 hover:to-violet-500 rounded-xl font-medium transition-all shadow-lg shadow-indigo-500/25 flex items-center gap-2"
            >
              📄 決策レポートを表示
            </button>
          ) : error ? (
            <>
              <button 
                onClick={handleRetry}
                className="px-6 py-3 bg-indigo-600 hover:bg-indigo-500 rounded-xl font-medium transition-all flex items-center gap-2"
              >
                🔄 リトライ
              </button>
              <button 
                onClick={handleCancel}
                disabled={isCancelling}
                className="px-6 py-3 bg-slate-800 hover:bg-slate-700 rounded-xl text-slate-400 transition-all"
              >
                戻る
              </button>
            </>
          ) : (
            <button 
              onClick={handleCancel}
              disabled={isCancelling}
              className="px-6 py-3 bg-slate-800 hover:bg-slate-700 rounded-xl text-slate-400 transition-all flex items-center gap-2"
            >
              {isCancelling ? (
                <>
                  <div className="w-4 h-4 border-2 border-slate-400/30 border-t-slate-400 rounded-full animate-spin" />
                  キャンセル中...
                </>
              ) : (
                'キャンセル'
              )}
            </button>
          )}
        </div>

        {/* ヒント */}
        {!isComplete && !error && (
          <div className="mt-8 text-center">
            <p className="text-sm text-slate-600">
              💡 各段階で深層分析を行っています。通常2〜3分で完了します。
            </p>
          </div>
        )}

        {/* 質問表示 */}
        <div className="mt-8 bg-[#12121a] rounded-xl p-4 border border-white/5">
          <div className="text-xs text-slate-500 mb-2">処理中の質問</div>
          <div className="text-slate-300">{question}</div>
        </div>

        {/* 思考ログパネル */}
        <ThinkingLogPanel
          logs={thinkingLogs}
          isExpanded={isLogExpanded}
          onToggle={() => setIsLogExpanded(!isLogExpanded)}
        />
      </main>
    </div>
  );
};


/**
 * 提案書画面コンポーネント v3.1.
 *
 * 目的: 提案書の表示・署名・エクスポート
 * API対接: GET /api/report/{id}/components, GET /api/report/{id}/pdf
 * 設計参考: design/decision-report-ui.tsx
 *
 * v3.1: 日本式ビジネス提案書フォーマット対応
 * - 提案書タイトル（日本語/英語/案件ID）
 * - 署名欄の自動出力
 */

import React, { useCallback, useState, useEffect } from 'react';
import { useDecisionStore } from '../store/useDecisionStore';
import { useAuthStore } from '../store/useAuthStore';
import { decisionApi } from '../api/client';
import { SignatureArea } from './HankoSeal';
import type { RecommendedPath, Phase, Implementation, SignatureData } from '../types';

/** 通知タイプ */
type NotificationType = 'success' | 'error' | 'info';

/** 通知コンポーネント */
const Notification: React.FC<{
  type: NotificationType;
  message: string;
  onClose: () => void;
}> = ({ type, message, onClose }) => {
  const colors = {
    success: 'bg-emerald-500/10 border-emerald-500/30 text-emerald-400',
    error: 'bg-red-500/10 border-red-500/30 text-red-400',
    info: 'bg-blue-500/10 border-blue-500/30 text-blue-400',
  };
  
  const icons = {
    success: '✅',
    error: '❌',
    info: 'ℹ️',
  };

  useEffect(() => {
    const timer = setTimeout(onClose, 5000);
    return () => clearTimeout(timer);
  }, [onClose]);

  return (
    <div className={`fixed top-4 right-4 z-50 px-4 py-3 rounded-lg border ${colors[type]} flex items-center gap-3 animate-fade-in`}>
      <span>{icons[type]}</span>
      <span className="text-sm">{message}</span>
      <button onClick={onClose} className="ml-2 hover:opacity-70">✕</button>
    </div>
  );
};

/** タブ定義 */
const TABS = [
  { id: 'summary', name: 'サマリー', icon: '📊' },
  { id: 'dao', name: '道', icon: '🎯' },
  { id: 'fa', name: '法', icon: '🛤️' },
  { id: 'shu', name: '術', icon: '📋' },
  { id: 'qi', name: '器', icon: '🔧' },
  { id: 'review', name: '検証', icon: '🔍' },
] as const;

type TabId = typeof TABS[number]['id'];

/** パスカード */
const PathCard: React.FC<{ path: RecommendedPath; isRecommended?: boolean }> = ({
  path,
  isRecommended,
}) => (
  <div className={`rounded-xl p-5 border ${isRecommended ? 'border-emerald-500/30 bg-emerald-500/5' : 'border-white/5 bg-[#0a0a0f] opacity-60'}`}>
    <div className="flex items-center justify-between mb-3">
      <div className="flex items-center gap-2">
        <span>{isRecommended ? '✓' : '✕'}</span>
        <span className="font-semibold">{path.name}</span>
        {!isRecommended && <span className="text-xs text-red-400 px-2 py-0.5 bg-red-500/10 rounded">不推奨</span>}
      </div>
      <span className={`text-sm ${isRecommended ? 'text-emerald-400' : 'text-slate-500'}`}>
        成功確率 {Math.round(path.success_probability * 100)}%
      </span>
    </div>
    <p className="text-sm text-slate-400 mb-4">{path.description}</p>
    
    <div className="grid grid-cols-2 gap-4">
      <div>
        <div className="text-xs text-emerald-400 mb-2">メリット</div>
        {path.pros.map((p, i) => (
          <div key={i} className="text-sm text-slate-400 flex items-center gap-2 mb-1">
            <span className="text-emerald-400">+</span> {p}
          </div>
        ))}
      </div>
      <div>
        <div className="text-xs text-amber-400 mb-2">デメリット</div>
        {path.cons.map((c, i) => (
          <div key={i} className="text-sm text-slate-400 flex items-center gap-2 mb-1">
            <span className="text-amber-400">-</span> {c}
          </div>
        ))}
      </div>
    </div>
  </div>
);

/** フェーズカード（タイムライン表示） */
const PhaseTimeline: React.FC<{ phases: Phase[] }> = ({ phases }) => (
  <div className="relative">
    {phases.map((phase, i) => (
      <div key={i} className="flex gap-4 mb-6 last:mb-0">
        <div className="flex flex-col items-center">
          <div className="w-10 h-10 rounded-full bg-blue-500/10 border-2 border-blue-500/30 flex items-center justify-center text-blue-400 font-semibold">
            {phase.phase_number}
          </div>
          {i < phases.length - 1 && (
            <div className="w-0.5 h-full bg-blue-500/20 mt-2" />
          )}
        </div>
        <div className="flex-1 bg-[#0a0a0f] rounded-lg p-4">
          <div className="flex items-center justify-between mb-2">
            <span className="font-medium">{phase.name}</span>
            <span className="text-xs text-slate-500 px-2 py-1 bg-slate-800 rounded">{phase.duration}</span>
          </div>
          <div className="flex flex-wrap gap-2">
            {phase.actions.map((action, j) => (
              <span key={j} className="text-xs text-slate-400 px-2 py-1 bg-slate-800/50 rounded">
                {action}
              </span>
            ))}
          </div>
        </div>
      </div>
    ))}
  </div>
);

export const ReportPage: React.FC = () => {
  const { report, reportId, setPage, reset } = useDecisionStore();
  const { user, performLogout } = useAuthStore();
  const [activeTab, setActiveTab] = useState<TabId>('summary');
  const [isExporting, setIsExporting] = useState(false);
  const [isSigning, setIsSigning] = useState(false);
  const [notification, setNotification] = useState<{type: NotificationType; message: string} | null>(null);
  const [signatureStatus, setSignatureStatus] = useState<'unsigned' | 'signed'>('unsigned');
  const [signatureData, setSignatureData] = useState<SignatureData | null>(null);
  const [showSignedAnimation, setShowSignedAnimation] = useState(false);

  // レポートがない場合は入力画面へ
  useEffect(() => {
    if (!report) {
      setPage('input');
    }
  }, [report, setPage]);

  /** PDF エクスポート */
  const handleExportPdf = useCallback(async () => {
    if (!reportId) return;
    setIsExporting(true);

    try {
      const blob = await decisionApi.exportPdf(reportId);
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `decision-report-${reportId}.pdf`;
      a.click();
      URL.revokeObjectURL(url);
      setNotification({ type: 'success', message: 'PDFをダウンロードしました' });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'PDF生成に失敗しました';
      setNotification({ type: 'error', message });
    } finally {
      setIsExporting(false);
    }
  }, [reportId]);

  /** 電子署名処理 */
  const handleSign = useCallback(async () => {
    if (!reportId || signatureStatus === 'signed' || !user) return;
    setIsSigning(true);

    try {
      // 署名確認ダイアログ
      const confirmed = window.confirm(
        `${user.display_name} として署名します。\nこのレポートに基づいて意思決定を行います。\n署名すると記録が残ります。\n\n続行しますか？`
      );
      
      if (!confirmed) {
        setIsSigning(false);
        return;
      }

      // API呼び出し
      const response = await decisionApi.signReport(reportId);
      
      if (response.success && response.signature) {
        setSignatureStatus('signed');
        setSignatureData(response.signature);
        setShowSignedAnimation(true);
        setNotification({ 
          type: 'success', 
          message: `${user.display_name} により署名されました` 
        });
        
        // アニメーション後にリセット
        setTimeout(() => setShowSignedAnimation(false), 1000);
      } else {
        setNotification({ type: 'error', message: response.message });
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : '署名に失敗しました';
      setNotification({ type: 'error', message });
    } finally {
      setIsSigning(false);
    }
  }, [reportId, signatureStatus, user]);

  /** ログアウト */
  const handleLogout = useCallback(async () => {
    await performLogout();
    reset();
    setPage('input');
  }, [performLogout, reset, setPage]);

  /** 新規質問 */
  const handleNewQuestion = useCallback(() => {
    reset();
    setPage('input');
  }, [reset, setPage]);

  if (!report) return null;

  const { executive_summary, dao, fa, shu, qi, review, proposal_title, signature_block } = report;

  // 提案書タイトル（デフォルト値）
  const titleJa = proposal_title?.title_ja || '課題解決提案書';
  const titleEn = proposal_title?.title_en || 'Solution_Proposal';
  const caseId = proposal_title?.case_id || reportId || '---';
  const subtitle = proposal_title?.subtitle || '';
  
  // 署名欄情報（デフォルト値）
  const authorName = signature_block?.author_name || 'Decision Agent';
  const authorDept = signature_block?.author_department || 'AI Decision Support';
  const authorPos = signature_block?.author_position || 'AI Assistant';
  const createdDate = signature_block?.created_date || new Date().toLocaleDateString('ja-JP');

  return (
    <div className="min-h-screen bg-[#0a0a0f] text-white">
      {/* 通知 */}
      {notification && (
        <Notification 
          type={notification.type} 
          message={notification.message}
          onClose={() => setNotification(null)}
        />
      )}
      {/* Header */}
      <header className="border-b border-white/5 px-6 py-4">
        <div className="max-w-6xl mx-auto flex items-center justify-between">
          <div className="flex items-center gap-3">
            <div className="w-10 h-10 rounded-xl bg-gradient-to-br from-indigo-500 to-violet-600 flex items-center justify-center">
              <span className="text-xl">📋</span>
            </div>
            <div>
              <h1 className="font-semibold text-lg">提案書</h1>
              <p className="text-xs text-slate-500 font-mono">{caseId}</p>
            </div>
          </div>
          <div className="flex items-center gap-4">
            {/* アクションボタン */}
            <div className="flex items-center gap-2">
              <button
                onClick={handleExportPdf}
                disabled={isExporting}
                className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all"
              >
                📄 {isExporting ? '生成中...' : 'PDF出力'}
              </button>
              <button
                onClick={handleNewQuestion}
                className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all"
              >
                🔄 再分析
              </button>
            </div>

            {/* ユーザーメニュー */}
            {user && (
              <div className="flex items-center gap-3 pl-4 border-l border-white/10">
                <div className="text-right">
                  <div className="text-sm font-medium text-white">{user.display_name}</div>
                  <div className="text-xs text-slate-500">{user.department}</div>
                </div>
                <button
                  onClick={handleLogout}
                  className="p-2 hover:bg-slate-800 rounded-lg transition-colors text-slate-400 hover:text-white"
                  title="ログアウト"
                >
                  🚪
                </button>
              </div>
            )}
          </div>
        </div>
      </header>

      <main className="max-w-5xl mx-auto px-6 py-8">
        {/* 提案書表紙 v3.1 */}
        <div className="bg-gradient-to-br from-[#12121a] to-[#1a1a24] rounded-2xl border-2 border-indigo-500/20 p-10 mb-8 text-center relative overflow-hidden">
          <div className="absolute top-0 right-0 w-64 h-64 bg-indigo-500/5 rounded-full blur-3xl -translate-y-1/2 translate-x-1/2" />
          <div className="absolute bottom-0 left-0 w-48 h-48 bg-violet-500/5 rounded-full blur-3xl translate-y-1/2 -translate-x-1/2" />
          
          <div className="relative">
            <p className="text-slate-400 mb-6">御中</p>
            <h1 className="text-3xl font-bold mb-2 tracking-wider">{titleJa}</h1>
            <p className="text-sm text-slate-500 font-mono mb-4">{titleEn}</p>
            {subtitle && <p className="text-slate-400 text-sm mb-4">{subtitle}</p>}
            <p className="text-xs text-slate-600 font-mono mb-8">案件ID: {caseId}</p>
            
            <div className="border-t border-white/10 pt-6 mt-6">
              <p className="text-sm text-slate-400">{createdDate}</p>
              <p className="text-sm text-slate-400 mt-2">{authorDept}</p>
              <p className="text-sm text-slate-400">{authorPos} {authorName}</p>
            </div>
          </div>
        </div>

        {/* エグゼクティブサマリー v3.1 */}
        <div className="bg-gradient-to-br from-[#12121a] to-[#1a1a24] rounded-2xl border border-white/5 p-8 mb-8 relative overflow-hidden">
          <div className="absolute top-0 right-0 w-64 h-64 bg-indigo-500/5 rounded-full blur-3xl -translate-y-1/2 translate-x-1/2" />
          
          <div className="relative">
            <div className="flex items-start justify-between mb-6">
              <div>
                <h2 className="text-xs text-slate-500 uppercase tracking-wider mb-1">1. EXECUTIVE SUMMARY</h2>
                <div className="text-2xl font-bold">エグゼクティブサマリー</div>
              </div>
              <div className="text-right">
                <div className="text-xs text-slate-500 mb-1">信頼度スコア</div>
                <div className="text-3xl font-bold text-emerald-400">{Math.round(review.confidence_score * 100)}%</div>
              </div>
            </div>

            {/* 結論 */}
            <div className="bg-[#0a0a0f] rounded-xl p-6 mb-6 border border-indigo-500/20">
              <div className="flex items-center gap-2 text-indigo-400 text-sm mb-2">
                <span>💡</span> 結論
              </div>
              <p className="text-lg font-medium">{executive_summary.one_line_decision}</p>
            </div>

            {/* v3.0: 本質の一文 */}
            {(executive_summary as any).essence_statement && (
              <div className="bg-purple-500/5 rounded-xl p-5 mb-6 border border-purple-500/20">
                <div className="flex items-center gap-2 text-purple-400 text-sm mb-2">
                  <span>📍</span> 本質
                </div>
                <p className="font-medium">{(executive_summary as any).essence_statement}</p>
              </div>
            )}

            {/* 最初の一歩 */}
            <div className="bg-emerald-500/5 rounded-xl p-5 mb-6 border border-emerald-500/20">
              <div className="flex items-center gap-2 text-emerald-400 text-sm mb-2">
                <span>🎯</span> 最初の一歩（明日実行可能）
              </div>
              <p className="font-medium">{executive_summary.first_step}</p>
            </div>

            {/* v3.0: 戦略的禁止事項サマリー */}
            {(executive_summary as any).strategic_prohibition_summary && (
              <div className="bg-red-500/5 rounded-xl p-5 mb-6 border border-red-500/20">
                <div className="flex items-center gap-2 text-red-400 text-sm mb-2">
                  <span>⛔</span> 戦略的禁止
                </div>
                <p className="text-sm text-slate-400">{(executive_summary as any).strategic_prohibition_summary}</p>
              </div>
            )}

            {/* v3.0: 撤退基準サマリー */}
            {(executive_summary as any).exit_criteria_summary && (
              <div className="bg-amber-500/5 rounded-xl p-5 mb-6 border border-amber-500/20">
                <div className="flex items-center gap-2 text-amber-400 text-sm mb-2">
                  <span>🚪</span> 撤退基準
                </div>
                <p className="text-sm text-slate-400">{(executive_summary as any).exit_criteria_summary}</p>
              </div>
            )}

            {/* 主要リスク */}
            <div>
              <div className="flex items-center gap-2 text-amber-400 text-sm mb-3">
                <span>⚠️</span> 主要リスク
              </div>
              <div className="grid grid-cols-1 gap-2">
                {executive_summary.key_risks.map((risk, i) => (
                  <div key={i} className="flex items-center gap-3 text-sm text-slate-400">
                    <span className="w-1.5 h-1.5 rounded-full bg-amber-500" />
                    {risk}
                  </div>
                ))}
              </div>
            </div>
          </div>
        </div>

        {/* タブナビゲーション */}
        <div className="flex gap-2 mb-6 border-b border-white/5 pb-4">
          {TABS.map((tab) => (
            <button
              key={tab.id}
              onClick={() => setActiveTab(tab.id)}
              className={`px-4 py-2 rounded-lg text-sm font-medium transition-all flex items-center gap-2 ${
                activeTab === tab.id
                  ? 'bg-indigo-500/10 text-indigo-400 border border-indigo-500/30'
                  : 'text-slate-400 hover:text-white hover:bg-slate-800'
              }`}
            >
              <span>{tab.icon}</span>
              {tab.name}
            </button>
          ))}
        </div>

        {/* タブコンテンツ */}
        <div className="bg-[#12121a] rounded-xl border border-white/5 p-6">
          {activeTab === 'summary' && (
            <div className="text-center py-8 text-slate-500">
              上記のエグゼクティブサマリーが全体概要です。<br />
              各タブで詳細を確認できます。
            </div>
          )}

          {activeTab === 'dao' && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-indigo-500/10 flex items-center justify-center">🎯</span>
                道 / 本質分析 v3.0
              </h3>

              <div className="grid grid-cols-2 gap-4">
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-1">問題タイプ</div>
                  <div className="px-3 py-1 bg-indigo-500/10 text-indigo-400 rounded inline-block text-sm">
                    {dao.problem_type}
                  </div>
                </div>
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-1">問題の本質的性質</div>
                  <div className="px-3 py-1 bg-purple-500/10 text-purple-400 rounded inline-block text-sm">
                    {dao.problem_nature || 'N/A'}
                  </div>
                </div>
              </div>

              {/* 本質（一文） */}
              <div className="bg-gradient-to-r from-indigo-500/10 to-purple-500/10 rounded-lg p-5 border border-indigo-500/20">
                <div className="text-xs text-indigo-400 mb-2">📍 本質（一文）</div>
                <div className="text-lg font-medium">{dao.essence}</div>
              </div>

              {/* v3.0: 本質導出プロセス */}
              {dao.essence_derivation && (
                <div className="bg-[#0a0a0f] rounded-lg p-5 border border-blue-500/20">
                  <div className="text-sm font-medium text-blue-400 mb-4 flex items-center gap-2">
                    <span>🔍</span> 本質導出プロセス
                  </div>
                  <div className="space-y-3">
                    <div>
                      <div className="text-xs text-slate-500">表面的問題</div>
                      <div className="text-sm mt-1">{dao.essence_derivation.surface_problem}</div>
                    </div>
                    <div className="w-full h-px bg-slate-800" />
                    <div>
                      <div className="text-xs text-slate-500">一段深い理由</div>
                      <div className="text-sm mt-1">{dao.essence_derivation.underlying_why}</div>
                    </div>
                    <div className="w-full h-px bg-slate-800" />
                    <div>
                      <div className="text-xs text-slate-500">根本制約</div>
                      <div className="text-sm mt-1">{dao.essence_derivation.root_constraint}</div>
                    </div>
                    <div className="w-full h-px bg-slate-800" />
                    <div className="bg-blue-500/5 rounded p-3">
                      <div className="text-xs text-blue-400">本質の一文</div>
                      <div className="text-sm mt-1 font-medium">{dao.essence_derivation.essence_statement}</div>
                    </div>
                  </div>
                </div>
              )}

              {/* v3.0: 既存代替手段 */}
              {dao.existing_alternatives && dao.existing_alternatives.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-sm font-medium text-amber-400 mb-3 flex items-center gap-2">
                    <span>🔄</span> 既存代替手段（なぜ使えないか）
                  </div>
                  <div className="space-y-3">
                    {dao.existing_alternatives.map((alt: any, i: number) => (
                      <div key={i} className="bg-amber-500/5 rounded p-3 border border-amber-500/10">
                        <div className="font-medium text-amber-400 text-sm">{alt.name}</div>
                        <div className="text-sm text-slate-400 mt-1">{alt.why_not_viable}</div>
                        <div className="text-xs text-slate-500 mt-1">制約: {alt.specific_constraint}</div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {dao.immutable_constraints && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-3">🔒 不可変制約</div>
                  <div className="space-y-2">
                    {dao.immutable_constraints.map((c: string, i: number) => (
                      <div key={i} className="flex items-center gap-2 text-sm">
                        <span className="text-red-400">🔒</span> {c}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {dao.hidden_assumptions && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-3">💭 隠れた前提</div>
                  <div className="space-y-2">
                    {dao.hidden_assumptions.map((a: string, i: number) => (
                      <div key={i} className="flex items-center gap-2 text-sm text-slate-400">
                        <span>💭</span> {a}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 因果齿轮 */}
              {dao.causal_gears && dao.causal_gears.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-sm font-medium text-cyan-400 mb-3 flex items-center gap-2">
                    <span>⚙️</span> 因果齿轮
                  </div>
                  <div className="space-y-2">
                    {dao.causal_gears.map((gear: any, i: number) => (
                      <div key={i} className={`flex items-start gap-3 p-2 rounded ${
                        gear.name === dao.bottleneck_gear ? 'bg-cyan-500/10 border border-cyan-500/30' : ''
                      }`}>
                        <span className="text-cyan-400">⚙️</span>
                        <div className="flex-1">
                          <div className="flex items-center gap-2">
                            <span className="font-medium text-sm">{gear.name}</span>
                            <span className="text-xs px-2 py-0.5 bg-slate-700 rounded">Leverage: {gear.leverage}</span>
                            {gear.name === dao.bottleneck_gear && (
                              <span className="text-xs px-2 py-0.5 bg-cyan-500/20 text-cyan-400 rounded">ボトルネック</span>
                            )}
                          </div>
                          <div className="text-xs text-slate-400 mt-1">{gear.description}</div>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 死穴 */}
              {dao.death_traps && dao.death_traps.length > 0 && (
                <div className="bg-red-500/5 rounded-lg p-4 border border-red-500/20">
                  <div className="text-sm font-medium text-red-400 mb-3 flex items-center gap-2">
                    <span>💀</span> 死穴（禁忌）
                  </div>
                  <div className="space-y-3">
                    {dao.death_traps.map((trap: any, i: number) => (
                      <div key={i} className="bg-red-500/10 rounded p-3">
                        <div className="flex items-center gap-2">
                          <span className="text-red-400">⚠️</span>
                          <span className="font-medium text-sm">{trap.action}</span>
                          <span className={`text-xs px-2 py-0.5 rounded ${
                            trap.severity === 'FATAL' ? 'bg-red-500/20 text-red-400' : 'bg-amber-500/20 text-amber-400'
                          }`}>{trap.severity}</span>
                        </div>
                        <div className="text-sm text-slate-400 mt-2">{trap.reason}</div>
                      </div>
                    ))}
                  </div>
                </div>
              )}
            </div>
          )}

          {activeTab === 'fa' && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-violet-500/10 flex items-center justify-center">🛤️</span>
                法 / 戦略選定 v3.0
              </h3>

              {/* v3.0: 戦略的禁止事項 */}
              {fa.strategic_prohibitions && fa.strategic_prohibitions.length > 0 && (
                <div className="bg-red-500/5 rounded-lg p-5 border border-red-500/20">
                  <div className="text-sm font-medium text-red-400 mb-4 flex items-center gap-2">
                    <span>🚫</span> 戦略的禁止事項（絶対にやってはいけない）
                  </div>
                  <div className="space-y-3">
                    {fa.strategic_prohibitions.map((p: any, i: number) => (
                      <div key={i} className="bg-red-500/10 rounded p-4">
                        <div className="flex items-start gap-2">
                          <span className="text-red-400 mt-0.5">⛔</span>
                          <div className="flex-1">
                            <div className="font-medium text-sm">{p.prohibition}</div>
                            <div className="text-sm text-slate-400 mt-2">理由: {p.rationale}</div>
                            <div className="text-sm text-red-400 mt-1">違反結果: {p.violation_consequence}</div>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 差別化軸 */}
              {fa.differentiation_axis && (
                <div className="bg-gradient-to-r from-violet-500/10 to-purple-500/10 rounded-lg p-5 border border-violet-500/20">
                  <div className="text-sm font-medium text-violet-400 mb-4 flex items-center gap-2">
                    <span>🎯</span> 差別化軸
                  </div>
                  <div className="space-y-4">
                    <div className="bg-violet-500/10 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">勝負する軸</div>
                      <div className="text-lg font-medium text-violet-400">{fa.differentiation_axis.axis_name}</div>
                      <div className="text-sm text-slate-400 mt-2">{fa.differentiation_axis.why_this_axis}</div>
                    </div>
                    <div className="bg-slate-800/50 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">勝負しない軸</div>
                      <div className="text-sm text-slate-400">{fa.differentiation_axis.not_this_axis}</div>
                    </div>
                  </div>
                </div>
              )}

              {/* v3.0: 既存解が使えない理由 */}
              {fa.why_existing_fails && (
                <div className="bg-amber-500/5 rounded-lg p-4 border border-amber-500/20">
                  <div className="text-xs text-amber-400 mb-2 flex items-center gap-2">
                    <span>⚠️</span> 既存解が使えない理由
                  </div>
                  <div className="text-sm text-slate-400">{fa.why_existing_fails}</div>
                </div>
              )}

              {/* 推奨パス */}
              {fa.recommended_paths?.map((path: RecommendedPath, i: number) => (
                <PathCard key={i} path={path} isRecommended />
              ))}

              {/* 不推奨パス */}
              {fa.rejected_paths?.map((path: RecommendedPath, i: number) => (
                <PathCard key={i} path={path} isRecommended={false} />
              ))}

              {/* 判断基準 */}
              {fa.decision_criteria && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2">判断基準</div>
                  <div className="flex flex-wrap gap-2">
                    {fa.decision_criteria.map((c: string, i: number) => (
                      <span key={i} className="px-2 py-1 bg-slate-800 text-slate-400 rounded text-xs">{c}</span>
                    ))}
                  </div>
                </div>
              )}
            </div>
          )}

          {activeTab === 'shu' && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-blue-500/10 flex items-center justify-center">📋</span>
                術 / 実行計画 v3.0
              </h3>

              {shu.first_action && (
                <div className="bg-emerald-500/5 rounded-lg p-4 border border-emerald-500/20">
                  <div className="text-xs text-emerald-400 mb-2">🎯 最初の一歩</div>
                  <div className="text-sm font-medium">{shu.first_action}</div>
                </div>
              )}

              {/* v3.0: 切り捨てリスト */}
              {shu.cut_list && shu.cut_list.length > 0 && (
                <div className="bg-red-500/5 rounded-lg p-4 border border-red-500/20">
                  <div className="text-sm font-medium text-red-400 mb-3 flex items-center gap-2">
                    <span>✂️</span> 切り捨てリスト（最初の30日間でやらないこと）
                  </div>
                  <div className="space-y-2">
                    {shu.cut_list.map((item: string, i: number) => (
                      <div key={i} className="flex items-center gap-2 text-sm text-red-400">
                        <span>❌</span> {item}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 文脈特化行動 */}
              {shu.context_specific_actions && shu.context_specific_actions.length > 0 && (
                <div className="bg-blue-500/5 rounded-lg p-4 border border-blue-500/20">
                  <div className="text-sm font-medium text-blue-400 mb-3 flex items-center gap-2">
                    <span>🎯</span> 文脈特化行動（この問題固有）
                  </div>
                  <div className="space-y-3">
                    {shu.context_specific_actions.map((action: any, i: number) => (
                      <div key={i} className="bg-blue-500/10 rounded p-3">
                        <div className="font-medium text-sm">{action.action}</div>
                        <div className="text-xs text-slate-500 mt-1">理由: {action.why_this_context}</div>
                        <div className="text-xs text-blue-400 mt-1">期待出力: {action.expected_output}</div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 単一検証ポイント */}
              {shu.single_validation_point && (
                <div className="bg-amber-500/5 rounded-lg p-4 border border-amber-500/20">
                  <div className="text-sm font-medium text-amber-400 mb-3 flex items-center gap-2">
                    <span>🔬</span> 単一検証ポイント（PoCで絶対に検証すべき1点）
                  </div>
                  <div className="space-y-3">
                    <div>
                      <div className="text-xs text-slate-500">検証対象</div>
                      <div className="text-sm mt-1 font-medium">{shu.single_validation_point.validation_target}</div>
                    </div>
                    <div>
                      <div className="text-xs text-slate-500">成功基準</div>
                      <div className="text-sm mt-1">{shu.single_validation_point.success_criteria}</div>
                    </div>
                    <div className="bg-amber-500/10 rounded p-2">
                      <div className="text-xs text-amber-400">失敗時行動</div>
                      <div className="text-sm mt-1">{shu.single_validation_point.failure_action}</div>
                    </div>
                  </div>
                </div>
              )}

              {/* v3.0: 撤退基準 */}
              {shu.exit_criteria && (
                <div className="bg-red-500/5 rounded-lg p-4 border border-red-500/20">
                  <div className="text-sm font-medium text-red-400 mb-3 flex items-center gap-2">
                    <span>🚪</span> 撤退基準（どこで止めるか）
                  </div>
                  <div className="space-y-3">
                    <div>
                      <div className="text-xs text-slate-500">チェックポイント</div>
                      <div className="text-sm mt-1">{shu.exit_criteria.checkpoint}</div>
                    </div>
                    <div>
                      <div className="text-xs text-slate-500">撤退トリガー</div>
                      <div className="text-sm mt-1 text-red-400">{shu.exit_criteria.exit_trigger}</div>
                    </div>
                    <div>
                      <div className="text-xs text-slate-500">撤退時行動</div>
                      <div className="text-sm mt-1">{shu.exit_criteria.exit_action}</div>
                    </div>
                  </div>
                </div>
              )}

              {/* タイムライン */}
              {shu.phases && shu.phases.length > 0 && (
                <div>
                  <div className="text-sm font-medium text-slate-400 mb-3 flex items-center gap-2">
                    <span>📅</span> フェーズ
                  </div>
                  <PhaseTimeline phases={shu.phases} />
                </div>
              )}

              {shu.dependencies && shu.dependencies.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2">前提条件</div>
                  <ul className="text-sm text-slate-400 space-y-1">
                    {shu.dependencies.map((d: string, i: number) => (
                      <li key={i}>• {d}</li>
                    ))}
                  </ul>
                </div>
              )}

              {/* v3.0: 30天行動節奏 */}
              {shu.rhythm_control && (
                <div className="bg-[#0a0a0f] rounded-lg p-4 border border-blue-500/20">
                  <div className="text-sm font-medium text-blue-400 mb-3 flex items-center gap-2">
                    <span>⏱️</span> 30天行動節奏
                  </div>
                  {shu.rhythm_control.focus && (
                    <div className="space-y-3">
                      <div className="bg-blue-500/10 rounded p-3">
                        <div className="text-xs text-blue-400">聚焦</div>
                        <div className="text-lg font-medium mt-1">{shu.rhythm_control.focus.name}</div>
                        <div className="text-sm text-slate-400 mt-1">{shu.rhythm_control.focus.description}</div>
                        <div className="text-xs text-emerald-400 mt-2">成功指標: {shu.rhythm_control.focus.success_metric}</div>
                      </div>
                      {shu.rhythm_control.focus.avoid_list && shu.rhythm_control.focus.avoid_list.length > 0 && (
                        <div>
                          <div className="text-xs text-slate-500 mb-2">この期間やらないこと</div>
                          {shu.rhythm_control.focus.avoid_list.map((avoid: string, i: number) => (
                            <div key={i} className="text-sm text-red-400 flex items-center gap-2">
                              <span>❌</span> {avoid}
                            </div>
                          ))}
                        </div>
                      )}
                      <div className="grid grid-cols-2 gap-4 mt-3">
                        <div>
                          <div className="text-xs text-slate-500">チェックポイント</div>
                          <div className="text-sm mt-1">{shu.rhythm_control.checkpoint_date}</div>
                        </div>
                        <div>
                          <div className="text-xs text-slate-500">次の判断</div>
                          <div className="text-sm mt-1">{shu.rhythm_control.next_decision_point}</div>
                        </div>
                      </div>
                    </div>
                  )}
                </div>
              )}
            </div>
          )}

          {activeTab === 'qi' && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-emerald-500/10 flex items-center justify-center">🔧</span>
                器 / 技術実装 v3.0
              </h3>

              {/* v3.0: ドメイン固有技術 */}
              {qi.domain_technologies && qi.domain_technologies.length > 0 && (
                <div className="bg-emerald-500/5 rounded-lg p-5 border border-emerald-500/20">
                  <div className="text-sm font-medium text-emerald-400 mb-4 flex items-center gap-2">
                    <span>🛠️</span> ドメイン固有技術（具体名詞）
                  </div>
                  <div className="space-y-3">
                    {qi.domain_technologies.map((tech: any, i: number) => (
                      <div key={i} className="bg-emerald-500/10 rounded p-4">
                        <div className="flex items-center gap-2 mb-2">
                          <span className="font-medium text-emerald-400">{tech.technology_name}</span>
                          <span className="text-xs px-2 py-0.5 bg-slate-700 rounded">{tech.category}</span>
                        </div>
                        <div className="text-sm text-slate-400">{tech.why_required}</div>
                        {tech.alternatives && tech.alternatives.length > 0 && (
                          <div className="text-xs text-slate-500 mt-2">
                            代替: {tech.alternatives.join(', ')}
                          </div>
                        )}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 規制対応 */}
              {qi.regulatory_considerations && qi.regulatory_considerations.length > 0 && (
                <div className="bg-amber-500/5 rounded-lg p-5 border border-amber-500/20">
                  <div className="text-sm font-medium text-amber-400 mb-4 flex items-center gap-2">
                    <span>📜</span> 規制対応事項
                  </div>
                  <div className="overflow-x-auto">
                    <table className="w-full text-sm">
                      <thead>
                        <tr className="border-b border-slate-700">
                          <th className="text-left py-2 text-slate-500">地域</th>
                          <th className="text-left py-2 text-slate-500">規制</th>
                          <th className="text-left py-2 text-slate-500">要件</th>
                          <th className="text-left py-2 text-slate-500">実装影響</th>
                        </tr>
                      </thead>
                      <tbody>
                        {qi.regulatory_considerations.map((reg: any, i: number) => (
                          <tr key={i} className="border-b border-slate-800">
                            <td className="py-2 text-amber-400">{reg.region}</td>
                            <td className="py-2">{reg.regulation}</td>
                            <td className="py-2 text-slate-400">{reg.requirement}</td>
                            <td className="py-2 text-slate-400">{reg.implementation_impact}</td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </div>
              )}

              {/* v3.0: 地理的考慮 */}
              {qi.geographic_considerations && qi.geographic_considerations.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4 border border-blue-500/20">
                  <div className="text-sm font-medium text-blue-400 mb-4 flex items-center gap-2">
                    <span>🌍</span> 地理的考慮事項
                  </div>
                  <div className="space-y-3">
                    {qi.geographic_considerations.map((geo: any, i: number) => (
                      <div key={i} className="flex items-start gap-4 p-3 bg-blue-500/5 rounded">
                        <div className="text-blue-400 font-medium">{geo.region}</div>
                        <div className="flex-1">
                          <div className="text-sm text-slate-400">レイテンシ: {geo.latency_requirement}</div>
                          <div className="text-sm text-slate-500">インフラ: {geo.infrastructure_need}</div>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* 実装要素 */}
              {qi.implementations && qi.implementations.length > 0 && (
                <div>
                  <div className="text-sm font-medium text-slate-400 mb-3 flex items-center gap-2">
                    <span>🔧</span> 実装要素
                  </div>
                  {qi.implementations.map((impl: Implementation, i: number) => (
                    <div key={i} className="bg-[#0a0a0f] rounded-lg p-4 mb-3">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-medium">{impl.component}</span>
                        <span className="text-xs text-slate-500">{impl.estimated_effort}</span>
                      </div>
                      <div className="text-sm text-indigo-400 mb-2">{impl.technology}</div>
                      {impl.risks && impl.risks.length > 0 && (
                        <div className="text-xs text-amber-400">
                          ⚠️ {impl.risks.join(', ')}
                        </div>
                      )}
                    </div>
                  ))}
                </div>
              )}

              {qi.tool_recommendations && qi.tool_recommendations.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2">🧰 ツール推奨</div>
                  <div className="flex flex-wrap gap-2">
                    {qi.tool_recommendations.map((t: string, i: number) => (
                      <span key={i} className="px-2 py-1 bg-indigo-500/10 text-indigo-400 rounded text-xs">{t}</span>
                    ))}
                  </div>
                </div>
              )}

              {qi.integration_points && qi.integration_points.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2">🔗 統合ポイント</div>
                  <ul className="text-sm text-slate-400 space-y-1">
                    {qi.integration_points.map((p: string, i: number) => (
                      <li key={i}>• {p}</li>
                    ))}
                  </ul>
                </div>
              )}

              {qi.technical_debt_warnings && qi.technical_debt_warnings.length > 0 && (
                <div className="bg-amber-500/5 rounded-lg p-4 border border-amber-500/20">
                  <div className="text-xs text-amber-400 mb-2">⚠️ 技術負債警告</div>
                  <ul className="text-sm text-slate-400 space-y-1">
                    {qi.technical_debt_warnings.map((w: string, i: number) => (
                      <li key={i}>• {w}</li>
                    ))}
                  </ul>
                </div>
              )}
            </div>
          )}

          {activeTab === 'review' && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-amber-500/10 flex items-center justify-center">🔍</span>
                検証 / ReviewAgent
              </h3>

              <div className="flex items-center gap-3 mb-6">
                <span className="text-sm text-slate-400">判定結果:</span>
                <span className={`px-3 py-1 rounded-lg text-sm font-medium ${
                  review.overall_verdict === 'PASS' 
                    ? 'bg-emerald-500/10 text-emerald-400' 
                    : review.overall_verdict === 'REVISE'
                    ? 'bg-amber-500/10 text-amber-400'
                    : 'bg-red-500/10 text-red-400'
                }`}>
                  {review.overall_verdict}
                </span>
              </div>

              {review.findings && review.findings.length > 0 && (
                <div className="space-y-3">
                  {review.findings.map((finding, i) => (
                    <div key={i} className={`rounded-lg p-4 border ${
                      finding.severity === 'CRITICAL' 
                        ? 'bg-red-500/5 border-red-500/20'
                        : finding.severity === 'WARNING' 
                        ? 'bg-amber-500/5 border-amber-500/20' 
                        : 'bg-blue-500/5 border-blue-500/20'
                    }`}>
                      <div className="flex items-center gap-2 mb-2">
                        <span className={`text-xs px-2 py-0.5 rounded ${
                          finding.severity === 'CRITICAL'
                            ? 'bg-red-500/10 text-red-400'
                            : finding.severity === 'WARNING' 
                            ? 'bg-amber-500/10 text-amber-400' 
                            : 'bg-blue-500/10 text-blue-400'
                        }`}>
                          {finding.severity}
                        </span>
                        <span className="text-xs text-slate-500">{finding.category}</span>
                      </div>
                      <p className="text-sm text-slate-400">{finding.description}</p>
                      {finding.suggested_revision && (
                        <p className="text-xs text-slate-500 mt-2">💡 {finding.suggested_revision}</p>
                      )}
                    </div>
                  ))}
                </div>
              )}

              {review.final_warnings && review.final_warnings.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2">最終警告</div>
                  <ul className="text-sm text-slate-400 space-y-1">
                    {review.final_warnings.map((w: string, i: number) => (
                      <li key={i}>⚠️ {w}</li>
                    ))}
                  </ul>
                </div>
              )}
            </div>
          )}
        </div>

        {/* 署名セクション v3.1 */}
        <div className="mt-8 bg-[#12121a] rounded-xl border border-white/5 p-6">
          <h2 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <span className="text-slate-500">7.</span>
            ✍️ 署名欄
          </h2>

          {/* 署名テーブル（日本式） */}
          <div className="overflow-x-auto mb-6">
            <table className="w-full border-collapse text-sm">
              <tbody>
                {/* 作成欄 */}
                <tr className="border border-slate-700">
                  <th rowSpan={2} className="bg-slate-800/50 px-3 py-2 text-left w-20 border-r border-slate-700">作成</th>
                  <th className="bg-slate-800/30 px-3 py-2 text-left w-16 border-r border-slate-700">部署</th>
                  <td className="px-3 py-2 border-r border-slate-700">{authorDept}</td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left w-16 border-r border-slate-700">役職</th>
                  <td className="px-3 py-2">{authorPos}</td>
                </tr>
                <tr className="border border-slate-700 border-t-0">
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">氏名</th>
                  <td className="px-3 py-2 border-r border-slate-700">{authorName}</td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">日付</th>
                  <td className="px-3 py-2">{createdDate}</td>
                </tr>
                {/* 承認欄 */}
                <tr className="border border-slate-700 border-t-0">
                  <th rowSpan={2} className="bg-slate-800/50 px-3 py-2 text-left border-r border-slate-700">承認</th>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">部署</th>
                  <td className="px-3 py-2 border-r border-slate-700 text-slate-500">
                    {signatureStatus === 'signed' && signatureData ? signatureData.department : '（未承認）'}
                  </td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">役職</th>
                  <td className="px-3 py-2 text-slate-500">
                    {signatureStatus === 'signed' && signatureData ? signatureData.position : ''}
                  </td>
                </tr>
                <tr className="border border-slate-700 border-t-0">
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">氏名</th>
                  <td className="px-3 py-2 border-r border-slate-700 text-slate-500">
                    {signatureStatus === 'signed' && signatureData ? signatureData.signed_by : ''}
                  </td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">日付</th>
                  <td className="px-3 py-2 text-slate-500">
                    {signatureStatus === 'signed' && signatureData ? signatureData.signed_at_display : ''}
                  </td>
                </tr>
              </tbody>
            </table>
          </div>

          {/* 承認印エリア */}
          <div className="flex items-center justify-center gap-8">
            {signatureStatus === 'signed' && signatureData ? (
              /* 署名済み - 判子表示 */
              <div className="space-y-4 text-center">
                <div className="flex items-center justify-center gap-2 text-emerald-400 text-sm mb-4">
                  <span>✅</span>
                  <span className="font-medium">提案書が承認されました</span>
                </div>
                
                <SignatureArea
                  signerName={signatureData.signed_by}
                  department={signatureData.department}
                  position={signatureData.position}
                  signedAt={signatureData.signed_at_display}
                  animated={showSignedAnimation}
                />
              </div>
            ) : (
              /* 未署名 - 署名ボタン */
              <div className="flex flex-col items-center gap-4">
                <div className="w-24 h-24 rounded-full border-2 border-dashed border-slate-600 flex items-center justify-center text-slate-500 text-xs">
                  承認印
                </div>
                <div className="text-center">
                  <div className="text-sm text-slate-500 mb-2">この提案書に基づいて意思決定を行う場合</div>
                  {user && (
                    <div className="text-xs text-slate-400 mb-3">
                      署名者: {user.display_name} ({user.department})
                    </div>
                  )}
                  <button 
                    onClick={handleSign}
                    disabled={isSigning || !user}
                    className={`px-6 py-3 rounded-xl font-medium transition-all flex items-center gap-2 ${
                      isSigning
                        ? 'bg-slate-700 text-slate-400 cursor-wait'
                        : !user
                        ? 'bg-slate-800 text-slate-500 cursor-not-allowed'
                        : 'bg-gradient-to-r from-red-700 to-red-600 hover:from-red-600 hover:to-red-500 shadow-lg shadow-red-500/25 text-white'
                    }`}
                  >
                    {isSigning ? (
                      <>
                        <div className="w-4 h-4 border-2 border-white/30 border-t-white rounded-full animate-spin" />
                        署名処理中...
                      </>
                    ) : (
                      <>
                        <span className="text-xl">印</span>
                        電子署名
                      </>
                    )}
                  </button>
                </div>
              </div>
            )}
          </div>
        </div>

        {/* フッター */}
        <div className="mt-8 text-center text-xs text-slate-600 border-t border-slate-800 pt-4">
          <p>本提案書は AI Decision Support により自動生成されました</p>
          <p className="mt-1 font-mono">案件ID: {caseId} | Version: {report.version || '3.1'}</p>
        </div>
      </main>
    </div>
  );
};

/**
 * レポート画面コンポーネント.
 *
 * 目的: 決策レポートの表示・署名・エクスポート
 * API対接: GET /api/report/{id}/components, GET /api/report/{id}/pdf
 * 設計参考: design/decision-report-ui.tsx
 */

import React, { useCallback, useState, useEffect } from 'react';
import { useDecisionStore } from '../store/useDecisionStore';
import { decisionApi } from '../api/client';
import type { ExecutiveSummary, RecommendedPath, Phase, Implementation } from '../types';

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
  const [activeTab, setActiveTab] = useState<TabId>('summary');
  const [isExporting, setIsExporting] = useState(false);

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
    } catch (err) {
      console.error('PDF export failed:', err);
    } finally {
      setIsExporting(false);
    }
  }, [reportId]);

  /** 新規質問 */
  const handleNewQuestion = useCallback(() => {
    reset();
    setPage('input');
  }, [reset, setPage]);

  if (!report) return null;

  const { executive_summary, dao, fa, shu, qi, review } = report;

  return (
    <div className="min-h-screen bg-[#0a0a0f] text-white">
      {/* Header */}
      <header className="border-b border-white/5 px-6 py-4">
        <div className="max-w-6xl mx-auto flex items-center justify-between">
          <div className="flex items-center gap-3">
            <div className="w-10 h-10 rounded-xl bg-gradient-to-br from-indigo-500 to-violet-600 flex items-center justify-center">
              <span className="text-xl">⚡</span>
            </div>
            <div>
              <h1 className="font-semibold text-lg">Decision Agent</h1>
              <p className="text-xs text-slate-500">Decision Report #{reportId?.slice(-6) || '------'}</p>
            </div>
          </div>
          <div className="flex items-center gap-3">
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
        </div>
      </header>

      <main className="max-w-5xl mx-auto px-6 py-8">
        {/* エグゼクティブサマリー */}
        <div className="bg-gradient-to-br from-[#12121a] to-[#1a1a24] rounded-2xl border border-white/5 p-8 mb-8 relative overflow-hidden">
          <div className="absolute top-0 right-0 w-64 h-64 bg-indigo-500/5 rounded-full blur-3xl -translate-y-1/2 translate-x-1/2" />
          
          <div className="relative">
            <div className="flex items-start justify-between mb-6">
              <div>
                <h2 className="text-xs text-slate-500 uppercase tracking-wider mb-1">EXECUTIVE SUMMARY</h2>
                <div className="text-2xl font-bold">意思決定レポート</div>
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

            {/* 最初の一歩 */}
            <div className="bg-emerald-500/5 rounded-xl p-5 mb-6 border border-emerald-500/20">
              <div className="flex items-center gap-2 text-emerald-400 text-sm mb-2">
                <span>🎯</span> 最初の一歩（明日実行可能）
              </div>
              <p className="font-medium">{executive_summary.first_step}</p>
            </div>

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
                道 / 本質分析
              </h3>

              <div className="grid grid-cols-2 gap-4">
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-1">問題タイプ</div>
                  <div className="px-3 py-1 bg-indigo-500/10 text-indigo-400 rounded inline-block text-sm">
                    {dao.problem_type}
                  </div>
                </div>
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-1">本質（一文）</div>
                  <div className="text-sm">{dao.essence}</div>
                </div>
              </div>

              {dao.immutable_constraints && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-3">不可変制約</div>
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
                  <div className="text-xs text-slate-500 mb-3">隠れた前提</div>
                  <div className="space-y-2">
                    {dao.hidden_assumptions.map((a: string, i: number) => (
                      <div key={i} className="flex items-center gap-2 text-sm text-slate-400">
                        <span>💭</span> {a}
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
                法 / 戦略選定
              </h3>

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
                術 / 実行計画
              </h3>

              {shu.phases && <PhaseTimeline phases={shu.phases} />}

              {shu.first_action && (
                <div className="bg-emerald-500/5 rounded-lg p-4 border border-emerald-500/20">
                  <div className="text-xs text-emerald-400 mb-2">🎯 最初の一歩</div>
                  <div className="text-sm">{shu.first_action}</div>
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
            </div>
          )}

          {activeTab === 'qi' && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-emerald-500/10 flex items-center justify-center">🔧</span>
                器 / 技術実装
              </h3>

              {qi.implementations?.map((impl: Implementation, i: number) => (
                <div key={i} className="bg-[#0a0a0f] rounded-lg p-4">
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

              {qi.tool_recommendations && qi.tool_recommendations.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2">ツール推奨</div>
                  <div className="flex flex-wrap gap-2">
                    {qi.tool_recommendations.map((t: string, i: number) => (
                      <span key={i} className="px-2 py-1 bg-indigo-500/10 text-indigo-400 rounded text-xs">{t}</span>
                    ))}
                  </div>
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
                  {review.findings.map((finding: any, i: number) => (
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

        {/* 署名セクション */}
        <div className="mt-8 bg-[#12121a] rounded-xl border border-white/5 p-6">
          <div className="flex items-center justify-between">
            <div>
              <div className="text-sm text-slate-500 mb-1">このレポートに基づいて意思決定を行う場合</div>
              <div className="text-lg font-medium">署名して確定</div>
            </div>
            <button className="px-6 py-3 bg-gradient-to-r from-indigo-600 to-violet-600 hover:from-indigo-500 hover:to-violet-500 rounded-xl font-medium transition-all shadow-lg shadow-indigo-500/25 flex items-center gap-2">
              ✍️ 電子署名
            </button>
          </div>
        </div>
      </main>
    </div>
  );
};

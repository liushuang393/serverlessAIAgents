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
import { useI18n } from '../i18n';

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

/** タブID型定義 */
type TabId = 'summary' | 'dao' | 'fa' | 'shu' | 'qi' | 'review';

/** パスカード（v3.1: 条件付き評価対応） */
const PathCard: React.FC<{ path: RecommendedPath; isRecommended?: boolean }> = ({
  path,
  isRecommended,
}) => {
  const { t } = useI18n();
  return (
    <div className={`rounded-xl p-5 border ${isRecommended ? 'border-emerald-500/30 bg-emerald-500/5' : 'border-white/5 bg-[#0a0a0f] opacity-60'}`}>
      <div className="flex items-center justify-between mb-3">
        <div className="flex items-center gap-2">
          <span>{isRecommended ? '✓' : '✕'}</span>
          <span className="font-semibold">{path.path_id}: {path.name}</span>
          {!isRecommended && <span className="text-xs text-red-400 px-2 py-0.5 bg-red-500/10 rounded">{t('report.path_not_recommended')}</span>}
        </div>
        {path.reversibility && (
          <span className="text-xs px-2 py-0.5 bg-slate-700 text-slate-300 rounded">{t('report.path_reversibility')} {path.reversibility}</span>
        )}
      </div>
      <p className="text-sm text-slate-400 mb-4">{path.description}</p>

      {/* v3.1: 条件付き評価 */}
      {path.conditional_evaluation && (
        <div className="bg-slate-800/50 rounded-lg p-3 mb-4 space-y-2">
          <div className="text-xs font-medium text-cyan-400 mb-2"><span aria-hidden="true">📋</span> {t('report.path_conditional_eval')}</div>
          {path.conditional_evaluation.success_conditions?.length > 0 && (
            <div>
              <span className="text-xs text-emerald-400">{t('report.path_success_conditions')}</span>
              <div className="flex flex-wrap gap-1 mt-1">
                {path.conditional_evaluation.success_conditions.map((c: string, ci: number) => (
                  <span key={`sc-${ci}`} className="text-xs px-2 py-0.5 bg-emerald-500/10 text-emerald-400 rounded">{c}</span>
                ))}
              </div>
            </div>
          )}
          {path.conditional_evaluation.risk_factors?.length > 0 && (
            <div>
              <span className="text-xs text-amber-400">{t('report.path_risk_factors')}</span>
              <div className="flex flex-wrap gap-1 mt-1">
                {path.conditional_evaluation.risk_factors.map((r: string, ri: number) => (
                  <span key={`rf-${ri}`} className="text-xs px-2 py-0.5 bg-amber-500/10 text-amber-400 rounded">{r}</span>
                ))}
              </div>
            </div>
          )}
          {path.conditional_evaluation.failure_modes?.length > 0 && (
            <div>
              <span className="text-xs text-red-400">{t('report.path_failure_modes')}</span>
              <div className="flex flex-wrap gap-1 mt-1">
                {path.conditional_evaluation.failure_modes.map((f: string, fi: number) => (
                  <span key={`fm-${fi}`} className="text-xs px-2 py-0.5 bg-red-500/10 text-red-400 rounded">{f}</span>
                ))}
              </div>
            </div>
          )}
        </div>
      )}

      {/* リスク集中点 */}
      {path.risk_concentration && (
        <div className="text-xs text-amber-400 mb-3"><span aria-hidden="true">⚡</span> {t('report.path_risk_concentration')} <span className="text-slate-400">{path.risk_concentration}</span></div>
      )}

      <div className="grid grid-cols-2 gap-4">
        <div>
          <div className="text-xs text-emerald-400 mb-2">{t('report.path_pros')}</div>
          {path.pros.map((p, i) => (
            <div key={`pro-${i}`} className="text-sm text-slate-400 flex items-center gap-2 mb-1">
              <span className="text-emerald-400">+</span> {p}
            </div>
          ))}
        </div>
        <div>
          <div className="text-xs text-amber-400 mb-2">{t('report.path_cons')}</div>
          {path.cons.map((c, i) => (
            <div key={`con-${i}`} className="text-sm text-slate-400 flex items-center gap-2 mb-1">
              <span className="text-amber-400">-</span> {c}
            </div>
          ))}
        </div>
      </div>
      {path.time_to_value && (
        <div className="mt-3 text-xs text-slate-500"><span aria-hidden="true">⏱️</span> {t('report.path_time_to_value')} {path.time_to_value}</div>
      )}
    </div>
  );
};

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
  const { report, reportId, requestId, question, setPage, reset } = useDecisionStore();
  const { user, performLogout } = useAuthStore();
  const { t } = useI18n();
  const [activeTab, setActiveTab] = useState<TabId>('summary');

  /** タブ定義（ロケール対応） */
  const TABS: readonly { id: TabId; name: string; icon: string }[] = [
    { id: 'summary', name: t('report.tab_summary'), icon: '📊' },
    { id: 'dao', name: t('report.tab_dao'), icon: '🎯' },
    { id: 'fa', name: t('report.tab_fa'), icon: '🛤️' },
    { id: 'shu', name: t('report.tab_shu'), icon: '📋' },
    { id: 'qi', name: t('report.tab_qi'), icon: '🔧' },
    { id: 'review', name: t('report.tab_review'), icon: '🔍' },
  ];
  const [exportingType, setExportingType] = useState<"pdf" | "html" | null>(null);
  const [isSigning, setIsSigning] = useState(false);
  const [notification, setNotification] = useState<{type: NotificationType; message: string} | null>(null);
  const [humanReviewNotes, setHumanReviewNotes] = useState<Record<number, string>>({});
  const [humanReviewChecks, setHumanReviewChecks] = useState<Record<number, boolean>>({});
  const [savingFindingNotes, setSavingFindingNotes] = useState<Record<number, boolean>>({});

  // v3.1 差分パッチ型: チェックポイント状態
  const [checkpointChecks, setCheckpointChecks] = useState<Record<string, boolean>>({});
  const [checkpointAnnotations, setCheckpointAnnotations] = useState<Record<string, string>>({});
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
    const exportId = requestId || reportId;
    if (!exportId) return;
    setExportingType("pdf");

    try {
      const exported = await decisionApi.exportPdf(exportId);
      const url = URL.createObjectURL(exported.blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = exported.filename;
      a.click();
      URL.revokeObjectURL(url);
      setNotification({ type: 'success', message: t('report.pdf_downloaded') });
    } catch (err) {
      const message = err instanceof Error ? err.message : t('report.pdf_failed');
      setNotification({ type: 'error', message });
    } finally {
      setExportingType(null);
    }
  }, [requestId, reportId]);

  /** HTML エクスポート */
  const handleExportHtml = useCallback(async () => {
    const exportId = requestId || reportId;
    if (!exportId) return;
    setExportingType("html");

    try {
      const exported = await decisionApi.exportHtml(exportId);
      const url = URL.createObjectURL(exported.blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = exported.filename;
      a.click();
      URL.revokeObjectURL(url);
      setNotification({ type: 'success', message: t('report.html_downloaded') });
    } catch (err) {
      const message = err instanceof Error ? err.message : t('report.html_failed');
      setNotification({ type: 'error', message });
    } finally {
      setExportingType(null);
    }
  }, [requestId, reportId]);

  /** 電子署名処理 */
  const handleSign = useCallback(async () => {
    if (!reportId || signatureStatus === 'signed' || !user) return;
    setIsSigning(true);

    try {
      // 署名確認ダイアログ
      const confirmed = window.confirm(
        t('report.sign_confirm').replaceAll('{name}', user.display_name)
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
          message: t('report.signed_by').replaceAll('{name}', user.display_name)
        });
        
        // アニメーション後にリセット
        setTimeout(() => setShowSignedAnimation(false), 1000);
      } else {
        setNotification({ type: 'error', message: response.message });
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : t('report.sign_failed');
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

  /** 所見メモ（任意）を保存 */
  const handleSaveFindingMemo = useCallback(async (findingIndex: number) => {
    if (!reportId) return;
    const memo = humanReviewNotes[findingIndex] ?? '';
    const acknowledged = Boolean(humanReviewChecks[findingIndex]);

    setSavingFindingNotes((prev) => ({ ...prev, [findingIndex]: true }));
    try {
      await decisionApi.logFindingNote({
        report_id: reportId,
        request_id: requestId || undefined,
        finding_index: findingIndex,
        acknowledged,
        memo,
        reviewer_name: user?.display_name || undefined,
      });
      setNotification({ type: 'success', message: t('report.finding_memo_saved').replaceAll('{index}', String(findingIndex + 1)) });
    } catch (err) {
      const message = err instanceof Error ? err.message : t('report.finding_memo_failed');
      setNotification({ type: 'error', message });
    } finally {
      setSavingFindingNotes((prev) => ({ ...prev, [findingIndex]: false }));
    }
  }, [humanReviewChecks, humanReviewNotes, reportId, requestId, user?.display_name]);



  if (!report) return null;

  /** 表示用テキストを安全に整形（object直表示の防止） */
  const toDisplayText = (value: unknown, fallback = ""): string => {
    if (value === null || value === undefined) {
      return fallback;
    }
    if (typeof value === "string") {
      const text = value.trim();
      return text || fallback;
    }
    if (typeof value === "number" || typeof value === "boolean") {
      return String(value);
    }
    if (Array.isArray(value)) {
      const items = value
        .map((item) => toDisplayText(item))
        .filter((item) => item.length > 0);
      return items.length > 0 ? items.join("、") : fallback;
    }
    if (typeof value === "object") {
      try {
        const compact = JSON.stringify(value);
        return compact === "{}" ? fallback : compact;
      } catch {
        return fallback;
      }
    }
    return fallback;
  };

  // 各セクションを安全に取得（古いデータ形式への対応）
  const { dao, fa, shu, qi, review, proposal_title, signature_block } = report;

  // 各セクションのデフォルト値（未定義の場合の安全対策）
  // executive_summary が undefined の場合のフォールバック
  const safeExecutiveSummary = report.executive_summary || {
    one_line_decision: '分析結果を確認してください',
    recommended_action: '詳細は下記セクションを参照',
    first_step: '詳細は下記セクションを参照',
    key_risks: [],
    estimated_impact: '',
  };

  const safeDao = dao || {
    problem_type: 'N/A',
    problem_nature: null,
    essence: '分析データがありません',
    essence_derivation: null,
    existing_alternatives: [],
    immutable_constraints: [],
    hidden_assumptions: [],
    causal_gears: [],
    bottleneck_gear: null,
    death_traps: [],
  };

  const safeFa = fa || {
    recommended_paths: [],
    rejected_paths: [],
    decision_criteria: [],
    path_comparison: null,
    strategic_prohibitions: [],
    differentiation_axis: null,
    why_existing_fails: '',
    competitive_hypothesis: null,
    judgment_framework: null,
    fa_self_check: null,
  };

  const safeShu = shu || {
    phases: [],
    first_action: '分析データがありません',
    dependencies: [],
    rhythm_control: null,
    cut_list: [],
    context_specific_actions: [],
    single_validation_point: null,
    exit_criteria: null,
  };

  const safeQi = qi || {
    implementations: [],
    tool_recommendations: [],
    integration_points: [],
    technical_debt_warnings: [],
    domain_technologies: [],
    regulatory_considerations: [],
    geographic_considerations: [],
  };

  const safeReview = review || {
    overall_verdict: 'REVISE',
    findings: [],
    confidence_score: 0,
    final_warnings: [],
  };
  const missingCountermeasureCount = (safeReview.findings || []).filter(
    (finding) => !(finding.suggested_revision || '').trim()
  ).length;

  // レビューが未生成の古いデータでは「未検証」を表示
  const reviewVerdict = review?.overall_verdict;
  const reviewStatusLabel = !reviewVerdict ? t('report.verdict_unverified') : reviewVerdict === "PASS" ? t('report.verdict_pass') : reviewVerdict === "REVISE" ? t('report.verdict_revise') : t('report.verdict_coach');
  const reviewStatusClass = !reviewVerdict
    ? "bg-slate-500/10 text-slate-400 border border-slate-500/30"
    : reviewVerdict === "PASS"
    ? "bg-emerald-500/20 text-emerald-400"
    : reviewVerdict === "REVISE"
    ? "bg-amber-500/20 text-amber-400"
    : "bg-blue-500/20 text-blue-400";
  const reviewStatusClassWithBorder = !reviewVerdict
    ? "bg-slate-500/10 text-slate-400 border border-slate-500/30"
    : reviewVerdict === "PASS"
    ? "bg-emerald-500/10 text-emerald-400 border border-emerald-500/30"
    : reviewVerdict === "REVISE"
    ? "bg-amber-500/10 text-amber-400 border border-amber-500/30"
    : "bg-blue-500/10 text-blue-400 border border-blue-500/30";
  const reviewStatusIcon = !reviewVerdict ? "🕒" : reviewVerdict === "PASS" ? "✅" : reviewVerdict === "REVISE" ? "⚠️" : "📋";
  const analysisQuestion = toDisplayText(
    report.original_question ?? (report as unknown as { question?: unknown }).question ?? question,
    t('report.question_not_set')
  );

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
              <h1 className="font-semibold text-lg">{t('report.proposal_header')}</h1>
              <p className="text-xs text-slate-500 font-mono">{caseId}</p>
            </div>
          </div>
          <div className="flex items-center gap-4">
            {/* アクションボタン */}
            <div className="flex items-center gap-2">
              <button
                onClick={handleExportPdf}
                disabled={exportingType !== null}
                className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all"
              >
                <span aria-hidden="true">📄</span> {exportingType === 'pdf' ? t('report.generating') : t('report.pdf_export')}
              </button>
              <button
                onClick={handleExportHtml}
                disabled={exportingType !== null}
                className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all"
              >
                <span aria-hidden="true">🧾</span> {exportingType === 'html' ? t('report.generating') : t('report.html_export')}
              </button>
              <button
                onClick={() => setPage('history')}
                className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all"
              >
                <span aria-hidden="true">📜</span> {t('report.history_btn')}
              </button>
              <button
                onClick={handleNewQuestion}
                className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all"
              >
                <span aria-hidden="true">🔄</span> {t('report.reanalyze_btn')}
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
                  title={t('report.logout_btn')}
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
            <p className="text-slate-400 mb-6">{t('report.dear')}</p>
            <h1 className="text-3xl font-bold mb-2 tracking-wider">{titleJa}</h1>
            <p className="text-sm text-slate-500 font-mono mb-4">{titleEn}</p>
            {subtitle && <p className="text-slate-400 text-sm mb-4">{subtitle}</p>}
            <p className="text-xs text-slate-600 font-mono mb-8">{t('report.case_id_prefix')} {caseId}</p>
            
            <div className="border-t border-white/10 pt-6 mt-6">
              <p className="text-sm text-slate-400">{createdDate}</p>
              <p className="text-sm text-slate-400 mt-2">{authorDept}</p>
              <p className="text-sm text-slate-400">{authorPos} {authorName}</p>
            </div>
          </div>
        </div>

        {/* エグゼクティブサマリー v3.2 */}
        <div className="bg-gradient-to-br from-[#12121a] to-[#1a1a24] rounded-2xl border border-white/5 p-8 mb-8 relative overflow-hidden">
          <div className="absolute top-0 right-0 w-64 h-64 bg-indigo-500/5 rounded-full blur-3xl -translate-y-1/2 translate-x-1/2" />

          <div className="relative">
            <div className="flex items-start justify-between mb-6">
              <div>
                <h2 className="text-xs text-slate-500 uppercase tracking-wider mb-1">1. EXECUTIVE SUMMARY</h2>
                <div className="text-2xl font-bold">{t('report.executive_summary')}</div>
              </div>
              {/* 信頼度スコア（判定結果と連動） */}
              <div className="text-right">
                <div className="flex items-center gap-2 justify-end mb-1">
                  <span className={`text-xs px-2 py-0.5 rounded ${reviewStatusClass}`}>
                    {reviewStatusLabel}
                  </span>
                </div>
                <div className="text-xs text-slate-500 mb-1">
                  {t('report.confidence_score')}
                  <span
                    className="ml-1 text-slate-600 cursor-help"
                    title={t('report.confidence_tooltip')}
                  >ⓘ</span>
                </div>
                <div className={`text-3xl font-bold ${
                  (safeReview?.confidence_score ?? 0) >= 0.7 ? 'text-emerald-400' :
                  (safeReview?.confidence_score ?? 0) >= 0.4 ? 'text-amber-400' : 'text-red-400'
                }`}>
                  {Math.round((safeReview?.confidence_score ?? 0) * 100)}%
                </div>
                {/* スコアが低い場合の警告 */}
                {(safeReview?.confidence_score ?? 0) < 0.4 && (
                  <div className="text-xs text-red-400 mt-1">
                    ⚠ {t('report.recommend_reanalysis')}
                  </div>
                )}
              </div>
            </div>

            {/* 結論 */}
            <div className="bg-[#0a0a0f] rounded-xl p-6 mb-6 border border-indigo-500/20">
              <div className="flex items-center gap-2 text-indigo-400 text-sm mb-2">
                <span aria-hidden="true">💡</span> {t('report.conclusion_label')}
              </div>
              <p className="text-lg font-medium">{safeExecutiveSummary.one_line_decision}</p>
            </div>

            {/* v3.0: 本質の一文 */}
            {(safeExecutiveSummary as any).essence_statement && (
              <div className="bg-purple-500/5 rounded-xl p-5 mb-6 border border-purple-500/20">
                <div className="flex items-center gap-2 text-purple-400 text-sm mb-2">
                  <span aria-hidden="true">📍</span> {t('report.essence_label')}
                </div>
                <p className="font-medium">{(safeExecutiveSummary as any).essence_statement}</p>
              </div>
            )}

            {/* 最初の一歩 */}
            <div className="bg-emerald-500/5 rounded-xl p-5 mb-6 border border-emerald-500/20">
              <div className="flex items-center gap-2 text-emerald-400 text-sm mb-2">
                <span aria-hidden="true">🎯</span> {t('report.first_step_label')}
              </div>
              <p className="font-medium">{safeExecutiveSummary.first_step}</p>
            </div>

            {/* v3.0: 戦略的禁止事項サマリー */}
            {(safeExecutiveSummary as any).strategic_prohibition_summary && (
              <div className="bg-red-500/5 rounded-xl p-5 mb-6 border border-red-500/20">
                <div className="flex items-center gap-2 text-red-400 text-sm mb-2">
                  <span aria-hidden="true">⛔</span> {t('report.strategic_prohibition_label')}
                </div>
                <p className="text-sm text-slate-400">{(safeExecutiveSummary as any).strategic_prohibition_summary}</p>
              </div>
            )}

            {/* v3.0: 撤退基準サマリー */}
            {(safeExecutiveSummary as any).exit_criteria_summary && (
              <div className="bg-amber-500/5 rounded-xl p-5 mb-6 border border-amber-500/20">
                <div className="flex items-center gap-2 text-amber-400 text-sm mb-2">
                  <span aria-hidden="true">🚪</span> {t('report.exit_criteria_label')}
                </div>
                <p className="text-sm text-slate-400">{(safeExecutiveSummary as any).exit_criteria_summary}</p>
              </div>
            )}

            {/* 主要リスク */}
            <div>
              <div className="flex items-center gap-2 text-amber-400 text-sm mb-3">
                <span aria-hidden="true">⚠️</span> {t('report.key_risks_label')}
              </div>
              <div className="grid grid-cols-1 gap-2">
                {safeExecutiveSummary.key_risks.map((risk, i) => (
                  <div key={i} className="flex items-center gap-3 text-sm text-slate-400">
                    <span className="w-1.5 h-1.5 rounded-full bg-amber-500" />
                    {risk}
                  </div>
                ))}
              </div>
            </div>
          </div>
        </div>

        {/* タブナビゲーション（検証タブに状態バッジ追加） */}
        <div className="flex gap-2 mb-6 border-b border-white/5 pb-4 flex-wrap">
          {TABS.map((tab) => {
            // 検証タブに特別なバッジを追加
            const isReviewTab = tab.id === 'review';
            const reviewBadgeColor = !reviewVerdict
              ? 'bg-slate-500'
              : safeReview.overall_verdict === 'PASS'
              ? 'bg-emerald-500'
              : safeReview.overall_verdict === 'REVISE'
              ? 'bg-amber-500'
              : 'bg-blue-500';

            return (
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
                {/* 検証タブには判定ステータスバッジを表示 */}
                {isReviewTab && (
                  <span className={`w-2 h-2 rounded-full ${reviewBadgeColor}`} />
                )}
                {/* 検証タブに指摘件数があれば表示 */}
                {isReviewTab && safeReview.findings && safeReview.findings.length > 0 && (
                  <span className="text-xs px-1.5 py-0.5 bg-amber-500/20 text-amber-400 rounded">
                    {safeReview.findings.length}
                  </span>
                )}
              </button>
            );
          })}
        </div>

        {/* タブコンテンツ */}
        <div className="bg-[#12121a] rounded-xl border border-white/5 p-6">
          {activeTab === 'summary' && (
            <div className="space-y-6">
              {/* 分析概要ヘッダー */}
              <div className="flex items-center justify-between">
                <h3 className="text-lg font-semibold flex items-center gap-2">
                  <span className="w-8 h-8 rounded-lg bg-indigo-500/10 flex items-center justify-center">📊</span>
                  {t('report.section_overview')}
                </h3>
                {/* 検証ステータスバッジ */}
                <div className={`px-4 py-2 rounded-lg text-sm font-medium flex items-center gap-2 ${reviewStatusClassWithBorder}`}>
                  <span>{reviewStatusIcon}</span>
                  {t('report.verification_label')} {reviewStatusLabel}
                </div>
              </div>

              {/* 質問の再掲示 */}
              <div className="bg-[#0a0a0f] rounded-lg p-4 border border-white/10">
                <div className="text-xs text-slate-500 mb-2"><span aria-hidden="true">📝</span> {t('report.analysis_question')}</div>
                <p className="text-slate-300">{analysisQuestion}</p>
              </div>

              {/* 分析セクションナビゲーション */}
              <div className="space-y-4">
                <div className="text-sm text-slate-400 mb-3">{t('report.section_nav_hint')}</div>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                  {/* 道 */}
                  <button
                    onClick={() => setActiveTab('dao')}
                    className="p-4 bg-[#0a0a0f] rounded-lg border border-white/5 hover:border-indigo-500/30 transition-all text-left group"
                  >
                    <div className="flex items-center gap-3 mb-2">
                      <span className="text-xl">🎯</span>
                      <span className="font-medium group-hover:text-indigo-400 transition-colors">{t('report.dao_title')}</span>
                    </div>
                    <p className="text-xs text-slate-500 line-clamp-2">
                      {safeDao.essence || t('report.dao_section_desc')}
                    </p>
                  </button>

                  {/* 法 */}
                  <button
                    onClick={() => setActiveTab('fa')}
                    className="p-4 bg-[#0a0a0f] rounded-lg border border-white/5 hover:border-violet-500/30 transition-all text-left group"
                  >
                    <div className="flex items-center gap-3 mb-2">
                      <span className="text-xl">🛤️</span>
                      <span className="font-medium group-hover:text-violet-400 transition-colors">{t('report.fa_title')}</span>
                    </div>
                    <p className="text-xs text-slate-500 line-clamp-2">
                      {safeFa.recommended_paths?.length
                        ? t('report.fa_section_desc_with_count').replaceAll('{count}', String(safeFa.recommended_paths.length))
                        : t('report.fa_section_desc')}
                    </p>
                  </button>

                  {/* 術 */}
                  <button
                    onClick={() => setActiveTab('shu')}
                    className="p-4 bg-[#0a0a0f] rounded-lg border border-white/5 hover:border-blue-500/30 transition-all text-left group"
                  >
                    <div className="flex items-center gap-3 mb-2">
                      <span className="text-xl">📋</span>
                      <span className="font-medium group-hover:text-blue-400 transition-colors">{t('report.shu_title')}</span>
                    </div>
                    <p className="text-xs text-slate-500 line-clamp-2">
                      {safeShu.phases?.length
                        ? t('report.shu_section_desc_with_count').replaceAll('{count}', String(safeShu.phases.length))
                        : t('report.shu_section_desc')}
                    </p>
                  </button>

                  {/* 器 */}
                  <button
                    onClick={() => setActiveTab('qi')}
                    className="p-4 bg-[#0a0a0f] rounded-lg border border-white/5 hover:border-emerald-500/30 transition-all text-left group"
                  >
                    <div className="flex items-center gap-3 mb-2">
                      <span className="text-xl">🔧</span>
                      <span className="font-medium group-hover:text-emerald-400 transition-colors">{t('report.qi_title')}</span>
                    </div>
                    <p className="text-xs text-slate-500 line-clamp-2">
                      {safeQi.implementations?.length
                        ? t('report.qi_section_desc_with_count').replaceAll('{count}', String(safeQi.implementations.length))
                        : t('report.qi_section_desc')}
                    </p>
                  </button>
                </div>

                {/* 検証セクション（特別強調） */}
                <button
                  onClick={() => setActiveTab('review')}
                  className={`w-full p-4 rounded-lg border-2 border-dashed transition-all text-left ${
                    !reviewVerdict
                      ? 'bg-slate-500/5 border-slate-500/30 hover:border-slate-500/50'
                      : safeReview.overall_verdict === 'PASS'
                      ? 'bg-emerald-500/5 border-emerald-500/30 hover:border-emerald-500/50'
                      : safeReview.overall_verdict === 'REVISE'
                      ? 'bg-amber-500/5 border-amber-500/30 hover:border-amber-500/50'
                      : 'bg-blue-500/5 border-blue-500/30 hover:border-blue-500/50'
                  }`}
                >
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-3">
                      <span className="text-xl">🔍</span>
                      <div>
                        <span className="font-medium">{t('report.review_title')}</span>
                        <p className="text-xs text-slate-500 mt-1">
                          {safeReview.findings?.length
                            ? t('report.review_findings_count').replaceAll('{count}', String(safeReview.findings.length))
                            : t('report.review_section_desc')}
                        </p>
                      </div>
                    </div>
                    <span className={`text-sm font-medium ${
                      !reviewVerdict ? 'text-slate-400' :
                      safeReview.overall_verdict === 'PASS' ? 'text-emerald-400' :
                      safeReview.overall_verdict === 'REVISE' ? 'text-amber-400' : 'text-blue-400'
                    }`}>
                      {reviewStatusLabel} →
                    </span>
                  </div>
                </button>
              </div>

              {/* 修正が必要な場合のガイダンス */}
              {Boolean(reviewVerdict) && safeReview.overall_verdict !== 'PASS' && (
                <div className="mt-4 p-4 bg-amber-500/5 rounded-lg border border-amber-500/20">
                  <div className="flex items-start gap-3">
                    <span className="text-amber-400 mt-0.5">💡</span>
                    <div>
                      <div className="text-sm font-medium text-amber-400 mb-1">{t('report.revision_needed')}</div>
                      <div className="text-sm text-slate-400">
                        {t('report.revision_guidance')}
                      </div>
                    </div>
                  </div>
                </div>
              )}
            </div>
          )}

          {activeTab === 'dao' && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-indigo-500/10 flex items-center justify-center">🎯</span>
                {t('report.dao_title')}
              </h3>

              <div className="grid grid-cols-2 gap-4">
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-1">{t('report.dao_problem_type')}</div>
                  <div className="px-3 py-1 bg-indigo-500/10 text-indigo-400 rounded inline-block text-sm">
                    {safeDao.problem_type}
                  </div>
                </div>
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-1">{t('report.dao_problem_nature')}</div>
                  <div className="px-3 py-1 bg-purple-500/10 text-purple-400 rounded inline-block text-sm">
                    {safeDao.problem_nature || 'N/A'}
                  </div>
                </div>
              </div>

              {/* 本質（一文） */}
              <div className="bg-gradient-to-r from-indigo-500/10 to-purple-500/10 rounded-lg p-5 border border-indigo-500/20">
                <div className="text-xs text-indigo-400 mb-2"><span aria-hidden="true">📍</span> {t('report.dao_essence_one_line')}</div>
                <div className="text-lg font-medium">{safeDao.essence}</div>
              </div>

              {/* v3.0: 本質導出プロセス */}
              {safeDao.essence_derivation && (
                <div className="bg-[#0a0a0f] rounded-lg p-5 border border-blue-500/20">
                  <div className="text-sm font-medium text-blue-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">🔍</span> {t('report.dao_essence_derivation')}
                  </div>
                  <div className="space-y-3">
                    <div>
                      <div className="text-xs text-slate-500">{t('report.dao_surface_problem')}</div>
                      <div className="text-sm mt-1">{safeDao.essence_derivation.surface_problem}</div>
                    </div>
                    <div className="w-full h-px bg-slate-800" />
                    <div>
                      <div className="text-xs text-slate-500">{t('report.dao_underlying_why')}</div>
                      <div className="text-sm mt-1">{safeDao.essence_derivation.underlying_why}</div>
                    </div>
                    <div className="w-full h-px bg-slate-800" />
                    <div>
                      <div className="text-xs text-slate-500">{t('report.dao_root_constraint')}</div>
                      <div className="text-sm mt-1">{safeDao.essence_derivation.root_constraint}</div>
                    </div>
                    <div className="w-full h-px bg-slate-800" />
                    <div className="bg-blue-500/5 rounded p-3">
                      <div className="text-xs text-blue-400">{t('report.dao_essence_statement')}</div>
                      <div className="text-sm mt-1 font-medium">{safeDao.essence_derivation.essence_statement}</div>
                    </div>
                  </div>
                </div>
              )}

              {/* v3.0: 既存代替手段 */}
              {safeDao.existing_alternatives && safeDao.existing_alternatives.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-sm font-medium text-amber-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">🔄</span> {t('report.dao_existing_alternatives')}
                  </div>
                  <div className="space-y-3">
                    {safeDao.existing_alternatives.map((alt: any, i: number) => (
                      <div key={i} className="bg-amber-500/5 rounded p-3 border border-amber-500/10">
                        <div className="font-medium text-amber-400 text-sm">{alt.name}</div>
                        <div className="text-sm text-slate-400 mt-1">{alt.why_not_viable}</div>
                        <div className="text-xs text-slate-500 mt-1">{t('report.dao_constraint_label')} {alt.specific_constraint}</div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {safeDao.immutable_constraints && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-3"><span aria-hidden="true">🔒</span> {t('report.dao_immutable_constraints')}</div>
                  <div className="space-y-2">
                    {safeDao.immutable_constraints.map((c: string, i: number) => (
                      <div key={i} className="flex items-center gap-2 text-sm">
                        <span className="text-red-400">🔒</span> {c}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {safeDao.hidden_assumptions && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-3"><span aria-hidden="true">💭</span> {t('report.dao_hidden_assumptions')}</div>
                  <div className="space-y-2">
                    {safeDao.hidden_assumptions.map((a: string, i: number) => (
                      <div key={i} className="flex items-center gap-2 text-sm text-slate-400">
                        <span>💭</span> {a}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 因果齿轮 */}
              {safeDao.causal_gears && safeDao.causal_gears.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-sm font-medium text-cyan-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">⚙️</span> {t('report.dao_causal_gears')}
                  </div>
                  <div className="space-y-2">
                    {safeDao.causal_gears.map((gear: any, i: number) => (
                      <div key={i} className={`flex items-start gap-3 p-2 rounded ${
                        gear.name === safeDao.bottleneck_gear ? 'bg-cyan-500/10 border border-cyan-500/30' : ''
                      }`}>
                        <span className="text-cyan-400">⚙️</span>
                        <div className="flex-1">
                          <div className="flex items-center gap-2">
                            <span className="font-medium text-sm">{gear.name}</span>
                            <span className="text-xs px-2 py-0.5 bg-slate-700 rounded">Leverage: {gear.leverage}</span>
                            {gear.name === safeDao.bottleneck_gear && (
                              <span className="text-xs px-2 py-0.5 bg-cyan-500/20 text-cyan-400 rounded">{t('report.dao_bottleneck')}</span>
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
              {safeDao.death_traps && safeDao.death_traps.length > 0 && (
                <div className="bg-red-500/5 rounded-lg p-4 border border-red-500/20">
                  <div className="text-sm font-medium text-red-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">💀</span> {t('report.dao_death_traps')}
                  </div>
                  <div className="space-y-3">
                    {safeDao.death_traps.map((trap: any, i: number) => (
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

              {/* v3.1: 制約境界条件 */}
              {safeDao.constraint_boundaries && safeDao.constraint_boundaries.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-sm font-medium text-orange-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">🚧</span> {t('report.dao_constraint_boundaries')}
                  </div>
                  <div className="overflow-x-auto">
                    <table className="w-full text-sm">
                      <thead>
                        <tr className="border-b border-slate-700">
                          <th className="text-left py-2 px-3 text-slate-400 font-medium">{t('report.dao_constraint_name')}</th>
                          <th className="text-left py-2 px-3 text-slate-400 font-medium">{t('report.dao_judgment_condition')}</th>
                          <th className="text-left py-2 px-3 text-slate-400 font-medium">{t('report.dao_violation_example')}</th>
                          <th className="text-left py-2 px-3 text-slate-400 font-medium">{t('report.dao_exception')}</th>
                        </tr>
                      </thead>
                      <tbody>
                        {safeDao.constraint_boundaries.map((cb: any, i: number) => (
                          <tr key={i} className="border-b border-slate-800">
                            <td className="py-2 px-3 text-orange-300 font-medium">{cb.constraint_name}</td>
                            <td className="py-2 px-3 text-slate-300">{cb.definition}</td>
                            <td className="py-2 px-3 text-red-400/80">{cb.violation_example}</td>
                            <td className="py-2 px-3 text-slate-500">{cb.exceptions}</td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </div>
              )}

              {/* v3.1: 成立ルート比較 */}
              {safeDao.solution_routes && safeDao.solution_routes.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-sm font-medium text-teal-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">🛤️</span> {t('report.dao_solution_routes')}
                  </div>
                  <div className="grid gap-3">
                    {safeDao.solution_routes.map((sr: any, i: number) => (
                      <div key={i} className="bg-teal-500/5 rounded p-3 border border-teal-500/10">
                        <div className="flex items-center gap-2 mb-2">
                          <span className="px-2 py-0.5 bg-teal-500/20 text-teal-400 rounded text-xs font-medium">{sr.route_type}</span>
                          <span className="text-xs text-slate-500">{t('report.dao_viability')} {sr.viability}</span>
                        </div>
                        <div className="text-sm text-slate-300">{sr.description}</div>
                        {sr.tradeoffs && sr.tradeoffs.length > 0 && (
                          <div className="flex gap-2 mt-2 flex-wrap">
                            {sr.tradeoffs.map((t: string, j: number) => (
                              <span key={j} className="text-xs px-2 py-0.5 bg-amber-500/10 text-amber-400 rounded">⚖️ {t}</span>
                            ))}
                          </div>
                        )}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.1: 定量指標 */}
              {safeDao.quantified_metrics && safeDao.quantified_metrics.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-sm font-medium text-sky-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">📊</span> {t('report.dao_quantified_metrics')}
                  </div>
                  <div className="overflow-x-auto">
                    <table className="w-full text-sm">
                      <thead>
                        <tr className="border-b border-slate-700">
                          <th className="text-left py-2 px-3 text-slate-400 font-medium">{t('report.dao_priority')}</th>
                          <th className="text-left py-2 px-3 text-slate-400 font-medium">{t('report.dao_metric_name')}</th>
                          <th className="text-left py-2 px-3 text-slate-400 font-medium">{t('report.dao_target_value')}</th>
                          <th className="text-left py-2 px-3 text-slate-400 font-medium">{t('report.dao_tradeoff')}</th>
                        </tr>
                      </thead>
                      <tbody>
                        {safeDao.quantified_metrics.map((qm: any, i: number) => (
                          <tr key={i} className="border-b border-slate-800">
                            <td className="py-2 px-3">
                              <span className={`px-2 py-0.5 rounded text-xs font-medium ${
                                qm.priority <= 2 ? 'bg-red-500/20 text-red-400' : qm.priority <= 5 ? 'bg-amber-500/20 text-amber-400' : 'bg-slate-500/20 text-slate-400'
                              }`}>P{qm.priority}</span>
                            </td>
                            <td className="py-2 px-3 text-sky-300 font-medium">{qm.metric_name}</td>
                            <td className="py-2 px-3 text-slate-300">{qm.target_value}</td>
                            <td className="py-2 px-3 text-slate-500">{qm.tradeoff_note}</td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </div>
              )}

              {/* v3.1: 監査証拠チェックリスト */}
              {safeDao.audit_evidence_checklist && safeDao.audit_evidence_checklist.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-sm font-medium text-emerald-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">📋</span> {t('report.dao_audit_checklist')}
                  </div>
                  <div className="space-y-2">
                    {safeDao.audit_evidence_checklist.map((ae: any, i: number) => (
                      <div key={i} className="bg-emerald-500/5 rounded p-3 border border-emerald-500/10">
                        <div className="flex items-center gap-2 mb-1">
                          <span className="px-2 py-0.5 bg-emerald-500/20 text-emerald-400 rounded text-xs font-medium">{ae.category}</span>
                        </div>
                        <div className="text-sm text-slate-300">{ae.required_evidence}</div>
                        <div className="text-xs text-slate-500 mt-1">{t('report.dao_verification_method')} {ae.verification_method}</div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.1: セルフチェック結果 */}
              {safeDao.self_check && (
                <div className={`rounded-lg p-4 border ${
                  safeDao.self_check.overall_status === 'PASS' ? 'bg-emerald-500/5 border-emerald-500/20' :
                  safeDao.self_check.overall_status === 'FATAL' ? 'bg-red-500/5 border-red-500/20' :
                  'bg-amber-500/5 border-amber-500/20'
                }`}>
                  <div className="flex items-center justify-between mb-3">
                    <div className="text-sm font-medium flex items-center gap-2">
                      <span aria-hidden="true">🔬</span> {t('report.dao_self_check')}
                    </div>
                    <span className={`px-3 py-1 rounded-full text-xs font-bold ${
                      safeDao.self_check.overall_status === 'PASS' ? 'bg-emerald-500/20 text-emerald-400' :
                      safeDao.self_check.overall_status === 'FATAL' ? 'bg-red-500/20 text-red-400' :
                      'bg-amber-500/20 text-amber-400'
                    }`}>{safeDao.self_check.overall_status}</span>
                  </div>
                  <div className="grid grid-cols-2 gap-2 text-xs">
                    {safeDao.self_check.boundary_undefined?.length > 0 && (
                      <div className="text-slate-400"><span className="text-amber-400">⚠</span> {t('report.dao_boundary_undefined')} {safeDao.self_check.boundary_undefined.join(', ')}</div>
                    )}
                    {safeDao.self_check.missing_alternatives?.length > 0 && (
                      <div className="text-slate-400"><span className="text-amber-400">⚠</span> {t('report.dao_missing_alternatives')} {safeDao.self_check.missing_alternatives.join(', ')}</div>
                    )}
                    {safeDao.self_check.ambiguous_metrics?.length > 0 && (
                      <div className="text-slate-400"><span className="text-amber-400">⚠</span> {t('report.dao_ambiguous_metrics')} {safeDao.self_check.ambiguous_metrics.join(', ')}</div>
                    )}
                    {safeDao.self_check.constraint_conflicts?.length > 0 && (
                      <div className="text-slate-400"><span className="text-red-400">❌</span> {t('report.dao_constraint_conflicts')} {safeDao.self_check.constraint_conflicts.join(', ')}</div>
                    )}
                    {safeDao.self_check.evidence_gaps?.length > 0 && (
                      <div className="text-slate-400"><span className="text-amber-400">⚠</span> {t('report.dao_evidence_gaps')} {safeDao.self_check.evidence_gaps.join(', ')}</div>
                    )}
                  </div>
                </div>
              )}
            </div>
          )}

          {activeTab === 'fa' && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-violet-500/10 flex items-center justify-center">🛤️</span>
                {t('report.fa_title')}
              </h3>

              {/* v3.1: 戦略的禁止事項（仕組み化） */}
              {safeFa.strategic_prohibitions && safeFa.strategic_prohibitions.length > 0 && (
                <div className="bg-red-500/5 rounded-lg p-5 border border-red-500/20">
                  <div className="text-sm font-medium text-red-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">🚫</span> {t('report.fa_strategic_prohibitions')}
                  </div>
                  <div className="space-y-3">
                    {safeFa.strategic_prohibitions.map((p: any, i: number) => (
                      <div key={`proh-${i}`} className="bg-red-500/10 rounded p-4">
                        <div className="flex items-start gap-2">
                          <span className="text-red-400 mt-0.5">⛔</span>
                          <div className="flex-1">
                            <div className="font-medium text-sm">{p.prohibition}</div>
                            <div className="text-sm text-slate-400 mt-2">{t('report.fa_rationale')} {p.rationale}</div>
                            <div className="text-sm text-red-400 mt-1">{t('report.fa_violation_consequence')} {p.violation_consequence}</div>
                            {/* v3.1: 仕組み化フィールド */}
                            {(p.prevention_measure || p.detection_metric || p.responsible_role) && (
                              <div className="mt-3 pt-3 border-t border-red-500/10 space-y-1">
                                {p.prevention_measure && (
                                  <div className="text-xs text-cyan-400"><span aria-hidden="true">🛡️</span> {t('report.fa_prevention_measure')} <span className="text-slate-400">{p.prevention_measure}</span></div>
                                )}
                                {p.detection_metric && (
                                  <div className="text-xs text-cyan-400"><span aria-hidden="true">📊</span> {t('report.fa_detection_metric')} <span className="text-slate-400">{p.detection_metric}</span></div>
                                )}
                                {p.responsible_role && (
                                  <div className="text-xs text-cyan-400"><span aria-hidden="true">👤</span> {t('report.fa_responsible_role')} <span className="text-slate-400">{p.responsible_role}</span></div>
                                )}
                              </div>
                            )}
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.1: 競争優位仮説 */}
              {safeFa.competitive_hypothesis && (
                <div className="bg-gradient-to-r from-violet-500/10 to-purple-500/10 rounded-lg p-5 border border-violet-500/20">
                  <div className="text-sm font-medium text-violet-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">🎯</span> {t('report.fa_competitive_hypothesis')}
                  </div>
                  <div className="grid grid-cols-2 gap-4">
                    <div className="bg-violet-500/10 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">{t('report.fa_axis_name')}</div>
                      <div className="text-lg font-medium text-violet-400">{safeFa.competitive_hypothesis.axis_name}</div>
                    </div>
                    <div className="bg-violet-500/10 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">{t('report.fa_target_customer')}</div>
                      <div className="text-sm text-slate-400">{safeFa.competitive_hypothesis.target_customer}</div>
                    </div>
                    <div className="bg-violet-500/10 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">{t('report.fa_substitution_barrier')}</div>
                      <div className="text-sm text-slate-400">{safeFa.competitive_hypothesis.substitution_barrier}</div>
                    </div>
                    <div className="bg-violet-500/10 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">{t('report.fa_winning_metric')}</div>
                      <div className="text-sm text-slate-400">{safeFa.competitive_hypothesis.winning_metric}</div>
                    </div>
                  </div>
                  <div className="mt-4 bg-slate-800/50 rounded p-4">
                    <div className="text-xs text-slate-500 mb-1">{t('report.fa_minimum_verification')}</div>
                    <div className="text-sm text-slate-400">{safeFa.competitive_hypothesis.minimum_verification}</div>
                  </div>
                </div>
              )}

              {/* v3.0互換: 差別化軸（competitive_hypothesisが無い場合のフォールバック） */}
              {!safeFa.competitive_hypothesis && safeFa.differentiation_axis && (
                <div className="bg-gradient-to-r from-violet-500/10 to-purple-500/10 rounded-lg p-5 border border-violet-500/20">
                  <div className="text-sm font-medium text-violet-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">🎯</span> {t('report.fa_differentiation_axis')}
                  </div>
                  <div className="space-y-4">
                    <div className="bg-violet-500/10 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">{t('report.fa_why_this_axis')}</div>
                      <div className="text-lg font-medium text-violet-400">{safeFa.differentiation_axis.axis_name}</div>
                      <div className="text-sm text-slate-400 mt-2">{safeFa.differentiation_axis.why_this_axis}</div>
                    </div>
                    <div className="bg-slate-800/50 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">{t('report.fa_not_this_axis')}</div>
                      <div className="text-sm text-slate-400">{safeFa.differentiation_axis.not_this_axis}</div>
                    </div>
                  </div>
                </div>
              )}

              {/* v3.0: 既存解が使えない理由 */}
              {safeFa.why_existing_fails && (
                <div className="bg-amber-500/5 rounded-lg p-4 border border-amber-500/20">
                  <div className="text-xs text-amber-400 mb-2 flex items-center gap-2">
                    <span aria-hidden="true">⚠️</span> {t('report.fa_why_existing_fails')}
                  </div>
                  <div className="text-sm text-slate-400">{safeFa.why_existing_fails}</div>
                </div>
              )}

              {/* 推奨パス（v3.1: 最低4案） */}
              {safeFa.recommended_paths?.map((path: RecommendedPath, i: number) => (
                <PathCard key={`rec-${path.path_id || i}`} path={path} isRecommended />
              ))}

              {/* 不推奨パス */}
              {safeFa.rejected_paths?.map((path: RecommendedPath, i: number) => (
                <PathCard key={`rej-${path.path_id || i}`} path={path} isRecommended={false} />
              ))}

              {/* v3.1: 判断フレームワーク（Must/Should分離） */}
              {safeFa.judgment_framework && (
                <div className="bg-slate-800/50 rounded-lg p-5 border border-slate-700">
                  <div className="text-sm font-medium text-cyan-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">⚖️</span> {t('report.fa_judgment_framework')}
                  </div>
                  {/* Must Gates */}
                  {safeFa.judgment_framework.must_gates?.length > 0 && (
                    <div className="mb-4">
                      <div className="text-xs text-red-400 font-medium mb-2">{t('report.fa_must_gates')}</div>
                      <div className="space-y-2">
                        {safeFa.judgment_framework.must_gates.map((gate: any, gi: number) => (
                          <div key={`must-${gi}`} className="bg-red-500/10 rounded p-3 flex items-start gap-3">
                            <span className="text-red-400 text-xs mt-0.5">🚪</span>
                            <div className="flex-1">
                              <div className="text-sm font-medium text-red-300">{gate.criterion}</div>
                              <div className="text-xs text-slate-400 mt-1">{t('report.fa_threshold')} {gate.threshold}</div>
                            </div>
                          </div>
                        ))}
                      </div>
                    </div>
                  )}
                  {/* Should Criteria */}
                  {safeFa.judgment_framework.should_criteria?.length > 0 && (
                    <div>
                      <div className="text-xs text-emerald-400 font-medium mb-2">{t('report.fa_should_criteria')}</div>
                      <div className="space-y-2">
                        {safeFa.judgment_framework.should_criteria.map((crit: any, si: number) => (
                          <div key={`should-${si}`} className="bg-emerald-500/10 rounded p-3">
                            <div className="flex items-center justify-between">
                              <div className="text-sm font-medium text-emerald-300">{crit.criterion}</div>
                              <span className={`text-xs px-2 py-0.5 rounded ${
                                crit.weight === 'High' ? 'bg-red-500/20 text-red-400' :
                                crit.weight === 'Med' ? 'bg-amber-500/20 text-amber-400' :
                                'bg-slate-700 text-slate-400'
                              }`}>{crit.weight}</span>
                            </div>
                            <div className="text-xs text-slate-400 mt-1">{t('report.fa_scoring_method')} {crit.scoring_method}</div>
                          </div>
                        ))}
                      </div>
                    </div>
                  )}
                </div>
              )}

              {/* 判断基準（v3.0互換フォールバック） */}
              {!safeFa.judgment_framework && safeFa.decision_criteria && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2">{t('report.fa_decision_criteria')}</div>
                  <div className="flex flex-wrap gap-2">
                    {safeFa.decision_criteria.map((c: string, ci: number) => (
                      <span key={`dc-${ci}`} className="px-2 py-1 bg-slate-800 text-slate-400 rounded text-xs">{c}</span>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.1: セルフチェック結果 */}
              {safeFa.fa_self_check && (
                <div className={`rounded-lg p-4 border ${
                  safeFa.fa_self_check.overall_status === 'PASS' ? 'bg-emerald-500/5 border-emerald-500/20' :
                  safeFa.fa_self_check.overall_status === 'WARNING' ? 'bg-amber-500/5 border-amber-500/20' :
                  'bg-red-500/5 border-red-500/20'
                }`}>
                  <div className="text-sm font-medium mb-3 flex items-center gap-2">
                    <span aria-hidden="true">🔍</span> {t('report.fa_self_check')}
                    <span className={`text-xs px-2 py-0.5 rounded ${
                      safeFa.fa_self_check.overall_status === 'PASS' ? 'bg-emerald-500/20 text-emerald-400' :
                      safeFa.fa_self_check.overall_status === 'WARNING' ? 'bg-amber-500/20 text-amber-400' :
                      'bg-red-500/20 text-red-400'
                    }`}>{safeFa.fa_self_check.overall_status}</span>
                  </div>
                  <div className="grid grid-cols-2 gap-2 text-xs">
                    {safeFa.fa_self_check.baseless_numbers?.length > 0 && (
                      <div className="bg-red-500/10 rounded p-2">
                        <div className="text-red-400 font-medium mb-1">{t('report.fa_baseless_numbers')}</div>
                        {safeFa.fa_self_check.baseless_numbers.map((item: string, bi: number) => (
                          <div key={`bn-${bi}`} className="text-slate-400">• {item}</div>
                        ))}
                      </div>
                    )}
                    {safeFa.fa_self_check.missing_intermediate?.length > 0 && (
                      <div className="bg-amber-500/10 rounded p-2">
                        <div className="text-amber-400 font-medium mb-1">{t('report.fa_missing_intermediate')}</div>
                        {safeFa.fa_self_check.missing_intermediate.map((item: string, mi: number) => (
                          <div key={`mi-${mi}`} className="text-slate-400">• {item}</div>
                        ))}
                      </div>
                    )}
                    {safeFa.fa_self_check.missing_gates?.length > 0 && (
                      <div className="bg-amber-500/10 rounded p-2">
                        <div className="text-amber-400 font-medium mb-1">{t('report.fa_missing_gates')}</div>
                        {safeFa.fa_self_check.missing_gates.map((item: string, mgi: number) => (
                          <div key={`mg-${mgi}`} className="text-slate-400">• {item}</div>
                        ))}
                      </div>
                    )}
                    {safeFa.fa_self_check.appearance_precision?.length > 0 && (
                      <div className="bg-red-500/10 rounded p-2">
                        <div className="text-red-400 font-medium mb-1">{t('report.fa_appearance_precision')}</div>
                        {safeFa.fa_self_check.appearance_precision.map((item: string, api: number) => (
                          <div key={`ap-${api}`} className="text-slate-400">• {item}</div>
                        ))}
                      </div>
                    )}
                  </div>
                </div>
              )}
            </div>
          )}

          {activeTab === 'shu' && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-blue-500/10 flex items-center justify-center">📋</span>
                {t('report.shu_title')}
              </h3>

              {/* v3.1: PoC完成定義 (DoD) */}
              {safeShu.poc_definition_of_done && (
                <div className="bg-emerald-500/5 rounded-lg p-5 border border-emerald-500/20">
                  <div className="text-sm font-medium text-emerald-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">🎯</span> {t('report.shu_poc_dod')}
                  </div>
                  <div className="space-y-4">
                    {safeShu.poc_definition_of_done.experience_conditions?.length > 0 && (
                      <div>
                        <div className="text-xs text-slate-500 mb-2">{t('report.shu_experience_conditions')}</div>
                        {safeShu.poc_definition_of_done.experience_conditions.map((c: string, i: number) => (
                          <div key={i} className="text-sm flex items-center gap-2 mb-1"><span className="text-emerald-400">✓</span> {c}</div>
                        ))}
                      </div>
                    )}
                    <div>
                      <div className="text-xs text-slate-500 mb-2">{t('report.shu_success_metrics')}</div>
                      <div className="overflow-x-auto">
                        <table className="w-full text-sm">
                          <thead><tr className="border-b border-slate-700">
                            <th className="text-left py-1 text-slate-500">{t('report.shu_metric')}</th>
                            <th className="text-left py-1 text-slate-500">{t('report.shu_target_value')}</th>
                            <th className="text-left py-1 text-slate-500">{t('report.shu_measurement_method')}</th>
                          </tr></thead>
                          <tbody>
                            {safeShu.poc_definition_of_done.success_metrics?.map((m: any, i: number) => (
                              <tr key={i} className="border-b border-slate-800">
                                <td className="py-1 text-emerald-400">{m.metric_name}</td>
                                <td className="py-1">{m.target_value}</td>
                                <td className="py-1 text-slate-400">{m.measurement_method}</td>
                              </tr>
                            ))}
                          </tbody>
                        </table>
                      </div>
                    </div>
                    <div className="bg-amber-500/10 rounded p-3">
                      <div className="text-xs text-amber-400">{t('report.shu_fallback')}</div>
                      <div className="text-sm mt-1">{safeShu.poc_definition_of_done.fallback_strategy}</div>
                    </div>
                  </div>
                </div>
              )}

              {/* v3.1: 2段ロケット */}
              {safeShu.two_stage_rocket && (
                <div className="space-y-4">
                  {[safeShu.two_stage_rocket.stage1_minimal_pipeline, safeShu.two_stage_rocket.stage2_governance].map((stage: any, si: number) => stage && (
                    <div key={si} className={`rounded-lg p-5 border ${si === 0 ? 'bg-blue-500/5 border-blue-500/20' : 'bg-purple-500/5 border-purple-500/20'}`}>
                      <div className={`text-sm font-medium mb-3 flex items-center gap-2 ${si === 0 ? 'text-blue-400' : 'text-purple-400'}`}>
                        <span>{si === 0 ? '🚀' : '🛡️'}</span> {stage.stage_name}
                      </div>
                      <div className="text-sm text-slate-400 mb-3">{stage.objective}</div>
                      {stage.gate_criteria?.length > 0 && (
                        <div className="mb-3 flex flex-wrap gap-2">
                          {stage.gate_criteria.map((g: string, gi: number) => (
                            <span key={gi} className="text-xs px-2 py-1 bg-slate-700 rounded">{t('report.shu_gate_criteria')} {g}</span>
                          ))}
                        </div>
                      )}
                      <div className="space-y-3">
                        {stage.phases?.map((p: any, pi: number) => (
                          <div key={pi} className="bg-[#0a0a0f] rounded p-4">
                            <div className="flex items-center justify-between mb-2">
                              <span className="font-medium text-sm">Phase {p.phase_number}: {p.name}</span>
                              <span className="text-xs text-slate-500">{p.duration}</span>
                            </div>
                            <div className="text-xs text-slate-500 mb-2">{t('report.shu_purpose')} {p.purpose}</div>
                            <div className="text-xs text-slate-400 mb-1">{t('report.shu_tasks')} {p.tasks?.join(', ')}</div>
                            {p.deliverables?.length > 0 && <div className="text-xs text-slate-500">{t('report.shu_deliverables')} {p.deliverables.join(', ')}</div>}
                            {p.measurement && <div className="text-xs text-emerald-400 mt-1">{t('report.shu_measurement')} {p.measurement}</div>}
                            {p.notes?.length > 0 && <div className="text-xs text-amber-400 mt-1">{t('report.shu_notes')} {p.notes.join(', ')}</div>}
                            {p.branches?.length > 0 && (
                              <div className="mt-2 space-y-1">
                                <div className="text-xs text-slate-500">{t('report.shu_branches')}</div>
                                {p.branches.map((b: any, bi: number) => (
                                  <div key={bi} className="text-xs bg-blue-500/10 rounded p-2">
                                    <span className="text-blue-400">{b.branch_name}</span>
                                    <span className="text-slate-500 mx-1">→</span>
                                    <span className="text-slate-400">{b.trigger_condition}</span>
                                    <div className="text-slate-500 mt-1">{b.description}</div>
                                  </div>
                                ))}
                              </div>
                            )}
                          </div>
                        ))}
                      </div>
                    </div>
                  ))}
                </div>
              )}

              {safeShu.first_action && (
                <div className="bg-emerald-500/5 rounded-lg p-4 border border-emerald-500/20">
                  <div className="text-xs text-emerald-400 mb-2"><span aria-hidden="true">🎯</span> {t('report.shu_first_action')}</div>
                  <div className="text-sm font-medium">{safeShu.first_action}</div>
                </div>
              )}

              {/* v3.0: 切り捨てリスト */}
              {safeShu.cut_list && safeShu.cut_list.length > 0 && (
                <div className="bg-red-500/5 rounded-lg p-4 border border-red-500/20">
                  <div className="text-sm font-medium text-red-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">✂️</span> {t('report.shu_cut_list')}
                  </div>
                  <div className="space-y-2">
                    {safeShu.cut_list.map((item: string, i: number) => (
                      <div key={i} className="flex items-center gap-2 text-sm text-red-400">
                        <span>❌</span> {item}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 文脈特化行動 */}
              {safeShu.context_specific_actions && safeShu.context_specific_actions.length > 0 && (
                <div className="bg-blue-500/5 rounded-lg p-4 border border-blue-500/20">
                  <div className="text-sm font-medium text-blue-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">🎯</span> {t('report.shu_context_actions')}
                  </div>
                  <div className="space-y-3">
                    {safeShu.context_specific_actions.map((action: any, i: number) => (
                      <div key={i} className="bg-blue-500/10 rounded p-3">
                        <div className="font-medium text-sm">{action.action}</div>
                        <div className="text-xs text-slate-500 mt-1">{t('report.shu_why_context')} {action.why_this_context}</div>
                        <div className="text-xs text-blue-400 mt-1">{t('report.shu_expected_output')} {action.expected_output}</div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 単一検証ポイント */}
              {safeShu.single_validation_point && (
                <div className="bg-amber-500/5 rounded-lg p-4 border border-amber-500/20">
                  <div className="text-sm font-medium text-amber-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">🔬</span> {t('report.shu_single_validation')}
                  </div>
                  <div className="space-y-3">
                    <div>
                      <div className="text-xs text-slate-500">{t('report.shu_validation_target')}</div>
                      <div className="text-sm mt-1 font-medium">{safeShu.single_validation_point.validation_target}</div>
                    </div>
                    <div>
                      <div className="text-xs text-slate-500">{t('report.shu_success_criteria')}</div>
                      <div className="text-sm mt-1">{safeShu.single_validation_point.success_criteria}</div>
                    </div>
                    <div className="bg-amber-500/10 rounded p-2">
                      <div className="text-xs text-amber-400">{t('report.shu_failure_action')}</div>
                      <div className="text-sm mt-1">{safeShu.single_validation_point.failure_action}</div>
                    </div>
                  </div>
                </div>
              )}

              {/* v3.0: 撤退基準 */}
              {safeShu.exit_criteria && (
                <div className="bg-red-500/5 rounded-lg p-4 border border-red-500/20">
                  <div className="text-sm font-medium text-red-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">🚪</span> {t('report.shu_exit_criteria')}
                  </div>
                  <div className="space-y-3">
                    <div>
                      <div className="text-xs text-slate-500">{t('report.shu_checkpoint')}</div>
                      <div className="text-sm mt-1">{safeShu.exit_criteria.checkpoint}</div>
                    </div>
                    <div>
                      <div className="text-xs text-slate-500">{t('report.shu_exit_trigger')}</div>
                      <div className="text-sm mt-1 text-red-400">{safeShu.exit_criteria.exit_trigger}</div>
                    </div>
                    <div>
                      <div className="text-xs text-slate-500">{t('report.shu_exit_action')}</div>
                      <div className="text-sm mt-1">{safeShu.exit_criteria.exit_action}</div>
                    </div>
                  </div>
                </div>
              )}

              {/* タイムライン */}
              {safeShu.phases && safeShu.phases.length > 0 && (
                <div>
                  <div className="text-sm font-medium text-slate-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">📅</span> {t('report.shu_phases')}
                  </div>
                  <PhaseTimeline phases={safeShu.phases} />
                </div>
              )}

              {safeShu.dependencies && safeShu.dependencies.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2">{t('report.shu_dependencies')}</div>
                  <ul className="text-sm text-slate-400 space-y-1">
                    {safeShu.dependencies.map((d: string, i: number) => (
                      <li key={i}>• {d}</li>
                    ))}
                  </ul>
                </div>
              )}

              {/* v3.0: 30天行動節奏 */}
              {safeShu.rhythm_control && (
                <div className="bg-[#0a0a0f] rounded-lg p-4 border border-blue-500/20">
                  <div className="text-sm font-medium text-blue-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">⏱️</span> {t('report.shu_rhythm_control')}
                  </div>
                  {safeShu.rhythm_control.focus && (
                    <div className="space-y-3">
                      <div className="bg-blue-500/10 rounded p-3">
                        <div className="text-xs text-blue-400">{t('report.shu_focus')}</div>
                        <div className="text-lg font-medium mt-1">{safeShu.rhythm_control.focus.name}</div>
                        <div className="text-sm text-slate-400 mt-1">{safeShu.rhythm_control.focus.description}</div>
                        <div className="text-xs text-emerald-400 mt-2">{t('report.shu_success_metric')} {safeShu.rhythm_control.focus.success_metric}</div>
                      </div>
                      {safeShu.rhythm_control.focus.avoid_list && safeShu.rhythm_control.focus.avoid_list.length > 0 && (
                        <div>
                          <div className="text-xs text-slate-500 mb-2">{t('report.shu_avoid_list')}</div>
                          {safeShu.rhythm_control.focus.avoid_list.map((avoid: string, i: number) => (
                            <div key={i} className="text-sm text-red-400 flex items-center gap-2">
                              <span>❌</span> {avoid}
                            </div>
                          ))}
                        </div>
                      )}
                      <div className="grid grid-cols-2 gap-4 mt-3">
                        <div>
                          <div className="text-xs text-slate-500">{t('report.shu_checkpoint_date')}</div>
                          <div className="text-sm mt-1">{safeShu.rhythm_control.checkpoint_date}</div>
                        </div>
                        <div>
                          <div className="text-xs text-slate-500">{t('report.shu_next_decision')}</div>
                          <div className="text-sm mt-1">{safeShu.rhythm_control.next_decision_point}</div>
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
                {t('report.qi_title')}
              </h3>

              {/* v3.1: PoC最小アーキテクチャ */}
              {safeQi.poc_minimal_architecture && (
                <div className="bg-emerald-500/5 rounded-lg p-5 border border-emerald-500/20">
                  <div className="text-sm font-medium text-emerald-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">🏗️</span> {t('report.qi_poc_architecture')}
                  </div>
                  <div className="space-y-4">
                    <div className="overflow-x-auto">
                      <table className="w-full text-sm">
                        <thead><tr className="border-b border-slate-700">
                          <th className="text-left py-2 text-slate-500">{t('report.qi_component')}</th>
                          <th className="text-left py-2 text-slate-500">{t('report.qi_purpose')}</th>
                          <th className="text-left py-2 text-slate-500">{t('report.qi_tech_choice')}</th>
                          <th className="text-left py-2 text-slate-500">{t('report.qi_remarks')}</th>
                        </tr></thead>
                        <tbody>
                          {safeQi.poc_minimal_architecture.components?.map((c: any, i: number) => (
                            <tr key={i} className="border-b border-slate-800">
                              <td className="py-2 text-emerald-400 font-medium">{c.name}</td>
                              <td className="py-2 text-slate-300">{c.purpose}</td>
                              <td className="py-2 text-blue-400">{c.technology_choice}</td>
                              <td className="py-2 text-slate-500">{c.notes}</td>
                            </tr>
                          ))}
                        </tbody>
                      </table>
                    </div>
                    {safeQi.poc_minimal_architecture.data_flow_description && (
                      <div className="bg-[#0a0a0f] rounded p-3">
                        <div className="text-xs text-slate-500 mb-1">{t('report.qi_data_flow')}</div>
                        <div className="text-sm text-slate-300 font-mono">{safeQi.poc_minimal_architecture.data_flow_description}</div>
                      </div>
                    )}
                    {safeQi.poc_minimal_architecture.minimal_logging && (
                      <div className="bg-blue-500/5 rounded p-3 border border-blue-500/10">
                        <div className="text-xs text-blue-400 mb-2">{t('report.qi_minimal_logging')}</div>
                        <div className="text-sm text-slate-400">{t('report.qi_correlation_id')} {safeQi.poc_minimal_architecture.minimal_logging.correlation_id_strategy}</div>
                        {(safeQi.poc_minimal_architecture.minimal_logging.timestamp_points?.length ?? 0) > 0 && (
                          <div className="text-sm text-slate-500 mt-1">{t('report.qi_timestamp_points')} {(safeQi.poc_minimal_architecture.minimal_logging.timestamp_points ?? []).join(' → ')}</div>
                        )}
                        {safeQi.poc_minimal_architecture.minimal_logging.storage && (
                          <div className="text-sm text-slate-500 mt-1">{t('report.qi_storage')} {safeQi.poc_minimal_architecture.minimal_logging.storage}</div>
                        )}
                      </div>
                    )}
                    {(safeQi.poc_minimal_architecture.deferred_components?.length ?? 0) > 0 && (
                      <div>
                        <div className="text-xs text-slate-500 mb-2">{t('report.qi_deferred_components')}</div>
                        <div className="flex flex-wrap gap-2">
                          {(safeQi.poc_minimal_architecture.deferred_components ?? []).map((d: string, i: number) => (
                            <span key={i} className="text-xs px-2 py-1 bg-slate-700/50 text-slate-400 rounded">⏳ {d}</span>
                          ))}
                        </div>
                      </div>
                    )}
                  </div>
                </div>
              )}

              {/* v3.1: 拡張アーキテクチャ段階 */}
              {safeQi.expansion_stages && safeQi.expansion_stages.length > 0 && (
                <div className="bg-purple-500/5 rounded-lg p-5 border border-purple-500/20">
                  <div className="text-sm font-medium text-purple-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">📈</span> {t('report.qi_expansion_stages')}
                  </div>
                  <div className="space-y-3">
                    {safeQi.expansion_stages.map((s: any, i: number) => (
                      <div key={i} className="bg-purple-500/10 rounded p-4">
                        <div className="font-medium text-purple-400 mb-1">{s.stage_name}</div>
                        <div className="text-xs text-amber-400 mb-1">{t('report.qi_introduction_condition')} {s.introduction_condition}</div>
                        <div className="text-xs text-slate-400 mb-1">{t('report.qi_added_components')} {s.added_components?.join(', ')}</div>
                        <div className="text-xs text-slate-500">{t('report.qi_rationale')} {s.rationale}</div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.1: 実装手順 */}
              {safeQi.implementation_steps && safeQi.implementation_steps.length > 0 && (
                <div className="bg-blue-500/5 rounded-lg p-5 border border-blue-500/20">
                  <div className="text-sm font-medium text-blue-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">📝</span> {t('report.qi_implementation_steps')}
                  </div>
                  <div className="space-y-3">
                    {safeQi.implementation_steps.map((step: any, i: number) => (
                      <div key={i} className="bg-[#0a0a0f] rounded p-4">
                        <div className="flex items-center gap-2 mb-2">
                          <span className="text-blue-400 font-bold">Step {step.step_number}</span>
                          <span className="font-medium text-sm">{step.objective}</span>
                        </div>
                        <div className="text-xs text-slate-400 mb-1">{t('report.shu_tasks')} {step.tasks?.join(', ')}</div>
                        {step.notes?.length > 0 && <div className="text-xs text-emerald-400 mt-1">📌 {step.notes.join(', ')}</div>}
                        {step.common_pitfalls?.length > 0 && (
                          <div className="mt-1 space-y-1">
                            {step.common_pitfalls.map((p: string, pi: number) => (
                              <div key={pi} className="text-xs text-amber-400">⚠️ {p}</div>
                            ))}
                          </div>
                        )}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.1: 将来スケール要件 */}
              {safeQi.future_scale_requirements && safeQi.future_scale_requirements.length > 0 && (
                <div className="bg-slate-500/5 rounded-lg p-4 border border-slate-500/20">
                  <div className="text-xs text-slate-500 mb-2"><span aria-hidden="true">🔮</span> {t('report.qi_future_scale')}</div>
                  <div className="flex flex-wrap gap-2">
                    {safeQi.future_scale_requirements.map((r: string, i: number) => (
                      <span key={i} className="text-xs px-2 py-1 bg-slate-700/50 text-slate-400 rounded">{r}</span>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: ドメイン固有技術 */}
              {safeQi.domain_technologies && safeQi.domain_technologies.length > 0 && (
                <div className="bg-emerald-500/5 rounded-lg p-5 border border-emerald-500/20">
                  <div className="text-sm font-medium text-emerald-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">🛠️</span> {t('report.qi_domain_technologies')}
                  </div>
                  <div className="space-y-3">
                    {safeQi.domain_technologies.map((tech: any, i: number) => (
                      <div key={i} className="bg-emerald-500/10 rounded p-4">
                        <div className="flex items-center gap-2 mb-2">
                          <span className="font-medium text-emerald-400">{tech.technology_name}</span>
                          <span className="text-xs px-2 py-0.5 bg-slate-700 rounded">{tech.category}</span>
                        </div>
                        <div className="text-sm text-slate-400">{tech.why_required}</div>
                        {tech.alternatives && tech.alternatives.length > 0 && (
                          <div className="text-xs text-slate-500 mt-2">
                            {t('report.qi_alternatives')} {tech.alternatives.join(', ')}
                          </div>
                        )}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 規制対応 */}
              {safeQi.regulatory_considerations && safeQi.regulatory_considerations.length > 0 && (
                <div className="bg-amber-500/5 rounded-lg p-5 border border-amber-500/20">
                  <div className="text-sm font-medium text-amber-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">📜</span> {t('report.qi_regulatory')}
                  </div>
                  <div className="overflow-x-auto">
                    <table className="w-full text-sm">
                      <thead>
                        <tr className="border-b border-slate-700">
                          <th className="text-left py-2 text-slate-500">{t('report.qi_region')}</th>
                          <th className="text-left py-2 text-slate-500">{t('report.qi_regulation')}</th>
                          <th className="text-left py-2 text-slate-500">{t('report.qi_requirement')}</th>
                          <th className="text-left py-2 text-slate-500">{t('report.qi_impl_impact')}</th>
                        </tr>
                      </thead>
                      <tbody>
                        {safeQi.regulatory_considerations.map((reg: any, i: number) => (
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
              {safeQi.geographic_considerations && safeQi.geographic_considerations.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4 border border-blue-500/20">
                  <div className="text-sm font-medium text-blue-400 mb-4 flex items-center gap-2">
                    <span aria-hidden="true">🌍</span> {t('report.qi_geographic')}
                  </div>
                  <div className="space-y-3">
                    {safeQi.geographic_considerations.map((geo: any, i: number) => (
                      <div key={i} className="flex items-start gap-4 p-3 bg-blue-500/5 rounded">
                        <div className="text-blue-400 font-medium">{geo.region}</div>
                        <div className="flex-1">
                          <div className="text-sm text-slate-400">{t('report.qi_latency')} {geo.latency_requirement}</div>
                          <div className="text-sm text-slate-500">{t('report.qi_infrastructure')} {geo.infrastructure_need}</div>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* 実装要素 */}
              {safeQi.implementations && safeQi.implementations.length > 0 && (
                <div>
                  <div className="text-sm font-medium text-slate-400 mb-3 flex items-center gap-2">
                    <span aria-hidden="true">🔧</span> {t('report.qi_implementations')}
                  </div>
                  {safeQi.implementations.map((impl: Implementation, i: number) => (
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

              {safeQi.tool_recommendations && safeQi.tool_recommendations.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2"><span aria-hidden="true">🧰</span> {t('report.qi_tool_recommendations')}</div>
                  <div className="flex flex-wrap gap-2">
                    {safeQi.tool_recommendations.map((t: string, i: number) => (
                      <span key={i} className="px-2 py-1 bg-indigo-500/10 text-indigo-400 rounded text-xs">{t}</span>
                    ))}
                  </div>
                </div>
              )}

              {safeQi.integration_points && safeQi.integration_points.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2"><span aria-hidden="true">🔗</span> {t('report.qi_integration_points')}</div>
                  <ul className="text-sm text-slate-400 space-y-1">
                    {safeQi.integration_points.map((p: string, i: number) => (
                      <li key={i}>• {p}</li>
                    ))}
                  </ul>
                </div>
              )}

              {safeQi.technical_debt_warnings && safeQi.technical_debt_warnings.length > 0 && (
                <div className="bg-amber-500/5 rounded-lg p-4 border border-amber-500/20">
                  <div className="text-xs text-amber-400 mb-2"><span aria-hidden="true">⚠️</span> {t('report.qi_tech_debt_warnings')}</div>
                  <ul className="text-sm text-slate-400 space-y-1">
                    {safeQi.technical_debt_warnings.map((w: string, i: number) => (
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

              {safeReview ? (
                <>
                  {/* 判定結果バナー（詳細注釈付き） */}
                  <div className={`rounded-xl p-5 border-2 ${
                    safeReview.overall_verdict === 'PASS'
                      ? 'bg-emerald-500/5 border-emerald-500/30'
                      : safeReview.overall_verdict === 'REVISE'
                      ? 'bg-amber-500/5 border-amber-500/30'
                      : 'bg-red-500/5 border-red-500/30'
                  }`}>
                    <div className="flex items-center justify-between">
                      <div className="flex items-center gap-3">
                        <span className="text-2xl">
                          {safeReview.overall_verdict === 'PASS' ? '✅' :
                           safeReview.overall_verdict === 'REVISE' ? '⚠️' : '❌'}
                        </span>
                        <div>
                          <div className="text-sm text-slate-400 mb-1">{t('report.review_overall_verdict')}</div>
                          <span className={`text-xl font-bold ${
                            safeReview.overall_verdict === 'PASS'
                              ? 'text-emerald-400'
                              : safeReview.overall_verdict === 'REVISE'
                              ? 'text-amber-400'
                              : 'text-red-400'
                          }`}>
                            {safeReview.overall_verdict || t('report.review_processing')}
                          </span>
                        </div>
                      </div>
                      <div className="text-right">
                        <div className="text-sm text-slate-400 mb-1">{t('report.review_confidence')}</div>
                        <div className={`text-xl font-bold ${
                          (safeReview?.confidence_score ?? 0) >= 0.7 ? 'text-emerald-400' :
                          (safeReview?.confidence_score ?? 0) >= 0.4 ? 'text-amber-400' : 'text-red-400'
                        }`}>
                          {Math.round((safeReview?.confidence_score ?? 0) * 100)}%
                        </div>
                      </div>
                    </div>

                    {/* 判定結果の意味を説明 */}
                    <div className="mt-4 pt-4 border-t border-white/10">
                      <div className="text-sm text-slate-400">
                        {safeReview.overall_verdict === 'PASS' && (
                          <>
                            <span className="text-emerald-400">✓ {t('report.review_pass_label')}</span>
                            {t('report.review_pass_desc')}
                          </>
                        )}
                        {safeReview.overall_verdict === 'REVISE' && (
                          <>
                            <span className="text-amber-400">⚠ {t('report.review_revise_label')}</span>
                            {t('report.review_revise_desc')}
                          </>
                        )}
                        {safeReview.overall_verdict === 'COACH' && (
                          <>
                            <span className="text-blue-400"><span aria-hidden="true">📋</span> {t('report.review_coach_label')}</span>
                            {t('report.review_coach_desc')}
                          </>
                        )}
                      </div>
                    </div>
                  </div>

                  {/* v3.1 差分パッチ型: 指摘事項（最大3件） */}
                  {safeReview.findings && safeReview.findings.length > 0 && (
                    <div className="space-y-4">
                      <div className="flex items-center gap-2 text-sm font-medium text-slate-300">
                        <span aria-hidden="true">🎯</span> {t('report.review_high_leverage')} ({safeReview.findings.length}{t('report.review_max_items')})
                      </div>
                      {missingCountermeasureCount > 0 && (
                        <div className="rounded-lg border border-amber-500/30 bg-amber-500/10 px-3 py-2 text-xs text-amber-300">
                          <span aria-hidden="true">⚠️</span> {t('report.review_missing_countermeasure').replaceAll('{count}', String(missingCountermeasureCount))}
                        </div>
                      )}
                      <div className="space-y-4">
                        {safeReview.findings.map((finding, i) => (
                          <div key={i} className={`rounded-lg p-4 border ${
                            finding.severity === 'CRITICAL'
                              ? 'bg-red-500/5 border-red-500/20'
                              : 'bg-amber-500/5 border-amber-500/20'
                          }`}>
                            {/* ヘッダー: 重大度 + カテゴリ + アクションタイプ */}
                            <div className="flex items-center gap-2 mb-3">
                              <span className={`text-xs px-2 py-0.5 rounded font-medium ${
                                finding.severity === 'CRITICAL' ? 'bg-red-500/20 text-red-400' : 'bg-amber-500/20 text-amber-400'
                              }`}>
                                {finding.severity === 'CRITICAL' ? t('report.review_severity_critical') : t('report.review_severity_warning')}
                              </span>
                              {finding.action_type && (
                                <span className={`text-xs px-2 py-0.5 rounded font-medium ${
                                  finding.action_type === 'PATCH' ? 'bg-emerald-500/20 text-emerald-400' :
                                  finding.action_type === 'RECALC' ? 'bg-blue-500/20 text-blue-400' :
                                  'bg-red-500/20 text-red-400'
                                }`}>
                                  {finding.action_type}
                                </span>
                              )}
                              {finding.affected_agent && (
                                <span className="text-xs text-slate-500">→ {finding.affected_agent}</span>
                              )}
                            </div>

                            {/* 所見本文 */}
                            <div className="mb-3 text-sm text-slate-300">{finding.description}</div>

                            {/* 破綻点 */}
                            {finding.failure_point && (
                              <div className="mb-2">
                                <span className="text-xs text-red-400 font-medium">{t('report.review_failure_point')} </span>
                                <span className="text-sm text-slate-300">{finding.failure_point}</span>
                              </div>
                            )}

                            {/* 影響範囲 */}
                            {finding.impact_scope && (
                              <div className="mb-3">
                                <span className="text-xs text-amber-400 font-medium">{t('report.review_impact_scope')} </span>
                                <span className="text-sm text-slate-400">{finding.impact_scope}</span>
                              </div>
                            )}

                            {/* 対策案 */}
                            <div className="mb-3 p-3 bg-emerald-500/5 rounded-lg border border-emerald-500/20">
                              <div className="text-xs text-emerald-400 font-medium mb-1">{t('report.review_countermeasure')}</div>
                              <div className="text-sm text-slate-300">
                                {(finding.suggested_revision || '').trim() || t('report.review_no_countermeasure')}
                              </div>
                            </div>

                            {/* 任意チェック＋メモ（最小パッチ有無に関係なく表示） */}
                            <div className="mt-3 p-3 bg-slate-800/50 rounded-lg border border-indigo-500/20">
                              <div className="text-xs text-indigo-400 mb-2 font-medium">{t('report.review_user_memo')}</div>
                              <label className="flex items-center gap-2 text-sm text-slate-300">
                                <input
                                  type="checkbox"
                                  checked={Boolean(humanReviewChecks[i])}
                                  onChange={(e) => setHumanReviewChecks((prev) => ({ ...prev, [i]: e.target.checked }))}
                                  className="rounded border-slate-500 bg-transparent"
                                />
                                {finding.minimal_patch?.checkbox_label || t('report.review_default_checkbox')}
                              </label>
                              <textarea
                                value={humanReviewNotes[i] ?? finding.minimal_patch?.default_value ?? ''}
                                onChange={(e) => setHumanReviewNotes((prev) => ({ ...prev, [i]: e.target.value }))}
                                placeholder={finding.minimal_patch?.annotation_hint || t('report.review_memo_placeholder')}
                                rows={2}
                                className="mt-2 w-full px-3 py-2 rounded bg-[#0a0a0f] border border-white/10 text-sm text-slate-200 placeholder:text-slate-500 focus:outline-none focus:border-indigo-500/50 resize-y"
                              />
                              <div className="mt-2 flex justify-end">
                                <button
                                  type="button"
                                  onClick={() => void handleSaveFindingMemo(i)}
                                  disabled={Boolean(savingFindingNotes[i])}
                                  className="px-3 py-1.5 text-xs rounded bg-indigo-500/15 hover:bg-indigo-500/25 text-indigo-300 border border-indigo-500/30 disabled:opacity-60"
                                >
                                  {savingFindingNotes[i] ? t('report.review_saving') : t('report.review_save_memo')}
                                </button>
                              </div>
                            </div>

                            {/* スコア改善見込み */}
                            {finding.score_improvements && finding.score_improvements.length > 0 && (
                              <div className="mt-3">
                                <div className="text-xs text-slate-500 mb-1">{t('report.review_score_improvement')}</div>
                                {finding.score_improvements.map((si, si_idx) => (
                                  <div key={si_idx} className="flex items-center gap-2 text-xs text-slate-400">
                                    <span>{si.target_score}:</span>
                                    <span className="text-slate-500">{si.current_estimate}%</span>
                                    <span>→</span>
                                    <span className="text-emerald-400">{si.improved_estimate}%</span>
                                    <span className="text-emerald-400 font-medium">(+{si.delta}点)</span>
                                  </div>
                                ))}
                              </div>
                            )}
                          </div>
                        ))}
                      </div>
                    </div>
                  )}

                  {/* v3.1 信頼度分解 */}
                  {safeReview.confidence_breakdown && (
                    <div className="bg-slate-800/30 rounded-lg p-4 border border-white/5">
                      <div className="text-sm font-medium text-slate-300 mb-3 flex items-center gap-2">
                        <span aria-hidden="true">📊</span> {t('report.review_confidence_breakdown')}
                      </div>
                      <div className="grid grid-cols-2 gap-3">
                        {(['input_sufficiency', 'logic_consistency', 'implementation_feasibility', 'risk_coverage'] as const).map((key) => {
                          const comp = safeReview.confidence_breakdown?.[key];
                          if (!comp) return null;
                          const hasChecks = Object.values(checkpointChecks).some(Boolean);
                          const displayScore = hasChecks ? Math.min(100, comp.score + comp.checkbox_boost) : comp.score;
                          return (
                            <div key={key} className="bg-[#0a0a0f] rounded-lg p-3 border border-white/5">
                              <div className="text-xs text-slate-500 mb-1">{comp.name}</div>
                              <div className="flex items-baseline gap-2">
                                <span className={`text-lg font-bold ${
                                  displayScore >= 70 ? 'text-emerald-400' : displayScore >= 40 ? 'text-amber-400' : 'text-red-400'
                                }`}>{Math.round(displayScore)}%</span>
                                {comp.checkbox_boost > 0 && (
                                  <span className="text-xs text-emerald-400/70">✓で+{comp.checkbox_boost}点</span>
                                )}
                              </div>
                              {comp.description && <div className="text-xs text-slate-600 mt-1">{comp.description}</div>}
                            </div>
                          );
                        })}
                      </div>
                    </div>
                  )}

                  {/* v3.1 チェックポイント項目 */}
                  {safeReview.checkpoint_items && safeReview.checkpoint_items.length > 0 && (
                    <div className="bg-indigo-500/5 rounded-lg p-4 border border-indigo-500/20">
                      <div className="text-sm font-medium text-indigo-400 mb-3 flex items-center gap-2">
                        <span aria-hidden="true">☑️</span> {t('report.review_checkpoint_items')}
                      </div>
                      <div className="space-y-3">
                        {safeReview.checkpoint_items.map((item) => (
                          <div key={item.item_id} className="flex items-start gap-3 p-3 bg-[#0a0a0f] rounded-lg border border-white/5">
                            <input
                              type="checkbox"
                              checked={checkpointChecks[item.item_id] ?? item.checked}
                              onChange={(e) => setCheckpointChecks((prev) => ({ ...prev, [item.item_id]: e.target.checked }))}
                              className="mt-0.5 rounded border-slate-500 bg-transparent"
                            />
                            <div className="flex-1 min-w-0">
                              <div className="flex items-center gap-2">
                                <span className="text-sm text-slate-300">{item.label}</span>
                                <span className="text-xs text-emerald-400/70">+{item.score_boost}点</span>
                              </div>
                              {item.default_suggestion && (
                                <div className="text-xs text-slate-500 mt-1">{t('report.review_default_suggestion')} {item.default_suggestion}</div>
                              )}
                              <input
                                type="text"
                                value={checkpointAnnotations[item.item_id] ?? ''}
                                onChange={(e) => setCheckpointAnnotations((prev) => ({ ...prev, [item.item_id]: e.target.value }))}
                                placeholder={t('report.review_annotation_placeholder')}
                                className="mt-2 w-full px-2 py-1 rounded bg-slate-800/50 border border-white/5 text-xs text-slate-300 placeholder:text-slate-600 focus:outline-none focus:border-indigo-500/30"
                              />
                            </div>
                          </div>
                        ))}
                      </div>

                      {/* 自動再計算ボタン */}
                      {safeReview.auto_recalc_enabled !== false && (
                        <button
                          disabled={!Object.values(checkpointChecks).some(Boolean)}
                          className={`mt-4 w-full px-4 py-3 rounded-lg text-sm font-medium transition-all flex items-center justify-center gap-2 ${
                            Object.values(checkpointChecks).some(Boolean)
                              ? 'bg-emerald-500/10 hover:bg-emerald-500/20 text-emerald-400 border border-emerald-500/20'
                              : 'bg-slate-800/50 text-slate-500 cursor-not-allowed border border-white/5'
                          }`}
                        >
                          <span aria-hidden="true">⚡</span> {t('report.review_auto_recalc')}
                        </button>
                      )}
                    </div>
                  )}

                  {/* 指摘なし */}
                  {(!safeReview.findings || safeReview.findings.length === 0) && (
                    <div className="text-center py-6 text-slate-500 bg-[#0a0a0f] rounded-lg">
                      <span className="text-3xl mb-2 block">✨</span>
                      <p>{t('report.review_no_findings')}</p>
                    </div>
                  )}
                </>
              ) : (
                <div className="text-center py-8 text-slate-500">
                  <div className="w-8 h-8 border-2 border-slate-600 border-t-slate-400 rounded-full animate-spin mx-auto mb-3" />
                  {t('report.review_loading')}
                </div>
              )}
            </div>
          )}
        </div>

        {/* 署名セクション v3.2 - 判定結果に応じた表示制御 */}
        <div className="mt-8 bg-[#12121a] rounded-xl border border-white/5 p-6">
          <h2 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <span className="text-slate-500">7.</span>
            <span aria-hidden="true">✍️</span> {t('report.sign_section_title')}
          </h2>

          {/* 署名テーブル（日本式） */}
          <div className="overflow-x-auto mb-6">
            <table className="w-full border-collapse text-sm">
              <tbody>
                {/* 作成欄 */}
                <tr className="border border-slate-700">
                  <th rowSpan={2} className="bg-slate-800/50 px-3 py-2 text-left w-20 border-r border-slate-700">{t('report.sign_author')}</th>
                  <th className="bg-slate-800/30 px-3 py-2 text-left w-16 border-r border-slate-700">{t('report.sign_department')}</th>
                  <td className="px-3 py-2 border-r border-slate-700">{authorDept}</td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left w-16 border-r border-slate-700">{t('report.sign_position')}</th>
                  <td className="px-3 py-2">{authorPos}</td>
                </tr>
                <tr className="border border-slate-700 border-t-0">
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">{t('report.sign_name')}</th>
                  <td className="px-3 py-2 border-r border-slate-700">{authorName}</td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">{t('report.sign_date')}</th>
                  <td className="px-3 py-2">{createdDate}</td>
                </tr>
                {/* 承認欄 */}
                <tr className="border border-slate-700 border-t-0">
                  <th rowSpan={2} className="bg-slate-800/50 px-3 py-2 text-left border-r border-slate-700">{t('report.sign_approver')}</th>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">{t('report.sign_department')}</th>
                  <td className="px-3 py-2 border-r border-slate-700 text-slate-500">
                    {signatureStatus === 'signed' && signatureData ? signatureData.department : t('report.sign_unsigned')}
                  </td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">{t('report.sign_position')}</th>
                  <td className="px-3 py-2 text-slate-500">
                    {signatureStatus === 'signed' && signatureData ? signatureData.position : ''}
                  </td>
                </tr>
                <tr className="border border-slate-700 border-t-0">
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">{t('report.sign_name')}</th>
                  <td className="px-3 py-2 border-r border-slate-700 text-slate-500">
                    {signatureStatus === 'signed' && signatureData ? signatureData.signed_by : ''}
                  </td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">{t('report.sign_date')}</th>
                  <td className="px-3 py-2 text-slate-500">
                    {signatureStatus === 'signed' && signatureData ? signatureData.signed_at_display : ''}
                  </td>
                </tr>
              </tbody>
            </table>
          </div>

          {/* 承認印エリア - 判定結果に応じた表示 */}
          <div className="flex items-center justify-center gap-8">
            {signatureStatus === 'signed' && signatureData ? (
              /* 署名済み - 判子表示 */
              <div className="space-y-4 text-center">
                <div className="flex items-center justify-center gap-2 text-emerald-400 text-sm mb-4">
                  <span>✅</span>
                  <span className="font-medium">{t('report.sign_approved')}</span>
                </div>

                <SignatureArea
                  signerName={signatureData.signed_by}
                  department={signatureData.department}
                  position={signatureData.position}
                  signedAt={signatureData.signed_at_display}
                  animated={showSignedAnimation}
                />
              </div>
            ) : safeReview.overall_verdict === 'PASS' ? (
              /* 検証通過 - 署名ボタン表示 */
              <div className="flex flex-col items-center gap-4">
                <div className="w-24 h-24 rounded-full border-2 border-dashed border-slate-600 flex items-center justify-center text-slate-500 text-xs">
                  {t('report.sign_stamp')}
                </div>
                <div className="text-center">
                  <div className="text-sm text-slate-500 mb-2">{t('report.sign_instruction')}</div>
                  {user && (
                    <div className="text-xs text-slate-400 mb-3">
                      {t('report.sign_signer_label')} {user.display_name} ({user.department})
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
                        {t('report.sign_processing')}
                      </>
                    ) : (
                      <>
                        <span className="text-xl">{t('report.sign_stamp_char')}</span>
                        {t('report.sign_button')}
                      </>
                    )}
                  </button>
                </div>
              </div>
            ) : (
              /* 検証未通過 - 修正ガイダンス表示 */
              <div className="flex flex-col items-center gap-4 w-full max-w-md">
                <div className={`w-24 h-24 rounded-full border-2 border-dashed flex items-center justify-center ${
                  safeReview.overall_verdict === 'REVISE'
                    ? 'border-amber-500/50 text-amber-500'
                    : 'border-blue-500/50 text-blue-500'
                }`}>
                  <div className="text-center">
                    <div className="text-2xl mb-1">
                      {safeReview.overall_verdict === 'REVISE' ? '⚠️' : '📋'}
                    </div>
                    <div className="text-xs">
                      {safeReview.overall_verdict === 'REVISE' ? t('report.sign_revise_label') : t('report.sign_coach_label')}
                    </div>
                  </div>
                </div>
                <div className="text-center">
                  <div className={`text-sm mb-3 ${
                    safeReview.overall_verdict === 'REVISE' ? 'text-amber-400' : 'text-blue-400'
                  }`}>
                    {safeReview.overall_verdict === 'REVISE'
                      ? t('report.sign_revise_msg')
                      : t('report.sign_coach_msg')}
                  </div>
                  <div className="text-sm text-slate-400 mb-4">
                    {t('report.sign_check_review')}
                  </div>
                  <div className="flex gap-3 justify-center">
                    <button
                      onClick={() => setActiveTab('review')}
                      className="px-4 py-2 bg-slate-700 hover:bg-slate-600 rounded-lg text-sm transition-all flex items-center gap-2"
                    >
                      <span aria-hidden="true">🔍</span> {t('report.sign_view_review')}
                    </button>
                    <button
                      onClick={handleNewQuestion}
                      className="px-4 py-2 bg-indigo-500/20 hover:bg-indigo-500/30 text-indigo-400 rounded-lg text-sm transition-all flex items-center gap-2"
                    >
                      <span aria-hidden="true">🔄</span> {t('report.reanalyze_btn')}
                    </button>
                  </div>
                </div>
              </div>
            )}
          </div>
        </div>

        {/* フッター */}
        <div className="mt-8 text-center text-xs text-slate-600 border-t border-slate-800 pt-4">
          <p>{t('report.footer_auto_generated')}</p>
          <p className="mt-1 font-mono">{t('report.footer_case_id')} {caseId} | Version: {report.version || '3.1'}</p>
        </div>
      </main>
    </div>
  );
};

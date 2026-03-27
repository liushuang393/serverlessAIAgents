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
/* eslint-disable @typescript-eslint/no-explicit-any */

import React, { useCallback, useState, useEffect } from "react";
import { useDecisionStore } from "../store/useDecisionStore";
import { useAuthStore } from "../store/useAuthStore";
import { decisionApi } from "../api/client";
import { SignatureArea } from "./HankoSeal";
import type {
  RecommendedPath,
  Phase,
  Implementation,
  SignatureData,
  CheckpointItem,
  ReviewFinding,
} from "../types";

/** 通知タイプ */
type NotificationType = "success" | "error" | "info";

/** 通知コンポーネント */
const Notification: React.FC<{
  type: NotificationType;
  message: string;
  onClose: () => void;
}> = ({ type, message, onClose }) => {
  const colors = {
    success: "bg-emerald-500/10 border-emerald-500/30 text-emerald-400",
    error: "bg-red-500/10 border-red-500/30 text-red-400",
    info: "bg-blue-500/10 border-blue-500/30 text-blue-400",
  };

  const icons = {
    success: "✅",
    error: "❌",
    info: "ℹ️",
  };

  useEffect(() => {
    const timer = setTimeout(onClose, 5000);
    return () => clearTimeout(timer);
  }, [onClose]);

  return (
    <div
      className={`fixed top-4 right-4 z-50 px-4 py-3 rounded-lg border ${colors[type]} flex items-center gap-3 animate-fade-in`}
    >
      <span>{icons[type]}</span>
      <span className="text-sm">{message}</span>
      <button onClick={onClose} className="ml-2 hover:opacity-70">
        ✕
      </button>
    </div>
  );
};

/** 問題タイプ → 日本語ラベル */
const PROBLEM_TYPE_LABELS: Record<string, string> = {
  RESOURCE_ALLOCATION: "リソース配分",
  TIMING_DECISION: "タイミング判断",
  TRADE_OFF: "トレードオフ",
  RISK_ASSESSMENT: "リスク評価",
  STRATEGY_DIRECTION: "戦略方向性",
};

/** 問題の本質的性質 → 日本語ラベル */
const PROBLEM_NATURE_LABELS: Record<string, string> = {
  TECHNICAL_LIMITATION: "技術的限界",
  INVESTMENT_DECISION: "投資判断",
  CONSTRAINT_DRIVEN: "制約主導型",
  STRATEGIC_CHOICE: "戦略的選択",
  REGULATORY_COMPLIANCE: "規制対応",
  MARKET_TIMING: "市場タイミング",
};

/** タブ定義 */
const TABS = [
  { id: "summary", name: "サマリー", icon: "📊" },
  { id: "dao", name: "道", icon: "🎯" },
  { id: "fa", name: "法", icon: "🛤️" },
  { id: "shu", name: "術", icon: "📋" },
  { id: "qi", name: "器", icon: "🔧" },
  { id: "review", name: "検証", icon: "🔍" },
] as const;

type TabId = (typeof TABS)[number]["id"];
const SIGNABLE_CONFIDENCE_THRESHOLD = 40;

type MetricTarget = "confidence" | "feasibility" | "both";

const inferCheckpointMetricTarget = (item: CheckpointItem): MetricTarget => {
  const component = (item.target_component || "").toLowerCase();
  if (
    component === "risk_coverage" ||
    component === "implementation_feasibility" ||
    component === "logic_consistency"
  ) {
    return "both";
  }
  if (
    item.label.includes("撤退") ||
    item.label.includes("最悪ケース") ||
    item.label.includes("Gate")
  ) {
    return "both";
  }
  return "confidence";
};

const inferFindingMetricTarget = (finding: ReviewFinding): MetricTarget => {
  const category = (finding.category || "").toUpperCase();
  if (category === "TIMELINE_UNREALISTIC") return "feasibility";
  if (category === "OVER_OPTIMISM" || category === "RESPONSIBILITY_GAP")
    return "both";
  if (
    (finding.score_improvements || []).some((si) =>
      /feasib|可行|実装|timeline|期間|risk|リスク|依存|cost|コスト/i.test(
        si.target_score || "",
      ),
    )
  ) {
    return "both";
  }
  return "confidence";
};

const metricTargetBadge = (
  target: MetricTarget,
): { label: string; className: string } => {
  if (target === "feasibility") {
    return {
      label: "可行度",
      className: "text-cyan-300 border-cyan-500/40 bg-cyan-500/10",
    };
  }
  if (target === "both") {
    return {
      label: "両方",
      className: "text-violet-300 border-violet-500/40 bg-violet-500/10",
    };
  }
  return {
    label: "信頼度",
    className: "text-emerald-300 border-emerald-500/40 bg-emerald-500/10",
  };
};

const parsePathFeasibility = (
  path: RecommendedPath,
  judgmentFramework: unknown,
): number => {
  const framework =
    judgmentFramework && typeof judgmentFramework === "object"
      ? (judgmentFramework as {
          gate_results?: Record<string, boolean[] | undefined>;
          should_scores?: Record<string, number[] | undefined>;
        })
      : {};
  const legacyProbability = Number(path.success_probability ?? 0);
  if (legacyProbability > 0) {
    return Math.max(0, Math.min(100, Math.round(legacyProbability * 100)));
  }

  const conditional = path.conditional_evaluation;
  const successCount = Math.min(
    3,
    conditional?.success_conditions?.length ?? 0,
  );
  const riskCount = Math.min(3, conditional?.risk_factors?.length ?? 0);
  const failureCount = Math.min(3, conditional?.failure_modes?.length ?? 0);
  const hasBasis = Boolean((conditional?.probability_basis || "").trim());

  const reversibilityRatio =
    path.reversibility === "HIGH"
      ? 0.85
      : path.reversibility === "LOW"
        ? 0.45
        : 0.65;

  const gateResults = framework.gate_results ?? {};
  const pathGates = gateResults[path.path_id] ?? [];
  const gatePassRatio =
    pathGates.length > 0
      ? pathGates.filter(Boolean).length / pathGates.length
      : 0.6;

  const shouldScores = framework.should_scores ?? {};
  const pathScores = shouldScores[path.path_id] ?? [];
  const shouldRatio =
    pathScores.length > 0
      ? pathScores
          .map((score) => Math.max(1, Math.min(5, Number(score || 3))))
          .reduce((a, b) => a + b, 0) /
        pathScores.length /
        5
      : 0.6;

  const successRatio = successCount / 3;
  const riskRatio = riskCount / 3;
  const failureRatio = failureCount / 3;
  const downsideRatio = (riskRatio + failureRatio) / 2;
  const basisBonus = hasBasis ? 0.05 : 0;
  const ratio =
    0.3 * successRatio +
    0.2 * gatePassRatio +
    0.15 * shouldRatio +
    0.2 * reversibilityRatio +
    0.15 * Math.max(0, 1 - downsideRatio) +
    basisBonus;
  return Math.max(0, Math.min(100, Math.round(ratio * 100)));
};

/** パスカード（v3.1: 条件付き評価対応） */
const PathCard: React.FC<{
  path: RecommendedPath;
  isRecommended?: boolean;
}> = ({ path, isRecommended }) => (
  <div
    className={`rounded-xl p-5 border ${isRecommended ? "border-emerald-500/30 bg-emerald-500/5" : "border-white/5 bg-[#0a0a0f] opacity-60"}`}
  >
    <div className="flex items-center justify-between mb-3">
      <div className="flex items-center gap-2">
        <span>{isRecommended ? "✓" : "✕"}</span>
        <span className="font-semibold">
          {path.path_id}: {path.name}
        </span>
        {!isRecommended && (
          <span className="text-xs text-red-400 px-2 py-0.5 bg-red-500/10 rounded">
            不推奨
          </span>
        )}
      </div>
      {path.reversibility && (
        <span className="text-xs px-2 py-0.5 bg-slate-700 text-slate-300 rounded">
          可逆性: {path.reversibility}
        </span>
      )}
    </div>
    <p className="text-sm text-slate-400 mb-4">{path.description}</p>

    {/* v3.1: 条件付き評価 */}
    {path.conditional_evaluation && (
      <div className="bg-slate-800/50 rounded-lg p-3 mb-4 space-y-2">
        <div className="text-xs font-medium text-cyan-400 mb-2">
          📋 条件付き評価
        </div>
        {path.conditional_evaluation.success_conditions?.length > 0 && (
          <div>
            <span className="text-xs text-emerald-400">成立条件:</span>
            <div className="flex flex-wrap gap-1 mt-1">
              {path.conditional_evaluation.success_conditions.map(
                (c: string, ci: number) => (
                  <span
                    key={`sc-${ci}`}
                    className="text-xs px-2 py-0.5 bg-emerald-500/10 text-emerald-400 rounded"
                  >
                    {c}
                  </span>
                ),
              )}
            </div>
          </div>
        )}
        {path.conditional_evaluation.risk_factors?.length > 0 && (
          <div>
            <span className="text-xs text-amber-400">リスク要因:</span>
            <div className="flex flex-wrap gap-1 mt-1">
              {path.conditional_evaluation.risk_factors.map(
                (r: string, ri: number) => (
                  <span
                    key={`rf-${ri}`}
                    className="text-xs px-2 py-0.5 bg-amber-500/10 text-amber-400 rounded"
                  >
                    {r}
                  </span>
                ),
              )}
            </div>
          </div>
        )}
        {path.conditional_evaluation.failure_modes?.length > 0 && (
          <div>
            <span className="text-xs text-red-400">失敗モード:</span>
            <div className="flex flex-wrap gap-1 mt-1">
              {path.conditional_evaluation.failure_modes.map(
                (f: string, fi: number) => (
                  <span
                    key={`fm-${fi}`}
                    className="text-xs px-2 py-0.5 bg-red-500/10 text-red-400 rounded"
                  >
                    {f}
                  </span>
                ),
              )}
            </div>
          </div>
        )}
      </div>
    )}

    {/* リスク集中点 */}
    {path.risk_concentration && (
      <div className="text-xs text-amber-400 mb-3">
        ⚡ リスク集中点:{" "}
        <span className="text-slate-400">{path.risk_concentration}</span>
      </div>
    )}

    <div className="grid grid-cols-2 gap-4">
      <div>
        <div className="text-xs text-emerald-400 mb-2">メリット</div>
        {path.pros.map((p, i) => (
          <div
            key={`pro-${i}`}
            className="text-sm text-slate-400 flex items-center gap-2 mb-1"
          >
            <span className="text-emerald-400">+</span> {p}
          </div>
        ))}
      </div>
      <div>
        <div className="text-xs text-amber-400 mb-2">デメリット</div>
        {path.cons.map((c, i) => (
          <div
            key={`con-${i}`}
            className="text-sm text-slate-400 flex items-center gap-2 mb-1"
          >
            <span className="text-amber-400">-</span> {c}
          </div>
        ))}
      </div>
    </div>
    {path.time_to_value && (
      <div className="mt-3 text-xs text-slate-500">
        ⏱️ 価値実現: {path.time_to_value}
      </div>
    )}
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
            <span className="text-xs text-slate-500 px-2 py-1 bg-slate-800 rounded">
              {phase.duration}
            </span>
          </div>
          <div className="flex flex-wrap gap-2">
            {phase.actions.map((action, j) => (
              <span
                key={j}
                className="text-xs text-slate-400 px-2 py-1 bg-slate-800/50 rounded"
              >
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
  const {
    report,
    reportId,
    requestId,
    question,
    setPage,
    reset,
    setReport: setReportInStore,
  } = useDecisionStore();
  const { user, performLogout } = useAuthStore();
  const [activeTab, setActiveTab] = useState<TabId>("summary");
  const [exportingType, setExportingType] = useState<"pdf" | "html" | null>(
    null,
  );
  const [isSigning, setIsSigning] = useState(false);
  const [notification, setNotification] = useState<{
    type: NotificationType;
    message: string;
  } | null>(null);
  const [humanReviewNotes, setHumanReviewNotes] = useState<
    Record<number, string>
  >({});
  const [humanReviewChecks, setHumanReviewChecks] = useState<
    Record<number, boolean>
  >({});
  const [savingFindingNotes, setSavingFindingNotes] = useState<
    Record<number, boolean>
  >({});

  // v3.1 差分パッチ型: チェックポイント状態
  const [checkpointChecks, setCheckpointChecks] = useState<
    Record<string, boolean>
  >({});
  const [checkpointAnnotations, setCheckpointAnnotations] = useState<
    Record<string, string>
  >({});
  const [signatureStatus, setSignatureStatus] = useState<"unsigned" | "signed">(
    "unsigned",
  );
  const [signatureData, setSignatureData] = useState<SignatureData | null>(
    null,
  );
  const [showSignedAnimation, setShowSignedAnimation] = useState(false);
  // v3.2: チェックポイント適用後の分析信頼度（null = バックエンド値をそのまま使用）
  const [recalculatedConfidence, setRecalculatedConfidence] = useState<
    number | null
  >(null);
  const [recalculatedFeasibility, setRecalculatedFeasibility] = useState<
    number | null
  >(null);
  const [signThresholdPct, setSignThresholdPct] = useState<number>(
    SIGNABLE_CONFIDENCE_THRESHOLD,
  );
  const [scoreBreakdown, setScoreBreakdown] = useState<{
    confidence: {
      base: number;
      checkpoint: number;
      finding: number;
      llm: number;
      final: number;
      signatureEligible: boolean;
    };
    feasibility: {
      base: number;
      checkpoint: number;
      finding: number;
      llm: number;
      final: number;
    };
    bonusReasons: string[];
    contributions: {
      source: "checkpoint" | "finding" | "llm_bonus";
      item_key: string;
      label: string;
      target_metric: MetricTarget;
      confidence_boost_pct: number;
      feasibility_boost_pct: number;
      note?: string | null;
    }[];
  } | null>(null);

  // レポートがない場合は入力画面へ
  useEffect(() => {
    if (!report) {
      setPage("input");
    }
  }, [report, setPage]);

  // マウント時に署名状態を復元
  useEffect(() => {
    const checkId = reportId || requestId;
    if (!checkId || signatureStatus === "signed") return;
    decisionApi
      .getSignature(checkId)
      .then((sigInfo) => {
        if (sigInfo.signed && sigInfo.signature) {
          setSignatureStatus("signed");
          setSignatureData(
            sigInfo.signature as Parameters<typeof setSignatureData>[0],
          );
        }
      })
      .catch(() => {
        /* silent fail */
      });
  }, [reportId, requestId]); // eslint-disable-line react-hooks/exhaustive-deps

  /** PDF エクスポート */
  const handleExportPdf = useCallback(async () => {
    const exportId = requestId || reportId;
    if (!exportId) return;
    setExportingType("pdf");

    try {
      const exported = await decisionApi.exportPdf(exportId);
      const url = URL.createObjectURL(exported.blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = exported.filename;
      a.click();
      URL.revokeObjectURL(url);
      setNotification({
        type: "success",
        message: "PDFをダウンロードしました",
      });
    } catch (err) {
      const message =
        err instanceof Error ? err.message : "PDF生成に失敗しました";
      setNotification({ type: "error", message });
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
      const a = document.createElement("a");
      a.href = url;
      a.download = exported.filename;
      a.click();
      URL.revokeObjectURL(url);
      setNotification({
        type: "success",
        message: "HTMLをダウンロードしました",
      });
    } catch (err) {
      const message =
        err instanceof Error ? err.message : "HTML生成に失敗しました";
      setNotification({ type: "error", message });
    } finally {
      setExportingType(null);
    }
  }, [requestId, reportId]);

  /** 電子署名処理 */
  const handleSign = useCallback(async () => {
    if (!reportId || signatureStatus === "signed" || !user) return;
    setIsSigning(true);

    try {
      // 署名確認ダイアログ
      const confirmed = window.confirm(
        `${user.display_name} として署名します。\nこのレポートに基づいて意思決定を行います。\n署名すると記録が残ります。\n\n続行しますか？`,
      );

      if (!confirmed) {
        setIsSigning(false);
        return;
      }

      // API呼び出し
      const response = await decisionApi.signReport(reportId);

      if (response.success && response.signature) {
        setSignatureStatus("signed");
        setSignatureData(response.signature);
        setShowSignedAnimation(true);
        setNotification({
          type: "success",
          message: `${user.display_name} により署名されました`,
        });

        // アニメーション後にリセット
        setTimeout(() => setShowSignedAnimation(false), 1000);
      } else {
        setNotification({ type: "error", message: response.message });
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : "署名に失敗しました";
      setNotification({ type: "error", message });
    } finally {
      setIsSigning(false);
    }
  }, [reportId, signatureStatus, user]);

  /** ログアウト */
  const handleLogout = useCallback(async () => {
    await performLogout();
    reset();
    setPage("input");
  }, [performLogout, reset, setPage]);

  /** 新規質問 */
  const handleNewQuestion = useCallback(() => {
    reset();
    setPage("input");
  }, [reset, setPage]);

  /** 所見メモ（任意）を保存 */
  const handleSaveFindingMemo = useCallback(
    async (findingIndex: number) => {
      if (!reportId) return;
      const memo = humanReviewNotes[findingIndex] ?? "";
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
        setNotification({
          type: "success",
          message: `所見 #${findingIndex + 1} のメモを保存しました`,
        });
      } catch (err) {
        const message =
          err instanceof Error ? err.message : "所見メモの保存に失敗しました";
        setNotification({ type: "error", message });
      } finally {
        setSavingFindingNotes((prev) => ({ ...prev, [findingIndex]: false }));
      }
    },
    [
      humanReviewChecks,
      humanReviewNotes,
      reportId,
      requestId,
      user?.display_name,
    ],
  );

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
    one_line_decision: "分析結果を確認してください",
    recommended_action: "詳細は下記セクションを参照",
    first_step: "詳細は下記セクションを参照",
    key_risks: [],
    estimated_impact: "",
  };

  const safeDao = dao || {
    problem_type: "N/A",
    problem_nature: null,
    essence: "分析データがありません",
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
    why_existing_fails: "",
    competitive_hypothesis: null,
    judgment_framework: null,
    fa_self_check: null,
  };

  const safeShu = shu || {
    phases: [],
    first_action: "分析データがありません",
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
    overall_verdict: "REVISE",
    findings: [],
    confidence_score: 0,
    final_warnings: [],
  };
  const missingCountermeasureCount = (safeReview.findings || []).filter(
    (finding) => !(finding.suggested_revision || "").trim(),
  ).length;

  // v3.2: 有効な分析信頼度スコア（チェックポイント再計算後があればそれを使用）
  const rawConfidencePct = Math.round(
    (safeReview?.confidence_score ?? 0) * 100,
  );
  const effectiveConfidencePct =
    recalculatedConfidence !== null ? recalculatedConfidence : rawConfidencePct;

  // v3.2: 戦略可行度（legacy success_probability または conditional_evaluation から算出）
  const rawFeasibilityPct: number | null =
    safeFa.recommended_paths.length > 0
      ? Math.max(
          ...safeFa.recommended_paths.map((p) =>
            parsePathFeasibility(p, safeFa.judgment_framework),
          ),
        )
      : null;
  const effectiveFeasibilityPct =
    recalculatedFeasibility !== null
      ? recalculatedFeasibility
      : rawFeasibilityPct;

  const hasCheckedCheckpoint =
    safeReview.checkpoint_items?.some(
      (item) => checkpointChecks[item.item_id] ?? item.checked,
    ) ?? false;
  const hasCheckedFinding = (safeReview.findings ?? []).some((_, idx) =>
    Boolean(humanReviewChecks[idx]),
  );
  const hasAnyRecalcSelection = hasCheckedCheckpoint || hasCheckedFinding;

  // 署名可能条件（policy値。未再計算時は40%）
  const canSign = effectiveConfidencePct >= signThresholdPct;

  /** チェックポイント項目を反映してスコア自動再計算 */
  const handleApplyCheckpoints = async (): Promise<void> => {
    if (!reportId) return;
    const checkpointItems = safeReview.checkpoint_items ?? [];
    const findingItems = safeReview.findings ?? [];

    try {
      const response = await decisionApi.applyCheckpoints({
        report_id: reportId,
        request_id: requestId || undefined,
        reviewer_name: user?.display_name || undefined,
        items: checkpointItems.map((item) => ({
          item_id: item.item_id,
          checked: checkpointChecks[item.item_id] ?? item.checked,
          annotation:
            (
              checkpointAnnotations[item.item_id] ??
              item.annotation ??
              ""
            ).trim() || undefined,
        })),
        finding_confirmations: findingItems.map((finding, index) => ({
          finding_index: index,
          checked: Boolean(humanReviewChecks[index]),
          note:
            (
              humanReviewNotes[index] ??
              finding.minimal_patch?.default_value ??
              ""
            ).trim() || undefined,
        })),
      });

      setRecalculatedConfidence(response.recalculated_confidence_pct);
      setRecalculatedFeasibility(response.recalculated_feasibility_pct);
      setSignThresholdPct(response.threshold_pct);
      setScoreBreakdown({
        confidence: {
          base: response.base_confidence_pct,
          checkpoint: response.checkpoint_boost_pct,
          finding: response.finding_boost_pct,
          llm: response.llm_bonus_pct,
          final: response.recalculated_confidence_pct,
          signatureEligible: response.signature_eligible,
        },
        feasibility: {
          base: response.base_feasibility_pct,
          checkpoint: response.checkpoint_feasibility_boost_pct,
          finding: response.finding_feasibility_boost_pct,
          llm: response.llm_feasibility_bonus_pct,
          final: response.recalculated_feasibility_pct,
        },
        bonusReasons: response.bonus_reasons ?? [],
        contributions: response.applied_contributions ?? [],
      });
      setReportInStore({
        ...report,
        review: response.updated_review,
      });
      setNotification({
        type: "success",
        message: `${response.message} 分析信頼度: ${response.recalculated_confidence_pct}% / 戦略可行度: ${response.recalculated_feasibility_pct}%${response.signature_eligible ? " ✓ 署名可能" : ""}`,
      });
    } catch (err) {
      const message =
        err instanceof Error ? err.message : "チェック項目の反映に失敗しました";
      setNotification({ type: "error", message });
    }
  };

  // レビューが未生成の古いデータでは「未検証」を表示
  const reviewVerdict = review?.overall_verdict;
  const reviewStatusLabel = reviewVerdict || "未検証";
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
  const reviewStatusIcon = !reviewVerdict
    ? "🕒"
    : reviewVerdict === "PASS"
      ? "✅"
      : reviewVerdict === "REVISE"
        ? "⚠️"
        : "📋";
  const analysisQuestion = toDisplayText(
    report.original_question ??
      (report as unknown as { question?: unknown }).question ??
      question,
    "（質問が設定されていません）",
  );

  // 提案書タイトル（デフォルト値）
  const titleJa = proposal_title?.title_ja || "課題解決提案書";
  const titleEn = proposal_title?.title_en || "Solution_Proposal";
  const caseId = proposal_title?.case_id || reportId || "---";
  const subtitle = proposal_title?.subtitle || "";

  // 署名欄情報（デフォルト値）
  const authorName = signature_block?.author_name || "Decision Agent";
  const authorDept =
    signature_block?.author_department || "AI Decision Support";
  const authorPos = signature_block?.author_position || "AI Assistant";
  const createdDate =
    signature_block?.created_date || new Date().toLocaleDateString("ja-JP");

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
                disabled={exportingType !== null}
                className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all"
              >
                📄 {exportingType === "pdf" ? "生成中..." : "PDF出力"}
              </button>
              <button
                onClick={handleExportHtml}
                disabled={exportingType !== null}
                className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all"
              >
                🧾 {exportingType === "html" ? "生成中..." : "HTML出力"}
              </button>
              <button
                onClick={() => setPage("history")}
                className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all"
              >
                📜 履歴
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
                  <div className="text-sm font-medium text-white">
                    {user.display_name}
                  </div>
                  <div className="text-xs text-slate-500">
                    {user.department}
                  </div>
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
            <h1 className="text-3xl font-bold mb-2 tracking-wider">
              {titleJa}
            </h1>
            <p className="text-sm text-slate-500 font-mono mb-4">{titleEn}</p>
            {subtitle && (
              <p className="text-slate-400 text-sm mb-4">{subtitle}</p>
            )}
            <p className="text-xs text-slate-600 font-mono mb-8">
              案件ID: {caseId}
            </p>

            <div className="border-t border-white/10 pt-6 mt-6">
              <p className="text-sm text-slate-400">{createdDate}</p>
              <p className="text-sm text-slate-400 mt-2">{authorDept}</p>
              <p className="text-sm text-slate-400">
                {authorPos} {authorName}
              </p>
            </div>
          </div>
        </div>

        {/* エグゼクティブサマリー v3.2 */}
        <div className="bg-gradient-to-br from-[#12121a] to-[#1a1a24] rounded-2xl border border-white/5 p-8 mb-8 relative overflow-hidden">
          <div className="absolute top-0 right-0 w-64 h-64 bg-indigo-500/5 rounded-full blur-3xl -translate-y-1/2 translate-x-1/2" />

          <div className="relative">
            <div className="flex items-start justify-between mb-6">
              <div>
                <h2 className="text-xs text-slate-500 uppercase tracking-wider mb-1">
                  1. EXECUTIVE SUMMARY
                </h2>
                <div className="text-2xl font-bold">エグゼクティブサマリー</div>
              </div>
              {/* v3.2: 2指標表示（分析信頼度 + 戦略可行度） */}
              <div className="flex items-start gap-4">
                {/* 分析信頼度（AI完成度） */}
                <div className="text-right">
                  <div className="flex items-center gap-2 justify-end mb-1">
                    <span
                      className={`text-xs px-2 py-0.5 rounded ${reviewStatusClass}`}
                    >
                      {!reviewVerdict
                        ? "未検証"
                        : reviewVerdict === "PASS"
                          ? "検証通過"
                          : reviewVerdict === "REVISE"
                            ? "要修正"
                            : "改善指導"}
                    </span>
                  </div>
                  <div className="text-xs text-slate-500 mb-1">
                    分析信頼度（AI完成度）
                    <span
                      className="ml-1 text-slate-600 cursor-help"
                      title={`AIが分析を論理的・網羅的に完成させた度合い。ReviewAgentが判定。70%以上で高信頼、${signThresholdPct}%以上で署名可能。`}
                    >
                      ⓘ
                    </span>
                  </div>
                  <div
                    className={`text-3xl font-bold ${
                      effectiveConfidencePct >= 70
                        ? "text-emerald-400"
                        : effectiveConfidencePct >= signThresholdPct
                          ? "text-amber-400"
                          : "text-red-400"
                    }`}
                  >
                    {effectiveConfidencePct}%
                  </div>
                  {effectiveConfidencePct < signThresholdPct && (
                    <div className="text-xs text-red-400 mt-1">
                      ⚠ {signThresholdPct}%未満 署名不可
                    </div>
                  )}
                  {effectiveConfidencePct >= signThresholdPct &&
                    safeReview.overall_verdict !== "PASS" && (
                      <div className="text-xs text-amber-400 mt-1">
                        ✓ 署名可能
                      </div>
                    )}
                </div>
                {/* 戦略可行度（成功確率） */}
                <div className="text-right border-l border-white/10 pl-4">
                  <div className="h-6 mb-1" />
                  <div className="text-xs text-slate-500 mb-1">
                    戦略可行度（成功確率）
                    <span
                      className="ml-1 text-slate-600 cursor-help"
                      title="FaAgentの条件付き評価・ゲート通過率・可逆性・リスク情報から算出した実行可能性スコア。チェック反映で再計算されます。"
                    >
                      ⓘ
                    </span>
                  </div>
                  <div
                    className={`text-3xl font-bold ${
                      effectiveFeasibilityPct === null
                        ? "text-slate-500"
                        : effectiveFeasibilityPct >= 70
                          ? "text-emerald-400"
                          : effectiveFeasibilityPct >= 50
                            ? "text-amber-400"
                            : "text-red-400"
                    }`}
                  >
                    {effectiveFeasibilityPct !== null
                      ? `${effectiveFeasibilityPct}%`
                      : "—"}
                  </div>
                  {effectiveFeasibilityPct === null && (
                    <div className="text-xs text-slate-600 mt-1">
                      パス未生成
                    </div>
                  )}
                  {recalculatedFeasibility !== null && (
                    <div className="text-xs text-slate-500 mt-1">
                      チェック反映済
                    </div>
                  )}
                </div>
              </div>
            </div>

            {/* 結論 */}
            <div className="bg-[#0a0a0f] rounded-xl p-6 mb-6 border border-indigo-500/20">
              <div className="flex items-center gap-2 text-indigo-400 text-sm mb-2">
                <span>💡</span> 結論
              </div>
              <p className="text-lg font-medium">
                {safeExecutiveSummary.one_line_decision}
              </p>
            </div>

            {/* v3.0: 本質の一文 */}
            {(safeExecutiveSummary as any).essence_statement && (
              <div className="bg-purple-500/5 rounded-xl p-5 mb-6 border border-purple-500/20">
                <div className="flex items-center gap-2 text-purple-400 text-sm mb-2">
                  <span>📍</span> 本質
                </div>
                <p className="font-medium">
                  {(safeExecutiveSummary as any).essence_statement}
                </p>
              </div>
            )}

            {/* 最初の一歩 */}
            <div className="bg-emerald-500/5 rounded-xl p-5 mb-6 border border-emerald-500/20">
              <div className="flex items-center gap-2 text-emerald-400 text-sm mb-2">
                <span>🎯</span> 最初の一歩（明日実行可能）
              </div>
              <p className="font-medium">{safeExecutiveSummary.first_step}</p>
            </div>

            {/* v3.0: 戦略的禁止事項サマリー */}
            {(safeExecutiveSummary as any).strategic_prohibition_summary && (
              <div className="bg-red-500/5 rounded-xl p-5 mb-6 border border-red-500/20">
                <div className="flex items-center gap-2 text-red-400 text-sm mb-2">
                  <span>⛔</span> 戦略的禁止
                </div>
                <p className="text-sm text-slate-400">
                  {(safeExecutiveSummary as any).strategic_prohibition_summary}
                </p>
              </div>
            )}

            {/* v3.0: 撤退基準サマリー */}
            {(safeExecutiveSummary as any).exit_criteria_summary && (
              <div className="bg-amber-500/5 rounded-xl p-5 mb-6 border border-amber-500/20">
                <div className="flex items-center gap-2 text-amber-400 text-sm mb-2">
                  <span>🚪</span> 撤退基準
                </div>
                <p className="text-sm text-slate-400">
                  {(safeExecutiveSummary as any).exit_criteria_summary}
                </p>
              </div>
            )}

            {/* 主要リスク */}
            <div>
              <div className="flex items-center gap-2 text-amber-400 text-sm mb-3">
                <span>⚠️</span> 主要リスク
              </div>
              <div className="grid grid-cols-1 gap-2">
                {safeExecutiveSummary.key_risks.map((risk, i) => (
                  <div
                    key={i}
                    className="flex items-center gap-3 text-sm text-slate-400"
                  >
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
            const isReviewTab = tab.id === "review";
            const reviewBadgeColor = !reviewVerdict
              ? "bg-slate-500"
              : safeReview.overall_verdict === "PASS"
                ? "bg-emerald-500"
                : safeReview.overall_verdict === "REVISE"
                  ? "bg-amber-500"
                  : "bg-blue-500";

            return (
              <button
                key={tab.id}
                onClick={() => setActiveTab(tab.id)}
                className={`px-4 py-2 rounded-lg text-sm font-medium transition-all flex items-center gap-2 ${
                  activeTab === tab.id
                    ? "bg-indigo-500/10 text-indigo-400 border border-indigo-500/30"
                    : "text-slate-400 hover:text-white hover:bg-slate-800"
                }`}
              >
                <span>{tab.icon}</span>
                {tab.name}
                {/* 検証タブには判定ステータスバッジを表示 */}
                {isReviewTab && (
                  <span
                    className={`w-2 h-2 rounded-full ${reviewBadgeColor}`}
                  />
                )}
                {/* 検証タブに指摘件数があれば表示 */}
                {isReviewTab &&
                  safeReview.findings &&
                  safeReview.findings.length > 0 && (
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
          {activeTab === "summary" && (
            <div className="space-y-6">
              {/* 分析概要ヘッダー */}
              <div className="flex items-center justify-between">
                <h3 className="text-lg font-semibold flex items-center gap-2">
                  <span className="w-8 h-8 rounded-lg bg-indigo-500/10 flex items-center justify-center">
                    📊
                  </span>
                  分析結果概要
                </h3>
                {/* 検証ステータスバッジ */}
                <div
                  className={`px-4 py-2 rounded-lg text-sm font-medium flex items-center gap-2 ${reviewStatusClassWithBorder}`}
                >
                  <span>{reviewStatusIcon}</span>
                  検証: {reviewStatusLabel}
                </div>
              </div>

              {/* 質問の再掲示 */}
              <div className="bg-[#0a0a0f] rounded-lg p-4 border border-white/10">
                <div className="text-xs text-slate-500 mb-2">
                  📝 分析対象の質問
                </div>
                <p className="text-slate-300">{analysisQuestion}</p>
              </div>

              {/* 分析セクションナビゲーション */}
              <div className="space-y-4">
                <div className="text-sm text-slate-400 mb-3">
                  各セクションの詳細を確認できます：
                </div>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                  {/* 道 */}
                  <button
                    onClick={() => setActiveTab("dao")}
                    className="p-4 bg-[#0a0a0f] rounded-lg border border-white/5 hover:border-indigo-500/30 transition-all text-left group"
                  >
                    <div className="flex items-center gap-3 mb-2">
                      <span className="text-xl">🎯</span>
                      <span className="font-medium group-hover:text-indigo-400 transition-colors">
                        道（本質分析）
                      </span>
                    </div>
                    <p className="text-xs text-slate-500 line-clamp-2">
                      {safeDao.essence || "問題の本質を分析します"}
                    </p>
                  </button>

                  {/* 法 */}
                  <button
                    onClick={() => setActiveTab("fa")}
                    className="p-4 bg-[#0a0a0f] rounded-lg border border-white/5 hover:border-violet-500/30 transition-all text-left group"
                  >
                    <div className="flex items-center gap-3 mb-2">
                      <span className="text-xl">🛤️</span>
                      <span className="font-medium group-hover:text-violet-400 transition-colors">
                        法（戦略選定）
                      </span>
                    </div>
                    <p className="text-xs text-slate-500 line-clamp-2">
                      {safeFa.recommended_paths?.length
                        ? `${safeFa.recommended_paths.length}つの推奨戦略を提案`
                        : "戦略オプションを評価します"}
                    </p>
                  </button>

                  {/* 術 */}
                  <button
                    onClick={() => setActiveTab("shu")}
                    className="p-4 bg-[#0a0a0f] rounded-lg border border-white/5 hover:border-blue-500/30 transition-all text-left group"
                  >
                    <div className="flex items-center gap-3 mb-2">
                      <span className="text-xl">📋</span>
                      <span className="font-medium group-hover:text-blue-400 transition-colors">
                        術（実行計画）
                      </span>
                    </div>
                    <p className="text-xs text-slate-500 line-clamp-2">
                      {safeShu.phases?.length
                        ? `${safeShu.phases.length}フェーズの実行計画`
                        : "実行計画を策定します"}
                    </p>
                  </button>

                  {/* 器 */}
                  <button
                    onClick={() => setActiveTab("qi")}
                    className="p-4 bg-[#0a0a0f] rounded-lg border border-white/5 hover:border-emerald-500/30 transition-all text-left group"
                  >
                    <div className="flex items-center gap-3 mb-2">
                      <span className="text-xl">🔧</span>
                      <span className="font-medium group-hover:text-emerald-400 transition-colors">
                        器（技術実装）
                      </span>
                    </div>
                    <p className="text-xs text-slate-500 line-clamp-2">
                      {safeQi.implementations?.length
                        ? `${safeQi.implementations.length}件の実装要素を特定`
                        : "技術要件を定義します"}
                    </p>
                  </button>
                </div>

                {/* 検証セクション（特別強調） */}
                <button
                  onClick={() => setActiveTab("review")}
                  className={`w-full p-4 rounded-lg border-2 border-dashed transition-all text-left ${
                    !reviewVerdict
                      ? "bg-slate-500/5 border-slate-500/30 hover:border-slate-500/50"
                      : safeReview.overall_verdict === "PASS"
                        ? "bg-emerald-500/5 border-emerald-500/30 hover:border-emerald-500/50"
                        : safeReview.overall_verdict === "REVISE"
                          ? "bg-amber-500/5 border-amber-500/30 hover:border-amber-500/50"
                          : "bg-blue-500/5 border-blue-500/30 hover:border-blue-500/50"
                  }`}
                >
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-3">
                      <span className="text-xl">🔍</span>
                      <div>
                        <span className="font-medium">検証（ReviewAgent）</span>
                        <p className="text-xs text-slate-500 mt-1">
                          {safeReview.findings?.length
                            ? safeReview.overall_verdict === "PASS"
                              ? `${safeReview.findings.length}件の改善提案あり`
                              : `${safeReview.findings.length}件の優先改善点あり`
                            : "分析結果の検証結果を確認"}
                        </p>
                      </div>
                    </div>
                    <span
                      className={`text-sm font-medium ${
                        !reviewVerdict
                          ? "text-slate-400"
                          : safeReview.overall_verdict === "PASS"
                            ? "text-emerald-400"
                            : safeReview.overall_verdict === "REVISE"
                              ? "text-amber-400"
                              : "text-blue-400"
                      }`}
                    >
                      {reviewStatusLabel} →
                    </span>
                  </div>
                </button>
              </div>

              {/* 修正が必要な場合のガイダンス（分析信頼度が閾値未満のとき表示） */}
              {Boolean(reviewVerdict) && !canSign && (
                <div className="mt-4 p-4 bg-amber-500/5 rounded-lg border border-amber-500/20">
                  <div className="flex items-start gap-3">
                    <span className="text-amber-400 mt-0.5">💡</span>
                    <div>
                      <div className="text-sm font-medium text-amber-400 mb-1">
                        補強後に再計算してください
                      </div>
                      <div className="text-sm text-slate-400">
                        検証タブで改善提案を確認し、checkboxと補足メモを入力して「重新算分」を実行してください。
                      </div>
                    </div>
                  </div>
                </div>
              )}
            </div>
          )}

          {activeTab === "dao" && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-indigo-500/10 flex items-center justify-center">
                  🎯
                </span>
                道 / 本質分析 v3.1
              </h3>

              <div className="grid grid-cols-2 gap-4">
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-1">問題タイプ</div>
                  <div className="px-3 py-1 bg-indigo-500/10 text-indigo-400 rounded inline-block text-sm">
                    {PROBLEM_TYPE_LABELS[safeDao.problem_type] ??
                      safeDao.problem_type}
                  </div>
                </div>
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-1">
                    問題の本質的性質
                  </div>
                  <div className="px-3 py-1 bg-purple-500/10 text-purple-400 rounded inline-block text-sm">
                    {safeDao.problem_nature
                      ? (PROBLEM_NATURE_LABELS[safeDao.problem_nature] ??
                        safeDao.problem_nature)
                      : "N/A"}
                  </div>
                </div>
              </div>

              {/* 本質（一文） */}
              <div className="bg-gradient-to-r from-indigo-500/10 to-purple-500/10 rounded-lg p-5 border border-indigo-500/20">
                <div className="text-xs text-indigo-400 mb-2">
                  📍 本質（一文）
                </div>
                <div className="text-lg font-medium">{safeDao.essence}</div>
              </div>

              {/* v3.0: 本質導出プロセス */}
              {safeDao.essence_derivation && (
                <div className="bg-[#0a0a0f] rounded-lg p-5 border border-blue-500/20">
                  <div className="text-sm font-medium text-blue-400 mb-4 flex items-center gap-2">
                    <span>🔍</span> 本質導出プロセス
                  </div>
                  <div className="space-y-3">
                    <div>
                      <div className="text-xs text-slate-500">表面的問題</div>
                      <div className="text-sm mt-1">
                        {safeDao.essence_derivation.surface_problem}
                      </div>
                    </div>
                    <div className="w-full h-px bg-slate-800" />
                    <div>
                      <div className="text-xs text-slate-500">一段深い理由</div>
                      <div className="text-sm mt-1">
                        {safeDao.essence_derivation.underlying_why}
                      </div>
                    </div>
                    <div className="w-full h-px bg-slate-800" />
                    <div>
                      <div className="text-xs text-slate-500">根本制約</div>
                      <div className="text-sm mt-1">
                        {safeDao.essence_derivation.root_constraint}
                      </div>
                    </div>
                    <div className="w-full h-px bg-slate-800" />
                    <div className="bg-blue-500/5 rounded p-3">
                      <div className="text-xs text-blue-400">本質の一文</div>
                      <div className="text-sm mt-1 font-medium">
                        {safeDao.essence_derivation.essence_statement}
                      </div>
                    </div>
                  </div>
                </div>
              )}

              {/* v3.0: 既存代替手段 */}
              {safeDao.existing_alternatives &&
                safeDao.existing_alternatives.length > 0 && (
                  <div className="bg-[#0a0a0f] rounded-lg p-4">
                    <div className="text-sm font-medium text-amber-400 mb-3 flex items-center gap-2">
                      <span>🔄</span> 既存代替手段（なぜ使えないか）
                    </div>
                    <div className="space-y-3">
                      {safeDao.existing_alternatives.map(
                        (alt: any, i: number) => (
                          <div
                            key={i}
                            className="bg-amber-500/5 rounded p-3 border border-amber-500/10"
                          >
                            <div className="font-medium text-amber-400 text-sm">
                              {alt.name}
                            </div>
                            <div className="text-sm text-slate-400 mt-1">
                              {alt.why_not_viable}
                            </div>
                            <div className="text-xs text-slate-500 mt-1">
                              制約: {alt.specific_constraint}
                            </div>
                          </div>
                        ),
                      )}
                    </div>
                  </div>
                )}

              {safeDao.immutable_constraints && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-3">
                    🔒 不可変制約
                  </div>
                  <div className="space-y-2">
                    {safeDao.immutable_constraints.map(
                      (c: string, i: number) => (
                        <div
                          key={i}
                          className="flex items-center gap-2 text-sm"
                        >
                          <span className="text-red-400">🔒</span> {c}
                        </div>
                      ),
                    )}
                  </div>
                </div>
              )}

              {safeDao.hidden_assumptions && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-3">
                    💭 隠れた前提
                  </div>
                  <div className="space-y-2">
                    {safeDao.hidden_assumptions.map((a: string, i: number) => (
                      <div
                        key={i}
                        className="flex items-center gap-2 text-sm text-slate-400"
                      >
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
                    <span>⚙️</span> 因果齿轮
                  </div>
                  <div className="space-y-2">
                    {safeDao.causal_gears.map((gear: any, i: number) => (
                      <div
                        key={i}
                        className={`flex items-start gap-3 p-2 rounded ${
                          gear.name === safeDao.bottleneck_gear
                            ? "bg-cyan-500/10 border border-cyan-500/30"
                            : ""
                        }`}
                      >
                        <span className="text-cyan-400">⚙️</span>
                        <div className="flex-1">
                          <div className="flex items-center gap-2">
                            <span className="font-medium text-sm">
                              {gear.name}
                            </span>
                            <span className="text-xs px-2 py-0.5 bg-slate-700 rounded">
                              Leverage: {gear.leverage}
                            </span>
                            {gear.name === safeDao.bottleneck_gear && (
                              <span className="text-xs px-2 py-0.5 bg-cyan-500/20 text-cyan-400 rounded">
                                ボトルネック
                              </span>
                            )}
                          </div>
                          <div className="text-xs text-slate-400 mt-1">
                            {gear.description}
                          </div>
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
                    <span>💀</span> 死穴（禁忌）
                  </div>
                  <div className="space-y-3">
                    {safeDao.death_traps.map((trap: any, i: number) => (
                      <div key={i} className="bg-red-500/10 rounded p-3">
                        <div className="flex items-center gap-2">
                          <span className="text-red-400">⚠️</span>
                          <span className="font-medium text-sm">
                            {trap.action}
                          </span>
                          <span
                            className={`text-xs px-2 py-0.5 rounded ${
                              trap.severity === "FATAL"
                                ? "bg-red-500/20 text-red-400"
                                : "bg-amber-500/20 text-amber-400"
                            }`}
                          >
                            {trap.severity}
                          </span>
                        </div>
                        <div className="text-sm text-slate-400 mt-2">
                          {trap.reason}
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.1: 制約境界条件 */}
              {safeDao.constraint_boundaries &&
                safeDao.constraint_boundaries.length > 0 && (
                  <div className="bg-[#0a0a0f] rounded-lg p-4">
                    <div className="text-sm font-medium text-orange-400 mb-3 flex items-center gap-2">
                      <span>🚧</span> 制約境界条件
                    </div>
                    <div className="overflow-x-auto">
                      <table className="w-full text-sm">
                        <thead>
                          <tr className="border-b border-slate-700">
                            <th className="text-left py-2 px-3 text-slate-400 font-medium">
                              制約名
                            </th>
                            <th className="text-left py-2 px-3 text-slate-400 font-medium">
                              判定条件
                            </th>
                            <th className="text-left py-2 px-3 text-slate-400 font-medium">
                              違反例
                            </th>
                            <th className="text-left py-2 px-3 text-slate-400 font-medium">
                              例外
                            </th>
                          </tr>
                        </thead>
                        <tbody>
                          {safeDao.constraint_boundaries.map(
                            (cb: any, i: number) => (
                              <tr key={i} className="border-b border-slate-800">
                                <td className="py-2 px-3 text-orange-300 font-medium">
                                  {cb.constraint_name}
                                </td>
                                <td className="py-2 px-3 text-slate-300">
                                  {cb.definition}
                                </td>
                                <td className="py-2 px-3 text-red-400/80">
                                  {cb.violation_example}
                                </td>
                                <td className="py-2 px-3 text-slate-500">
                                  {cb.exceptions}
                                </td>
                              </tr>
                            ),
                          )}
                        </tbody>
                      </table>
                    </div>
                  </div>
                )}

              {/* v3.1: 成立ルート比較 */}
              {safeDao.solution_routes &&
                safeDao.solution_routes.length > 0 && (
                  <div className="bg-[#0a0a0f] rounded-lg p-4">
                    <div className="text-sm font-medium text-teal-400 mb-3 flex items-center gap-2">
                      <span>🛤️</span> 成立ルート比較（解空間探索）
                    </div>
                    <div className="grid gap-3">
                      {safeDao.solution_routes.map((sr: any, i: number) => (
                        <div
                          key={i}
                          className="bg-teal-500/5 rounded p-3 border border-teal-500/10"
                        >
                          <div className="flex items-center gap-2 mb-2">
                            <span className="px-2 py-0.5 bg-teal-500/20 text-teal-400 rounded text-xs font-medium">
                              {sr.route_type}
                            </span>
                            <span className="text-xs text-slate-500">
                              実現可能性: {sr.viability}
                            </span>
                          </div>
                          <div className="text-sm text-slate-300">
                            {sr.description}
                          </div>
                          {sr.tradeoffs && sr.tradeoffs.length > 0 && (
                            <div className="flex gap-2 mt-2 flex-wrap">
                              {sr.tradeoffs.map((t: string, j: number) => (
                                <span
                                  key={j}
                                  className="text-xs px-2 py-0.5 bg-amber-500/10 text-amber-400 rounded"
                                >
                                  ⚖️ {t}
                                </span>
                              ))}
                            </div>
                          )}
                        </div>
                      ))}
                    </div>
                  </div>
                )}

              {/* v3.1: 定量指標 */}
              {safeDao.quantified_metrics &&
                safeDao.quantified_metrics.length > 0 && (
                  <div className="bg-[#0a0a0f] rounded-lg p-4">
                    <div className="text-sm font-medium text-sky-400 mb-3 flex items-center gap-2">
                      <span>📊</span> 定量指標
                    </div>
                    <div className="overflow-x-auto">
                      <table className="w-full text-sm">
                        <thead>
                          <tr className="border-b border-slate-700">
                            <th className="text-left py-2 px-3 text-slate-400 font-medium">
                              優先
                            </th>
                            <th className="text-left py-2 px-3 text-slate-400 font-medium">
                              指標名
                            </th>
                            <th className="text-left py-2 px-3 text-slate-400 font-medium">
                              目標値
                            </th>
                            <th className="text-left py-2 px-3 text-slate-400 font-medium">
                              トレードオフ
                            </th>
                          </tr>
                        </thead>
                        <tbody>
                          {safeDao.quantified_metrics.map(
                            (qm: any, i: number) => (
                              <tr key={i} className="border-b border-slate-800">
                                <td className="py-2 px-3">
                                  <span
                                    className={`px-2 py-0.5 rounded text-xs font-medium ${
                                      qm.priority <= 2
                                        ? "bg-red-500/20 text-red-400"
                                        : qm.priority <= 5
                                          ? "bg-amber-500/20 text-amber-400"
                                          : "bg-slate-500/20 text-slate-400"
                                    }`}
                                  >
                                    P{qm.priority}
                                  </span>
                                </td>
                                <td className="py-2 px-3 text-sky-300 font-medium">
                                  {qm.metric_name}
                                </td>
                                <td className="py-2 px-3 text-slate-300">
                                  {qm.target_value}
                                </td>
                                <td className="py-2 px-3 text-slate-500">
                                  {qm.tradeoff_note}
                                </td>
                              </tr>
                            ),
                          )}
                        </tbody>
                      </table>
                    </div>
                  </div>
                )}

              {/* v3.1: 監査証拠チェックリスト */}
              {safeDao.audit_evidence_checklist &&
                safeDao.audit_evidence_checklist.length > 0 && (
                  <div className="bg-[#0a0a0f] rounded-lg p-4">
                    <div className="text-sm font-medium text-emerald-400 mb-3 flex items-center gap-2">
                      <span>📋</span> 監査証拠チェックリスト
                    </div>
                    <div className="space-y-2">
                      {safeDao.audit_evidence_checklist.map(
                        (ae: any, i: number) => (
                          <div
                            key={i}
                            className="bg-emerald-500/5 rounded p-3 border border-emerald-500/10"
                          >
                            <div className="flex items-center gap-2 mb-1">
                              <span className="px-2 py-0.5 bg-emerald-500/20 text-emerald-400 rounded text-xs font-medium">
                                {ae.category}
                              </span>
                            </div>
                            <div className="text-sm text-slate-300">
                              {ae.required_evidence}
                            </div>
                            <div className="text-xs text-slate-500 mt-1">
                              確認方法: {ae.verification_method}
                            </div>
                          </div>
                        ),
                      )}
                    </div>
                  </div>
                )}

              {/* v3.1: セルフチェック結果 */}
              {safeDao.self_check && (
                <div
                  className={`rounded-lg p-4 border ${
                    safeDao.self_check.overall_status === "PASS"
                      ? "bg-emerald-500/5 border-emerald-500/20"
                      : safeDao.self_check.overall_status === "FATAL"
                        ? "bg-red-500/5 border-red-500/20"
                        : "bg-amber-500/5 border-amber-500/20"
                  }`}
                >
                  <div className="flex items-center justify-between mb-3">
                    <div className="text-sm font-medium flex items-center gap-2">
                      <span>🔬</span> セルフチェック
                    </div>
                    <span
                      className={`px-3 py-1 rounded-full text-xs font-bold ${
                        safeDao.self_check.overall_status === "PASS"
                          ? "bg-emerald-500/20 text-emerald-400"
                          : safeDao.self_check.overall_status === "FATAL"
                            ? "bg-red-500/20 text-red-400"
                            : "bg-amber-500/20 text-amber-400"
                      }`}
                    >
                      {safeDao.self_check.overall_status}
                    </span>
                  </div>
                  <div className="grid grid-cols-2 gap-2 text-xs">
                    {safeDao.self_check.boundary_undefined?.length > 0 && (
                      <div className="text-slate-400">
                        <span className="text-amber-400">⚠</span> 境界未定義:{" "}
                        {safeDao.self_check.boundary_undefined.join(", ")}
                      </div>
                    )}
                    {safeDao.self_check.missing_alternatives?.length > 0 && (
                      <div className="text-slate-400">
                        <span className="text-amber-400">⚠</span> 選択肢漏れ:{" "}
                        {safeDao.self_check.missing_alternatives.join(", ")}
                      </div>
                    )}
                    {safeDao.self_check.ambiguous_metrics?.length > 0 && (
                      <div className="text-slate-400">
                        <span className="text-amber-400">⚠</span> 曖昧な指標:{" "}
                        {safeDao.self_check.ambiguous_metrics.join(", ")}
                      </div>
                    )}
                    {safeDao.self_check.constraint_conflicts?.length > 0 && (
                      <div className="text-slate-400">
                        <span className="text-red-400">❌</span> 制約衝突:{" "}
                        {safeDao.self_check.constraint_conflicts.join(", ")}
                      </div>
                    )}
                    {safeDao.self_check.evidence_gaps?.length > 0 && (
                      <div className="text-slate-400">
                        <span className="text-amber-400">⚠</span> 証拠不足:{" "}
                        {safeDao.self_check.evidence_gaps.join(", ")}
                      </div>
                    )}
                  </div>
                </div>
              )}
            </div>
          )}

          {activeTab === "fa" && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-violet-500/10 flex items-center justify-center">
                  🛤️
                </span>
                法 / 戦略選定 v3.1
              </h3>

              {/* v3.1: 戦略的禁止事項（仕組み化） */}
              {safeFa.strategic_prohibitions &&
                safeFa.strategic_prohibitions.length > 0 && (
                  <div className="bg-red-500/5 rounded-lg p-5 border border-red-500/20">
                    <div className="text-sm font-medium text-red-400 mb-4 flex items-center gap-2">
                      <span>🚫</span> 戦略的禁止事項（仕組み化）
                    </div>
                    <div className="space-y-3">
                      {safeFa.strategic_prohibitions.map(
                        (p: any, i: number) => (
                          <div
                            key={`proh-${i}`}
                            className="bg-red-500/10 rounded p-4"
                          >
                            <div className="flex items-start gap-2">
                              <span className="text-red-400 mt-0.5">⛔</span>
                              <div className="flex-1">
                                <div className="font-medium text-sm">
                                  {p.prohibition}
                                </div>
                                <div className="text-sm text-slate-400 mt-2">
                                  理由: {p.rationale}
                                </div>
                                <div className="text-sm text-red-400 mt-1">
                                  違反結果: {p.violation_consequence}
                                </div>
                                {/* v3.1: 仕組み化フィールド */}
                                {(p.prevention_measure ||
                                  p.detection_metric ||
                                  p.responsible_role) && (
                                  <div className="mt-3 pt-3 border-t border-red-500/10 space-y-1">
                                    {p.prevention_measure && (
                                      <div className="text-xs text-cyan-400">
                                        🛡️ 防止策:{" "}
                                        <span className="text-slate-400">
                                          {p.prevention_measure}
                                        </span>
                                      </div>
                                    )}
                                    {p.detection_metric && (
                                      <div className="text-xs text-cyan-400">
                                        📊 検知指標:{" "}
                                        <span className="text-slate-400">
                                          {p.detection_metric}
                                        </span>
                                      </div>
                                    )}
                                    {p.responsible_role && (
                                      <div className="text-xs text-cyan-400">
                                        👤 責任者:{" "}
                                        <span className="text-slate-400">
                                          {p.responsible_role}
                                        </span>
                                      </div>
                                    )}
                                  </div>
                                )}
                              </div>
                            </div>
                          </div>
                        ),
                      )}
                    </div>
                  </div>
                )}

              {/* v3.1: 競争優位仮説 */}
              {safeFa.competitive_hypothesis && (
                <div className="bg-gradient-to-r from-violet-500/10 to-purple-500/10 rounded-lg p-5 border border-violet-500/20">
                  <div className="text-sm font-medium text-violet-400 mb-4 flex items-center gap-2">
                    <span>🎯</span> 競争優位仮説（v3.1）
                  </div>
                  <div className="grid grid-cols-2 gap-4">
                    <div className="bg-violet-500/10 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">
                        差別化軸
                      </div>
                      <div className="text-lg font-medium text-violet-400">
                        {safeFa.competitive_hypothesis.axis_name}
                      </div>
                    </div>
                    <div className="bg-violet-500/10 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">
                        対象顧客・利用シーン
                      </div>
                      <div className="text-sm text-slate-400">
                        {safeFa.competitive_hypothesis.target_customer}
                      </div>
                    </div>
                    <div className="bg-violet-500/10 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">
                        代替が難しい理由
                      </div>
                      <div className="text-sm text-slate-400">
                        {safeFa.competitive_hypothesis.substitution_barrier}
                      </div>
                    </div>
                    <div className="bg-violet-500/10 rounded p-4">
                      <div className="text-xs text-slate-500 mb-1">
                        勝ち筋指標
                      </div>
                      <div className="text-sm text-slate-400">
                        {safeFa.competitive_hypothesis.winning_metric}
                      </div>
                    </div>
                  </div>
                  <div className="mt-4 bg-slate-800/50 rounded p-4">
                    <div className="text-xs text-slate-500 mb-1">
                      最小検証計画
                    </div>
                    <div className="text-sm text-slate-400">
                      {safeFa.competitive_hypothesis.minimum_verification}
                    </div>
                  </div>
                </div>
              )}

              {/* v3.0互換: 差別化軸（competitive_hypothesisが無い場合のフォールバック） */}
              {!safeFa.competitive_hypothesis &&
                safeFa.differentiation_axis && (
                  <div className="bg-gradient-to-r from-violet-500/10 to-purple-500/10 rounded-lg p-5 border border-violet-500/20">
                    <div className="text-sm font-medium text-violet-400 mb-4 flex items-center gap-2">
                      <span>🎯</span> 差別化軸
                    </div>
                    <div className="space-y-4">
                      <div className="bg-violet-500/10 rounded p-4">
                        <div className="text-xs text-slate-500 mb-1">
                          勝負する軸
                        </div>
                        <div className="text-lg font-medium text-violet-400">
                          {safeFa.differentiation_axis.axis_name}
                        </div>
                        <div className="text-sm text-slate-400 mt-2">
                          {safeFa.differentiation_axis.why_this_axis}
                        </div>
                      </div>
                      <div className="bg-slate-800/50 rounded p-4">
                        <div className="text-xs text-slate-500 mb-1">
                          勝負しない軸
                        </div>
                        <div className="text-sm text-slate-400">
                          {safeFa.differentiation_axis.not_this_axis}
                        </div>
                      </div>
                    </div>
                  </div>
                )}

              {/* v3.0: 既存解が使えない理由 */}
              {safeFa.why_existing_fails && (
                <div className="bg-amber-500/5 rounded-lg p-4 border border-amber-500/20">
                  <div className="text-xs text-amber-400 mb-2 flex items-center gap-2">
                    <span>⚠️</span> 既存解が使えない理由
                  </div>
                  <div className="text-sm text-slate-400">
                    {safeFa.why_existing_fails}
                  </div>
                </div>
              )}

              {/* 推奨パス（v3.1: 最低4案） */}
              {safeFa.recommended_paths?.map(
                (path: RecommendedPath, i: number) => (
                  <PathCard
                    key={`rec-${path.path_id || i}`}
                    path={path}
                    isRecommended
                  />
                ),
              )}

              {/* 不推奨パス */}
              {safeFa.rejected_paths?.map(
                (path: RecommendedPath, i: number) => (
                  <PathCard
                    key={`rej-${path.path_id || i}`}
                    path={path}
                    isRecommended={false}
                  />
                ),
              )}

              {/* v3.1: 判断フレームワーク（Must/Should分離） */}
              {safeFa.judgment_framework && (
                <div className="bg-slate-800/50 rounded-lg p-5 border border-slate-700">
                  <div className="text-sm font-medium text-cyan-400 mb-4 flex items-center gap-2">
                    <span>⚖️</span> 判断フレームワーク（Must/Should分離）
                  </div>
                  {/* Must Gates */}
                  {safeFa.judgment_framework.must_gates?.length > 0 && (
                    <div className="mb-4">
                      <div className="text-xs text-red-400 font-medium mb-2">
                        Must（不可変ゲート）— 不合格=即却下
                      </div>
                      <div className="space-y-2">
                        {safeFa.judgment_framework.must_gates.map(
                          (gate: any, gi: number) => (
                            <div
                              key={`must-${gi}`}
                              className="bg-red-500/10 rounded p-3 flex items-start gap-3"
                            >
                              <span className="text-red-400 text-xs mt-0.5">
                                🚪
                              </span>
                              <div className="flex-1">
                                <div className="text-sm font-medium text-red-300">
                                  {gate.criterion}
                                </div>
                                <div className="text-xs text-slate-400 mt-1">
                                  閾値: {gate.threshold}
                                </div>
                              </div>
                            </div>
                          ),
                        )}
                      </div>
                    </div>
                  )}
                  {/* Should Criteria */}
                  {safeFa.judgment_framework.should_criteria?.length > 0 && (
                    <div>
                      <div className="text-xs text-emerald-400 font-medium mb-2">
                        Should（比較評価）— 重み付きスコア
                      </div>
                      <div className="space-y-2">
                        {safeFa.judgment_framework.should_criteria.map(
                          (crit: any, si: number) => (
                            <div
                              key={`should-${si}`}
                              className="bg-emerald-500/10 rounded p-3"
                            >
                              <div className="flex items-center justify-between">
                                <div className="text-sm font-medium text-emerald-300">
                                  {crit.criterion}
                                </div>
                                <span
                                  className={`text-xs px-2 py-0.5 rounded ${
                                    crit.weight === "High"
                                      ? "bg-red-500/20 text-red-400"
                                      : crit.weight === "Med"
                                        ? "bg-amber-500/20 text-amber-400"
                                        : "bg-slate-700 text-slate-400"
                                  }`}
                                >
                                  {crit.weight}
                                </span>
                              </div>
                              <div className="text-xs text-slate-400 mt-1">
                                採点: {crit.scoring_method}
                              </div>
                            </div>
                          ),
                        )}
                      </div>
                    </div>
                  )}
                </div>
              )}

              {/* 判断基準（v3.0互換フォールバック） */}
              {!safeFa.judgment_framework && safeFa.decision_criteria && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2">判断基準</div>
                  <div className="flex flex-wrap gap-2">
                    {safeFa.decision_criteria.map((c: string, ci: number) => (
                      <span
                        key={`dc-${ci}`}
                        className="px-2 py-1 bg-slate-800 text-slate-400 rounded text-xs"
                      >
                        {c}
                      </span>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.1: セルフチェック結果 */}
              {safeFa.fa_self_check && (
                <div
                  className={`rounded-lg p-4 border ${
                    safeFa.fa_self_check.overall_status === "PASS"
                      ? "bg-emerald-500/5 border-emerald-500/20"
                      : safeFa.fa_self_check.overall_status === "WARNING"
                        ? "bg-amber-500/5 border-amber-500/20"
                        : "bg-red-500/5 border-red-500/20"
                  }`}
                >
                  <div className="text-sm font-medium mb-3 flex items-center gap-2">
                    <span>🔍</span> セルフチェック結果
                    <span
                      className={`text-xs px-2 py-0.5 rounded ${
                        safeFa.fa_self_check.overall_status === "PASS"
                          ? "bg-emerald-500/20 text-emerald-400"
                          : safeFa.fa_self_check.overall_status === "WARNING"
                            ? "bg-amber-500/20 text-amber-400"
                            : "bg-red-500/20 text-red-400"
                      }`}
                    >
                      {safeFa.fa_self_check.overall_status}
                    </span>
                  </div>
                  <div className="grid grid-cols-2 gap-2 text-xs">
                    {safeFa.fa_self_check.baseless_numbers?.length > 0 && (
                      <div className="bg-red-500/10 rounded p-2">
                        <div className="text-red-400 font-medium mb-1">
                          根拠なき数値
                        </div>
                        {safeFa.fa_self_check.baseless_numbers.map(
                          (item: string, bi: number) => (
                            <div key={`bn-${bi}`} className="text-slate-400">
                              • {item}
                            </div>
                          ),
                        )}
                      </div>
                    )}
                    {safeFa.fa_self_check.missing_intermediate?.length > 0 && (
                      <div className="bg-amber-500/10 rounded p-2">
                        <div className="text-amber-400 font-medium mb-1">
                          中間案漏れ
                        </div>
                        {safeFa.fa_self_check.missing_intermediate.map(
                          (item: string, mi: number) => (
                            <div key={`mi-${mi}`} className="text-slate-400">
                              • {item}
                            </div>
                          ),
                        )}
                      </div>
                    )}
                    {safeFa.fa_self_check.missing_gates?.length > 0 && (
                      <div className="bg-amber-500/10 rounded p-2">
                        <div className="text-amber-400 font-medium mb-1">
                          ゲート不在
                        </div>
                        {safeFa.fa_self_check.missing_gates.map(
                          (item: string, mgi: number) => (
                            <div key={`mg-${mgi}`} className="text-slate-400">
                              • {item}
                            </div>
                          ),
                        )}
                      </div>
                    )}
                    {safeFa.fa_self_check.appearance_precision?.length > 0 && (
                      <div className="bg-red-500/10 rounded p-2">
                        <div className="text-red-400 font-medium mb-1">
                          見せかけ精度
                        </div>
                        {safeFa.fa_self_check.appearance_precision.map(
                          (item: string, api: number) => (
                            <div key={`ap-${api}`} className="text-slate-400">
                              • {item}
                            </div>
                          ),
                        )}
                      </div>
                    )}
                  </div>
                </div>
              )}
            </div>
          )}

          {activeTab === "shu" && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-blue-500/10 flex items-center justify-center">
                  📋
                </span>
                術 / 実行計画 v3.1
              </h3>

              {/* v3.1: PoC完成定義 (DoD) */}
              {safeShu.poc_definition_of_done && (
                <div className="bg-emerald-500/5 rounded-lg p-5 border border-emerald-500/20">
                  <div className="text-sm font-medium text-emerald-400 mb-4 flex items-center gap-2">
                    <span>🎯</span> PoC完成定義（Definition of Done）
                  </div>
                  <div className="space-y-4">
                    {safeShu.poc_definition_of_done.experience_conditions
                      ?.length > 0 && (
                      <div>
                        <div className="text-xs text-slate-500 mb-2">
                          体験条件
                        </div>
                        {safeShu.poc_definition_of_done.experience_conditions.map(
                          (c: string, i: number) => (
                            <div
                              key={i}
                              className="text-sm flex items-center gap-2 mb-1"
                            >
                              <span className="text-emerald-400">✓</span> {c}
                            </div>
                          ),
                        )}
                      </div>
                    )}
                    <div>
                      <div className="text-xs text-slate-500 mb-2">
                        成功指標
                      </div>
                      <div className="overflow-x-auto">
                        <table className="w-full text-sm">
                          <thead>
                            <tr className="border-b border-slate-700">
                              <th className="text-left py-1 text-slate-500">
                                指標
                              </th>
                              <th className="text-left py-1 text-slate-500">
                                目標値
                              </th>
                              <th className="text-left py-1 text-slate-500">
                                計測方法
                              </th>
                            </tr>
                          </thead>
                          <tbody>
                            {safeShu.poc_definition_of_done.success_metrics?.map(
                              (m: any, i: number) => (
                                <tr
                                  key={i}
                                  className="border-b border-slate-800"
                                >
                                  <td className="py-1 text-emerald-400">
                                    {m.metric_name}
                                  </td>
                                  <td className="py-1">{m.target_value}</td>
                                  <td className="py-1 text-slate-400">
                                    {m.measurement_method}
                                  </td>
                                </tr>
                              ),
                            )}
                          </tbody>
                        </table>
                      </div>
                    </div>
                    <div className="bg-amber-500/10 rounded p-3">
                      <div className="text-xs text-amber-400">
                        フォールバック
                      </div>
                      <div className="text-sm mt-1">
                        {safeShu.poc_definition_of_done.fallback_strategy}
                      </div>
                    </div>
                  </div>
                </div>
              )}

              {/* v3.1: 2段ロケット */}
              {safeShu.two_stage_rocket && (
                <div className="space-y-4">
                  {[
                    safeShu.two_stage_rocket.stage1_minimal_pipeline,
                    safeShu.two_stage_rocket.stage2_governance,
                  ].map(
                    (stage: any, si: number) =>
                      stage && (
                        <div
                          key={si}
                          className={`rounded-lg p-5 border ${si === 0 ? "bg-blue-500/5 border-blue-500/20" : "bg-purple-500/5 border-purple-500/20"}`}
                        >
                          <div
                            className={`text-sm font-medium mb-3 flex items-center gap-2 ${si === 0 ? "text-blue-400" : "text-purple-400"}`}
                          >
                            <span>{si === 0 ? "🚀" : "🛡️"}</span>{" "}
                            {stage.stage_name}
                          </div>
                          <div className="text-sm text-slate-400 mb-3">
                            {stage.objective}
                          </div>
                          {stage.gate_criteria?.length > 0 && (
                            <div className="mb-3 flex flex-wrap gap-2">
                              {stage.gate_criteria.map(
                                (g: string, gi: number) => (
                                  <span
                                    key={gi}
                                    className="text-xs px-2 py-1 bg-slate-700 rounded"
                                  >
                                    ゲート: {g}
                                  </span>
                                ),
                              )}
                            </div>
                          )}
                          <div className="space-y-3">
                            {stage.phases?.map((p: any, pi: number) => (
                              <div
                                key={pi}
                                className="bg-[#0a0a0f] rounded p-4"
                              >
                                <div className="flex items-center justify-between mb-2">
                                  <span className="font-medium text-sm">
                                    Phase {p.phase_number}: {p.name}
                                  </span>
                                  <span className="text-xs text-slate-500">
                                    {p.duration}
                                  </span>
                                </div>
                                <div className="text-xs text-slate-500 mb-2">
                                  目的: {p.purpose}
                                </div>
                                <div className="text-xs text-slate-400 mb-1">
                                  作業: {p.tasks?.join(", ")}
                                </div>
                                {p.deliverables?.length > 0 && (
                                  <div className="text-xs text-slate-500">
                                    成果物: {p.deliverables.join(", ")}
                                  </div>
                                )}
                                {p.measurement && (
                                  <div className="text-xs text-emerald-400 mt-1">
                                    計測: {p.measurement}
                                  </div>
                                )}
                                {p.notes?.length > 0 && (
                                  <div className="text-xs text-amber-400 mt-1">
                                    注意: {p.notes.join(", ")}
                                  </div>
                                )}
                                {p.branches?.length > 0 && (
                                  <div className="mt-2 space-y-1">
                                    <div className="text-xs text-slate-500">
                                      分岐（代替案）:
                                    </div>
                                    {p.branches.map((b: any, bi: number) => (
                                      <div
                                        key={bi}
                                        className="text-xs bg-blue-500/10 rounded p-2"
                                      >
                                        <span className="text-blue-400">
                                          {b.branch_name}
                                        </span>
                                        <span className="text-slate-500 mx-1">
                                          →
                                        </span>
                                        <span className="text-slate-400">
                                          {b.trigger_condition}
                                        </span>
                                        <div className="text-slate-500 mt-1">
                                          {b.description}
                                        </div>
                                      </div>
                                    ))}
                                  </div>
                                )}
                              </div>
                            ))}
                          </div>
                        </div>
                      ),
                  )}
                </div>
              )}

              {safeShu.first_action && (
                <div className="bg-emerald-500/5 rounded-lg p-4 border border-emerald-500/20">
                  <div className="text-xs text-emerald-400 mb-2">
                    🎯 最初の一歩
                  </div>
                  <div className="text-sm font-medium">
                    {safeShu.first_action}
                  </div>
                </div>
              )}

              {/* v3.0: 切り捨てリスト */}
              {safeShu.cut_list && safeShu.cut_list.length > 0 && (
                <div className="bg-red-500/5 rounded-lg p-4 border border-red-500/20">
                  <div className="text-sm font-medium text-red-400 mb-3 flex items-center gap-2">
                    <span>✂️</span> 切り捨てリスト（最初の30日間でやらないこと）
                  </div>
                  <div className="space-y-2">
                    {safeShu.cut_list.map((item: string, i: number) => (
                      <div
                        key={i}
                        className="flex items-center gap-2 text-sm text-red-400"
                      >
                        <span>❌</span> {item}
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* v3.0: 文脈特化行動 */}
              {safeShu.context_specific_actions &&
                safeShu.context_specific_actions.length > 0 && (
                  <div className="bg-blue-500/5 rounded-lg p-4 border border-blue-500/20">
                    <div className="text-sm font-medium text-blue-400 mb-3 flex items-center gap-2">
                      <span>🎯</span> 文脈特化行動（この問題固有）
                    </div>
                    <div className="space-y-3">
                      {safeShu.context_specific_actions.map(
                        (action: any, i: number) => (
                          <div key={i} className="bg-blue-500/10 rounded p-3">
                            <div className="font-medium text-sm">
                              {action.action}
                            </div>
                            <div className="text-xs text-slate-500 mt-1">
                              理由: {action.why_this_context}
                            </div>
                            <div className="text-xs text-blue-400 mt-1">
                              期待出力: {action.expected_output}
                            </div>
                          </div>
                        ),
                      )}
                    </div>
                  </div>
                )}

              {/* v3.0: 単一検証ポイント */}
              {safeShu.single_validation_point && (
                <div className="bg-amber-500/5 rounded-lg p-4 border border-amber-500/20">
                  <div className="text-sm font-medium text-amber-400 mb-3 flex items-center gap-2">
                    <span>🔬</span> 単一検証ポイント（PoCで絶対に検証すべき1点）
                  </div>
                  <div className="space-y-3">
                    <div>
                      <div className="text-xs text-slate-500">検証対象</div>
                      <div className="text-sm mt-1 font-medium">
                        {safeShu.single_validation_point.validation_target}
                      </div>
                    </div>
                    <div>
                      <div className="text-xs text-slate-500">成功基準</div>
                      <div className="text-sm mt-1">
                        {safeShu.single_validation_point.success_criteria}
                      </div>
                    </div>
                    <div className="bg-amber-500/10 rounded p-2">
                      <div className="text-xs text-amber-400">失敗時行動</div>
                      <div className="text-sm mt-1">
                        {safeShu.single_validation_point.failure_action}
                      </div>
                    </div>
                  </div>
                </div>
              )}

              {/* v3.0: 撤退基準 */}
              {safeShu.exit_criteria && (
                <div className="bg-red-500/5 rounded-lg p-4 border border-red-500/20">
                  <div className="text-sm font-medium text-red-400 mb-3 flex items-center gap-2">
                    <span>🚪</span> 撤退基準（どこで止めるか）
                  </div>
                  <div className="space-y-3">
                    <div>
                      <div className="text-xs text-slate-500">
                        チェックポイント
                      </div>
                      <div className="text-sm mt-1">
                        {safeShu.exit_criteria.checkpoint}
                      </div>
                    </div>
                    <div>
                      <div className="text-xs text-slate-500">撤退トリガー</div>
                      <div className="text-sm mt-1 text-red-400">
                        {safeShu.exit_criteria.exit_trigger}
                      </div>
                    </div>
                    <div>
                      <div className="text-xs text-slate-500">撤退時行動</div>
                      <div className="text-sm mt-1">
                        {safeShu.exit_criteria.exit_action}
                      </div>
                    </div>
                  </div>
                </div>
              )}

              {/* タイムライン */}
              {safeShu.phases && safeShu.phases.length > 0 && (
                <div>
                  <div className="text-sm font-medium text-slate-400 mb-3 flex items-center gap-2">
                    <span>📅</span> フェーズ
                  </div>
                  <PhaseTimeline phases={safeShu.phases} />
                </div>
              )}

              {safeShu.dependencies && safeShu.dependencies.length > 0 && (
                <div className="bg-[#0a0a0f] rounded-lg p-4">
                  <div className="text-xs text-slate-500 mb-2">前提条件</div>
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
                    <span>⏱️</span> 30天行動節奏
                  </div>
                  {safeShu.rhythm_control.focus && (
                    <div className="space-y-3">
                      <div className="bg-blue-500/10 rounded p-3">
                        <div className="text-xs text-blue-400">聚焦</div>
                        <div className="text-lg font-medium mt-1">
                          {safeShu.rhythm_control.focus.name}
                        </div>
                        <div className="text-sm text-slate-400 mt-1">
                          {safeShu.rhythm_control.focus.description}
                        </div>
                        <div className="text-xs text-emerald-400 mt-2">
                          成功指標:{" "}
                          {safeShu.rhythm_control.focus.success_metric}
                        </div>
                      </div>
                      {safeShu.rhythm_control.focus.avoid_list &&
                        safeShu.rhythm_control.focus.avoid_list.length > 0 && (
                          <div>
                            <div className="text-xs text-slate-500 mb-2">
                              この期間やらないこと
                            </div>
                            {safeShu.rhythm_control.focus.avoid_list.map(
                              (avoid: string, i: number) => (
                                <div
                                  key={i}
                                  className="text-sm text-red-400 flex items-center gap-2"
                                >
                                  <span>❌</span> {avoid}
                                </div>
                              ),
                            )}
                          </div>
                        )}
                      <div className="grid grid-cols-2 gap-4 mt-3">
                        <div>
                          <div className="text-xs text-slate-500">
                            チェックポイント
                          </div>
                          <div className="text-sm mt-1">
                            {safeShu.rhythm_control.checkpoint_date}
                          </div>
                        </div>
                        <div>
                          <div className="text-xs text-slate-500">次の判断</div>
                          <div className="text-sm mt-1">
                            {safeShu.rhythm_control.next_decision_point}
                          </div>
                        </div>
                      </div>
                    </div>
                  )}
                </div>
              )}
            </div>
          )}

          {activeTab === "qi" && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-emerald-500/10 flex items-center justify-center">
                  🔧
                </span>
                器 / 技術実装 v3.1
              </h3>

              {/* v3.1: PoC最小アーキテクチャ */}
              {safeQi.poc_minimal_architecture && (
                <div className="bg-emerald-500/5 rounded-lg p-5 border border-emerald-500/20">
                  <div className="text-sm font-medium text-emerald-400 mb-4 flex items-center gap-2">
                    <span>🏗️</span> PoC最小アーキテクチャ
                  </div>
                  <div className="space-y-4">
                    <div className="overflow-x-auto">
                      <table className="w-full text-sm">
                        <thead>
                          <tr className="border-b border-slate-700">
                            <th className="text-left py-2 text-slate-500">
                              コンポーネント
                            </th>
                            <th className="text-left py-2 text-slate-500">
                              目的
                            </th>
                            <th className="text-left py-2 text-slate-500">
                              技術選定
                            </th>
                            <th className="text-left py-2 text-slate-500">
                              備考
                            </th>
                          </tr>
                        </thead>
                        <tbody>
                          {safeQi.poc_minimal_architecture.components?.map(
                            (c: any, i: number) => (
                              <tr key={i} className="border-b border-slate-800">
                                <td className="py-2 text-emerald-400 font-medium">
                                  {c.name}
                                </td>
                                <td className="py-2 text-slate-300">
                                  {c.purpose}
                                </td>
                                <td className="py-2 text-blue-400">
                                  {c.technology_choice}
                                </td>
                                <td className="py-2 text-slate-500">
                                  {c.notes}
                                </td>
                              </tr>
                            ),
                          )}
                        </tbody>
                      </table>
                    </div>
                    {safeQi.poc_minimal_architecture.data_flow_description && (
                      <div className="bg-[#0a0a0f] rounded p-3">
                        <div className="text-xs text-slate-500 mb-1">
                          データフロー
                        </div>
                        <div className="text-sm text-slate-300 font-mono">
                          {
                            safeQi.poc_minimal_architecture
                              .data_flow_description
                          }
                        </div>
                      </div>
                    )}
                    {safeQi.poc_minimal_architecture.minimal_logging && (
                      <div className="bg-blue-500/5 rounded p-3 border border-blue-500/10">
                        <div className="text-xs text-blue-400 mb-2">
                          最小ログ設定
                        </div>
                        <div className="text-sm text-slate-400">
                          ID戦略:{" "}
                          {
                            safeQi.poc_minimal_architecture.minimal_logging
                              .correlation_id_strategy
                          }
                        </div>
                        {(safeQi.poc_minimal_architecture.minimal_logging
                          .timestamp_points?.length ?? 0) > 0 && (
                          <div className="text-sm text-slate-500 mt-1">
                            計測点:{" "}
                            {(
                              safeQi.poc_minimal_architecture.minimal_logging
                                .timestamp_points ?? []
                            ).join(" → ")}
                          </div>
                        )}
                        {safeQi.poc_minimal_architecture.minimal_logging
                          .storage && (
                          <div className="text-sm text-slate-500 mt-1">
                            保存先:{" "}
                            {
                              safeQi.poc_minimal_architecture.minimal_logging
                                .storage
                            }
                          </div>
                        )}
                      </div>
                    )}
                    {(safeQi.poc_minimal_architecture.deferred_components
                      ?.length ?? 0) > 0 && (
                      <div>
                        <div className="text-xs text-slate-500 mb-2">
                          後回しにするコンポーネント
                        </div>
                        <div className="flex flex-wrap gap-2">
                          {(
                            safeQi.poc_minimal_architecture
                              .deferred_components ?? []
                          ).map((d: string, i: number) => (
                            <span
                              key={i}
                              className="text-xs px-2 py-1 bg-slate-700/50 text-slate-400 rounded"
                            >
                              ⏳ {d}
                            </span>
                          ))}
                        </div>
                      </div>
                    )}
                  </div>
                </div>
              )}

              {/* v3.1: 拡張アーキテクチャ段階 */}
              {safeQi.expansion_stages &&
                safeQi.expansion_stages.length > 0 && (
                  <div className="bg-purple-500/5 rounded-lg p-5 border border-purple-500/20">
                    <div className="text-sm font-medium text-purple-400 mb-4 flex items-center gap-2">
                      <span>📈</span> 拡張アーキテクチャ（導入条件付き）
                    </div>
                    <div className="space-y-3">
                      {safeQi.expansion_stages.map((s: any, i: number) => (
                        <div key={i} className="bg-purple-500/10 rounded p-4">
                          <div className="font-medium text-purple-400 mb-1">
                            {s.stage_name}
                          </div>
                          <div className="text-xs text-amber-400 mb-1">
                            導入条件: {s.introduction_condition}
                          </div>
                          <div className="text-xs text-slate-400 mb-1">
                            追加: {s.added_components?.join(", ")}
                          </div>
                          <div className="text-xs text-slate-500">
                            理由: {s.rationale}
                          </div>
                        </div>
                      ))}
                    </div>
                  </div>
                )}

              {/* v3.1: 実装手順 */}
              {safeQi.implementation_steps &&
                safeQi.implementation_steps.length > 0 && (
                  <div className="bg-blue-500/5 rounded-lg p-5 border border-blue-500/20">
                    <div className="text-sm font-medium text-blue-400 mb-4 flex items-center gap-2">
                      <span>📝</span> 実装手順（Step1〜StepN）
                    </div>
                    <div className="space-y-3">
                      {safeQi.implementation_steps.map(
                        (step: any, i: number) => (
                          <div key={i} className="bg-[#0a0a0f] rounded p-4">
                            <div className="flex items-center gap-2 mb-2">
                              <span className="text-blue-400 font-bold">
                                Step {step.step_number}
                              </span>
                              <span className="font-medium text-sm">
                                {step.objective}
                              </span>
                            </div>
                            <div className="text-xs text-slate-400 mb-1">
                              作業: {step.tasks?.join(", ")}
                            </div>
                            {step.notes?.length > 0 && (
                              <div className="text-xs text-emerald-400 mt-1">
                                📌 {step.notes.join(", ")}
                              </div>
                            )}
                            {step.common_pitfalls?.length > 0 && (
                              <div className="mt-1 space-y-1">
                                {step.common_pitfalls.map(
                                  (p: string, pi: number) => (
                                    <div
                                      key={pi}
                                      className="text-xs text-amber-400"
                                    >
                                      ⚠️ {p}
                                    </div>
                                  ),
                                )}
                              </div>
                            )}
                          </div>
                        ),
                      )}
                    </div>
                  </div>
                )}

              {/* v3.1: 将来スケール要件 */}
              {safeQi.future_scale_requirements &&
                safeQi.future_scale_requirements.length > 0 && (
                  <div className="bg-slate-500/5 rounded-lg p-4 border border-slate-500/20">
                    <div className="text-xs text-slate-500 mb-2">
                      🔮 将来スケール要件（PoC範囲外）
                    </div>
                    <div className="flex flex-wrap gap-2">
                      {safeQi.future_scale_requirements.map(
                        (r: string, i: number) => (
                          <span
                            key={i}
                            className="text-xs px-2 py-1 bg-slate-700/50 text-slate-400 rounded"
                          >
                            {r}
                          </span>
                        ),
                      )}
                    </div>
                  </div>
                )}

              {/* v3.0: ドメイン固有技術 */}
              {safeQi.domain_technologies &&
                safeQi.domain_technologies.length > 0 && (
                  <div className="bg-emerald-500/5 rounded-lg p-5 border border-emerald-500/20">
                    <div className="text-sm font-medium text-emerald-400 mb-4 flex items-center gap-2">
                      <span>🛠️</span> ドメイン固有技術（具体名詞）
                    </div>
                    <div className="space-y-3">
                      {safeQi.domain_technologies.map(
                        (tech: any, i: number) => (
                          <div
                            key={i}
                            className="bg-emerald-500/10 rounded p-4"
                          >
                            <div className="flex items-center gap-2 mb-2">
                              <span className="font-medium text-emerald-400">
                                {tech.technology_name}
                              </span>
                              <span className="text-xs px-2 py-0.5 bg-slate-700 rounded">
                                {tech.category}
                              </span>
                            </div>
                            <div className="text-sm text-slate-400">
                              {tech.why_required}
                            </div>
                            {tech.alternatives &&
                              tech.alternatives.length > 0 && (
                                <div className="text-xs text-slate-500 mt-2">
                                  代替: {tech.alternatives.join(", ")}
                                </div>
                              )}
                          </div>
                        ),
                      )}
                    </div>
                  </div>
                )}

              {/* v3.0: 規制対応 */}
              {safeQi.regulatory_considerations &&
                safeQi.regulatory_considerations.length > 0 && (
                  <div className="bg-amber-500/5 rounded-lg p-5 border border-amber-500/20">
                    <div className="text-sm font-medium text-amber-400 mb-4 flex items-center gap-2">
                      <span>📜</span> 規制対応事項
                    </div>
                    <div className="overflow-x-auto">
                      <table className="w-full text-sm">
                        <thead>
                          <tr className="border-b border-slate-700">
                            <th className="text-left py-2 text-slate-500">
                              地域
                            </th>
                            <th className="text-left py-2 text-slate-500">
                              規制
                            </th>
                            <th className="text-left py-2 text-slate-500">
                              要件
                            </th>
                            <th className="text-left py-2 text-slate-500">
                              実装影響
                            </th>
                          </tr>
                        </thead>
                        <tbody>
                          {safeQi.regulatory_considerations.map(
                            (reg: any, i: number) => (
                              <tr key={i} className="border-b border-slate-800">
                                <td className="py-2 text-amber-400">
                                  {reg.region}
                                </td>
                                <td className="py-2">{reg.regulation}</td>
                                <td className="py-2 text-slate-400">
                                  {reg.requirement}
                                </td>
                                <td className="py-2 text-slate-400">
                                  {reg.implementation_impact}
                                </td>
                              </tr>
                            ),
                          )}
                        </tbody>
                      </table>
                    </div>
                  </div>
                )}

              {/* v3.0: 地理的考慮 */}
              {safeQi.geographic_considerations &&
                safeQi.geographic_considerations.length > 0 && (
                  <div className="bg-[#0a0a0f] rounded-lg p-4 border border-blue-500/20">
                    <div className="text-sm font-medium text-blue-400 mb-4 flex items-center gap-2">
                      <span>🌍</span> 地理的考慮事項
                    </div>
                    <div className="space-y-3">
                      {safeQi.geographic_considerations.map(
                        (geo: any, i: number) => (
                          <div
                            key={i}
                            className="flex items-start gap-4 p-3 bg-blue-500/5 rounded"
                          >
                            <div className="text-blue-400 font-medium">
                              {geo.region}
                            </div>
                            <div className="flex-1">
                              <div className="text-sm text-slate-400">
                                レイテンシ: {geo.latency_requirement}
                              </div>
                              <div className="text-sm text-slate-500">
                                インフラ: {geo.infrastructure_need}
                              </div>
                            </div>
                          </div>
                        ),
                      )}
                    </div>
                  </div>
                )}

              {/* 実装要素 */}
              {safeQi.implementations && safeQi.implementations.length > 0 && (
                <div>
                  <div className="text-sm font-medium text-slate-400 mb-3 flex items-center gap-2">
                    <span>🔧</span> 実装要素
                  </div>
                  {safeQi.implementations.map(
                    (impl: Implementation, i: number) => (
                      <div key={i} className="bg-[#0a0a0f] rounded-lg p-4 mb-3">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-medium">{impl.component}</span>
                          <span className="text-xs text-slate-500">
                            {impl.estimated_effort}
                          </span>
                        </div>
                        <div className="text-sm text-indigo-400 mb-2">
                          {impl.technology}
                        </div>
                        {impl.risks && impl.risks.length > 0 && (
                          <div className="text-xs text-amber-400">
                            ⚠️ {impl.risks.join(", ")}
                          </div>
                        )}
                      </div>
                    ),
                  )}
                </div>
              )}

              {safeQi.tool_recommendations &&
                safeQi.tool_recommendations.length > 0 && (
                  <div className="bg-[#0a0a0f] rounded-lg p-4">
                    <div className="text-xs text-slate-500 mb-2">
                      🧰 ツール推奨
                    </div>
                    <div className="flex flex-wrap gap-2">
                      {safeQi.tool_recommendations.map(
                        (t: string, i: number) => (
                          <span
                            key={i}
                            className="px-2 py-1 bg-indigo-500/10 text-indigo-400 rounded text-xs"
                          >
                            {t}
                          </span>
                        ),
                      )}
                    </div>
                  </div>
                )}

              {safeQi.integration_points &&
                safeQi.integration_points.length > 0 && (
                  <div className="bg-[#0a0a0f] rounded-lg p-4">
                    <div className="text-xs text-slate-500 mb-2">
                      🔗 統合ポイント
                    </div>
                    <ul className="text-sm text-slate-400 space-y-1">
                      {safeQi.integration_points.map((p: string, i: number) => (
                        <li key={i}>• {p}</li>
                      ))}
                    </ul>
                  </div>
                )}

              {safeQi.technical_debt_warnings &&
                safeQi.technical_debt_warnings.length > 0 && (
                  <div className="bg-amber-500/5 rounded-lg p-4 border border-amber-500/20">
                    <div className="text-xs text-amber-400 mb-2">
                      ⚠️ 技術負債警告
                    </div>
                    <ul className="text-sm text-slate-400 space-y-1">
                      {safeQi.technical_debt_warnings.map(
                        (w: string, i: number) => (
                          <li key={i}>• {w}</li>
                        ),
                      )}
                    </ul>
                  </div>
                )}
            </div>
          )}

          {activeTab === "review" && (
            <div className="space-y-6">
              <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
                <span className="w-8 h-8 rounded-lg bg-amber-500/10 flex items-center justify-center">
                  🔍
                </span>
                検証 / ReviewAgent
              </h3>

              {safeReview ? (
                <>
                  {/* 判定結果バナー（詳細注釈付き） */}
                  <div
                    className={`rounded-xl p-5 border-2 ${
                      safeReview.overall_verdict === "PASS"
                        ? "bg-emerald-500/5 border-emerald-500/30"
                        : safeReview.overall_verdict === "REVISE"
                          ? "bg-amber-500/5 border-amber-500/30"
                          : "bg-red-500/5 border-red-500/30"
                    }`}
                  >
                    <div className="flex items-center justify-between">
                      <div className="flex items-center gap-3">
                        <span className="text-2xl">
                          {safeReview.overall_verdict === "PASS"
                            ? "✅"
                            : safeReview.overall_verdict === "REVISE"
                              ? "⚠️"
                              : "❌"}
                        </span>
                        <div>
                          <div className="text-sm text-slate-400 mb-1">
                            総合判定
                          </div>
                          <span
                            className={`text-xl font-bold ${
                              safeReview.overall_verdict === "PASS"
                                ? "text-emerald-400"
                                : safeReview.overall_verdict === "REVISE"
                                  ? "text-amber-400"
                                  : "text-red-400"
                            }`}
                          >
                            {safeReview.overall_verdict || "処理中..."}
                          </span>
                        </div>
                      </div>
                      {/* v3.2: 2指標表示 */}
                      <div className="flex items-start gap-4">
                        <div className="text-right">
                          <div className="text-xs text-slate-400 mb-1">
                            分析信頼度（AI完成度）
                          </div>
                          <div
                            className={`text-xl font-bold ${
                              effectiveConfidencePct >= 70
                                ? "text-emerald-400"
                                : effectiveConfidencePct >= signThresholdPct
                                  ? "text-amber-400"
                                  : "text-red-400"
                            }`}
                          >
                            {effectiveConfidencePct}%
                          </div>
                          {recalculatedConfidence !== null && (
                            <div className="text-xs text-slate-500 mt-0.5">
                              チェック反映済
                            </div>
                          )}
                        </div>
                        <div className="text-right border-l border-white/10 pl-4">
                          <div className="text-xs text-slate-400 mb-1">
                            戦略可行度（成功確率）
                          </div>
                          <div
                            className={`text-xl font-bold ${
                              effectiveFeasibilityPct === null
                                ? "text-slate-500"
                                : effectiveFeasibilityPct >= 70
                                  ? "text-emerald-400"
                                  : effectiveFeasibilityPct >= 50
                                    ? "text-amber-400"
                                    : "text-red-400"
                            }`}
                          >
                            {effectiveFeasibilityPct !== null
                              ? `${effectiveFeasibilityPct}%`
                              : "—"}
                          </div>
                          {effectiveFeasibilityPct === null && (
                            <div className="text-xs text-slate-600 mt-0.5">
                              パス未生成
                            </div>
                          )}
                          {recalculatedFeasibility !== null && (
                            <div className="text-xs text-slate-500 mt-0.5">
                              チェック反映済
                            </div>
                          )}
                        </div>
                      </div>
                    </div>

                    {/* 判定結果の意味を説明 */}
                    <div className="mt-4 pt-4 border-t border-white/10">
                      <div className="text-sm text-slate-400">
                        {safeReview.overall_verdict === "PASS" && (
                          <>
                            <span className="text-emerald-400">
                              ✓ 承認可能：
                            </span>
                            この提案書は署名可能です。以下の改善提案を確認しつつ、意思決定を進めることができます。
                          </>
                        )}
                        {safeReview.overall_verdict === "REVISE" && (
                          <>
                            <span className="text-amber-400">
                              ⚠ 差分パッチで補完可能：
                            </span>
                            以下のチェックボックスで不足項目を補完すると、スコアが自動再計算されます。
                          </>
                        )}
                        {safeReview.overall_verdict === "COACH" && (
                          <>
                            <span className="text-blue-400">📋 改善指導：</span>
                            優先改善点があります。提案を確認し、補強内容を反映して再計算してください。
                          </>
                        )}
                      </div>
                    </div>
                  </div>

                  {/* v3.1 差分パッチ型: 指摘事項（最大3件） */}
                  {safeReview.findings && safeReview.findings.length > 0 && (
                    <div className="space-y-4">
                      <div className="flex items-center gap-2 text-sm font-medium text-slate-300">
                        <span>
                          {safeReview.overall_verdict === "PASS" ? "💡" : "🎯"}
                        </span>
                        {safeReview.overall_verdict === "PASS"
                          ? `改善提案 (${safeReview.findings.length}件)`
                          : `優先改善点 (${safeReview.findings.length}件、最大3件)`}
                      </div>
                      {missingCountermeasureCount > 0 && (
                        <div className="rounded-lg border border-amber-500/30 bg-amber-500/10 px-3 py-2 text-xs text-amber-300">
                          ⚠️ 提案文が不足している改善点が{" "}
                          {missingCountermeasureCount}{" "}
                          件あります。メモ欄で補足してください。
                        </div>
                      )}
                      <div className="space-y-4">
                        {safeReview.findings.map((finding, i) => (
                          <div
                            key={i}
                            className={`rounded-lg p-4 border ${
                              finding.severity === "CRITICAL"
                                ? "bg-red-500/5 border-red-500/20"
                                : "bg-amber-500/5 border-amber-500/20"
                            }`}
                          >
                            {/* ヘッダー: 優先度 + カテゴリ + アクションタイプ */}
                            <div className="flex items-center gap-2 mb-3">
                              <span
                                className={`text-xs px-2 py-0.5 rounded font-medium ${
                                  finding.severity === "CRITICAL"
                                    ? "bg-red-500/20 text-red-400"
                                    : "bg-amber-500/20 text-amber-400"
                                }`}
                              >
                                {finding.severity === "CRITICAL"
                                  ? "優先改善"
                                  : "改善提案"}
                              </span>
                              {finding.action_type && (
                                <span
                                  className={`text-xs px-2 py-0.5 rounded font-medium ${
                                    finding.action_type === "PATCH"
                                      ? "bg-emerald-500/20 text-emerald-400"
                                      : finding.action_type === "RECALC"
                                        ? "bg-blue-500/20 text-blue-400"
                                        : "bg-red-500/20 text-red-400"
                                  }`}
                                >
                                  {finding.action_type}
                                </span>
                              )}
                              {finding.affected_agent && (
                                <span className="text-xs text-slate-500">
                                  → {finding.affected_agent}
                                </span>
                              )}
                            </div>

                            {/* 所見本文 */}
                            <div className="mb-3 text-sm text-slate-300">
                              {finding.description}
                            </div>

                            {/* 注意点 */}
                            {finding.failure_point && (
                              <div className="mb-2">
                                <span className="text-xs text-red-400 font-medium">
                                  注意点:{" "}
                                </span>
                                <span className="text-sm text-slate-300">
                                  {finding.failure_point}
                                </span>
                              </div>
                            )}

                            {/* 影響範囲 */}
                            {finding.impact_scope && (
                              <div className="mb-3">
                                <span className="text-xs text-amber-400 font-medium">
                                  影響範囲:{" "}
                                </span>
                                <span className="text-sm text-slate-400">
                                  {finding.impact_scope}
                                </span>
                              </div>
                            )}

                            {/* 対策案 */}
                            <div className="mb-3 p-3 bg-emerald-500/5 rounded-lg border border-emerald-500/20">
                              <div className="text-xs text-emerald-400 font-medium mb-1">
                                対策案
                              </div>
                              <div className="text-sm text-slate-300">
                                {(finding.suggested_revision || "").trim() ||
                                  "（対策案が未生成です。必要に応じて再分析してください）"}
                              </div>
                            </div>

                            {/* 任意チェック＋メモ（最小パッチ有無に関係なく表示） */}
                            <div className="mt-3 p-3 bg-slate-800/50 rounded-lg border border-indigo-500/20">
                              <div className="text-xs text-indigo-400 mb-2 font-medium">
                                ユーザー確認メモ（任意）
                              </div>
                              <label className="flex items-center gap-2 text-sm text-slate-300">
                                <input
                                  type="checkbox"
                                  checked={Boolean(humanReviewChecks[i])}
                                  onChange={(e) =>
                                    setHumanReviewChecks((prev) => ({
                                      ...prev,
                                      [i]: e.target.checked,
                                    }))
                                  }
                                  className="rounded border-slate-500 bg-transparent"
                                />
                                {finding.minimal_patch?.checkbox_label ||
                                  "この対策案を確認した"}
                                {(() => {
                                  const badge = metricTargetBadge(
                                    inferFindingMetricTarget(finding),
                                  );
                                  return (
                                    <span
                                      className={`inline-flex items-center rounded border px-1.5 py-0.5 text-[10px] ${badge.className}`}
                                    >
                                      {badge.label}
                                    </span>
                                  );
                                })()}
                              </label>
                              <textarea
                                value={
                                  humanReviewNotes[i] ??
                                  finding.minimal_patch?.default_value ??
                                  ""
                                }
                                onChange={(e) =>
                                  setHumanReviewNotes((prev) => ({
                                    ...prev,
                                    [i]: e.target.value,
                                  }))
                                }
                                placeholder={
                                  finding.minimal_patch?.annotation_hint ||
                                  "メモ（任意）"
                                }
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
                                  {savingFindingNotes[i]
                                    ? "保存中..."
                                    : "メモ保存"}
                                </button>
                              </div>
                            </div>

                            {/* スコア改善見込み */}
                            {finding.score_improvements &&
                              finding.score_improvements.length > 0 && (
                                <div className="mt-3">
                                  <div className="text-xs text-slate-500 mb-1">
                                    パッチ適用後のスコア改善見込み:
                                  </div>
                                  {finding.score_improvements.map(
                                    (si, si_idx) => (
                                      <div
                                        key={si_idx}
                                        className="flex items-center gap-2 text-xs text-slate-400"
                                      >
                                        <span>{si.target_score}:</span>
                                        <span className="text-slate-500">
                                          {si.current_estimate}%
                                        </span>
                                        <span>→</span>
                                        <span className="text-emerald-400">
                                          {si.improved_estimate}%
                                        </span>
                                        <span className="text-emerald-400 font-medium">
                                          (+{si.delta}点)
                                        </span>
                                      </div>
                                    ),
                                  )}
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
                        <span>📊</span> 信頼度分解
                      </div>
                      <div className="grid grid-cols-2 gap-3">
                        {(
                          [
                            "input_sufficiency",
                            "logic_consistency",
                            "implementation_feasibility",
                            "risk_coverage",
                          ] as const
                        ).map((key) => {
                          const comp = safeReview.confidence_breakdown?.[key];
                          if (!comp) return null;
                          const hasChecks =
                            safeReview.checkpoint_items?.some(
                              (ci) =>
                                checkpointChecks[ci.item_id] ?? ci.checked,
                            ) ?? false;
                          const displayScore = hasChecks
                            ? Math.min(100, comp.score + comp.checkbox_boost)
                            : comp.score;
                          return (
                            <div
                              key={key}
                              className="bg-[#0a0a0f] rounded-lg p-3 border border-white/5"
                            >
                              <div className="text-xs text-slate-500 mb-1">
                                {comp.name}
                              </div>
                              <div className="flex items-baseline gap-2">
                                <span
                                  className={`text-lg font-bold ${
                                    displayScore >= 70
                                      ? "text-emerald-400"
                                      : displayScore >= 40
                                        ? "text-amber-400"
                                        : "text-red-400"
                                  }`}
                                >
                                  {Math.round(displayScore)}%
                                </span>
                                {comp.checkbox_boost > 0 && (
                                  <span className="text-xs text-emerald-400/70">
                                    ✓で+{comp.checkbox_boost}点
                                  </span>
                                )}
                              </div>
                              {comp.description && (
                                <div className="text-xs text-slate-600 mt-1">
                                  {comp.description}
                                </div>
                              )}
                            </div>
                          );
                        })}
                      </div>
                    </div>
                  )}

                  {/* v3.1 チェックポイント項目 */}
                  {safeReview.checkpoint_items &&
                    safeReview.checkpoint_items.length > 0 && (
                      <div className="bg-indigo-500/5 rounded-lg p-4 border border-indigo-500/20">
                        <div className="text-sm font-medium text-indigo-400 mb-3 flex items-center gap-2">
                          <span>☑️</span> 確認チェックポイント
                        </div>
                        <div className="space-y-3">
                          {safeReview.checkpoint_items.map((item) => (
                            <div
                              key={item.item_id}
                              className="flex items-start gap-3 p-3 bg-[#0a0a0f] rounded-lg border border-white/5"
                            >
                              <input
                                type="checkbox"
                                checked={
                                  checkpointChecks[item.item_id] ?? item.checked
                                }
                                onChange={(e) =>
                                  setCheckpointChecks((prev) => ({
                                    ...prev,
                                    [item.item_id]: e.target.checked,
                                  }))
                                }
                                className="mt-0.5 rounded border-slate-500 bg-transparent"
                              />
                              <div className="flex-1 min-w-0">
                                <div className="flex items-center gap-2">
                                  <span className="text-sm text-slate-300">
                                    {item.label}
                                  </span>
                                  <span className="text-xs text-emerald-400/70">
                                    +{item.score_boost}点
                                  </span>
                                  {(() => {
                                    const badge = metricTargetBadge(
                                      inferCheckpointMetricTarget(item),
                                    );
                                    return (
                                      <span
                                        className={`inline-flex items-center rounded border px-1.5 py-0.5 text-[10px] ${badge.className}`}
                                      >
                                        {badge.label}
                                      </span>
                                    );
                                  })()}
                                </div>
                                {item.default_suggestion && (
                                  <div className="text-xs text-slate-500 mt-1">
                                    暫定案: {item.default_suggestion}
                                  </div>
                                )}
                                <input
                                  type="text"
                                  value={
                                    checkpointAnnotations[item.item_id] ??
                                    item.annotation ??
                                    ""
                                  }
                                  onChange={(e) =>
                                    setCheckpointAnnotations((prev) => ({
                                      ...prev,
                                      [item.item_id]: e.target.value,
                                    }))
                                  }
                                  placeholder="注釈（任意）"
                                  className="mt-2 w-full px-2 py-1 rounded bg-slate-800/50 border border-white/5 text-xs text-slate-300 placeholder:text-slate-600 focus:outline-none focus:border-indigo-500/30"
                                />
                              </div>
                            </div>
                          ))}
                        </div>

                        {/* 自動再計算ボタン */}
                        {safeReview.auto_recalc_enabled !== false && (
                          <button
                            type="button"
                            onClick={handleApplyCheckpoints}
                            disabled={!hasAnyRecalcSelection}
                            className={`mt-4 w-full px-4 py-3 rounded-lg text-sm font-medium transition-all flex items-center justify-center gap-2 ${
                              hasAnyRecalcSelection
                                ? "bg-emerald-500/10 hover:bg-emerald-500/20 text-emerald-400 border border-emerald-500/20"
                                : "bg-slate-800/50 text-slate-500 cursor-not-allowed border border-white/5"
                            }`}
                          >
                            ⚡ 重新算分（信頼度 + 戦略可行度）
                          </button>
                        )}
                      </div>
                    )}

                  {safeReview.auto_recalc_enabled !== false &&
                    (!safeReview.checkpoint_items ||
                      safeReview.checkpoint_items.length === 0) && (
                      <div className="bg-indigo-500/5 rounded-lg p-4 border border-indigo-500/20">
                        <div className="text-sm font-medium text-indigo-400 mb-3 flex items-center gap-2">
                          <span>⚡</span> 改善反映スコア再計算
                        </div>
                        <button
                          type="button"
                          onClick={handleApplyCheckpoints}
                          disabled={!hasAnyRecalcSelection}
                          className={`w-full px-4 py-3 rounded-lg text-sm font-medium transition-all flex items-center justify-center gap-2 ${
                            hasAnyRecalcSelection
                              ? "bg-emerald-500/10 hover:bg-emerald-500/20 text-emerald-400 border border-emerald-500/20"
                              : "bg-slate-800/50 text-slate-500 cursor-not-allowed border border-white/5"
                          }`}
                        >
                          重新算分（信頼度 + 戦略可行度）
                        </button>
                      </div>
                    )}

                  {scoreBreakdown && (
                    <div className="rounded-lg border border-emerald-500/20 bg-emerald-500/5 p-4">
                      <div className="text-sm font-medium text-emerald-300 mb-3">
                        再計算内訳
                      </div>
                      <div className="grid grid-cols-1 md:grid-cols-2 gap-3 text-xs text-slate-300">
                        <div className="rounded border border-emerald-500/20 bg-black/20 p-3 space-y-1">
                          <div className="font-medium text-emerald-300">
                            分析信頼度
                          </div>
                          <div>基礎点: {scoreBreakdown.confidence.base}%</div>
                          <div>
                            checkpoint加点: +
                            {scoreBreakdown.confidence.checkpoint}%
                          </div>
                          <div>
                            finding加点: +{scoreBreakdown.confidence.finding}%
                          </div>
                          <div>LLM加点: +{scoreBreakdown.confidence.llm}%</div>
                          <div className="font-semibold text-white">
                            最終点: {scoreBreakdown.confidence.final}%
                          </div>
                          <div
                            className={
                              scoreBreakdown.confidence.signatureEligible
                                ? "text-emerald-300"
                                : "text-amber-300"
                            }
                          >
                            署名可否:{" "}
                            {scoreBreakdown.confidence.signatureEligible
                              ? `可（${signThresholdPct}%以上）`
                              : `不可（${signThresholdPct}%未満）`}
                          </div>
                        </div>
                        <div className="rounded border border-cyan-500/20 bg-black/20 p-3 space-y-1">
                          <div className="font-medium text-cyan-300">
                            戦略可行度
                          </div>
                          <div>基礎点: {scoreBreakdown.feasibility.base}%</div>
                          <div>
                            checkpoint加点: +
                            {scoreBreakdown.feasibility.checkpoint}%
                          </div>
                          <div>
                            finding加点: +{scoreBreakdown.feasibility.finding}%
                          </div>
                          <div>LLM加点: +{scoreBreakdown.feasibility.llm}%</div>
                          <div className="font-semibold text-white">
                            最終点: {scoreBreakdown.feasibility.final}%
                          </div>
                        </div>
                      </div>
                      {scoreBreakdown.bonusReasons.length > 0 && (
                        <div className="mt-3 text-xs text-slate-400 space-y-1">
                          {scoreBreakdown.bonusReasons.map((reason, idx) => (
                            <div key={`bonus-reason-${idx}`}>• {reason}</div>
                          ))}
                        </div>
                      )}
                      {scoreBreakdown.contributions.length > 0 && (
                        <div className="mt-3 space-y-2">
                          <div className="text-xs text-slate-400">
                            適用項目（影響範囲付き）
                          </div>
                          {scoreBreakdown.contributions.map((item) => {
                            const badge = metricTargetBadge(item.target_metric);
                            return (
                              <div
                                key={`${item.source}-${item.item_key}`}
                                className="rounded border border-white/10 bg-black/20 p-2 text-xs text-slate-300"
                              >
                                <div className="flex items-center gap-2 flex-wrap">
                                  <span className="font-medium text-white">
                                    {item.label}
                                  </span>
                                  <span
                                    className={`inline-flex items-center rounded border px-1.5 py-0.5 text-[10px] ${badge.className}`}
                                  >
                                    {badge.label}
                                  </span>
                                  <span className="text-emerald-300">
                                    信頼度 +{item.confidence_boost_pct}%
                                  </span>
                                  <span className="text-cyan-300">
                                    可行度 +{item.feasibility_boost_pct}%
                                  </span>
                                </div>
                                {item.note && (
                                  <div className="text-slate-500 mt-1">
                                    メモ: {item.note}
                                  </div>
                                )}
                              </div>
                            );
                          })}
                        </div>
                      )}
                    </div>
                  )}

                  {/* 指摘なし */}
                  {(!safeReview.findings ||
                    safeReview.findings.length === 0) && (
                    <div className="text-center py-6 text-slate-500 bg-[#0a0a0f] rounded-lg">
                      <span className="text-3xl mb-2 block">✨</span>
                      <p>優先改善点は検出されませんでした</p>
                    </div>
                  )}
                </>
              ) : (
                <div className="text-center py-8 text-slate-500">
                  <div className="w-8 h-8 border-2 border-slate-600 border-t-slate-400 rounded-full animate-spin mx-auto mb-3" />
                  検証結果を取得中...
                </div>
              )}
            </div>
          )}
        </div>

        {/* 署名セクション v3.2 - 判定結果に応じた表示制御 */}
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
                  <th
                    rowSpan={2}
                    className="bg-slate-800/50 px-3 py-2 text-left w-20 border-r border-slate-700"
                  >
                    作成
                  </th>
                  <th className="bg-slate-800/30 px-3 py-2 text-left w-16 border-r border-slate-700">
                    部署
                  </th>
                  <td className="px-3 py-2 border-r border-slate-700">
                    {authorDept}
                  </td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left w-16 border-r border-slate-700">
                    役職
                  </th>
                  <td className="px-3 py-2">{authorPos}</td>
                </tr>
                <tr className="border border-slate-700 border-t-0">
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">
                    氏名
                  </th>
                  <td className="px-3 py-2 border-r border-slate-700">
                    {authorName}
                  </td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">
                    日付
                  </th>
                  <td className="px-3 py-2">{createdDate}</td>
                </tr>
                {/* 承認欄 */}
                <tr className="border border-slate-700 border-t-0">
                  <th
                    rowSpan={2}
                    className="bg-slate-800/50 px-3 py-2 text-left border-r border-slate-700"
                  >
                    承認
                  </th>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">
                    部署
                  </th>
                  <td className="px-3 py-2 border-r border-slate-700 text-slate-500">
                    {signatureStatus === "signed" && signatureData
                      ? signatureData.department
                      : "（未承認）"}
                  </td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">
                    役職
                  </th>
                  <td className="px-3 py-2 text-slate-500">
                    {signatureStatus === "signed" && signatureData
                      ? signatureData.position
                      : ""}
                  </td>
                </tr>
                <tr className="border border-slate-700 border-t-0">
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">
                    氏名
                  </th>
                  <td className="px-3 py-2 border-r border-slate-700 text-slate-500">
                    {signatureStatus === "signed" && signatureData
                      ? signatureData.signed_by
                      : ""}
                  </td>
                  <th className="bg-slate-800/30 px-3 py-2 text-left border-r border-slate-700">
                    日付
                  </th>
                  <td className="px-3 py-2 text-slate-500">
                    {signatureStatus === "signed" && signatureData
                      ? signatureData.signed_at_display
                      : ""}
                  </td>
                </tr>
              </tbody>
            </table>
          </div>

          {/* 承認印エリア - 判定結果に応じた表示 */}
          <div className="flex items-center justify-center gap-8">
            {signatureStatus === "signed" && signatureData ? (
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
            ) : canSign ? (
              /* 署名可能（分析信頼度が閾値以上） */
              <div className="flex flex-col items-center gap-4">
                <div className="w-24 h-24 rounded-full border-2 border-dashed border-slate-600 flex items-center justify-center text-slate-500 text-xs">
                  承認印
                </div>
                <div className="text-center">
                  <div className="text-sm text-slate-500 mb-2">
                    この提案書に基づいて意思決定を行う場合
                  </div>
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
                        ? "bg-slate-700 text-slate-400 cursor-wait"
                        : !user
                          ? "bg-slate-800 text-slate-500 cursor-not-allowed"
                          : "bg-gradient-to-r from-red-700 to-red-600 hover:from-red-600 hover:to-red-500 shadow-lg shadow-red-500/25 text-white"
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
            ) : (
              /* 検証未通過 - 修正ガイダンス表示 */
              <div className="flex flex-col items-center gap-4 w-full max-w-md">
                <div
                  className={`w-24 h-24 rounded-full border-2 border-dashed flex items-center justify-center ${
                    safeReview.overall_verdict === "REVISE"
                      ? "border-amber-500/50 text-amber-500"
                      : "border-blue-500/50 text-blue-500"
                  }`}
                >
                  <div className="text-center">
                    <div className="text-2xl mb-1">
                      {safeReview.overall_verdict === "REVISE" ? "⚠️" : "📋"}
                    </div>
                    <div className="text-xs">
                      {safeReview.overall_verdict === "REVISE"
                        ? "要修正"
                        : "改善指導"}
                    </div>
                  </div>
                </div>
                <div className="text-center">
                  <div
                    className={`text-sm mb-3 ${
                      safeReview.overall_verdict === "REVISE"
                        ? "text-amber-400"
                        : "text-blue-400"
                    }`}
                  >
                    {safeReview.overall_verdict === "REVISE"
                      ? "⚠️ 署名前に改善項目の反映が必要です"
                      : "📋 改善提案あり — 内容を確認してください"}
                  </div>
                  <div className="text-sm text-slate-400 mb-4">
                    「検証」タブで改善提案を確認し、checkboxと補足内容を反映してください。
                  </div>
                  <div className="flex gap-3 justify-center">
                    <button
                      onClick={() => setActiveTab("review")}
                      className="px-4 py-2 bg-slate-700 hover:bg-slate-600 rounded-lg text-sm transition-all flex items-center gap-2"
                    >
                      🔍 検証結果を確認
                    </button>
                    <button
                      onClick={handleNewQuestion}
                      className="px-4 py-2 bg-indigo-500/20 hover:bg-indigo-500/30 text-indigo-400 rounded-lg text-sm transition-all flex items-center gap-2"
                    >
                      🔄 再分析
                    </button>
                  </div>
                </div>
              </div>
            )}
          </div>
        </div>

        {/* フッター */}
        <div className="mt-8 text-center text-xs text-slate-600 border-t border-slate-800 pt-4">
          <p>本提案書は AI Decision Support により自動生成されました</p>
          <p className="mt-1 font-mono">
            案件ID: {caseId} | Version: {report.version || "3.1"}
          </p>
        </div>
      </main>
    </div>
  );
};

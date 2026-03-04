/**
 * Decision 状態管理ストア.
 *
 * 目的: 画面間の状態共有・永続化
 * 技術: Zustand
 * 
 * 機能:
 *   - 入力データの永続化
 *   - 履歴管理（最大10件）
 *   - レポートの永続化
 */

import { create } from 'zustand';
import { persist, createJSONStorage } from 'zustand/middleware';
import type { DecisionRequest, DecisionReport, StakeholderInfo, DaoOutput, FaOutput, ShuOutput, QiOutput, ReviewOutput } from '../types';
import { decisionApi } from '../api/client';

/** 画面状態 */
export type PageState = 'input' | 'processing' | 'report' | 'history' | 'knowledge-shu' | 'knowledge-qi' | 'knowledge';

/** 履歴アイテム */
export interface HistoryItem {
  id: string;
  question: string;
  createdAt: string;
  reportId: string | null;
  /** resume用のリクエストID */
  requestId: string | null;
  status: 'completed' | 'failed' | 'signed';
}

/** 最大履歴数 */
const MAX_HISTORY_ITEMS = 10;

/** ストア状態 */
interface DecisionState {
  // 現在の画面
  currentPage: PageState;
  previousPage: PageState | null;

  // 入力データ
  question: string;
  constraints: {
    budget: string;
    timeline: string;
    team: string;
    technical: string[];
    regulatory: string[];
  };

  // ステークホルダー（責任者）情報
  stakeholders: StakeholderInfo;

  // 処理結果
  /** 履歴照会・PDF出力用のリクエストID（UUID） */
  requestId: string | null;
  reportId: string | null;
  report: DecisionReport | null;

  // 履歴
  history: HistoryItem[];

  // エラー状態
  error: string | null;

  // アクション
  setQuestion: (q: string) => void;
  setConstraints: (c: Partial<DecisionState['constraints']>) => void;
  setStakeholders: (s: Partial<StakeholderInfo>) => void;
  setPage: (p: PageState) => void;
  loadHistoryReport: (requestId: string) => Promise<void>;
  setRequestId: (id: string | null) => void;
  setReportId: (id: string) => void;
  setReport: (r: DecisionReport) => void;
  setError: (e: string | null) => void;
  reset: () => void;

  // 履歴アクション
  addToHistory: (item: Omit<HistoryItem, 'id' | 'createdAt'>) => void;
  updateHistoryStatus: (id: string, status: HistoryItem['status']) => void;
  clearHistory: () => void;
  loadFromHistory: (id: string) => void;

  // 便利メソッド
  buildRequest: () => DecisionRequest;
}

/** 初期制約 */
const initialConstraints = {
  budget: '',
  timeline: '',
  team: '',
  technical: [] as string[],
  regulatory: [] as string[],
};

/** 初期ステークホルダー */
const initialStakeholders: StakeholderInfo = {
  product_owner: '',
  tech_lead: '',
  business_owner: '',
  legal_reviewer: '',
};

/**
 * Decision ストア.
 */
export const useDecisionStore = create<DecisionState>()(
  persist(
    (set, get) => ({
      // 初期状態
      currentPage: 'input',
      previousPage: null,
      question: '',
      constraints: { ...initialConstraints },
      stakeholders: { ...initialStakeholders },
      reportId: null,
      requestId: null,
      report: null,
      history: [],
      error: null,

      // アクション
      setQuestion: (q) => set({ question: q }),

      setConstraints: (c) =>
        set((state) => ({
          constraints: { ...state.constraints, ...c },
        })),

      setStakeholders: (s) =>
        set((state) => ({
          stakeholders: { ...state.stakeholders, ...s },
        })),

      setPage: (p) => set((state) => ({ previousPage: state.currentPage, currentPage: p })),

      setRequestId: (id) => set({ requestId: id }),

      setReportId: (id) => set({ reportId: id }),

      setReport: (r) => set({ report: r, reportId: r.report_id }),

      setError: (e) => set({ error: e }),

      reset: () =>
        set({
          currentPage: 'input',
          question: '',
          constraints: { ...initialConstraints },
          stakeholders: { ...initialStakeholders },
          reportId: null,
          requestId: null,
          report: null,
          error: null,
        }),

      // 履歴アクション
      addToHistory: (item) =>
        set((state) => {
          const newItem: HistoryItem = {
            ...item,
            id: `history-${Date.now()}`,
            createdAt: new Date().toISOString(),
          };
          const newHistory = [newItem, ...state.history].slice(0, MAX_HISTORY_ITEMS);
          return { history: newHistory };
        }),

      updateHistoryStatus: (id, status) =>
        set((state) => ({
          history: state.history.map((item) =>
            item.id === id ? { ...item, status } : item
          ),
        })),

      clearHistory: () => set({ history: [] }),

      loadFromHistory: (id) => {
        const state = get();
        const item = state.history.find((h) => h.id === id);
        if (item) {
          set({
            question: item.question,
            requestId: item.requestId ?? null,
            currentPage: 'input',
          });
        }
      },

      // 履歴レポート復元
      loadHistoryReport: async (requestId: string) => {
        try {
          const response = await decisionApi.getHistoryDetail(requestId);
          if (response.status !== 'success' && response.status !== 'fallback') return;
          const d = response.data;

          const dao = (d.dao_result ?? { problem_type: 'UNKNOWN', essence: '', immutable_constraints: [], hidden_assumptions: [] }) as unknown as DaoOutput;
          const fa = (d.fa_result ?? { recommended_paths: [], rejected_paths: [], decision_criteria: [] }) as unknown as FaOutput;
          const shu = (d.shu_result ?? { phases: [], first_action: '', dependencies: [] }) as unknown as ShuOutput;
          const qi = (d.qi_result ?? { implementations: [], tool_recommendations: [], integration_points: [], technical_debt_warnings: [] }) as unknown as QiOutput;
          const review = (d.review_result ?? { overall_verdict: 'PASS' as const, confidence_score: d.confidence ?? 0, findings: [], final_warnings: [] }) as unknown as ReviewOutput;

          const topPath = (fa as { recommended_paths?: { name?: string; description?: string; time_to_value?: string }[] }).recommended_paths?.[0];
          const executive_summary = {
            one_line_decision: topPath?.name ?? (dao as { essence?: string }).essence ?? '',
            recommended_action: topPath?.description ?? '',
            key_risks: (review as { findings?: { severity?: string; description?: string }[] }).findings?.filter((f) => f.severity === 'CRITICAL').map((f) => f.description ?? '') ?? [],
            first_step: (shu as { first_action?: string }).first_action ?? '',
            estimated_impact: topPath?.time_to_value ?? '',
          };

          const report: DecisionReport = {
            report_id: d.report_case_id ?? d.request_id,
            created_at: d.created_at,
            version: '3.1',
            dao,
            fa,
            shu,
            qi,
            review,
            executive_summary,
            original_question: d.question,
          };

          set({
            report,
            reportId: d.report_case_id ?? null,
            requestId: d.request_id,
            question: d.question,
            previousPage: 'history',
            currentPage: 'report',
          });
        } catch (err) {
          console.error('Failed to load history report:', err);
        }
      },

      // リクエスト構築
      buildRequest: () => {
        const state = get();
        const req: DecisionRequest = {
          question: state.question,
          technical_constraints: state.constraints.technical,
          regulatory_constraints: state.constraints.regulatory,
          human_resources: state.constraints.team
            ? [state.constraints.team]
            : [],
        };

        // 数値変換
        if (state.constraints.budget) {
          const b = Number.parseFloat(state.constraints.budget);
          if (!Number.isNaN(b)) req.budget = b;
        }
        if (state.constraints.timeline) {
          const t = Number.parseInt(state.constraints.timeline, 10);
          if (!Number.isNaN(t)) req.timeline_months = t;
        }

        // ステークホルダー情報（値がある場合のみ設定）
        const sh = state.stakeholders;
        if (sh.product_owner) req.stakeholder_product_owner = sh.product_owner;
        if (sh.tech_lead) req.stakeholder_tech_lead = sh.tech_lead;
        if (sh.business_owner) req.stakeholder_business_owner = sh.business_owner;
        if (sh.legal_reviewer) req.stakeholder_legal_reviewer = sh.legal_reviewer;

        return req;
      },
    }),
    {
      name: 'decision-storage',
      storage: createJSONStorage(() => localStorage),
      partialize: (state) => ({
        question: state.question,
        constraints: state.constraints,
        stakeholders: state.stakeholders,
        history: state.history,
        // レポートも永続化（大きい場合は除外を検討）
        requestId: state.requestId,
        reportId: state.reportId,
        report: state.report,
        // previousPage は永続化しない（セッション内のみ有効）
      }),
    }
  )
);


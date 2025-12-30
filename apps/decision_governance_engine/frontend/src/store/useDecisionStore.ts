/**
 * Decision 状態管理ストア.
 *
 * 目的: 画面間の状態共有・永続化
 * 技術: Zustand
 */

import { create } from 'zustand';
import { persist } from 'zustand/middleware';
import type { DecisionRequest, DecisionReport } from '../types';

/** 画面状態 */
export type PageState = 'input' | 'processing' | 'report';

/** ストア状態 */
interface DecisionState {
  // 現在の画面
  currentPage: PageState;

  // 入力データ
  question: string;
  constraints: {
    budget: string;
    timeline: string;
    team: string;
    technical: string[];
    regulatory: string[];
  };

  // 処理結果
  reportId: string | null;
  report: DecisionReport | null;

  // エラー状態
  error: string | null;

  // アクション
  setQuestion: (q: string) => void;
  setConstraints: (c: Partial<DecisionState['constraints']>) => void;
  setPage: (p: PageState) => void;
  setReportId: (id: string) => void;
  setReport: (r: DecisionReport) => void;
  setError: (e: string | null) => void;
  reset: () => void;

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

/**
 * Decision ストア.
 */
export const useDecisionStore = create<DecisionState>()(
  persist(
    (set, get) => ({
      // 初期状態
      currentPage: 'input',
      question: '',
      constraints: { ...initialConstraints },
      reportId: null,
      report: null,
      error: null,

      // アクション
      setQuestion: (q) => set({ question: q }),

      setConstraints: (c) =>
        set((state) => ({
          constraints: { ...state.constraints, ...c },
        })),

      setPage: (p) => set({ currentPage: p }),

      setReportId: (id) => set({ reportId: id }),

      setReport: (r) => set({ report: r }),

      setError: (e) => set({ error: e }),

      reset: () =>
        set({
          currentPage: 'input',
          question: '',
          constraints: { ...initialConstraints },
          reportId: null,
          report: null,
          error: null,
        }),

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
          const b = parseFloat(state.constraints.budget);
          if (!isNaN(b)) req.budget = b;
        }
        if (state.constraints.timeline) {
          const t = parseInt(state.constraints.timeline, 10);
          if (!isNaN(t)) req.timeline_months = t;
        }

        return req;
      },
    }),
    {
      name: 'decision-storage',
      partialize: (state) => ({
        question: state.question,
        constraints: state.constraints,
      }),
    }
  )
);


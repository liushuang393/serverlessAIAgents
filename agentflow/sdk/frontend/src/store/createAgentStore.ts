/**
 * createAgentStore - Zustand Store 工厂.
 * 
 * AgentFlow アプリケーション用の Zustand Store を生成。
 * 自動的に以下を提供:
 * - 履歴管理（最大10件）
 * - LocalStorage 永続化
 * - DevTools 統合
 * 
 * @example
 * ```tsx
 * interface DecisionState {
 *   question: string;
 *   constraints: ConstraintSet;
 * }
 * 
 * const useDecisionStore = createAgentStore<DecisionState>({
 *   name: 'decision',
 *   initialState: {
 *     question: '',
 *     constraints: { budget: '', timeline: '' },
 *   },
 *   actions: (set, get) => ({
 *     setQuestion: (q: string) => set({ data: { ...get().data, question: q } }),
 *   }),
 * });
 * ```
 */

import { create } from 'zustand';
import { persist, createJSONStorage, devtools } from 'zustand/middleware';

// ========================================
// 型定義
// ========================================

/** 画面状態 */
export type PageState = 'input' | 'processing' | 'result' | string;

/** 履歴アイテム */
export interface HistoryItem {
  id: string;
  summary: string;
  createdAt: string;
  resultId: string | null;
  status: 'completed' | 'failed' | 'signed';
}

/** 基底 Store 状態 */
export interface BaseAgentState<T> {
  /** 業務データ */
  data: T;
  
  /** 現在の画面 */
  currentPage: PageState;
  
  /** ローディング状態 */
  isLoading: boolean;
  
  /** エラー */
  error: string | null;
  
  /** 履歴 */
  history: HistoryItem[];
  
  /** 結果ID */
  resultId: string | null;
  
  /** 結果データ */
  result: unknown | null;
}

/** 基底 Store アクション */
export interface BaseAgentActions<T> {
  /** 業務データ設定 */
  setData: (data: Partial<T>) => void;
  
  /** 画面遷移 */
  setPage: (page: PageState) => void;
  
  /** ローディング設定 */
  setLoading: (loading: boolean) => void;
  
  /** エラー設定 */
  setError: (error: string | null) => void;
  
  /** 結果設定 */
  setResult: (resultId: string, result: unknown) => void;
  
  /** 履歴追加 */
  addToHistory: (item: Omit<HistoryItem, 'id' | 'createdAt'>) => void;
  
  /** 履歴状態更新 */
  updateHistoryStatus: (id: string, status: HistoryItem['status']) => void;
  
  /** 履歴クリア */
  clearHistory: () => void;
  
  /** 履歴から復元 */
  loadFromHistory: (id: string, dataExtractor: (item: HistoryItem) => Partial<T>) => void;
  
  /** リセット */
  reset: () => void;
}

/** Store 設定 */
export interface CreateAgentStoreConfig<T, A = object> {
  /** Store 名 (永続化キー) */
  name: string;
  
  /** 初期業務データ */
  initialState: T;
  
  /** カスタムアクション */
  actions?: (
    set: (state: Partial<BaseAgentState<T>>) => void,
    get: () => BaseAgentState<T> & BaseAgentActions<T> & A
  ) => A;
  
  /** 最大履歴数 */
  maxHistoryItems?: number;
  
  /** 永続化するフィールド */
  persistFields?: (keyof BaseAgentState<T>)[];
  
  /** DevTools を有効化 */
  enableDevtools?: boolean;
}

// ========================================
// デフォルト設定
// ========================================

const DEFAULT_MAX_HISTORY = 10;
const DEFAULT_PERSIST_FIELDS: (keyof BaseAgentState<unknown>)[] = [
  'data',
  'history',
  'resultId',
  'result',
];

// ========================================
// Store 工厂
// ========================================

/**
 * AgentFlow Store を生成.
 * 
 * @param config Store 設定
 * @returns Zustand Store Hook
 */
export function createAgentStore<T, A = object>(
  config: CreateAgentStoreConfig<T, A>
) {
  const {
    name,
    initialState,
    actions: customActions,
    maxHistoryItems = DEFAULT_MAX_HISTORY,
    persistFields = DEFAULT_PERSIST_FIELDS as (keyof BaseAgentState<T>)[],
    enableDevtools = true,
  } = config;

  type StoreState = BaseAgentState<T> & BaseAgentActions<T> & A;

  const initialFullState: BaseAgentState<T> = {
    data: initialState,
    currentPage: 'input',
    isLoading: false,
    error: null,
    history: [],
    resultId: null,
    result: null,
  };

  // Store 作成
  const storeCreator = (
    set: (state: Partial<StoreState> | ((state: StoreState) => Partial<StoreState>)) => void,
    get: () => StoreState
  ): StoreState => {
    // 基底アクション
    const baseActions: BaseAgentActions<T> = {
      setData: (data) =>
        set((state) => ({
          data: { ...state.data, ...data },
        } as Partial<StoreState>)),

      setPage: (page) => set({ currentPage: page } as Partial<StoreState>),

      setLoading: (loading) => set({ isLoading: loading } as Partial<StoreState>),

      setError: (error) => set({ error } as Partial<StoreState>),

      setResult: (resultId, result) =>
        set({ resultId, result } as Partial<StoreState>),

      addToHistory: (item) =>
        set((state) => {
          const newItem: HistoryItem = {
            ...item,
            id: `history-${Date.now()}`,
            createdAt: new Date().toISOString(),
          };
          const newHistory = [newItem, ...state.history].slice(0, maxHistoryItems);
          return { history: newHistory } as Partial<StoreState>;
        }),

      updateHistoryStatus: (id, status) =>
        set((state) => ({
          history: state.history.map((item) =>
            item.id === id ? { ...item, status } : item
          ),
        } as Partial<StoreState>)),

      clearHistory: () => set({ history: [] } as Partial<StoreState>),

      loadFromHistory: (id, dataExtractor) => {
        const item = get().history.find((h) => h.id === id);
        if (item) {
          const extractedData = dataExtractor(item);
          set({
            data: { ...get().data, ...extractedData },
            currentPage: 'input',
          } as Partial<StoreState>);
        }
      },

      reset: () =>
        set({
          ...initialFullState,
          history: get().history, // 履歴は保持
        } as unknown as Partial<StoreState>),
    };

    // カスタムアクション
    const custom = customActions
      ? customActions(
          set as (state: Partial<BaseAgentState<T>>) => void,
          get
        )
      : ({} as A);

    return {
      ...initialFullState,
      ...baseActions,
      ...custom,
    } as StoreState;
  };

  // ミドルウェア適用
  let store = persist(storeCreator, {
    name: `agentflow-${name}`,
    storage: createJSONStorage(() => localStorage),
    partialize: (state) => {
      const persisted: Partial<BaseAgentState<T>> = {};
      for (const field of persistFields) {
        (persisted as Record<string, unknown>)[field] = state[field];
      }
      return persisted;
    },
  });

  if (enableDevtools) {
    store = devtools(store, { name: `AgentFlow/${name}` }) as typeof store;
  }

  return create(store);
}


/**
 * RAG ストアファクトリ.
 *
 * Zustand ストアを生成し、コレクション・ドキュメント・設定の状態を管理。
 * 各アプリが独自の RAGApiAdapter を渡してインスタンス化する。
 */
import { create } from 'zustand';
import type {
  CollectionInfo,
  DocumentInfo,
  IngestRunSummary,
  RAGApiAdapter,
  RAGConfigValues,
} from '../types/rag';

/** RAG ストアの状態 */
export interface RAGStoreState {
  /** コレクション一覧 */
  collections: CollectionInfo[];
  /** 選択中のコレクション名 */
  selectedCollection: string | null;
  /** ドキュメント一覧 */
  documents: DocumentInfo[];
  /** インジェスト履歴 */
  ingestRuns: IngestRunSummary[];
  /** アクティブタブ */
  activeTab: string;
  /** ローディング状態 */
  loading: Record<string, boolean>;
  /** エラー状態 */
  errors: Record<string, string | null>;
}

/** RAG ストアのアクション */
export interface RAGStoreActions {
  /** コレクション一覧を読み込み */
  fetchCollections: () => Promise<void>;
  /** コレクションを選択 */
  selectCollection: (name: string | null) => void;
  /** ドキュメント一覧を読み込み */
  fetchDocuments: (collection: string) => Promise<void>;
  /** インジェスト履歴を読み込み */
  fetchIngestRuns: () => Promise<void>;
  /** アクティブタブを変更 */
  setActiveTab: (tab: string) => void;
  /** エラーをクリア */
  clearError: (key: string) => void;
}

export type RAGStore = RAGStoreState & RAGStoreActions;

/** RAG ストアファクトリ */
export function createRAGStore(adapter: RAGApiAdapter) {
  return create<RAGStore>((set) => ({
    // --- 状態 ---
    collections: [],
    selectedCollection: null,
    documents: [],
    ingestRuns: [],
    activeTab: 'dashboard',
    loading: {},
    errors: {},

    // --- アクション ---
    fetchCollections: async () => {
      set((s) => ({ loading: { ...s.loading, collections: true } }));
      try {
        const collections = await adapter.listCollections();
        set((s) => ({
          collections,
          loading: { ...s.loading, collections: false },
          errors: { ...s.errors, collections: null },
        }));
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : '取得に失敗しました';
        set((s) => ({
          loading: { ...s.loading, collections: false },
          errors: { ...s.errors, collections: msg },
        }));
      }
    },

    selectCollection: (name: string | null) => {
      set({ selectedCollection: name });
    },

    fetchDocuments: async (collection: string) => {
      set((s) => ({ loading: { ...s.loading, documents: true } }));
      try {
        const result = await adapter.listDocuments(collection);
        set((s) => ({
          documents: result.documents,
          loading: { ...s.loading, documents: false },
          errors: { ...s.errors, documents: null },
        }));
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : '取得に失敗しました';
        set((s) => ({
          loading: { ...s.loading, documents: false },
          errors: { ...s.errors, documents: msg },
        }));
      }
    },

    fetchIngestRuns: async () => {
      set((s) => ({ loading: { ...s.loading, ingest: true } }));
      try {
        const runs = await adapter.listIngestRuns();
        set((s) => ({
          ingestRuns: runs,
          loading: { ...s.loading, ingest: false },
          errors: { ...s.errors, ingest: null },
        }));
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : '取得に失敗しました';
        set((s) => ({
          loading: { ...s.loading, ingest: false },
          errors: { ...s.errors, ingest: msg },
        }));
      }
    },

    setActiveTab: (tab: string) => {
      set({ activeTab: tab });
    },

    clearError: (key: string) => {
      set((s) => ({ errors: { ...s.errors, [key]: null } }));
    },
  }));
}

/**
 * FAQ System RAG 管理ストア.
 *
 * コレクション・ドキュメント・インジェスト状態を管理する Zustand ストア。
 */
import { create } from "zustand";
import {
  ragApi,
  type CollectionInfo,
  type DocumentInfo,
  type DocumentUploadOptions,
  type IngestRunSummary,
  type ChunkPreview,
  type KBLoadDirectoryRequest,
  type KBLoadDirectoryResult,
} from "../api/rag";

export type RAGTab =
  | "dashboard"
  | "collections"
  | "documents"
  | "retrieval"
  | "ingest"
  | "access"
  | "role_management";

interface RAGState {
  // タブ
  activeTab: RAGTab;
  setActiveTab: (tab: RAGTab) => void;

  // コレクション
  collections: CollectionInfo[];
  selectedCollection: string | null;
  collectionsLoading: boolean;
  fetchCollections: () => Promise<void>;
  selectCollection: (name: string | null) => void;
  createCollection: (data: Partial<CollectionInfo>) => Promise<CollectionInfo>;
  updateCollection: (
    name: string,
    data: Partial<CollectionInfo>,
  ) => Promise<CollectionInfo>;
  deleteCollection: (name: string) => Promise<void>;

  // ドキュメント
  documents: DocumentInfo[];
  documentsLoading: boolean;
  fetchDocuments: (collection: string) => Promise<void>;
  uploadDocument: (
    collection: string,
    file: File,
    autoIndex?: boolean,
    options?: DocumentUploadOptions,
  ) => Promise<DocumentInfo>;
  deleteDocument: (collection: string, docId: string) => Promise<void>;
  indexDocument: (collection: string, docId: string) => Promise<void>;
  reindexDocument: (collection: string, docId: string) => Promise<void>;

  // チャンクプレビュー
  chunkPreviews: ChunkPreview[];
  chunksLoading: boolean;
  previewChunks: (collection: string, docId: string) => Promise<void>;

  // バッチアップロード
  uploadDocuments: (
    collection: string,
    files: File[],
    autoIndex?: boolean,
    options?: DocumentUploadOptions,
  ) => Promise<DocumentInfo[]>;

  // ディレクトリロード
  directoryLoadResult: KBLoadDirectoryResult | null;
  directoryLoading: boolean;
  loadDirectory: (req: KBLoadDirectoryRequest) => Promise<KBLoadDirectoryResult>;
  clearDirectoryResult: () => void;

  // インジェスト
  ingestRuns: IngestRunSummary[];
  ingestLoading: boolean;
  fetchIngestRuns: () => Promise<void>;
  triggerIngest: () => Promise<void>;

  // エラー
  error: string | null;
  clearError: () => void;
}

export const useRAGStore = create<RAGState>((set, get) => ({
  // タブ
  activeTab: "dashboard",
  setActiveTab: (tab) => set({ activeTab: tab }),

  // コレクション
  collections: [],
  selectedCollection: null,
  collectionsLoading: false,
  fetchCollections: async () => {
    set({ collectionsLoading: true, error: null });
    try {
      const resp = await ragApi.listCollections();
      set({ collections: resp.collections, collectionsLoading: false });
    } catch (e) {
      set({ error: (e as Error).message, collectionsLoading: false });
    }
  },
  selectCollection: (name) => set({ selectedCollection: name }),
  createCollection: async (data) => {
    const resp = await ragApi.createCollection(data);
    await get().fetchCollections();
    return resp.collection;
  },
  updateCollection: async (name, data) => {
    const resp = await ragApi.updateCollection(name, data);
    await get().fetchCollections();
    return resp.collection;
  },
  deleteCollection: async (name) => {
    await ragApi.deleteCollection(name);
    if (get().selectedCollection === name) set({ selectedCollection: null });
    await get().fetchCollections();
  },

  // ドキュメント
  documents: [],
  documentsLoading: false,
  fetchDocuments: async (collection) => {
    set({ documentsLoading: true, error: null });
    try {
      const resp = await ragApi.listDocuments(collection);
      set({ documents: resp.documents, documentsLoading: false });
    } catch (e) {
      set({ error: (e as Error).message, documentsLoading: false });
    }
  },
  uploadDocument: async (collection, file, autoIndex, options) => {
    const resp = await ragApi.uploadDocument(collection, file, autoIndex, options);
    await get().fetchDocuments(collection);
    return resp.document;
  },
  deleteDocument: async (collection, docId) => {
    await ragApi.deleteDocument(collection, docId);
    await get().fetchDocuments(collection);
  },
  indexDocument: async (collection, docId) => {
    await ragApi.indexDocument(collection, docId);
    await get().fetchDocuments(collection);
  },
  reindexDocument: async (collection, docId) => {
    await ragApi.reindexDocument(collection, docId);
    await get().fetchDocuments(collection);
  },

  // バッチアップロード
  uploadDocuments: async (collection, files, autoIndex, options) => {
    const results = await ragApi.uploadDocuments(collection, files, autoIndex, options);
    await get().fetchDocuments(collection);
    return results;
  },

  // ディレクトリロード
  directoryLoadResult: null,
  directoryLoading: false,
  loadDirectory: async (req) => {
    set({ directoryLoading: true, error: null });
    try {
      const result = await ragApi.loadDirectory(req);
      set({ directoryLoadResult: result, directoryLoading: false });
      return result;
    } catch (e) {
      const message = (e as Error).message;
      set({ error: message, directoryLoading: false });
      throw e;
    }
  },
  clearDirectoryResult: () => set({ directoryLoadResult: null }),

  // チャンクプレビュー
  chunkPreviews: [],
  chunksLoading: false,
  previewChunks: async (collection, docId) => {
    set({ chunksLoading: true, chunkPreviews: [] });
    try {
      const resp = await ragApi.previewChunks(collection, docId);
      set({ chunkPreviews: resp.chunks, chunksLoading: false });
    } catch (e) {
      set({ error: (e as Error).message, chunksLoading: false });
    }
  },

  // インジェスト
  ingestRuns: [],
  ingestLoading: false,
  fetchIngestRuns: async () => {
    set({ ingestLoading: true });
    try {
      const resp = await ragApi.listIngestRuns();
      set({ ingestRuns: resp.runs, ingestLoading: false });
    } catch (e) {
      set({ error: (e as Error).message, ingestLoading: false });
    }
  },
  triggerIngest: async () => {
    set({ ingestLoading: true });
    try {
      await ragApi.triggerIngest();
      await get().fetchIngestRuns();
    } catch (e) {
      set({ error: (e as Error).message, ingestLoading: false });
    }
  },

  // エラー
  error: null,
  clearError: () => set({ error: null }),
}));

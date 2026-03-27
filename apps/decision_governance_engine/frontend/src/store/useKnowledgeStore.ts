/**
 * 知識ベースコレクション管理ストア.
 *
 * コレクション / ドキュメント / チャンクプレビューの状態管理。
 */

import { create } from "zustand";
import type {
  CollectionInfo,
  DocumentInfo,
  ChunkPreview,
} from "../api/knowledge";
import { knowledgeApi } from "../api/knowledge";

/** アクティブタブ */
export type KBTab = "dashboard" | "collections" | "documents" | "config";

interface KnowledgeState {
  activeTab: KBTab;
  collections: CollectionInfo[];
  documents: DocumentInfo[];
  chunkPreviews: ChunkPreview[];
  selectedCollection: string | null;
  error: string | null;
  loading: boolean;

  setActiveTab: (tab: KBTab) => void;
  setSelectedCollection: (name: string | null) => void;
  setError: (err: string | null) => void;

  fetchCollections: () => Promise<void>;
  createCollection: (data: Record<string, unknown>) => Promise<void>;
  updateCollection: (
    name: string,
    data: Record<string, unknown>,
  ) => Promise<void>;
  deleteCollection: (name: string) => Promise<void>;

  fetchDocuments: (collection: string) => Promise<void>;
  uploadDocument: (collection: string, file: File) => Promise<void>;
  deleteDocument: (collection: string, docId: string) => Promise<void>;
  indexDocument: (collection: string, docId: string) => Promise<void>;
  previewChunks: (
    collection: string,
    docId: string,
    opts?: Record<string, unknown>,
  ) => Promise<void>;
}

export const useKnowledgeStore = create<KnowledgeState>()((set, get) => ({
  activeTab: "dashboard",
  collections: [],
  documents: [],
  chunkPreviews: [],
  selectedCollection: null,
  error: null,
  loading: false,

  setActiveTab: (tab) => set({ activeTab: tab }),
  setSelectedCollection: (name) =>
    set({ selectedCollection: name, documents: [], chunkPreviews: [] }),
  setError: (err) => set({ error: err }),

  fetchCollections: async () => {
    set({ loading: true, error: null });
    try {
      const res = await knowledgeApi.listCollections();
      set({ collections: res.collections, loading: false });
    } catch (e) {
      set({
        error: e instanceof Error ? e.message : "Unknown error",
        loading: false,
      });
    }
  },

  createCollection: async (data) => {
    set({ error: null });
    try {
      await knowledgeApi.createCollection(data);
      await get().fetchCollections();
    } catch (e) {
      set({ error: e instanceof Error ? e.message : "Unknown error" });
    }
  },

  updateCollection: async (name, data) => {
    set({ error: null });
    try {
      await knowledgeApi.updateCollection(name, data);
      await get().fetchCollections();
    } catch (e) {
      set({ error: e instanceof Error ? e.message : "Unknown error" });
    }
  },

  deleteCollection: async (name) => {
    set({ error: null });
    try {
      await knowledgeApi.deleteCollection(name);
      const s = get();
      if (s.selectedCollection === name) set({ selectedCollection: null });
      await get().fetchCollections();
    } catch (e) {
      set({ error: e instanceof Error ? e.message : "Unknown error" });
    }
  },

  fetchDocuments: async (collection) => {
    set({ loading: true, error: null });
    try {
      const res = await knowledgeApi.listDocuments(collection);
      set({ documents: res.documents, loading: false });
    } catch (e) {
      set({
        error: e instanceof Error ? e.message : "Unknown error",
        loading: false,
      });
    }
  },

  uploadDocument: async (collection, file) => {
    set({ error: null });
    try {
      await knowledgeApi.uploadDocument(collection, file);
      await get().fetchDocuments(collection);
    } catch (e) {
      set({ error: e instanceof Error ? e.message : "Unknown error" });
    }
  },

  deleteDocument: async (collection, docId) => {
    set({ error: null });
    try {
      await knowledgeApi.deleteDocument(collection, docId);
      await get().fetchDocuments(collection);
    } catch (e) {
      set({ error: e instanceof Error ? e.message : "Unknown error" });
    }
  },

  indexDocument: async (collection, docId) => {
    set({ error: null });
    try {
      await knowledgeApi.indexDocument(collection, docId);
      await get().fetchDocuments(collection);
    } catch (e) {
      set({ error: e instanceof Error ? e.message : "Unknown error" });
    }
  },

  previewChunks: async (collection, docId, opts) => {
    set({ error: null });
    try {
      const res = await knowledgeApi.previewChunks(collection, docId, opts);
      set({ chunkPreviews: res.chunks });
    } catch (e) {
      set({ error: e instanceof Error ? e.message : "Unknown error" });
    }
  },
}));

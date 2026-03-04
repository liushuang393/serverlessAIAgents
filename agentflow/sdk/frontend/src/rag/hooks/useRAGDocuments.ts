/**
 * ドキュメント管理フック.
 *
 * RAGApiAdapter を受け取り、ドキュメントのアップロード・一覧・削除等を管理。
 */
import { useCallback, useState } from 'react';
import type {
  ChunkPreview,
  DocumentInfo,
  ListDocumentsOptions,
  PaginatedDocs,
  RAGApiAdapter,
} from '../types/rag';

export interface UseRAGDocumentsReturn {
  documents: DocumentInfo[];
  total: number;
  loading: boolean;
  error: string | null;
  fetch: (collection: string, options?: ListDocumentsOptions) => Promise<void>;
  upload: (collection: string, file: File) => Promise<DocumentInfo | null>;
  remove: (collection: string, docId: string) => Promise<boolean>;
  previewChunks: (collection: string, docId: string) => Promise<ChunkPreview[]>;
  index: (collection: string, docId: string) => Promise<DocumentInfo | null>;
  reindex: (collection: string, docId: string) => Promise<boolean>;
}

/** ドキュメント管理フック */
export function useRAGDocuments(adapter: RAGApiAdapter): UseRAGDocumentsReturn {
  const [documents, setDocuments] = useState<DocumentInfo[]>([]);
  const [total, setTotal] = useState(0);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const fetch = useCallback(
    async (collection: string, options?: ListDocumentsOptions) => {
      setLoading(true);
      setError(null);
      try {
        const result: PaginatedDocs = await adapter.listDocuments(collection, options);
        setDocuments(result.documents);
        setTotal(result.total);
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : '取得に失敗しました';
        setError(msg);
      } finally {
        setLoading(false);
      }
    },
    [adapter],
  );

  const upload = useCallback(
    async (collection: string, file: File): Promise<DocumentInfo | null> => {
      setError(null);
      try {
        const doc = await adapter.uploadDocument(collection, file);
        setDocuments((prev) => [doc, ...prev]);
        setTotal((prev) => prev + 1);
        return doc;
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : 'アップロードに失敗しました';
        setError(msg);
        return null;
      }
    },
    [adapter],
  );

  const remove = useCallback(
    async (collection: string, docId: string): Promise<boolean> => {
      setError(null);
      try {
        await adapter.deleteDocument(collection, docId);
        setDocuments((prev) => prev.filter((d) => d.document_id !== docId));
        setTotal((prev) => Math.max(prev - 1, 0));
        return true;
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : '削除に失敗しました';
        setError(msg);
        return false;
      }
    },
    [adapter],
  );

  const previewChunks = useCallback(
    async (collection: string, docId: string): Promise<ChunkPreview[]> => {
      try {
        return await adapter.previewChunks(collection, docId);
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : 'プレビューに失敗しました';
        setError(msg);
        return [];
      }
    },
    [adapter],
  );

  const index = useCallback(
    async (collection: string, docId: string): Promise<DocumentInfo | null> => {
      setError(null);
      try {
        const doc = await adapter.indexDocument(collection, docId);
        setDocuments((prev) =>
          prev.map((d) => (d.document_id === docId ? doc : d)),
        );
        return doc;
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : 'インデックスに失敗しました';
        setError(msg);
        return null;
      }
    },
    [adapter],
  );

  const reindex = useCallback(
    async (collection: string, docId: string): Promise<boolean> => {
      setError(null);
      try {
        await adapter.reindexDocument(collection, docId);
        return true;
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : '再インデックスに失敗しました';
        setError(msg);
        return false;
      }
    },
    [adapter],
  );

  return { documents, total, loading, error, fetch, upload, remove, previewChunks, index, reindex };
}

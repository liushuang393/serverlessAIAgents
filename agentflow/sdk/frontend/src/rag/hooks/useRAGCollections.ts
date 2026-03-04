/**
 * コレクション CRUD フック.
 *
 * RAGApiAdapter を受け取り、コレクション管理の状態と操作を提供。
 */
import { useCallback, useEffect, useState } from 'react';
import type { CollectionInfo, CreateCollectionRequest, RAGApiAdapter, RAGConfigValues } from '../types/rag';

export interface UseRAGCollectionsReturn {
  collections: CollectionInfo[];
  loading: boolean;
  error: string | null;
  refresh: () => Promise<void>;
  create: (config: CreateCollectionRequest) => Promise<CollectionInfo | null>;
  update: (name: string, config: Partial<RAGConfigValues>) => Promise<CollectionInfo | null>;
  remove: (name: string) => Promise<boolean>;
}

/** コレクション管理フック */
export function useRAGCollections(adapter: RAGApiAdapter): UseRAGCollectionsReturn {
  const [collections, setCollections] = useState<CollectionInfo[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const refresh = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const result = await adapter.listCollections();
      setCollections(result);
    } catch (err: unknown) {
      const msg = err instanceof Error ? err.message : '取得に失敗しました';
      setError(msg);
    } finally {
      setLoading(false);
    }
  }, [adapter]);

  useEffect(() => {
    void refresh();
  }, [refresh]);

  const create = useCallback(
    async (config: CreateCollectionRequest): Promise<CollectionInfo | null> => {
      setError(null);
      try {
        const created = await adapter.createCollection(config);
        setCollections((prev) => [...prev, created]);
        return created;
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : '作成に失敗しました';
        setError(msg);
        return null;
      }
    },
    [adapter],
  );

  const update = useCallback(
    async (name: string, config: Partial<RAGConfigValues>): Promise<CollectionInfo | null> => {
      setError(null);
      try {
        const updated = await adapter.updateCollection(name, config);
        setCollections((prev) =>
          prev.map((c) => (c.collection_name === name ? updated : c)),
        );
        return updated;
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : '更新に失敗しました';
        setError(msg);
        return null;
      }
    },
    [adapter],
  );

  const remove = useCallback(
    async (name: string): Promise<boolean> => {
      setError(null);
      try {
        await adapter.deleteCollection(name);
        setCollections((prev) =>
          prev.filter((c) => c.collection_name !== name),
        );
        return true;
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : '削除に失敗しました';
        setError(msg);
        return false;
      }
    },
    [adapter],
  );

  return { collections, loading, error, refresh, create, update, remove };
}

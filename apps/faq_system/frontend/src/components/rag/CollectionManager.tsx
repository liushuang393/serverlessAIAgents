/**
 * コレクション管理ページ.
 *
 * コレクションの作成・設定変更・削除を行う。
 */
import { useEffect, useState } from 'react';
import { Plus, Trash2, Settings, Database } from 'lucide-react';
import { useI18n } from '../../i18n';
import { useRAGStore } from '../../stores/ragStore';
import type { CollectionInfo } from '../../api/rag';

/** コレクション作成/編集フォーム */
function CollectionForm({
  initial,
  onSave,
  onCancel,
}: {
  initial?: Partial<CollectionInfo>;
  onSave: (data: Partial<CollectionInfo>) => void;
  onCancel: () => void;
}) {
  const { t } = useI18n();
  const [name, setName] = useState(initial?.collection_name ?? '');
  const [displayName, setDisplayName] = useState(initial?.display_name ?? '');
  const [description, setDescription] = useState(initial?.description ?? '');
  const [chunkStrategy, setChunkStrategy] = useState(initial?.chunk_strategy ?? 'recursive');
  const [chunkSize, setChunkSize] = useState(initial?.chunk_size ?? 1000);
  const [chunkOverlap, setChunkOverlap] = useState(initial?.chunk_overlap ?? 200);
  const [topK, setTopK] = useState(initial?.top_k ?? 5);
  const [retrievalMethod, setRetrievalMethod] = useState(initial?.retrieval_method ?? 'semantic');
  const [reranker, setReranker] = useState(initial?.reranker ?? '');

  const isEdit = !!initial?.collection_name;

  const PATTERNS: Record<string, { chunk_strategy: string; chunk_size: number; chunk_overlap: number; top_k: number }> = {
    faq_precision: { chunk_strategy: 'sentence', chunk_size: 500, chunk_overlap: 80, top_k: 8 },
    balanced_knowledge: { chunk_strategy: 'recursive', chunk_size: 800, chunk_overlap: 120, top_k: 6 },
    long_doc_reasoning: { chunk_strategy: 'markdown', chunk_size: 1200, chunk_overlap: 180, top_k: 10 },
  };

  const applyPattern = (pattern: string) => {
    const p = PATTERNS[pattern];
    if (p) {
      setChunkStrategy(p.chunk_strategy);
      setChunkSize(p.chunk_size);
      setChunkOverlap(p.chunk_overlap);
      setTopK(p.top_k);
    }
  };

  return (
    <div data-testid="collection-form-modal" className="rounded-xl bg-white/[0.03] border border-white/10 p-5 space-y-4">
      <h3 className="text-sm font-semibold text-white">
        {isEdit ? t('rag.edit_collection') : t('rag.create_collection')}
      </h3>

      {!isEdit && (
        <label className="block">
          <span className="text-xs text-[var(--text-muted)]">パターンプリセット</span>
          <select
            data-testid="pattern-select"
            defaultValue=""
            onChange={(e) => applyPattern(e.target.value)}
            className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
          >
            <option value="">(手動設定)</option>
            <option value="faq_precision">FAQ Precision</option>
            <option value="balanced_knowledge">Balanced Knowledge</option>
            <option value="long_doc_reasoning">Long Doc Reasoning</option>
          </select>
        </label>
      )}

      <div className="grid grid-cols-1 sm:grid-cols-2 gap-3">
        <label className="block">
          <span className="text-xs text-[var(--text-muted)]">{t('rag.collection_name')}</span>
          <input
            value={name}
            onChange={(e) => setName(e.target.value)}
            disabled={isEdit}
            className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white placeholder:text-[var(--text-muted)] disabled:opacity-50"
            placeholder="my_collection"
          />
        </label>
        <label className="block">
          <span className="text-xs text-[var(--text-muted)]">{t('rag.display_name')}</span>
          <input
            value={displayName}
            onChange={(e) => setDisplayName(e.target.value)}
            className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white placeholder:text-[var(--text-muted)]"
            placeholder={t('rag.display_name')}
          />
        </label>
      </div>

      <label className="block">
        <span className="text-xs text-[var(--text-muted)]">{t('rag.description')}</span>
        <textarea
          value={description}
          onChange={(e) => setDescription(e.target.value)}
          className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white resize-none h-16"
        />
      </label>

      <div className="grid grid-cols-2 sm:grid-cols-4 gap-3">
        <label className="block">
          <span className="text-xs text-[var(--text-muted)]">{t('rag.chunk_strategy')}</span>
          <select
            data-testid="chunk-strategy"
            value={chunkStrategy}
            onChange={(e) => setChunkStrategy(e.target.value)}
            className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
          >
            <option value="recursive">Recursive</option>
            <option value="sentence">Sentence</option>
            <option value="semantic">Semantic</option>
            <option value="token">Token</option>
            <option value="markdown">Markdown</option>
            <option value="fixed">Fixed</option>
          </select>
        </label>
        <label className="block">
          <span className="text-xs text-[var(--text-muted)]">{t('rag.chunk_size')}</span>
          <input
            data-testid="chunk-size"
            type="number"
            value={chunkSize}
            onChange={(e) => setChunkSize(Number(e.target.value))}
            className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
          />
        </label>
        <label className="block">
          <span className="text-xs text-[var(--text-muted)]">{t('rag.chunk_overlap')}</span>
          <input
            type="number"
            value={chunkOverlap}
            onChange={(e) => setChunkOverlap(Number(e.target.value))}
            className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
          />
        </label>
        <label className="block">
          <span className="text-xs text-[var(--text-muted)]">Top-K</span>
          <input
            type="number"
            value={topK}
            onChange={(e) => setTopK(Number(e.target.value))}
            className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
          />
        </label>
      </div>

      <div className="grid grid-cols-2 gap-3">
        <label className="block">
          <span className="text-xs text-[var(--text-muted)]">{t('rag.retrieval_method')}</span>
          <select
            value={retrievalMethod}
            onChange={(e) => setRetrievalMethod(e.target.value)}
            className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
          >
            <option value="semantic">Semantic</option>
            <option value="hybrid">Hybrid</option>
            <option value="keyword">Keyword</option>
          </select>
        </label>
        <label className="block">
          <span className="text-xs text-[var(--text-muted)]">{t('rag.reranker')}</span>
          <select
            value={reranker}
            onChange={(e) => setReranker(e.target.value)}
            className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
          >
            <option value="">{t('rag.none')}</option>
            <option value="cohere">Cohere</option>
            <option value="cross_encoder">Cross Encoder</option>
            <option value="bm25">BM25</option>
          </select>
        </label>
      </div>

      <div className="flex justify-end gap-2">
        <button
          onClick={onCancel}
          className="px-4 py-2 rounded-lg text-xs text-[var(--text-muted)] hover:bg-white/5 transition"
        >
          {t('common.cancel')}
        </button>
        <button
          onClick={() =>
            onSave({
              collection_name: name,
              display_name: displayName,
              description,
              chunk_strategy: chunkStrategy,
              chunk_size: chunkSize,
              chunk_overlap: chunkOverlap,
              top_k: topK,
              retrieval_method: retrievalMethod,
              reranker: reranker || undefined,
            })
          }
          disabled={!isEdit && !name.trim()}
          className="px-4 py-2 rounded-lg bg-[var(--primary)] text-black text-xs font-semibold hover:opacity-90 transition disabled:opacity-50"
        >
          {isEdit ? t('common.save') : t('rag.create')}
        </button>
      </div>
    </div>
  );
}

export function CollectionManager() {
  const { t } = useI18n();
  const {
    collections,
    collectionsLoading,
    fetchCollections,
    createCollection,
    updateCollection,
    deleteCollection,
    selectCollection,
    setActiveTab,
  } = useRAGStore();

  const [showForm, setShowForm] = useState(false);
  const [editing, setEditing] = useState<CollectionInfo | null>(null);

  useEffect(() => {
    void fetchCollections();
  }, [fetchCollections]);

  const handleCreate = async (data: Partial<CollectionInfo>) => {
    try {
      await createCollection(data);
      setShowForm(false);
    } catch {
      // error handled by store
    }
  };

  const handleUpdate = async (data: Partial<CollectionInfo>) => {
    if (!editing) return;
    try {
      await updateCollection(editing.collection_name, data);
      setEditing(null);
    } catch {
      // error handled by store
    }
  };

  return (
    <div className="space-y-4">
      {/* ヘッダー */}
      <div className="flex items-center justify-between">
        <h2 className="text-sm font-semibold text-white">{t('rag.collections')}</h2>
        <button
          data-testid="create-collection-button"
          onClick={() => { setShowForm(true); setEditing(null); }}
          className="flex items-center gap-1.5 px-3 py-1.5 rounded-lg bg-[var(--primary)]/10 text-[var(--primary)] text-xs font-medium hover:bg-[var(--primary)]/20 transition border border-[var(--primary)]/20"
        >
          <Plus size={14} />
          {t('rag.create_collection')}
        </button>
      </div>

      {/* 作成/編集フォーム */}
      {(showForm || editing) && (
        <CollectionForm
          initial={editing ?? undefined}
          onSave={editing ? handleUpdate : handleCreate}
          onCancel={() => { setShowForm(false); setEditing(null); }}
        />
      )}

      {/* コレクションリスト */}
      {collectionsLoading && collections.length === 0 && (
        <div className="text-center py-12 text-[var(--text-muted)] text-sm">
          {t('common.loading')}
        </div>
      )}

      {collections.length === 0 && !collectionsLoading && (
        <div className="text-center py-12 text-[var(--text-muted)] text-sm">
          {t('rag.no_collections')}
        </div>
      )}

      <div data-testid="collection-list" className="space-y-2">
        {collections.map((col) => (
          <div
            key={col.collection_name}
            className="flex items-center justify-between p-4 rounded-xl bg-white/[0.03] border border-white/5 hover:bg-white/[0.05] transition group"
          >
            <button
              type="button"
              className="flex items-center gap-3 flex-1 min-w-0 text-left bg-transparent border-none cursor-pointer"
              onClick={() => {
                selectCollection(col.collection_name);
                setActiveTab('documents');
              }}
            >
              <Database size={16} className="text-indigo-400 flex-shrink-0" />
              <div className="min-w-0">
                <p className="text-sm font-medium text-white truncate">
                  {col.display_name || col.collection_name}
                </p>
                <p className="text-xs text-[var(--text-muted)]">
                  {col.collection_name} &middot; {col.document_count} docs &middot; {col.chunk_strategy}/{col.chunk_size}
                </p>
              </div>
            </button>

            <div className="flex items-center gap-1 opacity-0 group-hover:opacity-100 transition">
              <button
                onClick={() => setEditing(col)}
                className="p-2 rounded-lg hover:bg-white/10 text-[var(--text-muted)] hover:text-white transition"
                title={t('rag.edit_collection')}
              >
                <Settings size={14} />
              </button>
              <button
                onClick={() => {
                  if (confirm(t('rag.confirm_delete_collection'))) {
                    void deleteCollection(col.collection_name);
                  }
                }}
                className="p-2 rounded-lg hover:bg-red-500/10 text-[var(--text-muted)] hover:text-red-400 transition"
                title={t('rag.delete_collection')}
              >
                <Trash2 size={14} />
              </button>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

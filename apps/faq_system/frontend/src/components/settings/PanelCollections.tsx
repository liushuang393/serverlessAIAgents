/**
 * パネル内コレクション管理コンポーネント.
 *
 * KnowledgePanel の「コレクション」タブで使用される。
 * コレクション一覧のカード表示、新規作成、設定編集を行う。
 */
import { useEffect, useState } from "react";
import { Plus, Database } from "lucide-react";
import { useI18n } from "../../i18n";
import { useRAGStore } from "../../stores/ragStore";
import type { CollectionInfo } from "../../api/rag";
import { PRESETS } from "./PanelCollections.constants";
import type { PresetKey } from "./PanelCollections.constants";

// re-export types for consumers that previously imported from this file
export type { PresetValues, PresetKey, PresetDef } from "./PanelCollections.constants";
export { PRESETS } from "./PanelCollections.constants";

// ---------------------------------------------------------------------------
// CollectionForm
// ---------------------------------------------------------------------------

interface CollectionFormProps {
  initial?: CollectionInfo;
  onSave: (data: Partial<CollectionInfo>) => void;
  onCancel: () => void;
}

function CollectionForm({ initial, onSave, onCancel }: CollectionFormProps) {
  const { t } = useI18n();
  const isEdit = !!initial;

  const [preset, setPreset] = useState<PresetKey>("custom");
  const [collectionName, setCollectionName] = useState(
    initial?.collection_name ?? "",
  );
  const [displayName, setDisplayName] = useState(initial?.display_name ?? "");
  const [description, setDescription] = useState(initial?.description ?? "");
  const [chunkStrategy, setChunkStrategy] = useState(
    initial?.chunk_strategy ?? "recursive",
  );
  const [chunkSize, setChunkSize] = useState(initial?.chunk_size ?? 800);
  const [chunkOverlap, setChunkOverlap] = useState(
    initial?.chunk_overlap ?? 120,
  );
  const [retrievalMethod, setRetrievalMethod] = useState(
    initial?.retrieval_method ?? "semantic",
  );
  const [reranker, setReranker] = useState(initial?.reranker ?? "none");
  const [topK, setTopK] = useState(initial?.top_k ?? 5);
  const [minSimilarity, setMinSimilarity] = useState(
    initial?.min_similarity ?? 0.2,
  );

  const applyPreset = (key: PresetKey) => {
    setPreset(key);
    const def = PRESETS.find((p) => p.key === key);
    if (def?.values) {
      setChunkStrategy(def.values.chunk_strategy);
      setChunkSize(def.values.chunk_size);
      setChunkOverlap(def.values.chunk_overlap);
      setRetrievalMethod(def.values.retrieval_method);
      setReranker(def.values.reranker);
      setTopK(def.values.top_k);
      setMinSimilarity(def.values.min_similarity);
    }
  };

  const handleSubmit = () => {
    const data: Partial<CollectionInfo> = {
      chunk_strategy: chunkStrategy,
      chunk_size: chunkSize,
      chunk_overlap: chunkOverlap,
      retrieval_method: retrievalMethod,
      reranker: reranker === "none" ? null : reranker,
      top_k: topK,
      min_similarity: minSimilarity,
    };
    if (!isEdit) {
      data.collection_name = collectionName;
    }
    data.display_name = displayName;
    data.description = description;
    onSave(data);
  };

  return (
    <div data-testid="panel-collection-form" className="space-y-4">
      {/* プリセット選択 */}
      <div>
        <h4 className="text-xs font-bold text-[var(--text-muted)] uppercase tracking-wider mb-2">
          {t("knowledge_panel.pattern_preset")}
        </h4>
        <div className="grid grid-cols-2 gap-4">
          {PRESETS.map((p) => (
            <button
              key={p.key}
              type="button"
              data-testid={`preset-${p.key}`}
              onClick={() => applyPreset(p.key)}
              className={`p-4 rounded-xl border text-left transition-all ${preset === p.key
                ? "border-[var(--primary)] bg-[var(--primary)]/10 shadow-sm"
                : "border-white/5 bg-white/[0.02] hover:bg-white/[0.04]"
                }`}
            >
              <p className="text-xs font-bold text-white">
                {t(p.labelKey)}
              </p>
              <p className="text-[10px] text-[var(--text-muted)] mt-1 tracking-tight">
                {t(p.descriptionKey)}
              </p>
            </button>
          ))}
        </div>
      </div>

      {/* 新規作成フィールド */}
      {!isEdit && (
        <div>
          <h4 className="text-xs font-bold text-[var(--text-muted)] uppercase tracking-wider mb-2">
            {t("knowledge_panel.basic_info")}
          </h4>
          <div className="space-y-3">
            <input
              data-testid="input-collection-name"
              value={collectionName}
              onChange={(e) => setCollectionName(e.target.value)}
              placeholder={t("knowledge_panel.collection_name_placeholder")}
              className="w-full px-4 py-2.5 rounded-xl bg-white/5 border border-white/10 text-sm text-white focus:border-[var(--primary)]/50 transition-colors"
            />
            <input
              data-testid="input-display-name"
              value={displayName}
              onChange={(e) => setDisplayName(e.target.value)}
              placeholder={t("knowledge_panel.display_name_placeholder")}
              className="w-full px-4 py-2.5 rounded-xl bg-white/5 border border-white/10 text-sm text-white focus:border-[var(--primary)]/50 transition-colors"
            />
            <textarea
              data-testid="input-description"
              value={description}
              onChange={(e) => setDescription(e.target.value)}
              placeholder={t("knowledge_panel.description_placeholder")}
              rows={3}
              className="w-full px-4 py-3 rounded-xl bg-white/5 border border-white/10 text-sm text-white focus:border-[var(--primary)]/50 transition-colors resize-none"
            />
          </div>
        </div>
      )}

      {/* チャンキング設定 */}
      <div>
        <h4 className="text-xs font-bold text-[var(--text-muted)] uppercase tracking-wider mb-2">
          {t("knowledge_panel.chunking_settings")}
        </h4>
        <div className="space-y-3">
          <label className="block">
            <span className="text-xs text-[var(--text-muted)]">
              {t("knowledge_panel.chunk_strategy")}
            </span>
            <select
              data-testid="select-chunk-strategy"
              value={chunkStrategy}
              onChange={(e) => setChunkStrategy(e.target.value)}
              className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
            >
              <option value="recursive">Recursive</option>
              <option value="sentence">Sentence</option>
              <option value="semantic">Semantic</option>
              <option value="token">Token</option>
              <option value="markdown">Markdown</option>
            </select>
          </label>
          <label className="block">
            <span className="text-xs text-[var(--text-muted)]">
              {t("knowledge_panel.chunk_size")}:{" "}
              <span data-testid="chunk-size-value">{chunkSize}</span>
            </span>
            <input
              data-testid="range-chunk-size"
              type="range"
              min={100}
              max={4000}
              step={50}
              value={chunkSize}
              onChange={(e) => setChunkSize(Number(e.target.value))}
              className="mt-1 w-full accent-[var(--primary)]"
            />
          </label>
          <label className="block">
            <span className="text-xs text-[var(--text-muted)]">
              {t("knowledge_panel.chunk_overlap")}:{" "}
              <span data-testid="chunk-overlap-value">{chunkOverlap}</span>
            </span>
            <input
              data-testid="range-chunk-overlap"
              type="range"
              min={0}
              max={500}
              step={10}
              value={chunkOverlap}
              onChange={(e) => setChunkOverlap(Number(e.target.value))}
              className="mt-1 w-full accent-[var(--primary)]"
            />
          </label>
        </div>
      </div>

      {/* 検索設定 */}
      <div>
        <h4 className="text-xs font-bold text-[var(--text-muted)] uppercase tracking-wider mb-2">
          {t("knowledge_panel.retrieval_settings")}
        </h4>
        <div className="space-y-3">
          <label className="block">
            <span className="text-xs text-[var(--text-muted)]">
              {t("knowledge_panel.retrieval_method")}
            </span>
            <select
              data-testid="select-retrieval-method"
              value={retrievalMethod}
              onChange={(e) => setRetrievalMethod(e.target.value)}
              className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
            >
              <option value="semantic">Semantic</option>
              <option value="hybrid">Hybrid</option>
              <option value="keyword">Keyword</option>
              <option value="multi_query">Multi Query</option>
            </select>
          </label>
          <label className="block">
            <span className="text-xs text-[var(--text-muted)]">
              {t("knowledge_panel.reranker")}
            </span>
            <select
              data-testid="select-reranker"
              value={reranker}
              onChange={(e) => setReranker(e.target.value)}
              className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
            >
              <option value="none">None</option>
              <option value="bm25">BM25</option>
              <option value="cohere">Cohere</option>
              <option value="cross_encoder">Cross Encoder</option>
            </select>
          </label>
          <label className="block">
            <span className="text-xs text-[var(--text-muted)]">
              {t("knowledge_panel.top_k")}:{" "}
              <span data-testid="top-k-value">{topK}</span>
            </span>
            <input
              data-testid="range-top-k"
              type="range"
              min={1}
              max={50}
              step={1}
              value={topK}
              onChange={(e) => setTopK(Number(e.target.value))}
              className="mt-1 w-full accent-[var(--primary)]"
            />
          </label>
          <label className="block">
            <span className="text-xs text-[var(--text-muted)]">
              {t("knowledge_panel.min_similarity")}:{" "}
              <span data-testid="min-similarity-value">
                {minSimilarity.toFixed(2)}
              </span>
            </span>
            <input
              data-testid="range-min-similarity"
              type="range"
              min={0}
              max={1}
              step={0.05}
              value={minSimilarity}
              onChange={(e) => setMinSimilarity(Number(e.target.value))}
              className="mt-1 w-full accent-[var(--primary)]"
            />
          </label>
        </div>
      </div>

      {/* アクション */}
      <div className="flex justify-end gap-2 pt-2">
        <button
          type="button"
          onClick={onCancel}
          className="px-4 py-2 rounded-lg text-xs text-[var(--text-muted)] hover:bg-white/5 transition"
        >
          {t("knowledge_panel.cancel")}
        </button>
        <button
          type="button"
          data-testid="btn-save-collection"
          onClick={handleSubmit}
          disabled={!isEdit && !collectionName.trim()}
          className="px-4 py-2 rounded-lg bg-[var(--primary)] text-black text-xs font-semibold hover:opacity-90 transition disabled:opacity-50"
        >
          {isEdit ? t("knowledge_panel.save") : t("knowledge_panel.create")}
        </button>
      </div>
    </div>
  );
}

// ---------------------------------------------------------------------------
// PanelCollections (メインエクスポート)
// ---------------------------------------------------------------------------

export function PanelCollections() {
  const { t } = useI18n();
  const {
    collections,
    collectionsLoading,
    fetchCollections,
    createCollection,
    updateCollection,
    selectCollection,
    selectedCollection,
  } = useRAGStore();

  const [mode, setMode] = useState<"list" | "create" | "edit">("list");
  const [editingCollection, setEditingCollection] =
    useState<CollectionInfo | null>(null);

  useEffect(() => {
    void fetchCollections();
  }, [fetchCollections]);

  const handleCardClick = (col: CollectionInfo) => {
    if (selectedCollection === col.collection_name && mode === "edit") {
      // 同じカードを再クリックしたら閉じる
      setMode("list");
      setEditingCollection(null);
      selectCollection(null);
    } else {
      selectCollection(col.collection_name);
      setEditingCollection(col);
      setMode("edit");
    }
  };

  const handleCreate = async (data: Partial<CollectionInfo>) => {
    try {
      await createCollection(data);
      setMode("list");
    } catch {
      // error handled by store
    }
  };

  const handleUpdate = async (data: Partial<CollectionInfo>) => {
    if (!editingCollection) return;
    try {
      await updateCollection(editingCollection.collection_name, data);
      setMode("list");
      setEditingCollection(null);
    } catch {
      // error handled by store
    }
  };

  const handleCancel = () => {
    setMode("list");
    setEditingCollection(null);
    selectCollection(null);
  };

  return (
    <div data-testid="panel-collections" className="space-y-4">
      {/* ヘッダー + 新規作成ボタン */}
      <div className="flex items-center justify-between">
        <h3 className="text-xs font-bold text-[var(--text-muted)] uppercase tracking-wider">
          {t("knowledge_panel.collection_list")}
        </h3>
        <button
          data-testid="btn-new-collection"
          type="button"
          onClick={() => {
            setMode("create");
            setEditingCollection(null);
            selectCollection(null);
          }}
          className="flex items-center gap-1 px-2.5 py-1.5 rounded-lg bg-[var(--primary)]/10 text-[var(--primary)] text-xs font-medium hover:bg-[var(--primary)]/20 transition border border-[var(--primary)]/20"
        >
          <Plus size={14} />
          {t("knowledge_panel.new_collection")}
        </button>
      </div>

      {/* ローディング */}
      {collectionsLoading && collections.length === 0 && (
        <div className="text-center py-8 text-[var(--text-muted)] text-sm">
          {t("knowledge_panel.loading")}
        </div>
      )}

      {/* 空状態 */}
      {!collectionsLoading && collections.length === 0 && (
        <div className="text-center py-8 text-[var(--text-muted)] text-sm">
          {t("knowledge_panel.no_collections")}
        </div>
      )}

      {/* コレクションカード一覧 */}
      <div data-testid="collection-card-list" className="space-y-2">
        {collections.map((col) => {
          const isSelected =
            selectedCollection === col.collection_name && mode === "edit";
          return (
            <div key={col.collection_name}>
              <button
                type="button"
                data-testid={`collection-card-${col.collection_name}`}
                onClick={handleCardClick.bind(null, col)}
                className={`w-full text-left p-4 rounded-2xl border transition-all ${isSelected
                  ? "border-[var(--primary)] bg-[var(--primary)]/5 shadow-[0_0_15px_rgba(94,234,212,0.1)]"
                  : "border-white/5 bg-white/[0.02] hover:bg-white/[0.04] hover:border-white/10"
                  }`}
              >
                <div className="flex items-center gap-4">
                  <div className="w-10 h-10 rounded-xl bg-white/5 flex items-center justify-center transition-colors">
                    <Database size={18} className="text-indigo-400" />
                  </div>
                  <div className="flex-1 min-w-0">
                    <span className="text-sm font-semibold text-white block truncate">
                      {col.display_name || col.collection_name}
                    </span>
                    <div className="flex items-center gap-2 mt-1.5">
                      <span className="text-[10px] bg-white/5 px-2.5 py-1 rounded text-[var(--text-muted)] font-medium">
                        {col.document_count} docs
                      </span>
                      <span className="text-[10px] bg-white/5 px-2.5 py-1 rounded text-[var(--text-muted)] font-mono">
                        {col.chunk_strategy}
                      </span>
                    </div>
                  </div>
                </div>
              </button>

              {/* 選択中コレクションの設定フォーム */}
              {isSelected && editingCollection && (
                <div className="mt-2 p-3 rounded-xl glass border border-white/5">
                  <CollectionForm
                    initial={editingCollection}
                    onSave={handleUpdate}
                    onCancel={handleCancel}
                  />
                </div>
              )}
            </div>
          );
        })}
      </div>

      {/* 新規作成フォーム */}
      {mode === "create" && (
        <div className="p-3 rounded-xl glass border border-white/5">
          <CollectionForm onSave={handleCreate} onCancel={handleCancel} />
        </div>
      )}
    </div>
  );
}

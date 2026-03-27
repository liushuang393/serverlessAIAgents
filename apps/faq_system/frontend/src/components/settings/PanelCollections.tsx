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

// ---------------------------------------------------------------------------
// プリセット定義
// ---------------------------------------------------------------------------

export interface PresetValues {
  chunk_strategy: string;
  chunk_size: number;
  chunk_overlap: number;
  retrieval_method: string;
  reranker: string;
  top_k: number;
  min_similarity: number;
}

export type PresetKey =
  | "faq_precision"
  | "balanced_knowledge"
  | "long_doc_reasoning"
  | "custom";

export interface PresetDef {
  key: PresetKey;
  labelKey: string;
  descriptionKey: string;
  values: PresetValues | null;
}

export const PRESETS: PresetDef[] = [
  {
    key: "faq_precision",
    labelKey: "knowledge_panel.pattern_faq_precision",
    descriptionKey: "knowledge_panel.pattern_faq_precision_desc",
    values: {
      chunk_strategy: "sentence",
      chunk_size: 500,
      chunk_overlap: 80,
      retrieval_method: "hybrid",
      reranker: "cohere",
      top_k: 8,
      min_similarity: 0.25,
    },
  },
  {
    key: "balanced_knowledge",
    labelKey: "knowledge_panel.pattern_balanced",
    descriptionKey: "knowledge_panel.pattern_balanced_desc",
    values: {
      chunk_strategy: "recursive",
      chunk_size: 800,
      chunk_overlap: 120,
      retrieval_method: "hybrid",
      reranker: "bm25",
      top_k: 6,
      min_similarity: 0.2,
    },
  },
  {
    key: "long_doc_reasoning",
    labelKey: "knowledge_panel.pattern_long_doc",
    descriptionKey: "knowledge_panel.pattern_long_doc_desc",
    values: {
      chunk_strategy: "markdown",
      chunk_size: 1200,
      chunk_overlap: 180,
      retrieval_method: "multi_query",
      reranker: "cross_encoder",
      top_k: 10,
      min_similarity: 0.3,
    },
  },
  {
    key: "custom",
    labelKey: "knowledge_panel.pattern_custom",
    descriptionKey: "knowledge_panel.pattern_custom_desc",
    values: null,
  },
];

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
        <div className="grid grid-cols-2 gap-2">
          {PRESETS.map((p) => (
            <button
              key={p.key}
              type="button"
              data-testid={`preset-${p.key}`}
              onClick={() => applyPreset(p.key)}
              className={`p-2.5 rounded-xl border text-left transition-all ${
                preset === p.key
                  ? "border-[var(--primary)] bg-[var(--primary)]/10"
                  : "border-white/5 bg-white/[0.02] hover:bg-white/[0.04]"
              }`}
            >
              <p className="text-xs font-semibold text-white">
                {t(p.labelKey)}
              </p>
              <p className="text-[10px] text-[var(--text-muted)] mt-0.5">
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
          <div className="space-y-2">
            <input
              data-testid="input-collection-name"
              value={collectionName}
              onChange={(e) => setCollectionName(e.target.value)}
              placeholder={t("knowledge_panel.collection_name_placeholder")}
              className="w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white placeholder:text-[var(--text-muted)]"
            />
            <input
              data-testid="input-display-name"
              value={displayName}
              onChange={(e) => setDisplayName(e.target.value)}
              placeholder={t("knowledge_panel.display_name_placeholder")}
              className="w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white placeholder:text-[var(--text-muted)]"
            />
            <textarea
              data-testid="input-description"
              value={description}
              onChange={(e) => setDescription(e.target.value)}
              placeholder={t("knowledge_panel.description_placeholder")}
              rows={2}
              className="w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white placeholder:text-[var(--text-muted)] resize-none"
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
                onClick={() => handleCardClick(col)}
                className={`w-full text-left p-3 rounded-xl border transition-all ${
                  isSelected
                    ? "border-[var(--primary)] bg-[var(--primary)]/5"
                    : "border-white/5 bg-white/[0.02] hover:bg-white/[0.04]"
                }`}
              >
                <div className="flex items-center gap-2">
                  <Database
                    size={14}
                    className="text-indigo-400 flex-shrink-0"
                  />
                  <span className="text-sm font-medium text-white truncate">
                    {col.display_name || col.collection_name}
                  </span>
                </div>
                <div className="flex items-center gap-1.5 mt-1.5 ml-[22px]">
                  <span className="text-[10px] bg-white/5 px-2 py-0.5 rounded text-[var(--text-muted)]">
                    {col.document_count} docs
                  </span>
                  <span className="text-[10px] bg-white/5 px-2 py-0.5 rounded text-[var(--text-muted)]">
                    {col.chunk_strategy}
                  </span>
                  <span className="text-[10px] bg-white/5 px-2 py-0.5 rounded text-[var(--text-muted)]">
                    {col.retrieval_method}
                  </span>
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

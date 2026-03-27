/**
 * RAG 設定ビュー.
 *
 * コレクション選択 → 現在の設定表示 + テストクエリ。
 */

import React, { useState } from "react";
import { useKnowledgeStore } from "../../store/useKnowledgeStore";
import { useI18n } from "../../i18n";
import { knowledgeApi } from "../../api/knowledge";

export const KnowledgeRAGConfig: React.FC = () => {
  const { t } = useI18n();
  const { collections, selectedCollection, setSelectedCollection } =
    useKnowledgeStore();
  const [testQuery, setTestQuery] = useState("");
  const [topK, setTopK] = useState(5);
  const [queryResult, setQueryResult] = useState<Record<
    string,
    unknown
  > | null>(null);
  const [querying, setQuerying] = useState(false);

  const selected = collections.find(
    (c) => c.collection_name === selectedCollection,
  );

  const handleTestQuery = async () => {
    if (!selectedCollection || !testQuery.trim()) return;
    setQuerying(true);
    try {
      const result = await knowledgeApi.testQuery(
        selectedCollection,
        testQuery,
        topK,
      );
      setQueryResult(result);
    } catch (e) {
      setQueryResult({
        error: e instanceof Error ? e.message : "Unknown error",
      });
    } finally {
      setQuerying(false);
    }
  };

  return (
    <div className="space-y-6">
      {/* コレクション選択 */}
      <div className="bg-[#12121a] rounded-xl border border-white/5 p-4">
        <label className="block text-xs text-slate-500 mb-2">
          {t("kb.select_collection")}
        </label>
        <select
          value={selectedCollection || ""}
          onChange={(e) => setSelectedCollection(e.target.value || null)}
          className="w-full px-3 py-2 bg-[#0a0a0f] border border-white/10 rounded-lg text-sm text-white focus:outline-none focus:border-indigo-500"
        >
          <option value="">{t("kb.choose_collection")}</option>
          {collections.map((c) => (
            <option key={c.collection_name} value={c.collection_name}>
              {c.display_name || c.collection_name}
            </option>
          ))}
        </select>
      </div>

      {selected ? (
        <>
          {/* 現在の設定 */}
          <div className="bg-[#12121a] rounded-xl border border-white/5 p-6">
            <h2 className="text-sm font-medium text-slate-300 mb-4">
              {t("kb.current_settings")}
            </h2>
            <div className="grid grid-cols-2 gap-3">
              <ConfigRow
                label={t("kb.chunk_strategy")}
                value={selected.chunk_strategy}
              />
              <ConfigRow
                label={t("kb.chunk_size")}
                value={String(selected.chunk_size)}
              />
              <ConfigRow
                label={t("kb.chunk_overlap")}
                value={String(selected.chunk_overlap)}
              />
              <ConfigRow
                label={t("kb.retrieval_method")}
                value={selected.retrieval_method}
              />
              <ConfigRow
                label={t("kb.reranker")}
                value={selected.reranker || t("kb.none")}
              />
              <ConfigRow label="Top-K" value={String(selected.top_k)} />
              <ConfigRow
                label={t("kb.min_similarity")}
                value={String(selected.min_similarity)}
              />
              <ConfigRow
                label={t("kb.embedding_model")}
                value={selected.embedding_model || "default"}
              />
            </div>
          </div>

          {/* テストクエリ */}
          <div className="bg-[#12121a] rounded-xl border border-white/5 p-6">
            <h2 className="text-sm font-medium text-slate-300 mb-4">
              {t("kb.test_query")}
            </h2>
            <div className="flex gap-2 mb-4">
              <input
                value={testQuery}
                onChange={(e) => setTestQuery(e.target.value)}
                placeholder={t("kb.test_query_placeholder")}
                className="flex-1 px-3 py-2 bg-[#0a0a0f] border border-white/10 rounded-lg text-sm text-white placeholder-slate-600 focus:outline-none focus:border-indigo-500"
                onKeyDown={(e) => e.key === "Enter" && handleTestQuery()}
              />
              <input
                type="number"
                value={topK}
                onChange={(e) => setTopK(Number(e.target.value))}
                min={1}
                max={50}
                className="w-16 px-2 py-2 bg-[#0a0a0f] border border-white/10 rounded-lg text-sm text-white text-center focus:outline-none focus:border-indigo-500"
              />
              <button
                onClick={handleTestQuery}
                disabled={querying || !testQuery.trim()}
                className="px-4 py-2 bg-indigo-600 hover:bg-indigo-500 disabled:opacity-40 rounded-lg text-xs font-medium transition-colors"
              >
                {querying ? "..." : t("kb.run_query")}
              </button>
            </div>

            {queryResult && (
              <pre className="bg-[#0a0a0f] rounded-lg p-4 text-xs text-slate-400 overflow-x-auto max-h-60 custom-scrollbar">
                {JSON.stringify(queryResult, null, 2)}
              </pre>
            )}
          </div>
        </>
      ) : (
        <div className="text-center py-12 text-slate-500 text-sm">
          {t("kb.select_collection_prompt")}
        </div>
      )}
    </div>
  );
};

/** 設定行 */
const ConfigRow: React.FC<{ label: string; value: string }> = ({
  label,
  value,
}) => (
  <div className="bg-[#0a0a0f] rounded-lg p-3 border border-white/5">
    <p className="text-[10px] text-slate-600 mb-0.5">{label}</p>
    <p className="text-sm text-white font-medium">{value}</p>
  </div>
);

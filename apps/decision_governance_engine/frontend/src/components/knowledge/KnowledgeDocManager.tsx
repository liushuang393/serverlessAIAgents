/**
 * ドキュメント管理ページ.
 *
 * コレクション選択 → ファイルアップロード → チャンクプレビュー → インデックス。
 */

import React, { useCallback, useEffect, useRef, useState } from "react";
import { useKnowledgeStore } from "../../store/useKnowledgeStore";
import { useI18n } from "../../i18n";

/** ドキュメントステータスに対応するアイコン */
const STATUS_ICON: Record<string, string> = {
  uploaded: "📄",
  chunked: "🔪",
  indexed: "✅",
  error: "❌",
};

export const KnowledgeDocManager: React.FC = () => {
  const { t } = useI18n();
  const {
    collections,
    selectedCollection,
    setSelectedCollection,
    documents,
    fetchDocuments,
    uploadDocument,
    deleteDocument,
    indexDocument,
    previewChunks,
    chunkPreviews,
    loading,
  } = useKnowledgeStore();
  const fileRef = useRef<HTMLInputElement>(null);
  const [dragOver, setDragOver] = useState(false);

  useEffect(() => {
    if (selectedCollection) fetchDocuments(selectedCollection);
  }, [selectedCollection, fetchDocuments]);

  /** ファイルアップロード */
  const handleFiles = useCallback(
    async (files: FileList | null) => {
      if (!files || !selectedCollection) return;
      for (const file of Array.from(files)) {
        await uploadDocument(selectedCollection, file);
      }
    },
    [selectedCollection, uploadDocument],
  );

  const handleDrop = useCallback(
    (e: React.DragEvent) => {
      e.preventDefault();
      setDragOver(false);
      handleFiles(e.dataTransfer.files);
    },
    [handleFiles],
  );

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

      {selectedCollection ? (
        <>
          {/* アップロード */}
          <div
            className={`bg-[#12121a] rounded-xl border-2 border-dashed p-8 text-center transition-colors ${
              dragOver ? "border-indigo-500 bg-indigo-500/5" : "border-white/10"
            }`}
            onDragOver={(e) => {
              e.preventDefault();
              setDragOver(true);
            }}
            onDragLeave={() => setDragOver(false)}
            onDrop={handleDrop}
          >
            <p className="text-slate-400 text-sm mb-2">{t("kb.drop_files")}</p>
            <p className="text-xs text-slate-600 mb-3">
              PDF, DOCX, CSV, TXT, MD, JSON
            </p>
            <button
              onClick={() => fileRef.current?.click()}
              className="px-4 py-2 bg-indigo-600 hover:bg-indigo-500 rounded-lg text-xs font-medium transition-colors"
            >
              {t("kb.browse_files")}
            </button>
            <input
              ref={fileRef}
              type="file"
              multiple
              className="hidden"
              onChange={(e) => handleFiles(e.target.files)}
            />
          </div>

          {/* ドキュメント一覧 */}
          <div className="bg-[#12121a] rounded-xl border border-white/5 p-6">
            <h2 className="text-sm font-medium text-slate-300 mb-4">
              {t("kb.documents")}
            </h2>
            {loading ? (
              <div className="text-center py-6 text-slate-500">
                {t("common.loading")}
              </div>
            ) : documents.length === 0 ? (
              <div className="text-center py-6 text-slate-500">
                {t("kb.no_documents")}
              </div>
            ) : (
              <div className="space-y-2">
                {documents.map((doc) => (
                  <div
                    key={doc.document_id}
                    className="flex items-center justify-between bg-[#0a0a0f] rounded-lg p-3 border border-white/5 group"
                  >
                    <div className="flex items-center gap-3 flex-1 min-w-0">
                      <span className="text-lg">
                        {STATUS_ICON[doc.status] || "📄"}
                      </span>
                      <div className="min-w-0">
                        <p className="text-sm text-white truncate">
                          {doc.filename}
                        </p>
                        <div className="flex gap-3 text-[10px] text-slate-600 mt-0.5">
                          <span>{doc.status}</span>
                          <span>{doc.chunk_count} chunks</span>
                          <span>{(doc.file_size / 1024).toFixed(1)} KB</span>
                        </div>
                      </div>
                    </div>
                    <div className="flex gap-1 opacity-0 group-hover:opacity-100 transition-opacity">
                      <button
                        onClick={() =>
                          previewChunks(selectedCollection, doc.document_id)
                        }
                        className="px-2 py-1 text-[10px] bg-slate-800 hover:bg-slate-700 rounded text-slate-300 transition-colors"
                      >
                        {t("kb.preview_chunks")}
                      </button>
                      {doc.status !== "indexed" && (
                        <button
                          onClick={() =>
                            indexDocument(selectedCollection, doc.document_id)
                          }
                          className="px-2 py-1 text-[10px] bg-indigo-600/30 hover:bg-indigo-600/50 rounded text-indigo-300 transition-colors"
                        >
                          {t("kb.index_document")}
                        </button>
                      )}
                      <button
                        onClick={() =>
                          deleteDocument(selectedCollection, doc.document_id)
                        }
                        className="px-2 py-1 text-[10px] bg-red-500/20 hover:bg-red-500/30 rounded text-red-400 transition-colors"
                      >
                        {t("kb.delete")}
                      </button>
                    </div>
                  </div>
                ))}
              </div>
            )}
          </div>

          {/* チャンクプレビュー */}
          {chunkPreviews.length > 0 && (
            <div className="bg-[#12121a] rounded-xl border border-white/5 p-6">
              <h2 className="text-sm font-medium text-slate-300 mb-4">
                {t("kb.chunk_preview")} ({chunkPreviews.length})
              </h2>
              <div className="space-y-2 max-h-80 overflow-y-auto custom-scrollbar">
                {chunkPreviews.map((chunk) => (
                  <div
                    key={chunk.index}
                    className="bg-[#0a0a0f] rounded-lg p-3 border border-white/5"
                  >
                    <span className="text-[10px] text-indigo-400 font-mono">
                      #{chunk.index}
                    </span>
                    <p className="text-xs text-slate-400 mt-1 whitespace-pre-wrap break-words line-clamp-4">
                      {chunk.text}
                    </p>
                  </div>
                ))}
              </div>
            </div>
          )}
        </>
      ) : (
        <div className="text-center py-12 text-slate-500 text-sm">
          {t("kb.select_collection_prompt")}
        </div>
      )}
    </div>
  );
};

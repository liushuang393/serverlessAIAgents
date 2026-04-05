/**
 * パネル内ドキュメント管理コンポーネント.
 *
 * コレクション選択、ドラッグ＆ドロップアップロード（バッチ対応）、
 * ディレクトリロード、ドキュメント一覧表示（グループ表示付き）、
 * チャンクプレビューを提供する。
 */
import { useCallback, useEffect, useRef, useState } from "react";
import type { JSX } from "react";
import {
  Upload,
  Trash2,
  Eye,
  RotateCcw,
  FileText,
  CheckCircle,
  X,
  FolderOpen,
  Loader2,
  Tag,
} from "lucide-react";
import { useI18n } from "../../i18n";
import { useRAGStore } from "../../stores/ragStore";

/** ステータスバッジのスタイル定義 */
const STATUS_BADGE: Record<
  string,
  { bg: string; text: string; label: string }
> = {
  uploaded: {
    bg: "bg-amber-500/20",
    text: "text-amber-300",
    label: "uploaded",
  },
  chunked: { bg: "bg-blue-500/20", text: "text-blue-300", label: "chunked" },
  indexed: {
    bg: "bg-emerald-500/20",
    text: "text-emerald-300",
    label: "indexed",
  },
  error: { bg: "bg-rose-500/20", text: "text-rose-300", label: "error" },
};

const ACCEPTED_FORMATS =
  ".pdf,.docx,.doc,.xlsx,.xls,.csv,.txt,.md,.json,.jsonl,.html,.htm";

const FORMAT_LABELS = [
  { ext: "PDF", color: "text-red-400" },
  { ext: "Word", color: "text-blue-400" },
  { ext: "Excel", color: "text-green-400" },
  { ext: "CSV", color: "text-yellow-400" },
  { ext: "Markdown", color: "text-purple-400" },
  { ext: "Text", color: "text-gray-400" },
  { ext: "JSON", color: "text-orange-400" },
  { ext: "HTML", color: "text-cyan-400" },
];

function createBatchGroupId(): string {
  if (
    typeof globalThis.crypto !== "undefined" &&
    typeof globalThis.crypto.randomUUID === "function"
  ) {
    return globalThis.crypto.randomUUID();
  }
  return `group-${Date.now().toString(36)}`;
}

function parseTagInput(value: string): string[] {
  return value
    .split(",")
    .map((tag) => tag.trim())
    .filter((tag) => tag.length > 0);
}

/** パネル内ドキュメント管理 */
export function PanelDocuments(): JSX.Element {
  const { t } = useI18n();
  const {
    collections,
    fetchCollections,
    documents,
    documentsLoading,
    fetchDocuments,
    uploadDocument,
    deleteDocument,
    indexDocument,
    reindexDocument,
    chunkPreviews,
    chunksLoading,
    previewChunks,
    loadDirectory,
    directoryLoading,
    directoryLoadResult,
    clearDirectoryResult,
  } = useRAGStore();

  const fileInputRef = useRef<HTMLInputElement>(null);
  const [selectedCollection, setSelectedCollection] = useState<string>("");
  const [uploading, setUploading] = useState(false);
  const [autoIndex, setAutoIndex] = useState(false);
  const [previewDocId, setPreviewDocId] = useState<string | null>(null);
  const [dragOver, setDragOver] = useState(false);
  const [uploadError, setUploadError] = useState<string | null>(null);
  const [scenarioId, setScenarioId] = useState("");
  const [tagInput, setTagInput] = useState("");
  const [lastBatchGroupId, setLastBatchGroupId] = useState<string | null>(null);

  // ディレクトリロードフォーム
  const [showDirLoad, setShowDirLoad] = useState(false);
  const [dirPath, setDirPath] = useState("");
  const [dirRecursive, setDirRecursive] = useState(true);

  /** 初回マウント時にコレクション一覧を取得 */
  useEffect(() => {
    void fetchCollections();
  }, [fetchCollections]);

  /** コレクション選択変更時にドキュメント一覧を取得 */
  useEffect(() => {
    if (selectedCollection) {
      void fetchDocuments(selectedCollection);
      setPreviewDocId(null);
    }
  }, [selectedCollection, fetchDocuments]);

  /** ファイルアップロードハンドラー（バッチ対応） */
  const handleUpload = useCallback(
    async (files: FileList | null) => {
      if (!files || !selectedCollection) return;
      setUploading(true);
      setUploadError(null);
      const uploadFiles = Array.from(files);
      const tags = parseTagInput(tagInput);
      const documentGroupId = uploadFiles.length > 1 ? createBatchGroupId() : undefined;
      if (documentGroupId) {
        setLastBatchGroupId(documentGroupId);
      } else {
        setLastBatchGroupId(null);
      }
      try {
        for (const file of uploadFiles) {
          await uploadDocument(selectedCollection, file, autoIndex, {
            ...(documentGroupId ? { document_group_id: documentGroupId } : {}),
            ...(scenarioId.trim() ? { scenario_id: scenarioId.trim() } : {}),
            ...(tags.length > 0 ? { tags } : {}),
          });
        }
      } catch (e: unknown) {
        const message = e instanceof Error ? e.message : String(e);
        setUploadError(message);
      } finally {
        setUploading(false);
        if (fileInputRef.current) fileInputRef.current.value = "";
      }
    },
    [selectedCollection, uploadDocument, autoIndex, scenarioId, tagInput],
  );

  /** ドラッグ＆ドロップハンドラー */
  const handleDrop = useCallback(
    (e: React.DragEvent) => {
      e.preventDefault();
      setDragOver(false);
      void handleUpload(e.dataTransfer.files);
    },
    [handleUpload],
  );

  /** 削除確認ダイアログ付き削除 */
  const handleDelete = useCallback(
    (docId: string) => {
      if (window.confirm(t("knowledge_panel.confirm_delete_document"))) {
        void deleteDocument(selectedCollection, docId);
      }
    },
    [selectedCollection, deleteDocument, t],
  );

  /** ディレクトリロード実行 */
  const handleLoadDirectory = useCallback(async () => {
    if (!dirPath.trim() || !selectedCollection) return;
    try {
      await loadDirectory({
        directory: dirPath.trim(),
        collection: selectedCollection,
        recursive: dirRecursive,
        auto_group: true,
      });
      await fetchDocuments(selectedCollection);
    } catch {
      // エラーは store 側で処理済み
    }
  }, [dirPath, selectedCollection, dirRecursive, loadDirectory, fetchDocuments]);

  /** ドキュメントをグループ別に整理 */
  const groupedDocs = documents.reduce<
    Record<string, typeof documents>
  >((acc, doc) => {
    const gid = doc.document_group_id ?? "__ungrouped__";
    if (!acc[gid]) acc[gid] = [];
    acc[gid].push(doc);
    return acc;
  }, {});

  const hasGroups = Object.keys(groupedDocs).length > 1 ||
    !groupedDocs["__ungrouped__"];

  return (
    <div className="space-y-4" data-testid="panel-documents">
      {/* コレクション選択 */}
      <div>
        <label className="block text-xs text-[var(--text-muted)] mb-1.5">
          {t("knowledge_panel.collection_label")}
        </label>
        <select
          data-testid="collection-select"
          value={selectedCollection}
          onChange={(e) => setSelectedCollection(e.target.value)}
          className="w-full px-4 py-3 rounded-xl bg-white/5 border border-white/10 text-sm text-white focus:border-[var(--primary)]/50 transition-colors cursor-pointer"
        >
          <option value="">{t("knowledge_panel.select_collection")}</option>
          {collections.map((c) => (
            <option key={c.collection_name} value={c.collection_name}>
              {c.display_name || c.collection_name}
            </option>
          ))}
        </select>
      </div>

      {!selectedCollection && (
        <div className="text-center py-8 text-[var(--text-muted)] text-sm">
          {t("knowledge_panel.select_collection_prompt")}
        </div>
      )}

      {selectedCollection && (
        <>
          {/* 対応形式一覧 */}
          <div className="flex flex-wrap gap-1.5">
            {FORMAT_LABELS.map((f) => (
              <span
                key={f.ext}
                className={`px-2 py-0.5 rounded-md text-[10px] font-medium bg-white/5 border border-white/10 ${f.color}`}
              >
                {f.ext}
              </span>
            ))}
          </div>

          {/* ドラッグ＆ドロップアップロードエリア */}
          <div
            data-testid="upload-area"
            onDragOver={(e) => {
              e.preventDefault();
              setDragOver(true);
            }}
            onDragLeave={() => setDragOver(false)}
            onDrop={handleDrop}
            className={`border-2 border-dashed rounded-xl p-10 text-center transition-all ${dragOver
              ? "border-[var(--primary)]/40 bg-[var(--primary)]/5 shadow-[inset_0_0_20px_rgba(94,234,212,0.05)]"
              : "border-white/10 hover:border-[var(--primary)]/30 hover:bg-white/[0.02]"
              }`}
          >
            <Upload
              size={24}
              className="mx-auto mb-2 text-[var(--text-muted)]"
            />
            <p className="text-sm text-white mb-1">
              {t("knowledge_panel.upload_area")}
            </p>
            <p className="text-xs text-[var(--text-muted)] mb-3">
              PDF, Word, Excel, CSV, Markdown, Text, JSON, HTML
            </p>
            <input
              ref={fileInputRef}
              type="file"
              multiple
              accept={ACCEPTED_FORMATS}
              onChange={(e) => void handleUpload(e.target.files)}
              className="hidden"
              data-testid="file-input"
            />
            <button
              onClick={() => fileInputRef.current?.click()}
              disabled={uploading}
              className="px-4 py-1.5 rounded-lg bg-[var(--primary)]/10 text-[var(--primary)] text-xs font-medium border border-[var(--primary)]/20 hover:bg-[var(--primary)]/20 transition disabled:opacity-50"
            >
              {uploading
                ? t("knowledge_panel.uploading")
                : t("knowledge_panel.select_file")}
            </button>
          </div>

          <div className="grid gap-3 md:grid-cols-2">
            <div>
              <label className="mb-1.5 block text-xs text-[var(--text-muted)]">
                {t("knowledge_panel.scenario_id")}
              </label>
              <input
                data-testid="scenario-id-input"
                type="text"
                value={scenarioId}
                onChange={(e) => setScenarioId(e.target.value)}
                placeholder={t("knowledge_panel.scenario_id_placeholder")}
                className="w-full rounded-xl border border-white/10 bg-white/5 px-4 py-3 text-sm text-white placeholder:text-[var(--text-muted)]"
              />
            </div>
            <div>
              <label className="mb-1.5 block text-xs text-[var(--text-muted)]">
                {t("knowledge_panel.tags")}
              </label>
              <input
                data-testid="tag-input"
                type="text"
                value={tagInput}
                onChange={(e) => setTagInput(e.target.value)}
                placeholder={t("knowledge_panel.tags_placeholder")}
                className="w-full rounded-xl border border-white/10 bg-white/5 px-4 py-3 text-sm text-white placeholder:text-[var(--text-muted)]"
              />
            </div>
          </div>

          {lastBatchGroupId && (
            <div
              data-testid="batch-group-notice"
              className="rounded-xl border border-[var(--primary)]/20 bg-[var(--primary)]/5 px-4 py-3 text-xs text-[var(--text-muted)]"
            >
              <span className="font-medium text-[var(--primary)]">
                {t("knowledge_panel.batch_group_created")}
              </span>{" "}
              <span>{lastBatchGroupId}</span>
            </div>
          )}

          {/* オプション行 */}
          <div className="flex items-center justify-between gap-4">
            <label className="flex items-center gap-2 text-xs text-[var(--text-muted)] cursor-pointer">
              <input
                type="checkbox"
                data-testid="auto-index-checkbox"
                checked={autoIndex}
                onChange={(e) => setAutoIndex(e.target.checked)}
                className="rounded border-white/20"
              />
              {t("knowledge_panel.auto_index")}
            </label>

            {/* ディレクトリロードトグル */}
            <button
              data-testid="btn-toggle-dir-load"
              onClick={() => {
                setShowDirLoad((prev) => !prev);
                clearDirectoryResult();
              }}
              className="flex items-center gap-1.5 px-3 py-1.5 rounded-lg text-xs text-[var(--text-muted)] hover:text-white hover:bg-white/5 transition border border-white/10"
            >
              <FolderOpen size={14} />
              ディレクトリロード
            </button>
          </div>

          {/* ディレクトリロードフォーム */}
          {showDirLoad && (
            <div
              data-testid="dir-load-form"
              className="rounded-xl glass border border-white/10 p-4 space-y-3"
            >
              <h4 className="text-xs font-semibold text-white flex items-center gap-1.5">
                <FolderOpen size={14} className="text-[var(--primary)]" />
                ナレッジベース ディレクトリロード
              </h4>
              <input
                data-testid="dir-path-input"
                type="text"
                value={dirPath}
                onChange={(e) => setDirPath(e.target.value)}
                placeholder="例: /data/knowledge_base"
                className="w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white placeholder:text-[var(--text-muted)]"
              />
              <label className="flex items-center gap-2 text-xs text-[var(--text-muted)] cursor-pointer">
                <input
                  type="checkbox"
                  checked={dirRecursive}
                  onChange={(e) => setDirRecursive(e.target.checked)}
                  className="rounded border-white/20"
                />
                サブディレクトリを再帰スキャン
              </label>

              <div className="flex gap-2">
                <button
                  data-testid="btn-load-directory"
                  onClick={() => void handleLoadDirectory()}
                  disabled={directoryLoading || !dirPath.trim()}
                  className="flex items-center gap-1.5 px-3 py-1.5 rounded-lg bg-[var(--primary)]/10 text-[var(--primary)] text-xs font-medium border border-[var(--primary)]/20 hover:bg-[var(--primary)]/20 transition disabled:opacity-50"
                >
                  {directoryLoading && <Loader2 size={12} className="animate-spin" />}
                  {directoryLoading ? "ロード中..." : "ロード実行"}
                </button>
                <button
                  onClick={() => {
                    void loadDirectory({
                      directory: dirPath.trim(),
                      collection: selectedCollection,
                      recursive: dirRecursive,
                      dry_run: true,
                    });
                  }}
                  disabled={directoryLoading || !dirPath.trim()}
                  className="px-3 py-1.5 rounded-lg text-xs text-[var(--text-muted)] border border-white/10 hover:bg-white/5 transition disabled:opacity-50"
                >
                  プレビュー（dry run）
                </button>
              </div>

              {/* ディレクトリロード結果 */}
              {directoryLoadResult && (
                <div
                  data-testid="dir-load-result"
                  className="rounded-lg bg-white/[0.02] border border-white/5 p-3 text-xs space-y-2"
                >
                  <div className="flex items-center gap-2">
                    <span
                      className={`px-2 py-0.5 rounded-full font-medium ${directoryLoadResult.status === "success"
                        ? "bg-emerald-500/20 text-emerald-300"
                        : directoryLoadResult.status === "dry_run"
                          ? "bg-blue-500/20 text-blue-300"
                          : directoryLoadResult.status === "partial"
                            ? "bg-amber-500/20 text-amber-300"
                            : "bg-rose-500/20 text-rose-300"
                        }`}
                    >
                      {directoryLoadResult.status}
                    </span>
                    <span className="text-[var(--text-muted)]">
                      {directoryLoadResult.total_files} ファイル
                    </span>
                    {directoryLoadResult.success != null && (
                      <span className="text-emerald-400">
                        成功: {directoryLoadResult.success}
                      </span>
                    )}
                    {(directoryLoadResult.errors ?? 0) > 0 && (
                      <span className="text-rose-400">
                        エラー: {directoryLoadResult.errors}
                      </span>
                    )}
                    {(directoryLoadResult.skipped ?? 0) > 0 && (
                      <span className="text-[var(--text-muted)]">
                        スキップ: {directoryLoadResult.skipped}
                      </span>
                    )}
                  </div>

                  {directoryLoadResult.document_group_id && (
                    <div className="flex items-center gap-1 text-[var(--text-muted)]">
                      <Tag size={10} />
                      グループ: {directoryLoadResult.document_group_id}
                    </div>
                  )}

                  {/* ファイルリスト（dry_run またはロード結果） */}
                  {(directoryLoadResult.results ?? directoryLoadResult.files) && (
                    <div className="max-h-40 overflow-y-auto space-y-1">
                      {(directoryLoadResult.results ?? directoryLoadResult.files ?? []).map(
                        (item, idx) => (
                          <div
                            key={idx}
                            className="flex items-center justify-between py-1 px-2 rounded bg-white/[0.02]"
                          >
                            <span className="text-white truncate max-w-[60%]">
                              {"filename" in item ? item.filename : ""}
                            </span>
                            <span className="text-[var(--text-muted)]">
                              {((item.size ?? 0) / 1024).toFixed(1)} KB
                            </span>
                            {"status" in item && (
                              <span
                                className={`px-1.5 py-0.5 rounded text-[10px] ${item.status === "success"
                                  ? "text-emerald-400"
                                  : item.status === "skipped"
                                    ? "text-yellow-400"
                                    : "text-rose-400"
                                  }`}
                              >
                                {item.status}
                              </span>
                            )}
                          </div>
                        ),
                      )}
                    </div>
                  )}
                </div>
              )}
            </div>
          )}

          {/* アップロードエラー */}
          {uploadError && (
            <div
              data-testid="upload-error"
              className="flex items-center justify-between p-3 rounded-xl bg-rose-500/10 border border-rose-500/20 text-xs text-rose-400"
            >
              <span>
                {t("knowledge_panel.error_prefix")}: {uploadError}
              </span>
              <button
                onClick={() => setUploadError(null)}
                className="ml-2 hover:text-rose-300 transition"
              >
                <X size={14} />
              </button>
            </div>
          )}

          {/* ドキュメント一覧 */}
          {documentsLoading && documents.length === 0 && (
            <div className="text-center py-6 text-[var(--text-muted)] text-sm">
              {t("knowledge_panel.loading")}
            </div>
          )}

          {documents.length === 0 && !documentsLoading && (
            <div className="text-center py-6 text-[var(--text-muted)] text-sm">
              {t("knowledge_panel.no_documents")}
            </div>
          )}

          <div className="space-y-1.5" data-testid="document-list">
            {hasGroups
              ? Object.entries(groupedDocs).map(([gid, docs]) => (
                <div key={gid} className="space-y-1">
                  {gid !== "__ungrouped__" && (
                    <div className="flex items-center gap-1.5 px-2 py-1 text-[10px] text-[var(--text-muted)]">
                      <Tag size={10} className="text-[var(--primary)]" />
                      <span>グループ: {gid.slice(0, 8)}...</span>
                      <span className="text-white/30">({docs.length} ファイル)</span>
                    </div>
                  )}
                  {docs.map((doc) => (
                    <DocumentRow
                      key={doc.document_id}
                      doc={doc}
                      selectedCollection={selectedCollection}
                      onPreview={(docId) => {
                        setPreviewDocId(docId);
                        void previewChunks(selectedCollection, docId);
                      }}
                      onIndex={(docId) => void indexDocument(selectedCollection, docId)}
                      onReindex={(docId) => void reindexDocument(selectedCollection, docId)}
                      onDelete={handleDelete}
                      t={t}
                    />
                  ))}
                </div>
              ))
              : documents.map((doc) => (
                <DocumentRow
                  key={doc.document_id}
                  doc={doc}
                  selectedCollection={selectedCollection}
                  onPreview={(docId) => {
                    setPreviewDocId(docId);
                    void previewChunks(selectedCollection, docId);
                  }}
                  onIndex={(docId) => void indexDocument(selectedCollection, docId)}
                  onReindex={(docId) => void reindexDocument(selectedCollection, docId)}
                  onDelete={handleDelete}
                  t={t}
                />
              ))}
          </div>

          {/* チャンクプレビュー */}
          {previewDocId && (
            <div
              data-testid="chunk-preview"
              className="rounded-xl glass border border-white/10 p-4"
            >
              <div className="flex items-center justify-between mb-3">
                <h3 className="text-sm font-semibold text-white">
                  {t("knowledge_panel.chunk_preview")}
                  {!chunksLoading && chunkPreviews.length > 0 && (
                    <span className="ml-2 text-xs text-[var(--text-muted)] font-normal">
                      ({chunkPreviews.length}{" "}
                      {t("knowledge_panel.chunks_count")})
                    </span>
                  )}
                </h3>
                <button
                  data-testid="btn-close-preview"
                  onClick={() => setPreviewDocId(null)}
                  className="p-1 rounded-lg hover:bg-white/10 text-[var(--text-muted)] hover:text-white transition"
                >
                  <X size={14} />
                </button>
              </div>

              {chunksLoading ? (
                <p className="text-xs text-[var(--text-muted)]">
                  {t("knowledge_panel.loading")}
                </p>
              ) : chunkPreviews.length === 0 ? (
                <p className="text-xs text-[var(--text-muted)]">
                  {t("knowledge_panel.no_chunks")}
                </p>
              ) : (
                <div className="space-y-2 max-h-64 overflow-y-auto">
                  {chunkPreviews.map((chunk) => (
                    <div
                      key={chunk.index}
                      className="p-2.5 rounded-lg bg-white/[0.02] border border-white/5 text-xs"
                    >
                      <div className="flex justify-between text-[var(--text-muted)] mb-1">
                        <span>Chunk #{chunk.index}</span>
                        <span>
                          {chunk.length} chars &middot; {chunk.strategy}
                        </span>
                      </div>
                      <p className="text-white font-mono text-xs whitespace-pre-wrap line-clamp-3">
                        {chunk.content.slice(0, 200)}
                        {chunk.content.length > 200 ? "..." : ""}
                      </p>
                    </div>
                  ))}
                </div>
              )}
            </div>
          )}
        </>
      )}
    </div>
  );
}

/** ドキュメント行コンポーネント */
function DocumentRow({
  doc,
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  selectedCollection: _unused,
  onPreview,
  onIndex,
  onReindex,
  onDelete,
  t,
}: {
  doc: import("../../api/rag").DocumentInfo;
  selectedCollection: string;
  onPreview: (docId: string) => void;
  onIndex: (docId: string) => void;
  onReindex: (docId: string) => void;
  onDelete: (docId: string) => void;
  t: (key: string) => string;
}): JSX.Element {
  const badge = STATUS_BADGE[doc.status] ?? STATUS_BADGE.uploaded;
  return (
    <div
      data-testid={`doc-item-${doc.document_id}`}
      className="flex items-center gap-4 p-4 rounded-xl bg-white/[0.02] border border-white/5 group hover:bg-white/[0.04] transition-colors"
    >
      <div className="w-10 h-10 rounded-lg bg-white/5 flex items-center justify-center flex-shrink-0 group-hover:bg-white/10 transition-colors">
        <FileText
          size={18}
          className="text-[var(--text-muted)] group-hover:text-[var(--primary)]"
        />
      </div>
      <div className="flex-1 min-w-0">
        <p className="text-sm text-white truncate">{doc.filename}</p>
        <div className="flex items-center gap-2 text-xs text-[var(--text-muted)]">
          <span>{(doc.file_size / 1024).toFixed(1)} KB</span>
          {doc.chunk_count > 0 && <span>&middot; {doc.chunk_count} chunks</span>}
          {doc.document_group_id && (
            <span className="flex items-center gap-0.5">
              <Tag size={9} />
              {doc.document_group_id.slice(0, 6)}
            </span>
          )}
          {doc.tags && doc.tags.length > 0 && (
            <span className="text-[var(--primary)]">
              {doc.tags.slice(0, 2).join(", ")}
            </span>
          )}
        </div>
      </div>

      {/* ステータスバッジ */}
      <span
        data-testid={`status-badge-${doc.document_id}`}
        className={`px-2 py-0.5 rounded-full text-xs font-medium ${badge.bg} ${badge.text}`}
      >
        {badge.label}
      </span>

      {/* アクション */}
      <div className="flex items-center gap-0.5 opacity-0 group-hover:opacity-100 transition">
        <button
          data-testid={`btn-preview-${doc.document_id}`}
          onClick={() => onPreview(doc.document_id)}
          className="p-1.5 rounded-lg hover:bg-white/10 text-[var(--text-muted)] hover:text-white transition"
          title={t("knowledge_panel.chunk_preview_tooltip")}
        >
          <Eye size={14} />
        </button>

        {doc.status === "uploaded" && (
          <button
            data-testid={`btn-index-${doc.document_id}`}
            onClick={() => onIndex(doc.document_id)}
            className="p-1.5 rounded-lg hover:bg-emerald-500/10 text-[var(--text-muted)] hover:text-emerald-400 transition"
            title={t("knowledge_panel.index_tooltip")}
          >
            <CheckCircle size={14} />
          </button>
        )}

        {doc.status === "indexed" && (
          <button
            data-testid={`btn-reindex-${doc.document_id}`}
            onClick={() => onReindex(doc.document_id)}
            className="p-1.5 rounded-lg hover:bg-blue-500/10 text-[var(--text-muted)] hover:text-blue-400 transition"
            title={t("knowledge_panel.reindex_tooltip")}
          >
            <RotateCcw size={14} />
          </button>
        )}

        <button
          data-testid={`btn-delete-${doc.document_id}`}
          onClick={() => onDelete(doc.document_id)}
          className="p-1.5 rounded-lg hover:bg-red-500/10 text-[var(--text-muted)] hover:text-red-400 transition"
          title={t("knowledge_panel.delete_tooltip")}
        >
          <Trash2 size={14} />
        </button>
      </div>
    </div>
  );
}

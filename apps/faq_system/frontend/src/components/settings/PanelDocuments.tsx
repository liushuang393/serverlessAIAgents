/**
 * パネル内ドキュメント管理コンポーネント.
 *
 * コレクション選択、ドラッグ＆ドロップアップロード、
 * ドキュメント一覧表示、チャンクプレビューを提供する。
 */
import { useCallback, useEffect, useRef, useState } from 'react';
import type { JSX } from 'react';
import {
  Upload,
  Trash2,
  Eye,
  RotateCcw,
  FileText,
  CheckCircle,
  X,
} from 'lucide-react';
import { useI18n } from '../../i18n';
import { useRAGStore } from '../../stores/ragStore';

/** ステータスバッジのスタイル定義 */
const STATUS_BADGE: Record<string, { bg: string; text: string; label: string }> = {
  uploaded: { bg: 'bg-amber-500/20', text: 'text-amber-300', label: 'uploaded' },
  chunked: { bg: 'bg-blue-500/20', text: 'text-blue-300', label: 'chunked' },
  indexed: { bg: 'bg-emerald-500/20', text: 'text-emerald-300', label: 'indexed' },
  error: { bg: 'bg-rose-500/20', text: 'text-rose-300', label: 'error' },
};

const ACCEPTED_FORMATS = '.pdf,.docx,.doc,.csv,.txt,.md,.json';

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
  } = useRAGStore();

  const fileInputRef = useRef<HTMLInputElement>(null);
  const [selectedCollection, setSelectedCollection] = useState<string>('');
  const [uploading, setUploading] = useState(false);
  const [autoIndex, setAutoIndex] = useState(false);
  const [previewDocId, setPreviewDocId] = useState<string | null>(null);
  const [dragOver, setDragOver] = useState(false);
  const [uploadError, setUploadError] = useState<string | null>(null);

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

  /** ファイルアップロードハンドラー */
  const handleUpload = useCallback(
    async (files: FileList | null) => {
      if (!files || !selectedCollection) return;
      setUploading(true);
      setUploadError(null);
      try {
        for (const file of Array.from(files)) {
          await uploadDocument(selectedCollection, file, autoIndex);
        }
      } catch (e: unknown) {
        const message = e instanceof Error ? e.message : String(e);
        setUploadError(message);
      } finally {
        setUploading(false);
        if (fileInputRef.current) fileInputRef.current.value = '';
      }
    },
    [selectedCollection, uploadDocument, autoIndex],
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
      if (window.confirm(t('knowledge_panel.confirm_delete_document'))) {
        void deleteDocument(selectedCollection, docId);
      }
    },
    [selectedCollection, deleteDocument],
  );

  return (
    <div className="space-y-4" data-testid="panel-documents">
      {/* コレクション選択 */}
      <div>
        <label className="block text-xs text-[var(--text-muted)] mb-1.5">{t('knowledge_panel.collection_label')}</label>
        <select
          data-testid="collection-select"
          value={selectedCollection}
          onChange={(e) => setSelectedCollection(e.target.value)}
          className="w-full px-3 py-2 rounded-xl bg-white/5 border border-white/10 text-sm text-white"
        >
          <option value="">{t('knowledge_panel.select_collection')}</option>
          {collections.map((c) => (
            <option key={c.collection_name} value={c.collection_name}>
              {c.display_name || c.collection_name}
            </option>
          ))}
        </select>
      </div>

      {!selectedCollection && (
        <div className="text-center py-8 text-[var(--text-muted)] text-sm">
          {t('knowledge_panel.select_collection_prompt')}
        </div>
      )}

      {selectedCollection && (
        <>
          {/* ドラッグ＆ドロップアップロードエリア */}
          <div
            data-testid="upload-area"
            onDragOver={(e) => {
              e.preventDefault();
              setDragOver(true);
            }}
            onDragLeave={() => setDragOver(false)}
            onDrop={handleDrop}
            className={`border-2 border-dashed rounded-xl p-6 text-center transition-all ${
              dragOver
                ? 'border-[var(--primary)]/40 bg-[var(--primary)]/5'
                : 'border-white/20 hover:border-[var(--primary)]/40'
            }`}
          >
            <Upload size={24} className="mx-auto mb-2 text-[var(--text-muted)]" />
            <p className="text-sm text-white mb-1">{t('knowledge_panel.upload_area')}</p>
            <p className="text-xs text-[var(--text-muted)] mb-3">
              {t('knowledge_panel.supported_formats')}
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
              {uploading ? t('knowledge_panel.uploading') : t('knowledge_panel.select_file')}
            </button>
          </div>

          {/* 自動インデックスチェックボックス */}
          <label className="flex items-center gap-2 text-xs text-[var(--text-muted)] cursor-pointer">
            <input
              type="checkbox"
              data-testid="auto-index-checkbox"
              checked={autoIndex}
              onChange={(e) => setAutoIndex(e.target.checked)}
              className="rounded border-white/20"
            />
            {t('knowledge_panel.auto_index')}
          </label>

          {/* アップロードエラー */}
          {uploadError && (
            <div
              data-testid="upload-error"
              className="flex items-center justify-between p-3 rounded-xl bg-rose-500/10 border border-rose-500/20 text-xs text-rose-400"
            >
              <span>{t('knowledge_panel.error_prefix')}: {uploadError}</span>
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
            <div className="text-center py-6 text-[var(--text-muted)] text-sm">{t('knowledge_panel.loading')}</div>
          )}

          {documents.length === 0 && !documentsLoading && (
            <div className="text-center py-6 text-[var(--text-muted)] text-sm">
              {t('knowledge_panel.no_documents')}
            </div>
          )}

          <div className="space-y-1.5" data-testid="document-list">
            {documents.map((doc) => {
              const badge = STATUS_BADGE[doc.status] ?? STATUS_BADGE.uploaded;
              return (
                <div
                  key={doc.document_id}
                  data-testid={`doc-item-${doc.document_id}`}
                  className="flex items-center gap-2 p-3 rounded-xl bg-white/[0.02] border border-white/5 group"
                >
                  <FileText size={14} className="text-[var(--text-muted)] flex-shrink-0" />
                  <div className="flex-1 min-w-0">
                    <p className="text-sm text-white truncate">{doc.filename}</p>
                    <p className="text-xs text-[var(--text-muted)]">
                      {(doc.file_size / 1024).toFixed(1)} KB
                      {doc.chunk_count > 0 ? ` \u00B7 ${doc.chunk_count} chunks` : ''}
                    </p>
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
                    {/* プレビュー */}
                    <button
                      data-testid={`btn-preview-${doc.document_id}`}
                      onClick={() => {
                        setPreviewDocId(doc.document_id);
                        void previewChunks(selectedCollection, doc.document_id);
                      }}
                      className="p-1.5 rounded-lg hover:bg-white/10 text-[var(--text-muted)] hover:text-white transition"
                      title={t('knowledge_panel.chunk_preview_tooltip')}
                    >
                      <Eye size={14} />
                    </button>

                    {/* インデックス（uploaded 状態のみ） */}
                    {doc.status === 'uploaded' && (
                      <button
                        data-testid={`btn-index-${doc.document_id}`}
                        onClick={() => void indexDocument(selectedCollection, doc.document_id)}
                        className="p-1.5 rounded-lg hover:bg-emerald-500/10 text-[var(--text-muted)] hover:text-emerald-400 transition"
                        title={t('knowledge_panel.index_tooltip')}
                      >
                        <CheckCircle size={14} />
                      </button>
                    )}

                    {/* 再インデックス（indexed 状態のみ） */}
                    {doc.status === 'indexed' && (
                      <button
                        data-testid={`btn-reindex-${doc.document_id}`}
                        onClick={() => void reindexDocument(selectedCollection, doc.document_id)}
                        className="p-1.5 rounded-lg hover:bg-blue-500/10 text-[var(--text-muted)] hover:text-blue-400 transition"
                        title={t('knowledge_panel.reindex_tooltip')}
                      >
                        <RotateCcw size={14} />
                      </button>
                    )}

                    {/* 削除 */}
                    <button
                      data-testid={`btn-delete-${doc.document_id}`}
                      onClick={() => handleDelete(doc.document_id)}
                      className="p-1.5 rounded-lg hover:bg-red-500/10 text-[var(--text-muted)] hover:text-red-400 transition"
                      title={t('knowledge_panel.delete_tooltip')}
                    >
                      <Trash2 size={14} />
                    </button>
                  </div>
                </div>
              );
            })}
          </div>

          {/* チャンクプレビュー */}
          {previewDocId && (
            <div
              data-testid="chunk-preview"
              className="rounded-xl glass border border-white/10 p-4"
            >
              <div className="flex items-center justify-between mb-3">
                <h3 className="text-sm font-semibold text-white">
                  {t('knowledge_panel.chunk_preview')}
                  {!chunksLoading && chunkPreviews.length > 0 && (
                    <span className="ml-2 text-xs text-[var(--text-muted)] font-normal">
                      ({chunkPreviews.length} {t('knowledge_panel.chunks_count')})
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
                <p className="text-xs text-[var(--text-muted)]">{t('knowledge_panel.loading')}</p>
              ) : chunkPreviews.length === 0 ? (
                <p className="text-xs text-[var(--text-muted)]">{t('knowledge_panel.no_chunks')}</p>
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
                        {chunk.content.length > 200 ? '...' : ''}
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

/**
 * ドキュメント管理ページ.
 *
 * ドキュメントのアップロード・プレビュー・インデックス・削除を行う。
 */
import { useCallback, useEffect, useRef, useState } from 'react';
import { Upload, Trash2, Eye, RotateCcw, FileText, CheckCircle, AlertCircle, Clock } from 'lucide-react';
import { useI18n } from '../../i18n';
import { useRAGStore } from '../../stores/ragStore';

const STATUS_ICONS: Record<string, { icon: typeof CheckCircle; color: string }> = {
  uploaded: { icon: Clock, color: 'text-amber-400' },
  chunked: { icon: Eye, color: 'text-blue-400' },
  indexed: { icon: CheckCircle, color: 'text-emerald-400' },
  error: { icon: AlertCircle, color: 'text-rose-400' },
};

export function DocumentManager() {
  const { t } = useI18n();
  const {
    collections,
    selectedCollection,
    selectCollection,
    documents,
    documentsLoading,
    fetchDocuments,
    fetchCollections,
    uploadDocument,
    deleteDocument,
    indexDocument,
    chunkPreviews,
    chunksLoading,
    previewChunks,
  } = useRAGStore();

  const fileInputRef = useRef<HTMLInputElement>(null);
  const [uploading, setUploading] = useState(false);
  const [previewDocId, setPreviewDocId] = useState<string | null>(null);

  useEffect(() => {
    void fetchCollections();
  }, [fetchCollections]);

  useEffect(() => {
    if (selectedCollection) {
      void fetchDocuments(selectedCollection);
    }
  }, [selectedCollection, fetchDocuments]);

  const handleUpload = useCallback(
    async (files: FileList | null) => {
      if (!files || !selectedCollection) return;
      setUploading(true);
      try {
        for (const file of Array.from(files)) {
          await uploadDocument(selectedCollection, file);
        }
      } finally {
        setUploading(false);
        if (fileInputRef.current) fileInputRef.current.value = '';
      }
    },
    [selectedCollection, uploadDocument],
  );

  const handleDrop = useCallback(
    (e: React.DragEvent) => {
      e.preventDefault();
      void handleUpload(e.dataTransfer.files);
    },
    [handleUpload],
  );

  return (
    <div className="space-y-4">
      {/* コレクション選択 */}
      <div className="flex items-center gap-3">
        <label className="text-xs text-[var(--text-muted)]">{t('rag.select_collection')}</label>
        <select
          value={selectedCollection ?? ''}
          onChange={(e) => selectCollection(e.target.value || null)}
          className="px-3 py-1.5 rounded-lg bg-white/5 border border-white/10 text-sm text-white min-w-[200px]"
        >
          <option value="">{t('rag.choose_collection')}</option>
          {collections.map((c) => (
            <option key={c.collection_name} value={c.collection_name}>
              {c.display_name || c.collection_name}
            </option>
          ))}
        </select>
      </div>

      {!selectedCollection && (
        <div className="text-center py-16 text-[var(--text-muted)] text-sm">
          {t('rag.select_collection_prompt')}
        </div>
      )}

      {selectedCollection && (
        <>
          {/* アップロードエリア */}
          <div
            data-testid="upload-area"
            onDragOver={(e) => e.preventDefault()}
            onDrop={handleDrop}
            className="border-2 border-dashed border-white/10 rounded-xl p-8 text-center hover:border-[var(--primary)]/30 hover:bg-white/[0.02] transition"
          >
            <Upload size={28} className="mx-auto mb-3 text-[var(--text-muted)]" />
            <p className="text-sm text-white mb-1">{t('rag.drop_files')}</p>
            <p className="text-xs text-[var(--text-muted)] mb-3">{t('rag.supported_formats')}</p>
            <input
              ref={fileInputRef}
              type="file"
              multiple
              accept=".pdf,.docx,.doc,.csv,.txt,.md,.json"
              onChange={(e) => void handleUpload(e.target.files)}
              className="hidden"
            />
            <button
              onClick={() => fileInputRef.current?.click()}
              disabled={uploading}
              className="px-4 py-2 rounded-lg bg-[var(--primary)]/10 text-[var(--primary)] text-xs font-medium border border-[var(--primary)]/20 hover:bg-[var(--primary)]/20 transition disabled:opacity-50"
            >
              {uploading ? t('rag.uploading') : t('rag.browse_files')}
            </button>
          </div>

          {/* ドキュメントリスト */}
          {documentsLoading && documents.length === 0 && (
            <div className="text-center py-8 text-[var(--text-muted)] text-sm">
              {t('common.loading')}
            </div>
          )}

          {documents.length === 0 && !documentsLoading && (
            <div className="text-center py-8 text-[var(--text-muted)] text-sm">
              {t('rag.no_documents')}
            </div>
          )}

          <div className="space-y-1.5">
            {documents.map((doc) => {
              const statusInfo = STATUS_ICONS[doc.status] ?? STATUS_ICONS.uploaded;
              const StatusIcon = statusInfo.icon;
              return (
                <div
                  key={doc.document_id}
                  className="flex items-center gap-3 p-3 rounded-lg bg-white/[0.02] border border-white/5 group"
                >
                  <FileText size={16} className="text-[var(--text-muted)] flex-shrink-0" />
                  <div className="flex-1 min-w-0">
                    <p className="text-sm text-white truncate">{doc.filename}</p>
                    <p className="text-xs text-[var(--text-muted)]">
                      {(doc.file_size / 1024).toFixed(1)} KB &middot; {doc.chunk_count} chunks
                    </p>
                  </div>

                  <div className="flex items-center gap-1.5">
                    <span className={`flex items-center gap-1 text-xs ${statusInfo.color}`}>
                      <StatusIcon size={12} />
                      {doc.status}
                    </span>
                  </div>

                  <div className="flex items-center gap-1 opacity-0 group-hover:opacity-100 transition">
                    <button
                      onClick={() => {
                        setPreviewDocId(doc.document_id);
                        void previewChunks(selectedCollection, doc.document_id);
                      }}
                      className="p-1.5 rounded-lg hover:bg-white/10 text-[var(--text-muted)] hover:text-white transition"
                      title={t('rag.preview_chunks')}
                    >
                      <Eye size={14} />
                    </button>
                    {doc.status === 'uploaded' && (
                      <button
                        onClick={() => void indexDocument(selectedCollection, doc.document_id)}
                        className="p-1.5 rounded-lg hover:bg-emerald-500/10 text-[var(--text-muted)] hover:text-emerald-400 transition"
                        title={t('rag.index_document')}
                      >
                        <CheckCircle size={14} />
                      </button>
                    )}
                    {doc.status === 'indexed' && (
                      <button
                        onClick={() => void indexDocument(selectedCollection, doc.document_id)}
                        className="p-1.5 rounded-lg hover:bg-blue-500/10 text-[var(--text-muted)] hover:text-blue-400 transition"
                        title={t('rag.reindex')}
                      >
                        <RotateCcw size={14} />
                      </button>
                    )}
                    <button
                      onClick={() => {
                        if (confirm(t('rag.confirm_delete_document'))) {
                          void deleteDocument(selectedCollection, doc.document_id);
                        }
                      }}
                      className="p-1.5 rounded-lg hover:bg-red-500/10 text-[var(--text-muted)] hover:text-red-400 transition"
                      title={t('rag.delete_document')}
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
            <div className="rounded-xl bg-white/[0.03] border border-white/10 p-4">
              <div className="flex items-center justify-between mb-3">
                <h3 className="text-sm font-semibold text-white">{t('rag.chunk_preview')}</h3>
                <button
                  onClick={() => setPreviewDocId(null)}
                  className="text-[var(--text-muted)] hover:text-white text-xs"
                >
                  &times;
                </button>
              </div>
              {chunksLoading ? (
                <p className="text-xs text-[var(--text-muted)]">{t('common.loading')}</p>
              ) : (
                <div className="space-y-2 max-h-64 overflow-y-auto">
                  {chunkPreviews.map((chunk) => (
                    <div
                      key={chunk.index}
                      className="p-2 rounded-lg bg-white/[0.02] border border-white/5 text-xs"
                    >
                      <div className="flex justify-between text-[var(--text-muted)] mb-1">
                        <span>Chunk #{chunk.index}</span>
                        <span>{chunk.length} chars &middot; {chunk.strategy}</span>
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

/**
 * RAG ドキュメントリスト.
 *
 * コレクション内のドキュメント一覧を表示し、検索・削除・再インデックスを提供。
 */
import React, { useMemo, useState } from 'react';
import type { DocumentInfo, DocumentStatus } from '../types/rag';

export interface RAGDocumentListProps {
  documents: DocumentInfo[];
  onDelete?: (docId: string) => void;
  onReIndex?: (docId: string) => void;
  onPreviewChunks?: (docId: string) => void;
  loading?: boolean;
}

const STATUS_STYLES: Record<DocumentStatus, string> = {
  uploaded: 'text-blue-400 bg-blue-500/10',
  chunked: 'text-amber-400 bg-amber-500/10',
  indexed: 'text-emerald-400 bg-emerald-500/10',
  error: 'text-rose-400 bg-rose-500/10',
};

const STATUS_LABELS: Record<DocumentStatus, string> = {
  uploaded: 'アップロード済',
  chunked: 'チャンク済',
  indexed: 'インデックス済',
  error: 'エラー',
};

/** ファイルサイズのフォーマット */
function formatSize(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
  return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
}

/** ドキュメント一覧 */
export const RAGDocumentList: React.FC<RAGDocumentListProps> = ({
  documents,
  onDelete,
  onReIndex,
  onPreviewChunks,
  loading = false,
}) => {
  const [searchQuery, setSearchQuery] = useState('');

  const filtered = useMemo(() => {
    if (!searchQuery.trim()) return documents;
    const q = searchQuery.toLowerCase();
    return documents.filter(
      (d) =>
        d.filename.toLowerCase().includes(q) ||
        d.document_id.toLowerCase().includes(q),
    );
  }, [documents, searchQuery]);

  if (loading) {
    return (
      <div className="flex items-center justify-center py-12 text-slate-400">
        <span className="animate-spin mr-2">&#9696;</span>
        読み込み中...
      </div>
    );
  }

  return (
    <div className="space-y-3">
      {/* 検索バー */}
      <input
        type="text"
        value={searchQuery}
        onChange={(e) => setSearchQuery(e.target.value)}
        placeholder="ファイル名で検索..."
        className="w-full px-3 py-2 bg-slate-800 border border-slate-700 rounded-lg text-sm text-slate-200 placeholder-slate-500 focus:outline-none focus:ring-1 focus:ring-indigo-500"
      />

      {filtered.length === 0 ? (
        <div className="text-center py-8 text-slate-500">
          ドキュメントがありません
        </div>
      ) : (
        <div className="space-y-2">
          {filtered.map((doc) => (
            <div
              key={doc.document_id}
              className="flex items-center justify-between p-3 rounded-lg bg-slate-800/40 border border-slate-700/50 hover:bg-slate-800/60 transition-colors"
            >
              <div className="flex-1 min-w-0">
                <div className="flex items-center gap-2">
                  <span className="text-sm font-medium text-slate-200 truncate">
                    {doc.filename}
                  </span>
                  <span
                    className={`text-xs px-2 py-0.5 rounded-full ${
                      STATUS_STYLES[doc.status] ?? ''
                    }`}
                  >
                    {STATUS_LABELS[doc.status] ?? doc.status}
                  </span>
                </div>
                <div className="flex items-center gap-3 mt-1 text-xs text-slate-500">
                  <span>{formatSize(doc.file_size)}</span>
                  <span>{doc.chunk_count} チャンク</span>
                  {doc.uploaded_at && (
                    <span>
                      {new Date(doc.uploaded_at).toLocaleString('ja-JP')}
                    </span>
                  )}
                </div>
              </div>

              <div className="flex items-center gap-1 ml-2 shrink-0">
                {onPreviewChunks && (
                  <button
                    onClick={() => onPreviewChunks(doc.document_id)}
                    className="text-xs px-2 py-1 text-slate-400 hover:text-indigo-400 hover:bg-indigo-500/10 rounded transition-colors"
                  >
                    プレビュー
                  </button>
                )}
                {onReIndex && doc.status !== 'error' && (
                  <button
                    onClick={() => onReIndex(doc.document_id)}
                    className="text-xs px-2 py-1 text-slate-400 hover:text-amber-400 hover:bg-amber-500/10 rounded transition-colors"
                  >
                    再インデックス
                  </button>
                )}
                {onDelete && (
                  <button
                    onClick={() => onDelete(doc.document_id)}
                    className="text-xs px-2 py-1 text-slate-400 hover:text-rose-400 hover:bg-rose-500/10 rounded transition-colors"
                  >
                    削除
                  </button>
                )}
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
};

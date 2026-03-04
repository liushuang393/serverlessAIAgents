/**
 * RAG コレクションテーブル.
 *
 * コレクション一覧をテーブル形式で表示し、選択・削除操作を提供。
 */
import React from 'react';
import type { CollectionInfo } from '../types/rag';

export interface RAGCollectionTableProps {
  collections: CollectionInfo[];
  onSelect?: (collection: CollectionInfo) => void;
  onDelete?: (collectionName: string) => void;
  loading?: boolean;
  selectedName?: string;
}

/** コレクション一覧テーブル */
export const RAGCollectionTable: React.FC<RAGCollectionTableProps> = ({
  collections,
  onSelect,
  onDelete,
  loading = false,
  selectedName,
}) => {
  if (loading) {
    return (
      <div className="flex items-center justify-center py-12 text-slate-400">
        <span className="animate-spin mr-2">&#9696;</span>
        読み込み中...
      </div>
    );
  }

  if (collections.length === 0) {
    return (
      <div className="text-center py-12 text-slate-500">
        コレクションがありません
      </div>
    );
  }

  return (
    <div className="overflow-x-auto">
      <table className="w-full text-sm text-left">
        <thead className="text-xs text-slate-400 uppercase border-b border-slate-700">
          <tr>
            <th className="px-4 py-3">名前</th>
            <th className="px-4 py-3">チャンク戦略</th>
            <th className="px-4 py-3">検索手法</th>
            <th className="px-4 py-3">ドキュメント数</th>
            <th className="px-4 py-3">最終インデックス</th>
            {onDelete && <th className="px-4 py-3 text-right">操作</th>}
          </tr>
        </thead>
        <tbody>
          {collections.map((col) => (
            <tr
              key={col.collection_name}
              onClick={() => onSelect?.(col)}
              className={`border-b border-slate-700/50 cursor-pointer transition-colors ${
                selectedName === col.collection_name
                  ? 'bg-indigo-500/10'
                  : 'hover:bg-slate-800/50'
              }`}
            >
              <td className="px-4 py-3">
                <div className="font-medium text-slate-200">
                  {col.display_name || col.collection_name}
                </div>
                <div className="text-xs text-slate-500">{col.collection_name}</div>
              </td>
              <td className="px-4 py-3 text-slate-300">{col.chunk_strategy}</td>
              <td className="px-4 py-3 text-slate-300">{col.retrieval_method}</td>
              <td className="px-4 py-3 text-slate-300">{col.document_count}</td>
              <td className="px-4 py-3 text-slate-400 text-xs">
                {col.last_indexed_at
                  ? new Date(col.last_indexed_at).toLocaleString('ja-JP')
                  : '-'}
              </td>
              {onDelete && (
                <td className="px-4 py-3 text-right">
                  <button
                    onClick={(e) => {
                      e.stopPropagation();
                      onDelete(col.collection_name);
                    }}
                    className="text-rose-400 hover:text-rose-300 text-xs px-2 py-1 rounded hover:bg-rose-500/10 transition-colors"
                  >
                    削除
                  </button>
                </td>
              )}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

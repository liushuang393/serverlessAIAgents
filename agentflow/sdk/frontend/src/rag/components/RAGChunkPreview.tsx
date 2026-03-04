/**
 * RAG チャンクプレビュー.
 *
 * インデックス前にチャンク分割結果をプレビュー表示。
 */
import React from 'react';
import type { ChunkPreview } from '../types/rag';

export interface RAGChunkPreviewProps {
  chunks: ChunkPreview[];
  chunkStrategy?: string;
  onConfirmIndex?: () => void;
  loading?: boolean;
}

/** チャンクプレビューパネル */
export const RAGChunkPreview: React.FC<RAGChunkPreviewProps> = ({
  chunks,
  chunkStrategy,
  onConfirmIndex,
  loading = false,
}) => {
  if (loading) {
    return (
      <div className="flex items-center justify-center py-8 text-slate-400">
        <span className="animate-spin mr-2">&#9696;</span>
        チャンク生成中...
      </div>
    );
  }

  if (chunks.length === 0) {
    return (
      <div className="text-center py-8 text-slate-500">
        チャンクがありません
      </div>
    );
  }

  return (
    <div className="space-y-4">
      {/* ヘッダー */}
      <div className="flex items-center justify-between">
        <div className="text-sm text-slate-300">
          <span className="font-medium">{chunks.length}</span> チャンク
          {chunkStrategy && (
            <span className="ml-2 text-slate-500">({chunkStrategy})</span>
          )}
        </div>
        {onConfirmIndex && (
          <button
            onClick={onConfirmIndex}
            className="px-4 py-1.5 text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-500 rounded-lg transition-colors"
          >
            インデックスに登録
          </button>
        )}
      </div>

      {/* チャンクリスト */}
      <div className="space-y-2 max-h-96 overflow-y-auto">
        {chunks.map((chunk) => (
          <div
            key={chunk.index}
            className="p-3 rounded-lg bg-slate-800/40 border border-slate-700/50"
          >
            <div className="flex items-center justify-between mb-1">
              <span className="text-xs font-mono text-indigo-400">
                #{chunk.index}
              </span>
              <span className="text-xs text-slate-500">
                {chunk.length} 文字
              </span>
            </div>
            <pre className="text-xs text-slate-300 whitespace-pre-wrap break-words font-mono leading-relaxed max-h-24 overflow-hidden">
              {chunk.content.slice(0, 500)}
              {chunk.content.length > 500 && '...'}
            </pre>
          </div>
        ))}
      </div>
    </div>
  );
};

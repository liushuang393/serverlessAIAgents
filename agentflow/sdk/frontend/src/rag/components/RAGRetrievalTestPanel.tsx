/**
 * RAG テストクエリパネル.
 *
 * テストクエリを実行し、検索結果をリアルタイムに表示。
 */
import React, { useCallback, useState } from 'react';
import type { TestQueryResult } from '../types/rag';

export interface RAGRetrievalTestPanelProps {
  onTestQuery: (query: string, topK: number) => Promise<TestQueryResult>;
  defaultTopK?: number;
}

/** テストクエリパネル */
export const RAGRetrievalTestPanel: React.FC<RAGRetrievalTestPanelProps> = ({
  onTestQuery,
  defaultTopK = 5,
}) => {
  const [query, setQuery] = useState('');
  const [topK, setTopK] = useState(defaultTopK);
  const [result, setResult] = useState<TestQueryResult | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleQuery = useCallback(async () => {
    if (!query.trim()) return;
    setLoading(true);
    setError(null);
    try {
      const res = await onTestQuery(query, topK);
      setResult(res);
    } catch (err: unknown) {
      const msg = err instanceof Error ? err.message : 'クエリ実行に失敗しました';
      setError(msg);
    } finally {
      setLoading(false);
    }
  }, [query, topK, onTestQuery]);

  return (
    <div className="space-y-4">
      {/* クエリ入力 */}
      <div className="flex gap-2">
        <input
          type="text"
          value={query}
          onChange={(e) => setQuery(e.target.value)}
          onKeyDown={(e) => e.key === 'Enter' && handleQuery()}
          placeholder="テストクエリを入力..."
          className="flex-1 px-3 py-2 bg-slate-800 border border-slate-700 rounded-lg text-sm text-slate-200 placeholder-slate-500 focus:outline-none focus:ring-1 focus:ring-indigo-500"
        />
        <input
          type="number"
          value={topK}
          onChange={(e) => setTopK(Number(e.target.value))}
          min={1}
          max={50}
          className="w-16 px-2 py-2 bg-slate-800 border border-slate-700 rounded-lg text-sm text-slate-200 text-center focus:outline-none focus:ring-1 focus:ring-indigo-500"
          title="Top-K"
        />
        <button
          onClick={handleQuery}
          disabled={loading || !query.trim()}
          className="px-4 py-2 text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-500 disabled:opacity-50 rounded-lg transition-colors"
        >
          {loading ? '検索中...' : '検索'}
        </button>
      </div>

      {/* エラー */}
      {error && (
        <p className="text-xs text-rose-400 px-1">{error}</p>
      )}

      {/* 結果 */}
      {result && (
        <div className="space-y-3">
          <div className="flex items-center gap-3 text-xs text-slate-400">
            <span>{result.total} 件</span>
            <span>{result.took_ms}ms</span>
          </div>
          {result.results.map((hit, i) => (
            <div
              key={i}
              className="p-3 rounded-lg bg-slate-800/40 border border-slate-700/50"
            >
              <div className="flex items-center justify-between mb-1">
                <span className="text-xs text-slate-500">{hit.source}</span>
                <span className="text-xs font-mono text-emerald-400">
                  {(hit.score * 100).toFixed(1)}%
                </span>
              </div>
              <p className="text-sm text-slate-200 whitespace-pre-wrap break-words">
                {hit.content.slice(0, 400)}
                {hit.content.length > 400 && '...'}
              </p>
            </div>
          ))}
        </div>
      )}
    </div>
  );
};

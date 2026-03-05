/**
 * 検索設定ページ.
 *
 * コレクションの検索パラメータを表示・調整し、
 * テストクエリで結果を確認する。
 */
import { useEffect, useState } from 'react';
import { Search, Send } from 'lucide-react';
import { useI18n } from '../../i18n';
import { useRAGStore } from '../../stores/ragStore';
import { ragApi } from '../../api/rag';

export function RetrievalSettings() {
  const { t } = useI18n();
  const { collections, selectedCollection, selectCollection, fetchCollections } = useRAGStore();

  const [query, setQuery] = useState('');
  const [topK, setTopK] = useState(5);
  const [results, setResults] = useState<Record<string, unknown> | null>(null);
  const [testing, setTesting] = useState(false);

  useEffect(() => {
    void fetchCollections();
  }, [fetchCollections]);

  const selected = collections.find((c) => c.collection_name === selectedCollection);

  const handleTestQuery = async () => {
    if (!selectedCollection || !query.trim()) return;
    setTesting(true);
    setResults(null);
    try {
      const res = await ragApi.testQuery(selectedCollection, query, topK);
      setResults(res);
    } catch (e) {
      setResults({ error: (e as Error).message });
    } finally {
      setTesting(false);
    }
  };

  return (
    <div className="space-y-6">
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

      {selected && (
        <>
          {/* 現在の設定表示 */}
          <div className="rounded-xl bg-white/[0.03] border border-white/5 p-4">
            <h3 className="text-xs font-semibold text-[var(--text-muted)] uppercase mb-3">
              {t('rag.current_settings')}
            </h3>
            <div className="grid grid-cols-2 sm:grid-cols-4 gap-4">
              {[
                { label: t('rag.retrieval_method'), value: selected.retrieval_method },
                { label: t('rag.reranker'), value: selected.reranker || t('rag.none') },
                { label: 'Top-K', value: selected.top_k },
                { label: t('rag.min_similarity'), value: selected.min_similarity },
                { label: t('rag.chunk_strategy'), value: selected.chunk_strategy },
                { label: t('rag.chunk_size'), value: selected.chunk_size },
                { label: t('rag.chunk_overlap'), value: selected.chunk_overlap },
                { label: t('rag.embedding_model'), value: selected.embedding_model || '-' },
              ].map(({ label, value }) => (
                <div key={label}>
                  <p className="text-xs text-[var(--text-muted)]">{label}</p>
                  <p className="text-sm text-white font-mono">{value}</p>
                </div>
              ))}
            </div>
          </div>

          {/* テストクエリ */}
          <div className="rounded-xl bg-white/[0.03] border border-white/10 p-4">
            <h3 className="text-xs font-semibold text-[var(--text-muted)] uppercase mb-3 flex items-center gap-1.5">
              <Search size={14} />
              {t('rag.test_query')}
            </h3>

            <div className="flex gap-2 mb-3">
              <input
                data-testid="test-query-input"
                value={query}
                onChange={(e) => setQuery(e.target.value)}
                onKeyDown={(e) => e.key === 'Enter' && void handleTestQuery()}
                placeholder={t('rag.test_query_placeholder')}
                className="flex-1 px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white placeholder:text-[var(--text-muted)]"
              />
              <input
                type="number"
                value={topK}
                onChange={(e) => setTopK(Number(e.target.value))}
                min={1}
                max={50}
                className="w-16 px-2 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white text-center"
              />
              <button
                data-testid="run-test-query-button"
                onClick={() => void handleTestQuery()}
                disabled={testing || !query.trim()}
                className="px-4 py-2 rounded-lg bg-[var(--primary)] text-black text-xs font-semibold hover:opacity-90 transition disabled:opacity-50 flex items-center gap-1.5"
              >
                <Send size={12} />
                {testing ? t('common.loading') : t('rag.run_query')}
              </button>
            </div>

            <div data-testid="query-result-area">
              {results && (
                <pre className="p-3 rounded-lg bg-black/30 text-xs text-white font-mono overflow-x-auto max-h-64 overflow-y-auto whitespace-pre-wrap">
                  {JSON.stringify(results, null, 2)}
                </pre>
              )}
            </div>
          </div>
        </>
      )}
    </div>
  );
}

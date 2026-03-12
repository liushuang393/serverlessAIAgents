/**
 * パネル内検索設定＆テストクエリコンポーネント.
 *
 * KnowledgePanel の「検索設定」タブで使用される。
 * コレクション選択、パターンプリセット、詳細設定、テストクエリ実行を行う。
 */
import { useEffect, useState } from 'react';
import { Search, Send, ChevronDown, ChevronUp } from 'lucide-react';
import { useRAGStore } from '../../stores/ragStore';
import { ragApi } from '../../api/rag';
import type { CollectionInfo } from '../../api/rag';
import { PRESETS, type PresetKey } from './PanelCollections';

// ---------------------------------------------------------------------------
// テスト結果の型
// ---------------------------------------------------------------------------

interface TestResultItem {
  score?: number;
  content?: string;
  source?: string;
  metadata?: Record<string, unknown>;
}

interface TestQueryResult {
  results?: TestResultItem[];
  total?: number;
  error?: string;
}

// ---------------------------------------------------------------------------
// スコアバッジ
// ---------------------------------------------------------------------------

function ScoreBadge({ score }: { score: number }) {
  let colorClass = 'bg-red-500/20 text-red-400';
  if (score >= 0.5) {
    colorClass = 'bg-green-500/20 text-green-400';
  } else if (score >= 0.3) {
    colorClass = 'bg-yellow-500/20 text-yellow-400';
  }
  return (
    <span className={`px-2 py-0.5 rounded text-[10px] font-mono font-semibold ${colorClass}`}>
      {score.toFixed(3)}
    </span>
  );
}

// ---------------------------------------------------------------------------
// PanelRetrieval
// ---------------------------------------------------------------------------

export function PanelRetrieval() {
  const { collections, collectionsLoading, fetchCollections, updateCollection } = useRAGStore();

  // コレクション選択
  const [selectedName, setSelectedName] = useState<string>('');
  const selected = collections.find((c) => c.collection_name === selectedName) ?? null;

  // プリセット
  const [preset, setPreset] = useState<PresetKey>('custom');

  // チャンキング設定
  const [chunkStrategy, setChunkStrategy] = useState('recursive');
  const [chunkSize, setChunkSize] = useState(800);
  const [chunkOverlap, setChunkOverlap] = useState(120);

  // 検索設定
  const [retrievalMethod, setRetrievalMethod] = useState('semantic');
  const [reranker, setReranker] = useState('none');
  const [topK, setTopK] = useState(5);
  const [minSimilarity, setMinSimilarity] = useState(0.2);

  // テストクエリ
  const [query, setQuery] = useState('');
  const [testing, setTesting] = useState(false);
  const [testResults, setTestResults] = useState<TestQueryResult | null>(null);
  const [expandedChunks, setExpandedChunks] = useState<Set<number>>(new Set());

  // 保存中
  const [saving, setSaving] = useState(false);

  useEffect(() => {
    void fetchCollections();
  }, [fetchCollections]);

  // コレクション選択時に設定値を読み込む
  const loadCollectionSettings = (col: CollectionInfo) => {
    setChunkStrategy(col.chunk_strategy);
    setChunkSize(col.chunk_size);
    setChunkOverlap(col.chunk_overlap);
    setRetrievalMethod(col.retrieval_method);
    setReranker(col.reranker ?? 'none');
    setTopK(col.top_k);
    setMinSimilarity(col.min_similarity);
    setPreset('custom');
    setTestResults(null);
    setExpandedChunks(new Set());
  };

  const handleCollectionChange = (name: string) => {
    setSelectedName(name);
    const col = collections.find((c) => c.collection_name === name);
    if (col) {
      loadCollectionSettings(col);
    }
  };

  // プリセット適用
  const applyPreset = (key: PresetKey) => {
    setPreset(key);
    const def = PRESETS.find((p) => p.key === key);
    if (def?.values) {
      setChunkStrategy(def.values.chunk_strategy);
      setChunkSize(def.values.chunk_size);
      setChunkOverlap(def.values.chunk_overlap);
      setRetrievalMethod(def.values.retrieval_method);
      setReranker(def.values.reranker);
      setTopK(def.values.top_k);
      setMinSimilarity(def.values.min_similarity);
    }
  };

  // テスト実行
  const handleTestQuery = async () => {
    if (!selectedName || !query.trim()) return;
    setTesting(true);
    setTestResults(null);
    setExpandedChunks(new Set());
    try {
      const res = await ragApi.testQuery(selectedName, query, topK);
      setTestResults(res as TestQueryResult);
    } catch (e) {
      setTestResults({ error: (e as Error).message });
    } finally {
      setTesting(false);
    }
  };

  // 設定保存
  const handleSave = async () => {
    if (!selectedName) return;
    setSaving(true);
    try {
      await updateCollection(selectedName, {
        chunk_strategy: chunkStrategy,
        chunk_size: chunkSize,
        chunk_overlap: chunkOverlap,
        retrieval_method: retrievalMethod,
        reranker: reranker === 'none' ? null : reranker,
        top_k: topK,
        min_similarity: minSimilarity,
      });
    } finally {
      setSaving(false);
    }
  };

  const toggleChunkExpand = (index: number) => {
    setExpandedChunks((prev) => {
      const next = new Set(prev);
      if (next.has(index)) {
        next.delete(index);
      } else {
        next.add(index);
      }
      return next;
    });
  };

  return (
    <div data-testid="panel-retrieval" className="space-y-4">
      {/* コレクション選択 */}
      <div>
        <h4 className="text-xs font-bold text-[var(--text-muted)] uppercase tracking-wider mb-2">
          コレクション選択
        </h4>
        <select
          data-testid="select-collection"
          value={selectedName}
          onChange={(e) => handleCollectionChange(e.target.value)}
          className="w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
        >
          <option value="">コレクションを選択...</option>
          {collections.map((c) => (
            <option key={c.collection_name} value={c.collection_name}>
              {c.display_name || c.collection_name}
            </option>
          ))}
        </select>
        {collectionsLoading && collections.length === 0 && (
          <p className="text-xs text-[var(--text-muted)] mt-1">読み込み中...</p>
        )}
      </div>

      {!selected && (
        <div className="text-center py-8 text-[var(--text-muted)] text-sm">
          コレクションを選択してください
        </div>
      )}

      {selected && (
        <>
          {/* パターン選択 */}
          <div>
            <h4 className="text-xs font-bold text-[var(--text-muted)] uppercase tracking-wider mb-2">
              パターンプリセット
            </h4>
            <div className="grid grid-cols-2 gap-2">
              {PRESETS.map((p) => (
                <button
                  key={p.key}
                  type="button"
                  data-testid={`preset-${p.key}`}
                  onClick={() => applyPreset(p.key)}
                  className={`p-2.5 rounded-xl border text-left transition-all ${
                    preset === p.key
                      ? 'border-[var(--primary)] bg-[var(--primary)]/10'
                      : 'border-white/5 bg-white/[0.02] hover:bg-white/[0.04]'
                  }`}
                >
                  <p className="text-xs font-semibold text-white">{p.label}</p>
                  <p className="text-[10px] text-[var(--text-muted)] mt-0.5">{p.description}</p>
                </button>
              ))}
            </div>
          </div>

          {/* チャンキング設定 */}
          <div>
            <h4 className="text-xs font-bold text-[var(--text-muted)] uppercase tracking-wider mb-2">
              チャンキング設定
            </h4>
            <div className="space-y-3">
              <label className="block">
                <div className="flex items-center justify-between">
                  <span className="text-xs text-[var(--text-muted)]">戦略</span>
                  <span className="text-[10px] text-[var(--text-muted)]">テキスト分割の方法</span>
                </div>
                <select
                  data-testid="select-chunk-strategy"
                  value={chunkStrategy}
                  onChange={(e) => { setChunkStrategy(e.target.value); setPreset('custom'); }}
                  className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
                >
                  <option value="recursive">Recursive</option>
                  <option value="sentence">Sentence</option>
                  <option value="semantic">Semantic</option>
                  <option value="token">Token</option>
                  <option value="markdown">Markdown</option>
                </select>
              </label>

              <label className="block">
                <div className="flex items-center justify-between">
                  <span className="text-xs text-[var(--text-muted)]">
                    チャンクサイズ: <span data-testid="chunk-size-value">{chunkSize}</span>
                  </span>
                  <span className="text-[10px] text-[var(--text-muted)]">各チャンクの最大文字数</span>
                </div>
                <div className="flex items-center gap-2 mt-1">
                  <input
                    data-testid="range-chunk-size"
                    type="range"
                    min={100}
                    max={4000}
                    step={50}
                    value={chunkSize}
                    onChange={(e) => { setChunkSize(Number(e.target.value)); setPreset('custom'); }}
                    className="flex-1 accent-[var(--primary)]"
                  />
                  <input
                    data-testid="input-chunk-size"
                    type="number"
                    min={100}
                    max={4000}
                    step={50}
                    value={chunkSize}
                    onChange={(e) => { setChunkSize(Number(e.target.value)); setPreset('custom'); }}
                    className="w-20 px-2 py-1 rounded-lg bg-white/5 border border-white/10 text-xs text-white text-center"
                  />
                </div>
              </label>

              <label className="block">
                <div className="flex items-center justify-between">
                  <span className="text-xs text-[var(--text-muted)]">
                    オーバーラップ: <span data-testid="chunk-overlap-value">{chunkOverlap}</span>
                  </span>
                  <span className="text-[10px] text-[var(--text-muted)]">隣接チャンクとの重複文字数</span>
                </div>
                <div className="flex items-center gap-2 mt-1">
                  <input
                    data-testid="range-chunk-overlap"
                    type="range"
                    min={0}
                    max={500}
                    step={10}
                    value={chunkOverlap}
                    onChange={(e) => { setChunkOverlap(Number(e.target.value)); setPreset('custom'); }}
                    className="flex-1 accent-[var(--primary)]"
                  />
                  <input
                    data-testid="input-chunk-overlap"
                    type="number"
                    min={0}
                    max={500}
                    step={10}
                    value={chunkOverlap}
                    onChange={(e) => { setChunkOverlap(Number(e.target.value)); setPreset('custom'); }}
                    className="w-20 px-2 py-1 rounded-lg bg-white/5 border border-white/10 text-xs text-white text-center"
                  />
                </div>
              </label>
            </div>
          </div>

          {/* 検索設定 */}
          <div>
            <h4 className="text-xs font-bold text-[var(--text-muted)] uppercase tracking-wider mb-2">
              検索設定
            </h4>
            <div className="space-y-3">
              <label className="block">
                <div className="flex items-center justify-between">
                  <span className="text-xs text-[var(--text-muted)]">検索方式</span>
                  <span className="text-[10px] text-[var(--text-muted)]">ベクトル検索の方式</span>
                </div>
                <select
                  data-testid="select-retrieval-method"
                  value={retrievalMethod}
                  onChange={(e) => { setRetrievalMethod(e.target.value); setPreset('custom'); }}
                  className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
                >
                  <option value="semantic">Semantic</option>
                  <option value="hybrid">Hybrid</option>
                  <option value="keyword">Keyword</option>
                </select>
              </label>

              <label className="block">
                <div className="flex items-center justify-between">
                  <span className="text-xs text-[var(--text-muted)]">リランカー</span>
                  <span className="text-[10px] text-[var(--text-muted)]">検索結果の再順位付け</span>
                </div>
                <select
                  data-testid="select-reranker"
                  value={reranker}
                  onChange={(e) => { setReranker(e.target.value); setPreset('custom'); }}
                  className="mt-1 w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white"
                >
                  <option value="none">None</option>
                  <option value="bm25">BM25</option>
                  <option value="cohere">Cohere</option>
                  <option value="cross_encoder">Cross Encoder</option>
                  <option value="llm_listwise">LLM Listwise</option>
                </select>
              </label>

              <label className="block">
                <div className="flex items-center justify-between">
                  <span className="text-xs text-[var(--text-muted)]">
                    Top-K: <span data-testid="top-k-value">{topK}</span>
                  </span>
                  <span className="text-[10px] text-[var(--text-muted)]">検索結果の最大件数</span>
                </div>
                <div className="flex items-center gap-2 mt-1">
                  <input
                    data-testid="range-top-k"
                    type="range"
                    min={1}
                    max={50}
                    step={1}
                    value={topK}
                    onChange={(e) => { setTopK(Number(e.target.value)); setPreset('custom'); }}
                    className="flex-1 accent-[var(--primary)]"
                  />
                  <input
                    data-testid="input-top-k"
                    type="number"
                    min={1}
                    max={50}
                    step={1}
                    value={topK}
                    onChange={(e) => { setTopK(Number(e.target.value)); setPreset('custom'); }}
                    className="w-20 px-2 py-1 rounded-lg bg-white/5 border border-white/10 text-xs text-white text-center"
                  />
                </div>
              </label>

              <label className="block">
                <div className="flex items-center justify-between">
                  <span className="text-xs text-[var(--text-muted)]">
                    類似度閾値: <span data-testid="min-similarity-value">{minSimilarity.toFixed(2)}</span>
                  </span>
                  <span className="text-[10px] text-[var(--text-muted)]">最低スコアの閾値</span>
                </div>
                <div className="flex items-center gap-2 mt-1">
                  <input
                    data-testid="range-min-similarity"
                    type="range"
                    min={0}
                    max={1}
                    step={0.05}
                    value={minSimilarity}
                    onChange={(e) => { setMinSimilarity(Number(e.target.value)); setPreset('custom'); }}
                    className="flex-1 accent-[var(--primary)]"
                  />
                  <input
                    data-testid="input-min-similarity"
                    type="number"
                    min={0}
                    max={1}
                    step={0.05}
                    value={minSimilarity}
                    onChange={(e) => { setMinSimilarity(Number(e.target.value)); setPreset('custom'); }}
                    className="w-20 px-2 py-1 rounded-lg bg-white/5 border border-white/10 text-xs text-white text-center"
                  />
                </div>
              </label>
            </div>
          </div>

          {/* テストクエリ */}
          <div>
            <h4 className="text-xs font-bold text-[var(--text-muted)] uppercase tracking-wider mb-2 flex items-center gap-1.5">
              <Search size={12} />
              テストクエリ
            </h4>
            <textarea
              data-testid="test-query-input"
              value={query}
              onChange={(e) => setQuery(e.target.value)}
              placeholder="テストクエリを入力..."
              rows={3}
              className="w-full px-3 py-2 rounded-lg bg-white/5 border border-white/10 text-sm text-white placeholder:text-[var(--text-muted)] resize-none"
            />
            <button
              data-testid="btn-test-query"
              type="button"
              onClick={() => void handleTestQuery()}
              disabled={testing || !query.trim()}
              className="mt-2 w-full px-4 py-2 rounded-lg bg-[var(--primary)]/10 text-[var(--primary)] text-xs font-semibold hover:bg-[var(--primary)]/20 transition border border-[var(--primary)]/20 disabled:opacity-50 flex items-center justify-center gap-1.5"
            >
              <Send size={12} />
              {testing ? '実行中...' : 'テスト実行'}
            </button>

            {/* テスト結果 */}
            {testResults && (
              <div data-testid="test-results" className="mt-3 space-y-2">
                {testResults.error && (
                  <div className="p-3 rounded-xl glass border border-red-500/20 text-xs text-red-400">
                    {testResults.error}
                  </div>
                )}

                {testResults.results && (
                  <>
                    <p className="text-xs text-[var(--text-muted)]">
                      ヒット数: <span data-testid="hit-count" className="text-white font-semibold">{testResults.results.length}</span>
                    </p>
                    {testResults.results.map((item, idx) => {
                      const content = item.content ?? '';
                      const isLong = content.length > 200;
                      const isExpanded = expandedChunks.has(idx);
                      const displayContent = isExpanded ? content : content.slice(0, 200);
                      return (
                        <div
                          key={idx}
                          data-testid={`test-result-${idx}`}
                          className="glass rounded-xl border border-white/5 p-3 space-y-1.5"
                        >
                          <div className="flex items-center gap-2">
                            {item.score != null && <ScoreBadge score={item.score} />}
                            {item.source && (
                              <span className="text-[10px] text-[var(--text-muted)] truncate">
                                {item.source}
                              </span>
                            )}
                          </div>
                          <p className="text-xs text-white/80 whitespace-pre-wrap break-words">
                            {displayContent}
                            {isLong && !isExpanded && '...'}
                          </p>
                          {isLong && (
                            <button
                              type="button"
                              onClick={() => toggleChunkExpand(idx)}
                              className="flex items-center gap-0.5 text-[10px] text-[var(--primary)] hover:underline"
                            >
                              {isExpanded ? (
                                <>
                                  <ChevronUp size={10} />
                                  折りたたむ
                                </>
                              ) : (
                                <>
                                  <ChevronDown size={10} />
                                  全文を表示
                                </>
                              )}
                            </button>
                          )}
                        </div>
                      );
                    })}
                  </>
                )}
              </div>
            )}
          </div>

          {/* 保存ボタン */}
          <div className="pt-2">
            <button
              data-testid="btn-save-settings"
              type="button"
              onClick={() => void handleSave()}
              disabled={saving}
              className="w-full px-4 py-2.5 rounded-lg bg-[var(--primary)] text-black text-xs font-semibold hover:opacity-90 transition disabled:opacity-50"
            >
              {saving ? '保存中...' : '設定を保存'}
            </button>
          </div>
        </>
      )}
    </div>
  );
}

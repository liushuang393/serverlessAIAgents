/**
 * RAGOverview - RAG 機能概要 + App 単位設定管理.
 */

import { useEffect, useMemo, useState, type ReactNode } from 'react';
import {
  fetchAppRAGConfigs,
  fetchRAGPatterns,
  patchAppRAGConfig,
} from '@/api/client';
import type { AppRAGConfig, RAGDataSource, RAGPattern } from '@/types';
import { useAppStore } from '@/store/useAppStore';

interface RAGFormState {
  enabled: boolean;
  pattern: string;
  vector_provider: string;
  vector_url: string;
  vector_collection: string;
  embedding_model: string;
  chunk_strategy: string;
  chunk_size: number;
  chunk_overlap: number;
  retrieval_method: string;
  reranker: string;
  top_k: number;
  score_threshold: string;
  indexing_schedule: string;
  data_sources_text: string;
}

function sourcesToText(sources: RAGDataSource[]): string {
  return sources.map((s) => `${s.type}|${s.uri}|${s.label ?? ''}`).join('\n');
}

function textToSources(value: string): RAGDataSource[] {
  return value
    .split('\n')
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line) => {
      const [type = '', uri = '', label = ''] = line.split('|').map((part) => part.trim());
      return {
        type: type || 'web',
        uri,
        label,
        enabled: true,
      };
    })
    .filter((item) => item.uri.length > 0);
}

function toForm(config: AppRAGConfig): RAGFormState {
  return {
    enabled: config.rag.enabled,
    pattern: config.rag.pattern ?? '',
    vector_provider: config.rag.vector_provider ?? '',
    vector_url: config.rag.vector_url ?? '',
    vector_collection: config.rag.vector_collection ?? '',
    embedding_model: config.rag.embedding_model ?? '',
    chunk_strategy: config.rag.chunk_strategy,
    chunk_size: config.rag.chunk_size,
    chunk_overlap: config.rag.chunk_overlap,
    retrieval_method: config.rag.retrieval_method,
    reranker: config.rag.reranker ?? '',
    top_k: config.rag.top_k,
    score_threshold:
      config.rag.score_threshold === null || config.rag.score_threshold === undefined
        ? ''
        : String(config.rag.score_threshold),
    indexing_schedule: config.rag.indexing_schedule ?? '',
    data_sources_text: sourcesToText(config.rag.data_sources),
  };
}

export function RAGOverview() {
  const { ragOverview, loading, error, loadRAGOverview, clearError } = useAppStore();
  const [ragConfigs, setRagConfigs] = useState<AppRAGConfig[]>([]);
  const [patterns, setPatterns] = useState<RAGPattern[]>([]);
  const [selectedApp, setSelectedApp] = useState('');
  const [form, setForm] = useState<RAGFormState | null>(null);
  const [saving, setSaving] = useState(false);
  const [managerLoading, setManagerLoading] = useState(false);
  const [managerError, setManagerError] = useState<string | null>(null);
  const [managerMessage, setManagerMessage] = useState<string | null>(null);

  const selectedConfig = useMemo(
    () => ragConfigs.find((item) => item.app_name === selectedApp) ?? null,
    [ragConfigs, selectedApp],
  );

  useEffect(() => {
    loadRAGOverview();
    void loadManager();
  }, [loadRAGOverview]);

  useEffect(() => {
    if (!selectedConfig) {
      return;
    }
    setForm(toForm(selectedConfig));
  }, [selectedConfig]);

  const loadManager = async () => {
    setManagerLoading(true);
    setManagerError(null);
    try {
      const [configs, patternRes] = await Promise.all([
        fetchAppRAGConfigs(),
        fetchRAGPatterns(),
      ]);
      setRagConfigs(configs.apps);
      setPatterns(patternRes.patterns);

      if (!selectedApp && configs.apps.length > 0) {
        setSelectedApp(configs.apps[0].app_name);
      } else if (selectedApp && !configs.apps.some((item) => item.app_name === selectedApp)) {
        setSelectedApp(configs.apps[0]?.app_name ?? '');
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : 'RAG 設定一覧の取得に失敗しました';
      setManagerError(message);
    } finally {
      setManagerLoading(false);
    }
  };

  const applyPattern = (patternName: string) => {
    if (!form) {
      return;
    }
    const pattern = patterns.find((item) => item.name === patternName);
    if (!pattern) {
      setForm({ ...form, pattern: patternName });
      return;
    }
    setForm({
      ...form,
      pattern: patternName,
      chunk_strategy: pattern.config.chunk_strategy,
      chunk_size: pattern.config.chunk_size,
      chunk_overlap: pattern.config.chunk_overlap,
      retrieval_method: pattern.config.retrieval_method,
      reranker: pattern.config.reranker ?? '',
      top_k: pattern.config.top_k,
      score_threshold:
        pattern.config.score_threshold === null || pattern.config.score_threshold === undefined
          ? ''
          : String(pattern.config.score_threshold),
    });
  };

  const saveConfig = async () => {
    if (!selectedConfig || !form) {
      return;
    }
    setSaving(true);
    setManagerError(null);
    setManagerMessage(null);
    try {
      await patchAppRAGConfig(selectedConfig.app_name, {
        enabled: form.enabled,
        pattern: form.pattern || null,
        vector_provider: form.vector_provider || null,
        vector_url: form.vector_url || null,
        vector_collection: form.vector_collection || null,
        embedding_model: form.embedding_model || null,
        chunk_strategy: form.chunk_strategy,
        chunk_size: form.chunk_size,
        chunk_overlap: form.chunk_overlap,
        retrieval_method: form.retrieval_method,
        reranker: form.reranker || null,
        top_k: form.top_k,
        score_threshold: form.score_threshold === '' ? null : Number(form.score_threshold),
        indexing_schedule: form.indexing_schedule || null,
        data_sources: textToSources(form.data_sources_text),
      });
      await Promise.all([loadManager(), loadRAGOverview()]);
      setManagerMessage('RAG 設定を保存しました。');
    } catch (err) {
      const message = err instanceof Error ? err.message : 'RAG 設定の保存に失敗しました';
      setManagerError(message);
    } finally {
      setSaving(false);
    }
  };

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-slate-100">RAG Overview</h1>
        <p className="text-sm text-slate-500 mt-1">
          Retrieval-Augmented Generation 機能概要と App 単位設定
        </p>
      </div>

      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button onClick={clearError} className="text-red-400 hover:text-red-300 text-xs">✕</button>
        </div>
      )}

      {(managerError || managerMessage) && (
        <div
          className={`rounded-lg p-4 text-sm ${
            managerError
              ? 'bg-red-500/10 border border-red-500/20 text-red-300'
              : 'bg-emerald-500/10 border border-emerald-500/20 text-emerald-300'
          }`}
        >
          {managerError ?? managerMessage}
        </div>
      )}

      {loading && (
        <div className="flex justify-center py-12">
          <div className="w-10 h-10 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin" />
        </div>
      )}

      {ragOverview && !loading && (
        <>
          <div className="grid grid-cols-1 sm:grid-cols-3 gap-4">
            <StatCard icon="📄" label="Chunking Strategies" value={ragOverview.stats.total_strategies} color="indigo" />
            <StatCard icon="🔀" label="Rerankers" value={ragOverview.stats.total_rerankers} color="cyan" />
            <StatCard icon="📦" label="Apps Using RAG" value={ragOverview.stats.total_apps_using_rag} color="amber" />
          </div>

          <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5">
            <p className="text-sm text-slate-300 leading-relaxed">{ragOverview.description}</p>
          </div>

          <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
            <div className="flex items-center justify-between gap-3">
              <div>
                <h2 className="text-sm font-semibold text-slate-200">RAG Config Studio</h2>
                <p className="text-xs text-slate-500 mt-1">
                  App ごとにデータソース・分割方式・検索方式・再ランク設定を管理します
                </p>
              </div>
              <button
                onClick={() => void loadManager()}
                className="px-3 py-1.5 rounded-md bg-slate-800 hover:bg-slate-700 text-slate-300 text-xs"
              >
                Reload
              </button>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
              <div className="space-y-2">
                {managerLoading && (
                  <p className="text-xs text-slate-500">読み込み中...</p>
                )}
                {!managerLoading && ragConfigs.map((item) => (
                  <button
                    key={item.app_name}
                    onClick={() => setSelectedApp(item.app_name)}
                    className={`w-full text-left rounded-lg border px-3 py-2 transition-colors ${
                      selectedApp === item.app_name
                        ? 'border-indigo-500/50 bg-indigo-500/10'
                        : 'border-slate-700 bg-slate-950/40 hover:bg-slate-800/60'
                    }`}
                  >
                    <p className="text-sm text-slate-200 flex items-center gap-2">
                      <span>{item.icon}</span>
                      <span>{item.display_name}</span>
                    </p>
                    <p className="text-xs text-slate-500 mt-1">{item.app_name}</p>
                    <p className="text-xs mt-1 text-slate-400">
                      {item.rag.enabled ? `RAG ON · ${item.rag.retrieval_method}` : 'RAG OFF'}
                    </p>
                  </button>
                ))}
              </div>

              <div className="lg:col-span-2">
                {!form || !selectedConfig ? (
                  <div className="h-full rounded-lg border border-slate-700 bg-slate-950/40 p-4 text-xs text-slate-500">
                    編集対象の App を選択してください。
                  </div>
                ) : (
                  <div className="rounded-lg border border-slate-700 bg-slate-950/40 p-4 space-y-3">
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                      <Toggle
                        label="RAG 有効化"
                        checked={form.enabled}
                        onChange={(value) => setForm({ ...form, enabled: value })}
                      />
                      <Field label="パターン">
                        <select
                          value={form.pattern}
                          onChange={(e) => applyPattern(e.target.value)}
                          className="input"
                        >
                          <option value="">(手動設定)</option>
                          {patterns.map((pattern) => (
                            <option key={pattern.name} value={pattern.name}>
                              {pattern.label}
                            </option>
                          ))}
                        </select>
                      </Field>
                      <Field label="Vector Provider">
                        <input
                          value={form.vector_provider}
                          onChange={(e) => setForm({ ...form, vector_provider: e.target.value })}
                          className="input"
                          placeholder="qdrant"
                        />
                      </Field>
                      <Field label="Vector URL">
                        <input
                          value={form.vector_url}
                          onChange={(e) => setForm({ ...form, vector_url: e.target.value })}
                          className="input"
                          placeholder="http://localhost:6333"
                        />
                      </Field>
                      <Field label="Collection">
                        <input
                          value={form.vector_collection}
                          onChange={(e) => setForm({ ...form, vector_collection: e.target.value })}
                          className="input"
                          placeholder={`${selectedConfig.app_name}_knowledge`}
                        />
                      </Field>
                      <Field label="Embedding Model">
                        <input
                          value={form.embedding_model}
                          onChange={(e) => setForm({ ...form, embedding_model: e.target.value })}
                          className="input"
                          placeholder="text-embedding-3-small"
                        />
                      </Field>
                      <Field label="Chunk Strategy">
                        <select
                          value={form.chunk_strategy}
                          onChange={(e) => setForm({ ...form, chunk_strategy: e.target.value })}
                          className="input"
                        >
                          {ragOverview.chunk_strategies.map((strategy) => (
                            <option key={strategy.name} value={strategy.name}>
                              {strategy.label}
                            </option>
                          ))}
                        </select>
                      </Field>
                      <Field label="Retrieval Method">
                        <select
                          value={form.retrieval_method}
                          onChange={(e) => setForm({ ...form, retrieval_method: e.target.value })}
                          className="input"
                        >
                          {(ragOverview.retrieval_methods ?? []).map((method) => (
                            <option key={method.name} value={method.name}>
                              {method.label}
                            </option>
                          ))}
                        </select>
                      </Field>
                      <Field label="Chunk Size">
                        <input
                          type="number"
                          value={form.chunk_size}
                          onChange={(e) => setForm({ ...form, chunk_size: Number(e.target.value) || 800 })}
                          className="input"
                        />
                      </Field>
                      <Field label="Chunk Overlap">
                        <input
                          type="number"
                          value={form.chunk_overlap}
                          onChange={(e) => setForm({ ...form, chunk_overlap: Number(e.target.value) || 120 })}
                          className="input"
                        />
                      </Field>
                      <Field label="Reranker">
                        <select
                          value={form.reranker}
                          onChange={(e) => setForm({ ...form, reranker: e.target.value })}
                          className="input"
                        >
                          <option value="">none</option>
                          {ragOverview.rerankers.map((reranker) => (
                            <option key={reranker.name} value={reranker.name}>
                              {reranker.label}
                            </option>
                          ))}
                        </select>
                      </Field>
                      <Field label="Top K">
                        <input
                          type="number"
                          value={form.top_k}
                          onChange={(e) => setForm({ ...form, top_k: Number(e.target.value) || 5 })}
                          className="input"
                        />
                      </Field>
                      <Field label="Score Threshold (0-1)">
                        <input
                          type="number"
                          step="0.01"
                          value={form.score_threshold}
                          onChange={(e) => setForm({ ...form, score_threshold: e.target.value })}
                          className="input"
                          placeholder="0.2"
                        />
                      </Field>
                      <Field label="Indexing Schedule">
                        <input
                          value={form.indexing_schedule}
                          onChange={(e) => setForm({ ...form, indexing_schedule: e.target.value })}
                          className="input"
                          placeholder="0 */6 * * *"
                        />
                      </Field>
                    </div>

                    <Field label="Data Sources (1行=type|uri|label)">
                      <textarea
                        value={form.data_sources_text}
                        onChange={(e) => setForm({ ...form, data_sources_text: e.target.value })}
                        className="input min-h-24"
                        placeholder="web|https://example.com/docs|公式ドキュメント"
                      />
                    </Field>

                    <div className="flex items-center justify-end gap-2 pt-1">
                      <button
                        onClick={() => setForm(toForm(selectedConfig))}
                        className="px-3 py-1.5 rounded-md border border-slate-700 text-slate-300 hover:bg-slate-800 text-xs"
                      >
                        Reset
                      </button>
                      <button
                        onClick={() => void saveConfig()}
                        disabled={saving}
                        className="px-3 py-1.5 rounded-md bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 text-white text-xs"
                      >
                        {saving ? 'Saving...' : 'Save RAG Config'}
                      </button>
                    </div>
                  </div>
                )}
              </div>
            </div>
          </div>

          <div className="bg-slate-900/50 border border-slate-800 rounded-xl overflow-hidden">
            <div className="px-5 py-3.5 border-b border-slate-800">
              <h2 className="text-sm font-semibold text-slate-200">📦 Apps Using RAG</h2>
            </div>
            <div className="divide-y divide-slate-800/50">
              {ragOverview.apps_using_rag.map((app) => (
                <div key={app.app_name} className="px-5 py-3.5 flex items-center gap-4">
                  <span className="text-2xl">{app.icon}</span>
                  <div className="flex-1 min-w-0">
                    <p className="text-sm font-medium text-slate-200">{app.display_name}</p>
                    <p className="text-xs text-slate-500">{app.app_name}</p>
                  </div>
                  <div className="flex flex-wrap gap-1.5">
                    {app.rag_details.map((detail) => (
                      <span key={detail} className="px-2 py-0.5 bg-amber-500/10 text-amber-400 text-[10px] rounded-full">
                        {detail}
                      </span>
                    ))}
                  </div>
                </div>
              ))}
              {ragOverview.apps_using_rag.length === 0 && (
                <p className="px-5 py-8 text-center text-sm text-slate-500">No apps using RAG</p>
              )}
            </div>
          </div>
        </>
      )}
    </div>
  );
}

function Field({ label, children }: { label: string; children: ReactNode }) {
  return (
    <label className="block space-y-1">
      <span className="text-xs text-slate-400">{label}</span>
      {children}
    </label>
  );
}

function Toggle({
  label,
  checked,
  onChange,
}: {
  label: string;
  checked: boolean;
  onChange: (value: boolean) => void;
}) {
  return (
    <label className="flex items-center justify-between rounded-lg border border-slate-700 bg-slate-950/40 px-3 py-2">
      <span className="text-sm text-slate-300">{label}</span>
      <button
        type="button"
        onClick={() => onChange(!checked)}
        className={`w-10 h-6 rounded-full transition-colors ${checked ? 'bg-indigo-600' : 'bg-slate-700'}`}
      >
        <span
          className={`block w-4 h-4 bg-white rounded-full mt-1 transition-transform ${checked ? 'translate-x-5' : 'translate-x-1'}`}
        />
      </button>
    </label>
  );
}

function StatCard({
  icon,
  label,
  value,
  color,
}: {
  icon: string;
  label: string;
  value: number;
  color: 'indigo' | 'cyan' | 'amber';
}) {
  const borderColor = {
    indigo: 'border-indigo-500/20',
    cyan: 'border-cyan-500/20',
    amber: 'border-amber-500/20',
  }[color];
  const valueColor = {
    indigo: 'text-indigo-400',
    cyan: 'text-cyan-400',
    amber: 'text-amber-400',
  }[color];
  return (
    <div className={`bg-slate-900/50 border ${borderColor} rounded-xl p-5`}>
      <div className="flex items-center gap-3 mb-2">
        <span className="text-xl">{icon}</span>
        <span className="text-xs text-slate-500 uppercase tracking-wider">{label}</span>
      </div>
      <p className={`text-3xl font-bold ${valueColor}`}>{value}</p>
    </div>
  );
}

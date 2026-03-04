/**
 * RAG 設定フォーム.
 *
 * チャンク・検索・リランカーの設定を編集/表示する。
 */
import React, { useCallback, useState } from 'react';
import type { RAGConfigValues } from '../types/rag';

export interface RAGConfigFormProps {
  config: RAGConfigValues;
  onSave?: (config: RAGConfigValues) => void;
  readOnly?: boolean;
}

const CHUNK_STRATEGIES = [
  { value: 'fixed', label: '固定サイズ' },
  { value: 'recursive', label: '再帰分割' },
  { value: 'semantic', label: '意味ベース' },
  { value: 'sentence', label: '文単位' },
  { value: 'token', label: 'トークン' },
  { value: 'markdown', label: 'Markdown' },
];

const RETRIEVAL_METHODS = [
  { value: 'semantic', label: 'セマンティック検索' },
  { value: 'bm25', label: 'BM25' },
  { value: 'hybrid', label: 'ハイブリッド' },
];

const RERANKERS = [
  { value: 'none', label: 'なし' },
  { value: 'bm25', label: 'BM25' },
  { value: 'cohere', label: 'Cohere' },
  { value: 'cross_encoder', label: 'CrossEncoder' },
];

interface FieldProps {
  label: string;
  children: React.ReactNode;
}

const Field: React.FC<FieldProps> = ({ label, children }) => (
  <div className="space-y-1">
    <label className="block text-xs font-medium text-slate-400">{label}</label>
    {children}
  </div>
);

/** RAG 設定フォーム */
export const RAGConfigForm: React.FC<RAGConfigFormProps> = ({
  config,
  onSave,
  readOnly = false,
}) => {
  const [form, setForm] = useState<RAGConfigValues>(config);

  const update = useCallback(
    <K extends keyof RAGConfigValues>(key: K, value: RAGConfigValues[K]) => {
      setForm((prev) => ({ ...prev, [key]: value }));
    },
    [],
  );

  const inputCls =
    'w-full px-3 py-2 bg-slate-800 border border-slate-700 rounded-lg text-sm text-slate-200 disabled:opacity-50 focus:outline-none focus:ring-1 focus:ring-indigo-500';
  const selectCls = inputCls;

  return (
    <div className="space-y-6">
      {/* チャンキング */}
      <section className="space-y-3">
        <h3 className="text-sm font-semibold text-slate-300 border-b border-slate-700 pb-1">
          チャンキング設定
        </h3>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <Field label="戦略">
            <select
              value={form.chunk_strategy}
              onChange={(e) => update('chunk_strategy', e.target.value)}
              disabled={readOnly}
              className={selectCls}
            >
              {CHUNK_STRATEGIES.map((s) => (
                <option key={s.value} value={s.value}>
                  {s.label}
                </option>
              ))}
            </select>
          </Field>
          <Field label="チャンクサイズ">
            <input
              type="number"
              value={form.chunk_size}
              onChange={(e) => update('chunk_size', Number(e.target.value))}
              disabled={readOnly}
              min={100}
              max={10000}
              className={inputCls}
            />
          </Field>
          <Field label="オーバーラップ">
            <input
              type="number"
              value={form.chunk_overlap}
              onChange={(e) => update('chunk_overlap', Number(e.target.value))}
              disabled={readOnly}
              min={0}
              max={5000}
              className={inputCls}
            />
          </Field>
        </div>
      </section>

      {/* 検索 */}
      <section className="space-y-3">
        <h3 className="text-sm font-semibold text-slate-300 border-b border-slate-700 pb-1">
          検索設定
        </h3>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
          <Field label="検索手法">
            <select
              value={form.retrieval_method}
              onChange={(e) => update('retrieval_method', e.target.value)}
              disabled={readOnly}
              className={selectCls}
            >
              {RETRIEVAL_METHODS.map((m) => (
                <option key={m.value} value={m.value}>
                  {m.label}
                </option>
              ))}
            </select>
          </Field>
          <Field label="リランカー">
            <select
              value={form.reranker ?? 'none'}
              onChange={(e) =>
                update('reranker', e.target.value === 'none' ? null : e.target.value)
              }
              disabled={readOnly}
              className={selectCls}
            >
              {RERANKERS.map((r) => (
                <option key={r.value} value={r.value}>
                  {r.label}
                </option>
              ))}
            </select>
          </Field>
          <Field label="Top-K">
            <input
              type="number"
              value={form.top_k}
              onChange={(e) => update('top_k', Number(e.target.value))}
              disabled={readOnly}
              min={1}
              max={100}
              className={inputCls}
            />
          </Field>
          <Field label="最小類似度">
            <input
              type="number"
              value={form.min_similarity}
              onChange={(e) => update('min_similarity', Number(e.target.value))}
              disabled={readOnly}
              min={0}
              max={1}
              step={0.05}
              className={inputCls}
            />
          </Field>
        </div>
      </section>

      {/* 保存ボタン */}
      {!readOnly && onSave && (
        <div className="flex justify-end">
          <button
            onClick={() => onSave(form)}
            className="px-5 py-2 text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-500 rounded-lg transition-colors"
          >
            保存
          </button>
        </div>
      )}
    </div>
  );
};

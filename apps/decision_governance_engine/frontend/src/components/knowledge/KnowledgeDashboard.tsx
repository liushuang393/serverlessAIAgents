/**
 * 知識ベースダッシュボード & コレクション管理.
 *
 * 概要カード + コレクション一覧 + 作成/編集フォーム。
 */

import React, { useState } from 'react';
import { useKnowledgeStore } from '../../store/useKnowledgeStore';
import { useI18n } from '../../i18n';

interface Props {
  showCollectionForm?: boolean;
}

export const KnowledgeDashboard: React.FC<Props> = ({ showCollectionForm }) => {
  const { t } = useI18n();
  const { collections, createCollection, deleteCollection, loading } = useKnowledgeStore();
  const [showForm, setShowForm] = useState(!!showCollectionForm);
  const [form, setForm] = useState({
    collection_name: '',
    display_name: '',
    description: '',
    chunk_strategy: 'recursive',
    chunk_size: 1000,
    chunk_overlap: 200,
    retrieval_method: 'semantic',
    top_k: 5,
  });

  const totalDocs = collections.reduce((sum, c) => sum + (c.document_count || 0), 0);

  const handleCreate = async () => {
    if (!form.collection_name.trim()) return;
    await createCollection(form);
    setForm({ collection_name: '', display_name: '', description: '', chunk_strategy: 'recursive', chunk_size: 1000, chunk_overlap: 200, retrieval_method: 'semantic', top_k: 5 });
    setShowForm(false);
  };

  const handleDelete = async (name: string) => {
    if (!confirm(t('kb.confirm_delete_collection'))) return;
    await deleteCollection(name);
  };

  return (
    <div className="space-y-6">
      {/* 統計カード */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
        <StatCard label={t('kb.stat_collections')} value={String(collections.length)} color="indigo" />
        <StatCard label={t('kb.stat_documents')} value={String(totalDocs)} color="emerald" />
        <StatCard label={t('kb.stat_active')} value={String(collections.length)} color="amber" />
        <StatCard label={t('kb.stat_agents')} value="2" color="violet" />
      </div>

      {/* コレクション一覧 */}
      <div className="bg-[#12121a] rounded-xl border border-white/5 p-6">
        <div className="flex items-center justify-between mb-4">
          <h2 className="text-sm font-medium text-slate-300">{t('kb.collections')}</h2>
          <button
            onClick={() => setShowForm(!showForm)}
            className="px-3 py-1.5 bg-indigo-600 hover:bg-indigo-500 rounded-lg text-xs font-medium transition-colors"
          >
            {t('kb.create_collection')}
          </button>
        </div>

        {/* 作成フォーム */}
        {showForm && (
          <div className="mb-6 bg-[#0a0a0f] rounded-lg border border-white/10 p-4 space-y-3">
            <div className="grid grid-cols-2 gap-3">
              <input
                value={form.collection_name}
                onChange={(e) => setForm({ ...form, collection_name: e.target.value })}
                placeholder={t('kb.collection_name')}
                className="px-3 py-2 bg-transparent border border-white/10 rounded-lg text-sm text-white placeholder-slate-600 focus:outline-none focus:border-indigo-500"
              />
              <input
                value={form.display_name}
                onChange={(e) => setForm({ ...form, display_name: e.target.value })}
                placeholder={t('kb.display_name')}
                className="px-3 py-2 bg-transparent border border-white/10 rounded-lg text-sm text-white placeholder-slate-600 focus:outline-none focus:border-indigo-500"
              />
            </div>
            <textarea
              value={form.description}
              onChange={(e) => setForm({ ...form, description: e.target.value })}
              placeholder={t('kb.description')}
              rows={2}
              className="w-full px-3 py-2 bg-transparent border border-white/10 rounded-lg text-sm text-white placeholder-slate-600 resize-none focus:outline-none focus:border-indigo-500"
            />
            <div className="grid grid-cols-3 gap-3">
              <select
                value={form.chunk_strategy}
                onChange={(e) => setForm({ ...form, chunk_strategy: e.target.value })}
                className="px-3 py-2 bg-[#0a0a0f] border border-white/10 rounded-lg text-sm text-white focus:outline-none focus:border-indigo-500"
              >
                <option value="recursive">Recursive</option>
                <option value="fixed">Fixed</option>
                <option value="semantic">Semantic</option>
                <option value="sentence">Sentence</option>
              </select>
              <input
                type="number"
                value={form.chunk_size}
                onChange={(e) => setForm({ ...form, chunk_size: Number(e.target.value) })}
                placeholder={t('kb.chunk_size')}
                className="px-3 py-2 bg-transparent border border-white/10 rounded-lg text-sm text-white focus:outline-none focus:border-indigo-500"
              />
              <input
                type="number"
                value={form.chunk_overlap}
                onChange={(e) => setForm({ ...form, chunk_overlap: Number(e.target.value) })}
                placeholder={t('kb.chunk_overlap')}
                className="px-3 py-2 bg-transparent border border-white/10 rounded-lg text-sm text-white focus:outline-none focus:border-indigo-500"
              />
            </div>
            <div className="flex justify-end gap-2">
              <button
                onClick={() => setShowForm(false)}
                className="px-3 py-1.5 bg-slate-800 hover:bg-slate-700 rounded-lg text-xs transition-colors"
              >
                {t('common.cancel')}
              </button>
              <button
                onClick={handleCreate}
                disabled={!form.collection_name.trim()}
                className="px-3 py-1.5 bg-indigo-600 hover:bg-indigo-500 disabled:opacity-40 rounded-lg text-xs font-medium transition-colors"
              >
                {t('kb.create')}
              </button>
            </div>
          </div>
        )}

        {/* コレクション一覧 */}
        {loading ? (
          <div className="text-center py-8 text-slate-500">{t('common.loading')}</div>
        ) : collections.length === 0 ? (
          <div className="text-center py-8 text-slate-500">{t('kb.no_collections')}</div>
        ) : (
          <div className="space-y-2">
            {collections.map((col) => (
              <div
                key={col.collection_name}
                className="flex items-center justify-between bg-[#0a0a0f] rounded-lg p-4 border border-white/5 group"
              >
                <div className="flex-1 min-w-0">
                  <div className="flex items-center gap-2">
                    <span className="text-sm font-medium text-white">{col.display_name || col.collection_name}</span>
                    <span className="text-[10px] px-1.5 py-0.5 bg-indigo-500/20 text-indigo-300 rounded">{col.chunk_strategy}</span>
                  </div>
                  <p className="text-xs text-slate-500 mt-0.5 truncate">{col.description || col.collection_name}</p>
                  <div className="flex gap-4 mt-1 text-[10px] text-slate-600">
                    <span>{t('kb.stat_documents')}: {col.document_count}</span>
                    <span>Top-K: {col.top_k}</span>
                    <span>{col.retrieval_method}</span>
                  </div>
                </div>
                <button
                  onClick={() => handleDelete(col.collection_name)}
                  className="text-slate-600 hover:text-red-400 transition-colors opacity-0 group-hover:opacity-100 text-xs px-2 py-1"
                >
                  {t('kb.delete')}
                </button>
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  );
};

/** 統計カード */
const StatCard: React.FC<{ label: string; value: string; color: string }> = ({ label, value, color }) => (
  <div className="bg-[#12121a] rounded-xl border border-white/5 p-4">
    <p className="text-xs text-slate-500 mb-1">{label}</p>
    <p className={`text-2xl font-bold text-${color}-400`}>{value}</p>
  </div>
);

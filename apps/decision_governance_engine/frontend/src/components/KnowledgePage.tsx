/**
 * 知識ベース管理ページ.
 *
 * 目的: 術（shu）/器（qi）の RAG 知識ベースを管理
 * API: GET/POST/DELETE /api/knowledge/{agent_type}
 */

import React, { useState, useEffect, useCallback } from 'react';
import { useDecisionStore } from '../store/useDecisionStore';
import { useI18n } from '../i18n';

/** ドキュメント型 */
interface KnowledgeDoc {
  id: string;
  content: string;
  topic: string;
  metadata?: Record<string, unknown>;
}

interface KnowledgePageProps {
  agentType: 'shu' | 'qi';
}

export const KnowledgePage: React.FC<KnowledgePageProps> = ({ agentType }) => {
  const { t } = useI18n();
  const { setPage } = useDecisionStore();
  const [documents, setDocuments] = useState<KnowledgeDoc[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // 入力フォーム
  const [newContent, setNewContent] = useState('');
  const [newTopic, setNewTopic] = useState('default');
  const [isSubmitting, setIsSubmitting] = useState(false);

  const agentInfo = agentType === 'shu'
    ? { name: t('knowledge.agent_shu_name'), label: t('knowledge.agent_shu_label'), icon: '📋', color: 'indigo' }
    : { name: t('knowledge.agent_qi_name'), label: t('knowledge.agent_qi_label'), icon: '🔧', color: 'violet' };

  /** 一覧取得 */
  const fetchDocuments = useCallback(async () => {
    setIsLoading(true);
    try {
      const res = await fetch(`/api/knowledge/${agentType}`);
      if (!res.ok) throw new Error(t('knowledge.fetch_failed'));
      const data = await res.json();
      setDocuments(data.documents || []);
    } catch (err) {
      // エラー詳細をUIに表示（型安全）
      const errorMessage = err instanceof Error ? err.message : t('knowledge.unknown_error');
      setError(errorMessage);
    } finally {
      setIsLoading(false);
    }
  }, [agentType, t]);

  useEffect(() => {
    fetchDocuments();
  }, [fetchDocuments]);

  /** 追加 */
  const handleAdd = async () => {
    if (!newContent.trim() || newContent.length < 10) {
      setError(t('knowledge.min_chars_error'));
      return;
    }
    setIsSubmitting(true);
    setError(null);
    try {
      const res = await fetch(`/api/knowledge/${agentType}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ content: newContent, topic: newTopic }),
      });
      if (!res.ok) throw new Error(t('knowledge.add_failed'));
      setNewContent('');
      await fetchDocuments();
    } catch (err) {
      // エラー詳細をUIに表示（型安全）
      const errorMessage = err instanceof Error ? err.message : t('knowledge.unknown_error');
      setError(errorMessage);
    } finally {
      setIsSubmitting(false);
    }
  };

  /** 削除 */
  const handleDelete = async (docId: string) => {
    if (!confirm(t('knowledge.delete_confirm'))) return;
    try {
      const res = await fetch(`/api/knowledge/${agentType}/${docId}`, { method: 'DELETE' });
      if (!res.ok) throw new Error(t('knowledge.delete_failed'));
      await fetchDocuments();
    } catch (err) {
      // エラー詳細をUIに表示（型安全）
      const errorMessage = err instanceof Error ? err.message : t('knowledge.unknown_error');
      setError(errorMessage);
    }
  };

  return (
    <div className="min-h-screen bg-[#0a0a0f] text-white">
      {/* Header */}
      <header className="border-b border-white/5 px-6 py-4">
        <div className="max-w-4xl mx-auto flex items-center justify-between">
          <div className="flex items-center gap-3">
            <span className="text-2xl">{agentInfo.icon}</span>
            <div>
              <h1 className="font-semibold text-lg">{agentInfo.name}・{t('knowledge.page_title')}</h1>
              <p className="text-xs text-slate-500">{agentInfo.label} {t('knowledge.page_subtitle')}</p>
            </div>
          </div>
          <button
            onClick={() => setPage('input')}
            className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm transition-colors"
          >
            {t('knowledge.back')}
          </button>
        </div>
      </header>

      <main className="max-w-4xl mx-auto px-6 py-8">
        {/* エラー */}
        {error && (
          <div className="mb-6 bg-red-500/10 border border-red-500/20 rounded-xl p-4 text-red-400">
            ⚠️ {error}
            <button onClick={() => setError(null)} className="ml-2 text-slate-500 hover:text-white">✕</button>
          </div>
        )}

        {/* 追加フォーム */}
        <div className="bg-[#12121a] rounded-xl border border-white/5 p-6 mb-8">
          <h2 className="text-sm font-medium text-slate-300 mb-4"><span aria-hidden="true">📚</span> {t('knowledge.add_new')}</h2>
          <div className="space-y-4">
            <div>
              <label className="block text-xs text-slate-500 mb-2">{t('knowledge.topic')}</label>
              <select
                value={newTopic}
                onChange={(e) => setNewTopic(e.target.value)}
                className="w-full px-4 py-2 bg-[#0a0a0f] border border-white/10 rounded-lg text-white focus:outline-none focus:border-indigo-500"
              >
                {agentType === 'shu' ? (
                  <>
                    <option value="industry_practices">{t('knowledge.topic_industry')}</option>
                    <option value="case_studies">{t('knowledge.topic_case_studies')}</option>
                    <option value="methodology">{t('knowledge.topic_methodology')}</option>
                  </>
                ) : (
                  <>
                    <option value="technical_docs">{t('knowledge.topic_technical_docs')}</option>
                    <option value="compliance">{t('knowledge.topic_compliance')}</option>
                    <option value="architecture">{t('knowledge.topic_architecture')}</option>
                  </>
                )}
              </select>
            </div>
            <div>
              <label className="block text-xs text-slate-500 mb-2">{t('knowledge.content_label')}</label>
              <textarea
                value={newContent}
                onChange={(e) => setNewContent(e.target.value)}
                placeholder={t('knowledge.content_placeholder')}
                className="w-full h-32 px-4 py-3 bg-[#0a0a0f] border border-white/10 rounded-lg text-white resize-none focus:outline-none focus:border-indigo-500"
              />
              <div className="text-xs text-slate-600 mt-1">{t('knowledge.chars_count').replaceAll('{count}', String(newContent.length))}</div>
            </div>
            <button
              onClick={handleAdd}
              disabled={isSubmitting || newContent.length < 10}
              className={`w-full py-3 rounded-lg font-medium transition-all ${
                newContent.length >= 10 && !isSubmitting
                  ? `bg-${agentInfo.color}-600 hover:bg-${agentInfo.color}-500 text-white`
                  : 'bg-slate-800 text-slate-500 cursor-not-allowed'
              }`}
            >
              {isSubmitting ? t('knowledge.adding') : t('knowledge.add_btn')}
            </button>
          </div>
        </div>

        {/* 一覧 */}
        <div className="bg-[#12121a] rounded-xl border border-white/5 p-6">
          <h2 className="text-sm font-medium text-slate-300 mb-4">
            <span aria-hidden="true">📖</span> {t('knowledge.registered').replaceAll('{count}', String(documents.length))}
          </h2>
          {isLoading ? (
            <div className="text-center py-8 text-slate-500">{t('knowledge.loading')}</div>
          ) : documents.length === 0 ? (
            <div className="text-center py-8 text-slate-500">
              {t('knowledge.empty_hint')}<br />
              {t('knowledge.add_hint')}
            </div>
          ) : (
            <div className="space-y-3">
              {documents.map((doc) => (
                <div
                  key={doc.id}
                  className="bg-[#0a0a0f] rounded-lg p-4 border border-white/5 group"
                >
                  <div className="flex items-start justify-between gap-4">
                    <div className="flex-1 min-w-0">
                      <span className={`text-xs px-2 py-0.5 bg-${agentInfo.color}-500/20 text-${agentInfo.color}-300 rounded mb-2 inline-block`}>
                        {doc.topic}
                      </span>
                      <p className="text-sm text-slate-300 whitespace-pre-wrap break-words">
                        {doc.content}
                      </p>
                    </div>
                    <button
                      onClick={() => handleDelete(doc.id)}
                      className="text-slate-600 hover:text-red-400 transition-colors opacity-0 group-hover:opacity-100"
                      title={t('knowledge.delete')}
                    >
                      🗑️
                    </button>
                  </div>
                </div>
              ))}
            </div>
          )}
        </div>
      </main>
    </div>
  );
};


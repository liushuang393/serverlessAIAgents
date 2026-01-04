/**
 * 知識ベース管理ページ.
 *
 * 目的: 術（shu）/器（qi）の RAG 知識ベースを管理
 * API: GET/POST/DELETE /api/knowledge/{agent_type}
 */

import React, { useState, useEffect, useCallback } from 'react';
import { useDecisionStore } from '../store/useDecisionStore';

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
  const { setPage } = useDecisionStore();
  const [documents, setDocuments] = useState<KnowledgeDoc[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // 入力フォーム
  const [newContent, setNewContent] = useState('');
  const [newTopic, setNewTopic] = useState('default');
  const [isSubmitting, setIsSubmitting] = useState(false);

  const agentInfo = agentType === 'shu'
    ? { name: '術', label: '実行計画', icon: '📋', color: 'indigo' }
    : { name: '器', label: '技術実装', icon: '🔧', color: 'violet' };

  /** 一覧取得 */
  const fetchDocuments = useCallback(async () => {
    setIsLoading(true);
    try {
      const res = await fetch(`/api/knowledge/${agentType}`);
      if (!res.ok) throw new Error('取得失敗');
      const data = await res.json();
      setDocuments(data.documents || []);
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setIsLoading(false);
    }
  }, [agentType]);

  useEffect(() => {
    fetchDocuments();
  }, [fetchDocuments]);

  /** 追加 */
  const handleAdd = async () => {
    if (!newContent.trim() || newContent.length < 10) {
      setError('内容は10文字以上入力してください');
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
      if (!res.ok) throw new Error('追加失敗');
      setNewContent('');
      await fetchDocuments();
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setIsSubmitting(false);
    }
  };

  /** 削除 */
  const handleDelete = async (docId: string) => {
    if (!confirm('このドキュメントを削除しますか？')) return;
    try {
      const res = await fetch(`/api/knowledge/${agentType}/${docId}`, { method: 'DELETE' });
      if (!res.ok) throw new Error('削除失敗');
      await fetchDocuments();
    } catch (e) {
      setError((e as Error).message);
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
              <h1 className="font-semibold text-lg">{agentInfo.name}・知識ベース設定</h1>
              <p className="text-xs text-slate-500">{agentInfo.label}Agent の RAG 知識を管理</p>
            </div>
          </div>
          <button
            onClick={() => setPage('input')}
            className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm transition-colors"
          >
            ← 戻る
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
          <h2 className="text-sm font-medium text-slate-300 mb-4">📚 新しい知識を追加</h2>
          <div className="space-y-4">
            <div>
              <label className="block text-xs text-slate-500 mb-2">トピック</label>
              <select
                value={newTopic}
                onChange={(e) => setNewTopic(e.target.value)}
                className="w-full px-4 py-2 bg-[#0a0a0f] border border-white/10 rounded-lg text-white focus:outline-none focus:border-indigo-500"
              >
                {agentType === 'shu' ? (
                  <>
                    <option value="industry_practices">業界プラクティス</option>
                    <option value="case_studies">事例データ</option>
                    <option value="methodology">手法・方法論</option>
                  </>
                ) : (
                  <>
                    <option value="technical_docs">技術ドキュメント</option>
                    <option value="compliance">コンプライアンス</option>
                    <option value="architecture">アーキテクチャ</option>
                  </>
                )}
              </select>
            </div>
            <div>
              <label className="block text-xs text-slate-500 mb-2">内容（10文字以上）</label>
              <textarea
                value={newContent}
                onChange={(e) => setNewContent(e.target.value)}
                placeholder="例: アジャイル開発では2週間のスプリントが推奨される..."
                className="w-full h-32 px-4 py-3 bg-[#0a0a0f] border border-white/10 rounded-lg text-white resize-none focus:outline-none focus:border-indigo-500"
              />
              <div className="text-xs text-slate-600 mt-1">{newContent.length} 文字</div>
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
              {isSubmitting ? '追加中...' : '＋ 知識を追加'}
            </button>
          </div>
        </div>

        {/* 一覧 */}
        <div className="bg-[#12121a] rounded-xl border border-white/5 p-6">
          <h2 className="text-sm font-medium text-slate-300 mb-4">
            📖 登録済みの知識 ({documents.length}件)
          </h2>
          {isLoading ? (
            <div className="text-center py-8 text-slate-500">読み込み中...</div>
          ) : documents.length === 0 ? (
            <div className="text-center py-8 text-slate-500">
              まだ知識が登録されていません。<br />
              上のフォームから追加してください。
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
                      title="削除"
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


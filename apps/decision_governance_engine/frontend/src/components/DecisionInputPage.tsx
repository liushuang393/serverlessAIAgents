/**
 * 入力画面コンポーネント.
 *
 * 目的: 意思決定の質問と制約条件を入力
 * API対接: POST /api/decision (REST)
 * 設計参考: design/decision-input-ui.tsx
 */

import React, { useState, useCallback } from 'react';
import { useDecisionStore } from '../store/useDecisionStore';
import { decisionApi, DecisionApiError } from '../api/client';

/** 即時拒否パターン */
const REJECT_PATTERNS = [
  { regex: /(天気|気温|weather|何時)/i, message: '天気や時刻の情報にはお答えできません。', category: '事実確認' },
  { regex: /(このシステム|このAI|どうやって作|仕組み)/i, message: 'システム自体への質問にはお答えできません。', category: 'システム質問' },
  { regex: /(.+)(とは何|って何|とは？)/i, message: '用語や概念の説明にはお答えできません。', category: '定義質問' },
  { regex: /^(こんにちは|hello|hi|ありがとう)/i, message: '雑談には対応していません。', category: '雑談' },
  { regex: /(コード.*書いて|プログラム.*作)/i, message: 'コード生成には対応していません。', category: '技術実装' },
];

/** 即時拒否チェック */
const checkInstantReject = (text: string) => {
  for (const p of REJECT_PATTERNS) {
    if (p.regex.test(text)) {
      return { category: p.category, message: p.message };
    }
  }
  return null;
};

export const DecisionInputPage: React.FC = () => {
  const {
    question,
    constraints,
    setQuestion,
    setConstraints,
    setPage,
    setReportId,
    setError,
    buildRequest,
  } = useDecisionStore();

  const [isSubmitting, setIsSubmitting] = useState(false);
  const [rejection, setRejection] = useState<{ category: string; message: string } | null>(null);
  const [apiError, setApiError] = useState<string | null>(null);
  const [techInput, setTechInput] = useState('');
  const [regInput, setRegInput] = useState('');

  /** タグ追加 */
  const addTag = useCallback((type: 'technical' | 'regulatory', value: string) => {
    if (value.trim()) {
      setConstraints({
        [type]: [...constraints[type], value.trim()],
      });
      if (type === 'technical') setTechInput('');
      if (type === 'regulatory') setRegInput('');
    }
  }, [constraints, setConstraints]);

  /** タグ削除 */
  const removeTag = useCallback((type: 'technical' | 'regulatory', index: number) => {
    setConstraints({
      [type]: constraints[type].filter((_, i) => i !== index),
    });
  }, [constraints, setConstraints]);

  /** 送信処理 - REST API */
  const handleSubmit = useCallback(async () => {
    if (question.length < 10) return;

    // クライアント側即時拒否チェック
    const rejectResult = checkInstantReject(question);
    if (rejectResult) {
      setRejection(rejectResult);
      return;
    }

    setRejection(null);
    setApiError(null);
    setIsSubmitting(true);

    try {
      const request = buildRequest();
      const response = await decisionApi.processDecision(request);

      if (response.status === 'rejected') {
        // サーバー側拒否
        setRejection({
          category: 'サーバー検証',
          message: response.message || response.reason || '質問が受理されませんでした',
        });
      } else if (response.status === 'success') {
        // 成功 - 進捗画面へ遷移
        setReportId(response.report_id);
        setPage('processing');
      }
    } catch (err) {
      if (err instanceof DecisionApiError) {
        setApiError(err.message);
      } else {
        setApiError('API呼び出しに失敗しました');
      }
      setError(err instanceof Error ? err.message : 'Unknown error');
    } finally {
      setIsSubmitting(false);
    }
  }, [question, buildRequest, setReportId, setPage, setError]);

  /** SSE モードで送信 */
  const handleSubmitWithStream = useCallback(() => {
    if (question.length < 10) return;

    const rejectResult = checkInstantReject(question);
    if (rejectResult) {
      setRejection(rejectResult);
      return;
    }

    // 進捗画面へ遷移（SSE接続は進捗画面で開始）
    setPage('processing');
  }, [question, setPage]);

  const isValid = question.length >= 10;

  return (
    <div className="min-h-screen bg-[#0a0a0f] text-white">
      {/* Header */}
      <header className="border-b border-white/5 px-6 py-4">
        <div className="max-w-6xl mx-auto flex items-center justify-between">
          <div className="flex items-center gap-3">
            <div className="w-10 h-10 rounded-xl bg-gradient-to-br from-indigo-500 to-violet-600 flex items-center justify-center">
              <span className="text-xl">⚡</span>
            </div>
            <div>
              <h1 className="font-semibold text-lg">Decision Agent</h1>
              <p className="text-xs text-slate-500">Enterprise Decision Platform</p>
            </div>
          </div>
        </div>
      </header>

      {/* Main */}
      <main className="max-w-3xl mx-auto px-6 py-12">
        <div className="text-center mb-12">
          <h2 className="text-3xl font-bold mb-3 bg-gradient-to-r from-white to-slate-400 bg-clip-text text-transparent">
            意思決定を構造化する
          </h2>
          <p className="text-slate-400">
            複雑な問題を「道・法・術・器」のフレームワークで分析
          </p>
        </div>

        {/* Input Card - 続きは str-replace-editor で追加 */}
        <div className="bg-[#12121a] rounded-2xl border border-white/5 p-8">
          {/* エラー表示 */}
          {apiError && (
            <div className="mb-6 bg-orange-500/5 border border-orange-500/20 rounded-xl p-4">
              <span className="text-orange-400">🔌 API接続エラー: {apiError}</span>
            </div>
          )}

          {rejection && (
            <div className="mb-6 bg-red-500/5 border border-red-500/20 rounded-xl p-4">
              <span className="text-red-400">⚠️ {rejection.category}: {rejection.message}</span>
            </div>
          )}

          {/* 質問入力 */}
          <div className="mb-8">
            <label className="block text-sm font-medium text-slate-300 mb-3">
              解決したい問題・意思決定事項
            </label>
            <textarea
              value={question}
              onChange={(e) => setQuestion(e.target.value)}
              placeholder="例）新規事業として SaaS プロダクトを立ち上げるべきか、それとも受託開発を強化すべきか。現在のキャッシュフロー状況と市場環境を考慮して判断したい..."
              className="w-full h-40 bg-[#0a0a0f] border border-white/10 rounded-xl px-4 py-3 text-white placeholder-slate-600 resize-none focus:outline-none focus:border-indigo-500/50 focus:ring-2 focus:ring-indigo-500/20 transition-all"
            />
            <div className="flex justify-between mt-2">
              <span className="text-xs text-slate-600">
                {question.length < 10 ? '最低10文字以上入力してください' : '✓ 入力OK'}
              </span>
              <span className="text-xs text-slate-600">{question.length} 文字</span>
            </div>
          </div>

          {/* 制約条件セクション */}
          <div className="mb-8">
            <h3 className="text-sm font-medium text-slate-300 mb-4 flex items-center gap-2">
              <span className="w-5 h-5 rounded bg-slate-800 flex items-center justify-center text-xs">⚙️</span>
              制約条件（任意）
            </h3>

            {/* 主要制約（3カラム） */}
            <div className="grid grid-cols-3 gap-4 mb-6">
              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <div className="flex items-center gap-2 mb-2">
                  <span>💰</span>
                  <span className="text-xs text-slate-400">予算</span>
                </div>
                <div className="flex items-center">
                  <span className="text-slate-600 mr-1">¥</span>
                  <input
                    type="text"
                    value={constraints.budget}
                    onChange={(e) => setConstraints({ budget: e.target.value })}
                    placeholder="500"
                    className="w-full bg-transparent text-white text-lg font-medium focus:outline-none"
                  />
                  <span className="text-slate-600 ml-1">万</span>
                </div>
              </div>

              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <div className="flex items-center gap-2 mb-2">
                  <span>⏱️</span>
                  <span className="text-xs text-slate-400">期限</span>
                </div>
                <div className="flex items-center">
                  <input
                    type="text"
                    value={constraints.timeline}
                    onChange={(e) => setConstraints({ timeline: e.target.value })}
                    placeholder="6"
                    className="w-full bg-transparent text-white text-lg font-medium focus:outline-none"
                  />
                  <span className="text-slate-600 ml-1">ヶ月</span>
                </div>
              </div>

              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <div className="flex items-center gap-2 mb-2">
                  <span>👥</span>
                  <span className="text-xs text-slate-400">チーム</span>
                </div>
                <div className="flex items-center">
                  <input
                    type="text"
                    value={constraints.team}
                    onChange={(e) => setConstraints({ team: e.target.value })}
                    placeholder="5"
                    className="w-full bg-transparent text-white text-lg font-medium focus:outline-none"
                  />
                  <span className="text-slate-600 ml-1">名</span>
                </div>
              </div>
            </div>

            {/* タグ入力（2カラム） */}
            <div className="grid grid-cols-2 gap-4">
              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <div className="flex items-center gap-2 mb-3">
                  <span>🔧</span>
                  <span className="text-xs text-slate-400">技術制約</span>
                </div>
                <div className="flex flex-wrap gap-2 mb-2">
                  {constraints.technical.map((tag, i) => (
                    <span key={i} className="px-2 py-1 bg-indigo-500/20 text-indigo-300 rounded-lg text-xs flex items-center gap-1">
                      {tag}
                      <button onClick={() => removeTag('technical', i)} className="hover:text-white">×</button>
                    </span>
                  ))}
                </div>
                <input
                  type="text"
                  value={techInput}
                  onChange={(e) => setTechInput(e.target.value)}
                  onKeyPress={(e) => e.key === 'Enter' && addTag('technical', techInput)}
                  placeholder="例: AWS, Python... (Enter追加)"
                  className="w-full bg-transparent text-sm text-white focus:outline-none placeholder-slate-600"
                />
              </div>

              <div className="bg-[#0a0a0f] rounded-xl p-4 border border-white/5">
                <div className="flex items-center gap-2 mb-3">
                  <span>📋</span>
                  <span className="text-xs text-slate-400">規制・コンプライアンス</span>
                </div>
                <div className="flex flex-wrap gap-2 mb-2">
                  {constraints.regulatory.map((tag, i) => (
                    <span key={i} className="px-2 py-1 bg-amber-500/20 text-amber-300 rounded-lg text-xs flex items-center gap-1">
                      {tag}
                      <button onClick={() => removeTag('regulatory', i)} className="hover:text-white">×</button>
                    </span>
                  ))}
                </div>
                <input
                  type="text"
                  value={regInput}
                  onChange={(e) => setRegInput(e.target.value)}
                  onKeyPress={(e) => e.key === 'Enter' && addTag('regulatory', regInput)}
                  placeholder="例: GDPR, 金融規制... (Enter追加)"
                  className="w-full bg-transparent text-sm text-white focus:outline-none placeholder-slate-600"
                />
              </div>
            </div>
          </div>

          {/* 送信ボタン */}
          <button
            onClick={handleSubmitWithStream}
            disabled={!isValid || isSubmitting}
            className={`w-full py-4 rounded-xl font-medium text-lg transition-all flex items-center justify-center gap-3 ${
              isValid && !isSubmitting
                ? 'bg-gradient-to-r from-indigo-600 to-violet-600 hover:from-indigo-500 hover:to-violet-500 text-white shadow-lg shadow-indigo-500/25'
                : 'bg-slate-800 text-slate-500 cursor-not-allowed'
            }`}
          >
            {isSubmitting ? (
              <>
                <div className="w-5 h-5 border-2 border-white/30 border-t-white rounded-full animate-spin" />
                分析開始中...
              </>
            ) : (
              <>
                <span>▶</span>
                決策分析を開始する
              </>
            )}
          </button>

          {/* 処理時間の目安 */}
          <p className="text-center text-xs text-slate-600 mt-4">
            通常2〜3分で分析完了 • 結果はPDF出力可能
          </p>
        </div>

        {/* 機能説明カード */}
        <div className="grid grid-cols-4 gap-4 mt-8">
          {[
            { icon: '🎯', label: '道', desc: '本質抽出' },
            { icon: '🛤️', label: '法', desc: '戦略選定' },
            { icon: '📋', label: '術', desc: '実行計画' },
            { icon: '🔧', label: '器', desc: '技術実装' },
          ].map((item, i) => (
            <div key={i} className="bg-[#12121a]/50 rounded-xl p-4 text-center border border-white/5">
              <span className="text-2xl">{item.icon}</span>
              <div className="text-sm font-medium mt-2">{item.label}</div>
              <div className="text-xs text-slate-500">{item.desc}</div>
            </div>
          ))}
        </div>
      </main>
    </div>
  );
};


/**
 * 履歴画面コンポーネント.
 *
 * 目的: 過去の決策履歴を表示・検索・レポート再ダウンロード
 * API対接: GET /api/decision/history, GET /api/report/{id}/pdf
 */

import React, { useCallback, useEffect, useState } from 'react';
import { useDecisionStore } from '../store/useDecisionStore';
import { useAuthStore } from '../store/useAuthStore';
import { decisionApi } from '../api/client';
import type { ServerHistoryItem } from '../types';

/** 決策結果のバッジ色 */
const DECISION_ROLE_COLORS: Record<string, string> = {
  GO: 'bg-emerald-500/10 text-emerald-400 border-emerald-500/30',
  NO_GO: 'bg-red-500/10 text-red-400 border-red-500/30',
  DELAY: 'bg-amber-500/10 text-amber-400 border-amber-500/30',
  PILOT: 'bg-blue-500/10 text-blue-400 border-blue-500/30',
};

/** 決策結果の日本語ラベル */
const DECISION_ROLE_LABELS: Record<string, string> = {
  GO: '立項',
  NO_GO: '不立項',
  DELAY: '延後',
  PILOT: 'パイロット',
};

export const HistoryPage: React.FC = () => {
  const { setPage, reset } = useDecisionStore();
  const { user, performLogout } = useAuthStore();

  const [items, setItems] = useState<ServerHistoryItem[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [exportingId, setExportingId] = useState<string | null>(null);

  // フィルター状態
  const [filterRole, setFilterRole] = useState<string>('');
  const [filterMode, setFilterMode] = useState<string>('');
  const [limit, setLimit] = useState(20);

  /** 履歴取得 */
  const fetchHistory = useCallback(async () => {
    setIsLoading(true);
    setError(null);

    try {
      const response = await decisionApi.getHistory({
        limit,
        decision_role: filterRole || undefined,
        mode: filterMode || undefined,
      });

      if (response.status === 'success') {
        setItems(response.items);
      } else if (response.status === 'disabled') {
        setError('履歴機能が無効になっています');
        setItems([]);
      } else {
        setError('履歴の取得に失敗しました');
        setItems([]);
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : '履歴の取得に失敗しました';
      setError(message);
      setItems([]);
    } finally {
      setIsLoading(false);
    }
  }, [limit, filterRole, filterMode]);

  // 初回読み込み & フィルター変更時
  useEffect(() => {
    fetchHistory();
  }, [fetchHistory]);

  /** PDF ダウンロード */
  const handleDownloadPdf = useCallback(async (requestId: string) => {
    setExportingId(requestId);
    try {
      const blob = await decisionApi.exportPdf(requestId);
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `decision-report-${requestId}.pdf`;
      a.click();
      URL.revokeObjectURL(url);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'PDFのダウンロードに失敗しました';
      setError(message);
    } finally {
      setExportingId(null);
    }
  }, []);

  /** ログアウト */
  const handleLogout = useCallback(async () => {
    await performLogout();
    reset();
    setPage('input');
  }, [performLogout, reset, setPage]);

  /** 日付フォーマット */
  const formatDate = (dateStr: string) => {
    const date = new Date(dateStr);
    return date.toLocaleString('ja-JP', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  return (
    <div className="min-h-screen bg-[#0a0a0f] text-white">
      {/* Header */}
      <header className="border-b border-white/5 px-6 py-4">
        <div className="max-w-6xl mx-auto flex items-center justify-between">
          <div className="flex items-center gap-3">
            <button
              onClick={() => setPage('input')}
              className="w-10 h-10 rounded-xl bg-gradient-to-br from-indigo-500 to-violet-600 flex items-center justify-center hover:opacity-90 transition-opacity"
            >
              <span className="text-xl">⚡</span>
            </button>
            <div>
              <h1 className="font-semibold text-lg">決策履歴</h1>
              <p className="text-xs text-slate-500">過去の意思決定レポート一覧</p>
            </div>
          </div>

          {/* ナビゲーション */}
          <div className="flex items-center gap-4">
            <button
              onClick={() => setPage('input')}
              className="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all"
            >
              ⚡ 新規分析
            </button>

            {/* ユーザーメニュー */}
            {user && (
              <div className="flex items-center gap-3 pl-4 border-l border-white/10">
                <div className="text-right">
                  <div className="text-sm font-medium text-white">{user.display_name}</div>
                  <div className="text-xs text-slate-500">{user.department}</div>
                </div>
                <button
                  onClick={handleLogout}
                  className="p-2 hover:bg-slate-800 rounded-lg transition-colors text-slate-400 hover:text-white"
                  title="ログアウト"
                >
                  <span role="img" aria-label="logout">&#128682;</span>
                </button>
              </div>
            )}
          </div>
        </div>
      </header>

      <main className="max-w-5xl mx-auto px-6 py-8">
        {/* フィルターセクション */}
        <div className="bg-[#12121a] rounded-xl border border-white/5 p-4 mb-6">
          <div className="flex items-center gap-4 flex-wrap">
            {/* 決策結果フィルター */}
            <div className="flex items-center gap-2">
              <span className="text-xs text-slate-500">結果:</span>
              <select
                value={filterRole}
                onChange={(e) => setFilterRole(e.target.value)}
                className="bg-[#0a0a0f] border border-white/10 rounded-lg px-3 py-1.5 text-sm focus:outline-none focus:border-indigo-500/50"
              >
                <option value="">すべて</option>
                <option value="GO">立項 (GO)</option>
                <option value="NO_GO">不立項 (NO_GO)</option>
                <option value="DELAY">延後 (DELAY)</option>
                <option value="PILOT">パイロット (PILOT)</option>
              </select>
            </div>

            {/* モードフィルター */}
            <div className="flex items-center gap-2">
              <span className="text-xs text-slate-500">モード:</span>
              <select
                value={filterMode}
                onChange={(e) => setFilterMode(e.target.value)}
                className="bg-[#0a0a0f] border border-white/10 rounded-lg px-3 py-1.5 text-sm focus:outline-none focus:border-indigo-500/50"
              >
                <option value="">すべて</option>
                <option value="FAST">FAST</option>
                <option value="STANDARD">STANDARD</option>
                <option value="AUDIT">AUDIT</option>
              </select>
            </div>

            {/* 表示件数 */}
            <div className="flex items-center gap-2">
              <span className="text-xs text-slate-500">表示:</span>
              <select
                value={limit}
                onChange={(e) => setLimit(Number(e.target.value))}
                className="bg-[#0a0a0f] border border-white/10 rounded-lg px-3 py-1.5 text-sm focus:outline-none focus:border-indigo-500/50"
              >
                <option value={10}>10件</option>
                <option value={20}>20件</option>
                <option value={50}>50件</option>
                <option value={100}>100件</option>
              </select>
            </div>

            {/* リフレッシュボタン */}
            <button
              onClick={fetchHistory}
              disabled={isLoading}
              className="ml-auto px-4 py-1.5 bg-indigo-600/20 hover:bg-indigo-600/30 text-indigo-300 rounded-lg text-sm transition-all flex items-center gap-2"
            >
              {isLoading ? (
                <div className="w-4 h-4 border-2 border-indigo-300/30 border-t-indigo-300 rounded-full animate-spin" />
              ) : (
                <span>&#x21BB;</span>
              )}
              更新
            </button>
          </div>
        </div>

        {/* エラー表示 */}
        {error && (
          <div className="mb-6 bg-red-500/5 border border-red-500/20 rounded-xl p-4">
            <span className="text-red-400">&#x26A0; {error}</span>
          </div>
        )}

        {/* 履歴リスト */}
        {isLoading ? (
          <div className="flex items-center justify-center py-12">
            <div className="w-8 h-8 border-3 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin" />
          </div>
        ) : items.length === 0 ? (
          <div className="text-center py-12 text-slate-500">
            <div className="text-4xl mb-4">&#128196;</div>
            <p>履歴がありません</p>
            <button
              onClick={() => setPage('input')}
              className="mt-4 px-4 py-2 bg-indigo-600 hover:bg-indigo-700 text-white rounded-lg text-sm transition-colors"
            >
              新規分析を開始
            </button>
          </div>
        ) : (
          <div className="space-y-3">
            {items.map((item) => (
              <div
                key={item.id}
                className="bg-[#12121a] rounded-xl border border-white/5 p-5 hover:border-white/10 transition-all"
              >
                <div className="flex items-start justify-between gap-4">
                  {/* 左側: 質問と詳細 */}
                  <div className="flex-1 min-w-0">
                    <div className="flex items-center gap-2 mb-2">
                      <span
                        className={`px-2 py-0.5 rounded border text-xs font-medium ${
                          DECISION_ROLE_COLORS[item.decision_role] || 'bg-slate-500/10 text-slate-400'
                        }`}
                      >
                        {DECISION_ROLE_LABELS[item.decision_role] || item.decision_role}
                      </span>
                      {item.confidence !== null && (
                        <span className="text-xs text-slate-500">
                          信頼度: {Math.round(item.confidence * 100)}%
                        </span>
                      )}
                      <span className="text-xs text-slate-600 px-2 py-0.5 bg-slate-800 rounded">
                        {item.mode}
                      </span>
                    </div>

                    <p className="text-sm text-slate-300 mb-2 line-clamp-2">{item.question}</p>

                    <div className="flex items-center gap-4 text-xs text-slate-500">
                      <span>&#128197; {formatDate(item.created_at)}</span>
                      <span className="font-mono">ID: {item.request_id.substring(0, 8)}...</span>
                    </div>
                  </div>

                  {/* 右側: アクションボタン */}
                  <div className="flex items-center gap-2 flex-shrink-0">
                    <button
                      onClick={() => handleDownloadPdf(item.request_id)}
                      disabled={exportingId === item.request_id}
                      className="px-3 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm flex items-center gap-2 transition-all disabled:opacity-50"
                    >
                      {exportingId === item.request_id ? (
                        <div className="w-4 h-4 border-2 border-white/30 border-t-white rounded-full animate-spin" />
                      ) : (
                        <span>&#128196;</span>
                      )}
                      PDF
                    </button>
                  </div>
                </div>
              </div>
            ))}
          </div>
        )}

        {/* ページネーション情報 */}
        {!isLoading && items.length > 0 && (
          <div className="mt-6 text-center text-xs text-slate-500">
            {items.length} 件表示中
          </div>
        )}
      </main>
    </div>
  );
};

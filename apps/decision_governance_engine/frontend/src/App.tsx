/**
 * Decision Governance Engine - メインアプリ.
 *
 * 目的: 認証・画面切り替えとルーティング
 */

import React, { useEffect } from 'react';
import { useDecisionStore } from './store/useDecisionStore';
import { useAuthStore } from './store/useAuthStore';
import { DecisionInputPage } from './components/DecisionInputPage';
import { ProcessingPage } from './components/ProcessingPage';
import { ReportPage } from './components/ReportPage';
import { LoginPage } from './components/LoginPage';
import { KnowledgePage } from './components/KnowledgePage';

/** ローディング画面 */
const LoadingScreen: React.FC = () => (
  <div className="min-h-screen bg-[#0a0a0f] flex items-center justify-center">
    <div className="text-center">
      <div className="w-16 h-16 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin mb-4 mx-auto" />
      <p className="text-slate-400">認証状態を確認中...</p>
    </div>
  </div>
);

const App: React.FC = () => {
  const currentPage = useDecisionStore((state) => state.currentPage);
  const { isAuthenticated, isLoading, checkAuth } = useAuthStore();

  // 起動時に認証状態をチェック
  useEffect(() => {
    checkAuth();
  }, [checkAuth]);

  // ローディング中
  if (isLoading) {
    return <LoadingScreen />;
  }

  // 未認証の場合はログイン画面
  if (!isAuthenticated) {
    return <LoginPage />;
  }

  // 認証済み: メインコンテンツ
  switch (currentPage) {
    case 'input':
      return <DecisionInputPage />;
    case 'processing':
      return <ProcessingPage />;
    case 'report':
      return <ReportPage />;
    case 'knowledge-shu':
      return <KnowledgePage agentType="shu" />;
    case 'knowledge-qi':
      return <KnowledgePage agentType="qi" />;
    default:
      return <DecisionInputPage />;
  }
};

export default App;


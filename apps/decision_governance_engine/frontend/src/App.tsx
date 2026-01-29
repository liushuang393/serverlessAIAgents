/**
 * Decision Governance Engine - メインアプリ.
 *
 * 目的: 認証・画面切り替えとルーティング
 *
 * 使用フレームワーク:
 *   - ErrorBoundary: グローバルエラーキャッチ（@agentflow/ui）
 */

import React, { useEffect } from 'react';
import { useDecisionStore } from './store/useDecisionStore';
import { useAuthStore } from './store/useAuthStore';
import { DecisionInputPage } from './components/DecisionInputPage';
import { ProcessingPage } from './components/ProcessingPage';
import { ReportPage } from './components/ReportPage';
import { HistoryPage } from './components/HistoryPage';
import { LoginPage } from './components/LoginPage';
import { KnowledgePage } from './components/KnowledgePage';

// ========================================
// エラーフォールバック UI
// ========================================

/** グローバルエラーフォールバック */
const GlobalErrorFallback: React.FC<{ error: Error; onReset: () => void }> = ({
  error,
  onReset,
}) => (
  <div className="min-h-screen bg-[#0a0a0f] flex items-center justify-center p-4">
    <div className="max-w-lg w-full bg-slate-900/80 border border-red-500/30 rounded-xl p-6 text-center">
      <div className="text-4xl mb-4">⚠️</div>
      <h1 className="text-xl font-bold text-red-400 mb-2">
        アプリケーションエラー
      </h1>
      <p className="text-slate-400 mb-4 text-sm">
        予期せぬエラーが発生しました。問題が続く場合は管理者にお問い合わせください。
      </p>
      <div className="bg-slate-800/50 rounded-lg p-3 mb-4 text-left">
        <code className="text-xs text-red-300 break-all">{error.message}</code>
      </div>
      <button
        onClick={onReset}
        className="px-4 py-2 bg-indigo-600 hover:bg-indigo-700 text-white rounded-lg transition-colors"
      >
        再読み込み
      </button>
    </div>
  </div>
);

// ========================================
// エラー境界コンポーネント（簡易版）
// ========================================
// 注: @agentflow/ui の ErrorBoundary を使用する場合は import してください
// import { ErrorBoundary } from '@agentflow/ui';

interface ErrorBoundaryState {
  hasError: boolean;
  error: Error | null;
}

class ErrorBoundary extends React.Component<
  { children: React.ReactNode },
  ErrorBoundaryState
> {
  constructor(props: { children: React.ReactNode }) {
    super(props);
    this.state = { hasError: false, error: null };
  }

  static getDerivedStateFromError(error: Error): ErrorBoundaryState {
    return { hasError: true, error };
  }

  handleReset = (): void => {
    this.setState({ hasError: false, error: null });
    globalThis.location.reload();
  };

  render(): React.ReactNode {
    if (this.state.hasError && this.state.error) {
      return (
        <GlobalErrorFallback
          error={this.state.error}
          onReset={this.handleReset}
        />
      );
    }
    return this.props.children;
  }
}

/** ローディング画面 */
const LoadingScreen: React.FC = () => (
  <div className="min-h-screen bg-[#0a0a0f] flex items-center justify-center">
    <div className="text-center">
      <div className="w-16 h-16 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin mb-4 mx-auto" />
      <p className="text-slate-400">認証状態を確認中...</p>
    </div>
  </div>
);

/** メインコンテンツ */
const AppContent: React.FC = () => {
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
    case 'history':
      return <HistoryPage />;
    case 'knowledge-shu':
      return <KnowledgePage agentType="shu" />;
    case 'knowledge-qi':
      return <KnowledgePage agentType="qi" />;
    default:
      return <DecisionInputPage />;
  }
};

/** アプリケーションルート（ErrorBoundary でラップ） */
const App: React.FC = () => (
  <ErrorBoundary>
    <AppContent />
  </ErrorBoundary>
);

export default App;


/**
 * Decision Governance Engine - メインアプリ.
 *
 * 目的: 画面切り替えとルーティング
 */

import React from 'react';
import { useDecisionStore } from './store/useDecisionStore';
import { DecisionInputPage } from './components/DecisionInputPage';
import { ProcessingPage } from './components/ProcessingPage';
import { ReportPage } from './components/ReportPage';

const App: React.FC = () => {
  const currentPage = useDecisionStore((state) => state.currentPage);

  switch (currentPage) {
    case 'input':
      return <DecisionInputPage />;
    case 'processing':
      return <ProcessingPage />;
    case 'report':
      return <ReportPage />;
    default:
      return <DecisionInputPage />;
  }
};

export default App;


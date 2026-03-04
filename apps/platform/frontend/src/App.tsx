/**
 * AgentFlow Platform - ルーティング設定.
 *
 * 4 画面: Dashboard, Apps, App Detail, Settings
 * Layout コンポーネントでサイドバー + コンテンツ構成。
 */

import { Routes, Route } from 'react-router-dom';
import { Layout } from '@/components/Layout';
import { Dashboard } from '@/components/Dashboard';
import { AppList } from '@/components/AppList';
import { AppDetail } from '@/components/AppDetail';
import { AgentBrowser } from '@/components/AgentBrowser';
import { AgentOrchestration } from '@/components/AgentOrchestration';
import { AgentPatterns } from '@/components/AgentPatterns';
import { SkillCatalog } from '@/components/SkillCatalog';
import { RAGDashboard } from '@/components/rag/RAGDashboard';
import { MCPManager } from '@/components/MCPManager';
import { CLIReference } from '@/components/CLIReference';
import { Settings } from '@/components/Settings';

/** 404 フォールバック */
function NotFound() {
  return (
    <div className="flex items-center justify-center h-full py-24">
      <div className="text-center">
        <p className="text-6xl mb-4">🔍</p>
        <h2 className="text-xl font-bold text-slate-200 mb-2">
          Page Not Found
        </h2>
        <p className="text-sm text-slate-500 mb-4">
          お探しのページが見つかりません
        </p>
        <a
          href="/"
          className="text-sm text-indigo-400 hover:text-indigo-300 transition-colors"
        >
          ← Dashboard に戻る
        </a>
      </div>
    </div>
  );
}

export default function App() {
  return (
    <Routes>
      <Route element={<Layout />}>
        <Route index element={<Dashboard />} />
        <Route path="apps" element={<AppList />} />
        <Route path="apps/:name" element={<AppDetail />} />
        <Route path="agents" element={<AgentBrowser />} />
        <Route path="agent-orchestration" element={<AgentOrchestration />} />
        <Route path="agent-patterns" element={<AgentPatterns />} />
        <Route path="skills" element={<SkillCatalog />} />
        <Route path="rag" element={<RAGDashboard />} />
        <Route path="mcp" element={<MCPManager />} />
        <Route path="cli" element={<CLIReference />} />
        <Route path="settings" element={<Settings />} />
        <Route path="*" element={<NotFound />} />
      </Route>
    </Routes>
  );
}

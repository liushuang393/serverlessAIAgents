/**
 * AgentFlow Platform - ルーティング設定.
 *
 * 4 画面: Dashboard, Apps, App Detail, Settings
 * Layout コンポーネントでサイドバー + コンテンツ構成。
 */

import { Suspense, lazy, type ReactNode } from "react";
import { Routes, Route } from "react-router-dom";
import { Layout } from "@/components/Layout";
import { Dashboard } from "@/components/Dashboard";

const AppList = lazy(async () => ({
  default: (await import("@/components/AppList")).AppList,
}));
const Builder = lazy(async () => ({
  default: (await import("@/components/Builder")).Builder,
}));
const AppDetail = lazy(async () => ({
  default: (await import("@/components/AppDetail")).AppDetail,
}));
const AgentBrowser = lazy(async () => ({
  default: (await import("@/components/AgentBrowser")).AgentBrowser,
}));
const AgentOrchestration = lazy(async () => ({
  default: (await import("@/components/AgentOrchestration")).AgentOrchestration,
}));
const AgentPatterns = lazy(async () => ({
  default: (await import("@/components/AgentPatterns")).AgentPatterns,
}));
const SkillCatalog = lazy(async () => ({
  default: (await import("@/components/SkillCatalog")).SkillCatalog,
}));
const RAGDashboard = lazy(async () => ({
  default: (await import("@/components/rag/RAGDashboard")).RAGDashboard,
}));
const MCPManager = lazy(async () => ({
  default: (await import("@/components/MCPManager")).MCPManager,
}));
const CLIReference = lazy(async () => ({
  default: (await import("@/components/CLIReference")).CLIReference,
}));
const LLMManagement = lazy(async () => ({
  default: (await import("@/components/LLMManagement")).LLMManagement,
}));
const Settings = lazy(async () => ({
  default: (await import("@/components/Settings")).Settings,
}));

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

function RouteFallback() {
  return (
    <div className="flex justify-center items-center h-full py-24">
      <div className="w-10 h-10 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin" />
    </div>
  );
}

function withSuspense(node: ReactNode) {
  return <Suspense fallback={<RouteFallback />}>{node}</Suspense>;
}

export default function App() {
  return (
    <Routes>
      <Route element={<Layout />}>
        <Route index element={<Dashboard />} />
        <Route path="builder" element={withSuspense(<Builder />)} />
        <Route path="apps" element={withSuspense(<AppList />)} />
        <Route path="apps/:name" element={withSuspense(<AppDetail />)} />
        <Route path="agents" element={withSuspense(<AgentBrowser />)} />
        <Route
          path="agent-orchestration"
          element={withSuspense(<AgentOrchestration />)}
        />
        <Route
          path="agent-patterns"
          element={withSuspense(<AgentPatterns />)}
        />
        <Route path="skills" element={withSuspense(<SkillCatalog />)} />
        <Route path="rag" element={withSuspense(<RAGDashboard />)} />
        <Route path="mcp" element={withSuspense(<MCPManager />)} />
        <Route path="cli" element={withSuspense(<CLIReference />)} />
        <Route
          path="llm-management"
          element={withSuspense(<LLMManagement />)}
        />
        <Route path="settings" element={withSuspense(<Settings />)} />
        <Route path="*" element={<NotFound />} />
      </Route>
    </Routes>
  );
}

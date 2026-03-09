import { lazy, Suspense } from "react";
import { Route, Routes } from "react-router-dom";
import Layout from "./components/Layout";

const Dashboard = lazy(() => import("./components/Dashboard"));
const Platforms = lazy(() => import("./components/Platforms"));
const Sessions = lazy(() => import("./components/Sessions"));
const Conversations = lazy(() => import("./components/Conversations"));
const Approvals = lazy(() => import("./components/Approvals"));
const Timeline = lazy(() => import("./components/Timeline"));
const SkillsManager = lazy(() => import("./components/SkillsManager"));
const FileOrganizer = lazy(() => import("./components/FileOrganizer"));
const Settings = lazy(() => import("./components/Settings"));
const NotFound = lazy(() => import("./components/NotFound"));

function PageLoading() {
  return (
    <div className="glass-panel p-6 text-sm text-muted">
      画面を読み込み中...
    </div>
  );
}

/**
 * Messaging Hub 管理画面 メインアプリケーション
 */
function App() {
  return (
    <Suspense fallback={<PageLoading />}>
      <Routes>
        <Route path="/" element={<Layout />}>
          <Route index element={<Dashboard />} />
          <Route path="platforms" element={<Platforms />} />
          <Route path="sessions" element={<Sessions />} />
          <Route path="conversations" element={<Conversations />} />
          <Route path="approvals" element={<Approvals />} />
          <Route path="timeline" element={<Timeline />} />
          <Route path="skills" element={<SkillsManager />} />
          <Route path="file-organizer" element={<FileOrganizer />} />
          <Route path="settings" element={<Settings />} />
          <Route path="*" element={<NotFound />} />
        </Route>
      </Routes>
    </Suspense>
  );
}

export default App;

import { useState, useCallback } from 'react';
import { BrowserRouter, Routes, Route, Navigate, Outlet } from 'react-router-dom';
import { AuthGuard } from './components/auth/AuthGuard';
import { LoginForm } from './components/auth/LoginForm';
import { RegisterForm } from './components/auth/RegisterForm';
import { AuthCallback } from './components/auth/AuthCallback';
import { Sidebar } from './components/layout/Sidebar';
import { ChatWindow } from './components/chat/ChatWindow';
import { PanelLeft } from 'lucide-react';

/**
 * 認証済みページのレイアウトラッパー。
 * サイドバーの展開/折り畳み状態を管理する。
 */
const AppLayout = () => {
  const [sidebarOpen, setSidebarOpen] = useState(true);
  const toggleSidebar = useCallback(() => setSidebarOpen((prev) => !prev), []);

  return (
    <div className="flex h-screen w-full overflow-hidden bg-[var(--bg-main)]">
      <Sidebar isOpen={sidebarOpen} onToggle={toggleSidebar} />
      <div className="flex-1 min-w-0 flex flex-col h-full relative transition-all duration-300">
        {/* サイドバー折り畳み時の開くボタン */}
        {!sidebarOpen && (
          <button
            onClick={toggleSidebar}
            className="absolute top-4 left-[11px] z-30 p-2 rounded-xl bg-white/5 border border-white/10 text-[var(--text-muted)] hover:text-white hover:bg-white/10 transition-all"
            title="サイドバーを開く"
          >
            <PanelLeft size={24} />
          </button>
        )}
        <Outlet context={{ sidebarOpen }} />
      </div>
    </div>
  );
};

function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/login" element={<LoginForm />} />
        <Route path="/register" element={<RegisterForm />} />
        <Route path="/auth/callback" element={<AuthCallback />} />

        <Route path="/" element={
          <AuthGuard>
            <AppLayout />
          </AuthGuard>
        }>
          <Route index element={<ChatWindow />} />
        </Route>

        <Route path="*" element={<Navigate to="/" replace />} />
      </Routes>
    </BrowserRouter>
  );
}

export default App;

import { BrowserRouter, Routes, Route, Navigate, Outlet } from 'react-router-dom';
import { AuthGuard } from './components/auth/AuthGuard';
import { LoginForm } from './components/auth/LoginForm';
import { RegisterForm } from './components/auth/RegisterForm';
import { AuthCallback } from './components/auth/AuthCallback';
import { Sidebar } from './components/layout/Sidebar';
import { ChatWindow } from './components/chat/ChatWindow';

// Layout wrapper for authenticated pages
const AppLayout = () => {
  return (
    <div className="flex h-screen w-full overflow-hidden bg-[var(--bg-main)]">
      <Sidebar />
      <div className="flex-1 flex flex-col h-full relative">
        <Outlet />
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
          {/* Add more routes here if needed */}
        </Route>

        <Route path="*" element={<Navigate to="/" replace />} />
      </Routes>
    </BrowserRouter>
  );
}

export default App;

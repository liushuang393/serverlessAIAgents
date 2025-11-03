// frontend/src/App.js
// メインアプリケーションコンポーネント - ルーティングと認証管理
import React, { useState, useEffect } from 'react';
import { Routes, Route, Navigate } from 'react-router-dom';
import { Container, Box, Alert, Snackbar } from '@mui/material';

// コンポーネントのインポート
import Navbar from './components/Layout/Navbar';
import Footer from './components/Layout/Footer';
import HomePage from './pages/HomePage';
import LoginPage from './pages/LoginPage';
import RegisterPage from './pages/RegisterPage';
import DashboardPage from './pages/DashboardPage';
import SkillAssessmentPage from './pages/SkillAssessmentPage';
import LearningPathPage from './pages/LearningPathPage';
import ChapterPage from './pages/ChapterPage';
import ProfilePage from './pages/ProfilePage';
import LoadingSpinner from './components/Common/LoadingSpinner';

// サービスのインポート
import { authService } from './services/authService';

function App() {
  // 状態管理
  const [user, setUser] = useState(null);
  const [loading, setLoading] = useState(true);
  const [notification, setNotification] = useState({
    open: false,
    message: '',
    severity: 'info'
  });

  // 初期化時にユーザー認証状態をチェック
  useEffect(() => {
    const initializeAuth = async () => {
      try {
        const token = localStorage.getItem('access_token');
        if (token) {
          const userData = await authService.getCurrentUser();
          setUser(userData);
        }
      } catch (error) {
        console.error('認証初期化エラー:', error);
        localStorage.removeItem('access_token');
      } finally {
        setLoading(false);
      }
    };

    initializeAuth();
  }, []);

  // ログイン処理
  const handleLogin = async (credentials) => {
    try {
      const response = await authService.login(credentials);
      setUser(response.user);
      localStorage.setItem('access_token', response.access_token);
      showNotification('ログインしました', 'success');
      return { success: true };
    } catch (error) {
      showNotification(error.message || 'ログインに失敗しました', 'error');
      return { success: false, error: error.message };
    }
  };

  // ログアウト処理
  const handleLogout = async () => {
    try {
      await authService.logout();
      setUser(null);
      localStorage.removeItem('access_token');
      showNotification('ログアウトしました', 'info');
    } catch (error) {
      console.error('ログアウトエラー:', error);
    }
  };

  // ユーザー登録処理
  const handleRegister = async (userData) => {
    try {
      await authService.register(userData);
      showNotification('アカウントが作成されました。ログインしてください。', 'success');
      return { success: true };
    } catch (error) {
      showNotification(error.message || '登録に失敗しました', 'error');
      return { success: false, error: error.message };
    }
  };

  // 通知表示
  const showNotification = (message, severity = 'info') => {
    setNotification({
      open: true,
      message,
      severity
    });
  };

  // 通知を閉じる
  const handleCloseNotification = () => {
    setNotification(prev => ({ ...prev, open: false }));
  };

  // 認証が必要なルートのラッパー
  const ProtectedRoute = ({ children }) => {
    if (loading) {
      return <LoadingSpinner />;
    }
    
    if (!user) {
      return <Navigate to="/login" replace />;
    }
    
    return children;
  };

  // ローディング中の表示
  if (loading) {
    return <LoadingSpinner />;
  }

  return (
    <Box sx={{ display: 'flex', flexDirection: 'column', minHeight: '100vh' }}>
      {/* ナビゲーションバー */}
      <Navbar user={user} onLogout={handleLogout} />

      {/* メインコンテンツ */}
      <Container component="main" sx={{ flex: 1, py: 4 }}>
        <Routes>
          {/* パブリックルート */}
          <Route path="/" element={<HomePage />} />
          <Route 
            path="/login" 
            element={
              user ? <Navigate to="/dashboard" replace /> : 
              <LoginPage onLogin={handleLogin} />
            } 
          />
          <Route 
            path="/register" 
            element={
              user ? <Navigate to="/dashboard" replace /> : 
              <RegisterPage onRegister={handleRegister} />
            } 
          />

          {/* 認証が必要なルート */}
          <Route 
            path="/dashboard" 
            element={
              <ProtectedRoute>
                <DashboardPage user={user} />
              </ProtectedRoute>
            } 
          />
          <Route 
            path="/skill-assessment" 
            element={
              <ProtectedRoute>
                <SkillAssessmentPage />
              </ProtectedRoute>
            } 
          />
          <Route 
            path="/learning-path" 
            element={
              <ProtectedRoute>
                <LearningPathPage />
              </ProtectedRoute>
            } 
          />
          <Route 
            path="/chapter/:chapterId" 
            element={
              <ProtectedRoute>
                <ChapterPage />
              </ProtectedRoute>
            } 
          />
          <Route 
            path="/profile" 
            element={
              <ProtectedRoute>
                <ProfilePage user={user} setUser={setUser} />
              </ProtectedRoute>
            } 
          />

          {/* 404ページ */}
          <Route path="*" element={<Navigate to="/" replace />} />
        </Routes>
      </Container>

      {/* フッター */}
      <Footer />

      {/* 通知スナックバー */}
      <Snackbar
        open={notification.open}
        autoHideDuration={6000}
        onClose={handleCloseNotification}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
      >
        <Alert 
          onClose={handleCloseNotification} 
          severity={notification.severity}
          sx={{ width: '100%' }}
        >
          {notification.message}
        </Alert>
      </Snackbar>
    </Box>
  );
}

export default App;

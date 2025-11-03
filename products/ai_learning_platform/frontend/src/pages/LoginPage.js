// frontend/src/pages/LoginPage.js
// ログインページコンポーネント - ユーザー認証
import React, { useState } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import {
  Box,
  Paper,
  TextField,
  Button,
  Typography,
  Alert,
  Container,
  InputAdornment,
  IconButton,
} from '@mui/material';
import {
  Visibility,
  VisibilityOff,
  Person,
  Lock,
} from '@mui/icons-material';
import LoadingSpinner from '../components/Common/LoadingSpinner';

const LoginPage = ({ onLogin }) => {
  const navigate = useNavigate();

  // 状態管理
  const [formData, setFormData] = useState({
    username: '',
    password: '',
  });
  const [showPassword, setShowPassword] = useState(false);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState('');

  // フォーム入力の処理
  const handleInputChange = (event) => {
    const { name, value } = event.target;
    setFormData(prev => ({
      ...prev,
      [name]: value
    }));
    // エラーをクリア
    if (error) setError('');
  };

  // パスワード表示切り替え
  const handleTogglePasswordVisibility = () => {
    setShowPassword(!showPassword);
  };

  // フォーム送信処理
  const handleSubmit = async (event) => {
    event.preventDefault();
    
    // バリデーション
    if (!formData.username.trim()) {
      setError('ユーザー名を入力してください');
      return;
    }
    
    if (!formData.password) {
      setError('パスワードを入力してください');
      return;
    }

    setLoading(true);
    setError('');

    try {
      const result = await onLogin(formData);
      if (result.success) {
        navigate('/dashboard');
      } else {
        setError(result.error || 'ログインに失敗しました');
      }
    } catch (error) {
      setError('ログイン処理中にエラーが発生しました');
    } finally {
      setLoading(false);
    }
  };

  // ローディング中の表示
  if (loading) {
    return <LoadingSpinner message="ログイン中..." />;
  }

  return (
    <Container component="main" maxWidth="sm">
      <Box
        sx={{
          marginTop: 8,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
        }}
      >
        <Paper
          elevation={3}
          sx={{
            padding: 4,
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            width: '100%',
          }}
        >
          {/* ヘッダー */}
          <Typography component="h1" variant="h4" sx={{ mb: 3 }}>
            ログイン
          </Typography>

          {/* エラーメッセージ */}
          {error && (
            <Alert severity="error" sx={{ width: '100%', mb: 2 }}>
              {error}
            </Alert>
          )}

          {/* ログインフォーム */}
          <Box component="form" onSubmit={handleSubmit} sx={{ width: '100%' }}>
            <TextField
              margin="normal"
              required
              fullWidth
              id="username"
              label="ユーザー名"
              name="username"
              autoComplete="username"
              autoFocus
              value={formData.username}
              onChange={handleInputChange}
              InputProps={{
                startAdornment: (
                  <InputAdornment position="start">
                    <Person />
                  </InputAdornment>
                ),
              }}
              sx={{ mb: 2 }}
            />
            
            <TextField
              margin="normal"
              required
              fullWidth
              name="password"
              label="パスワード"
              type={showPassword ? 'text' : 'password'}
              id="password"
              autoComplete="current-password"
              value={formData.password}
              onChange={handleInputChange}
              InputProps={{
                startAdornment: (
                  <InputAdornment position="start">
                    <Lock />
                  </InputAdornment>
                ),
                endAdornment: (
                  <InputAdornment position="end">
                    <IconButton
                      aria-label="toggle password visibility"
                      onClick={handleTogglePasswordVisibility}
                      edge="end"
                    >
                      {showPassword ? <VisibilityOff /> : <Visibility />}
                    </IconButton>
                  </InputAdornment>
                ),
              }}
              sx={{ mb: 3 }}
            />

            <Button
              type="submit"
              fullWidth
              variant="contained"
              sx={{ mt: 3, mb: 2, py: 1.5 }}
              disabled={loading}
            >
              ログイン
            </Button>

            {/* リンク */}
            <Box sx={{ textAlign: 'center', mt: 2 }}>
              <Typography variant="body2">
                アカウントをお持ちでない方は{' '}
                <Link 
                  to="/register" 
                  style={{ 
                    color: '#1976d2', 
                    textDecoration: 'none',
                    fontWeight: 500
                  }}
                >
                  こちらから新規登録
                </Link>
              </Typography>
            </Box>

            {/* パスワードリセット（将来実装予定） */}
            <Box sx={{ textAlign: 'center', mt: 1 }}>
              <Typography variant="body2" color="text.secondary">
                パスワードをお忘れの方は{' '}
                <Link 
                  to="/forgot-password" 
                  style={{ 
                    color: '#666', 
                    textDecoration: 'none' 
                  }}
                >
                  こちら
                </Link>
                {' '}（準備中）
              </Typography>
            </Box>
          </Box>

          {/* デモアカウント情報（開発用） */}
          <Box sx={{ mt: 4, p: 2, backgroundColor: '#f5f5f5', borderRadius: 1, width: '100%' }}>
            <Typography variant="body2" color="text.secondary" sx={{ mb: 1 }}>
              <strong>デモアカウント（テスト用）:</strong>
            </Typography>
            <Typography variant="body2" color="text.secondary">
              ユーザー名: demo_user
            </Typography>
            <Typography variant="body2" color="text.secondary">
              パスワード: demo_password
            </Typography>
          </Box>
        </Paper>
      </Box>
    </Container>
  );
};

export default LoginPage;

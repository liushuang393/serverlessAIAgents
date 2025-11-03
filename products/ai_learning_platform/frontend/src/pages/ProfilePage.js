// frontend/src/pages/ProfilePage.js
// プロフィールページコンポーネント - ユーザー情報管理
import React, { useState } from 'react';
import {
  Box,
  Typography,
  Card,
  CardContent,
  TextField,
  Button,
  Avatar,
  Grid,
  Paper,
  Divider,
  Alert,
  Chip,
} from '@mui/material';
import {
  Person,
  Email,
  Edit,
  Save,
  Cancel,
  School,
  TrendingUp,
  Assessment,
} from '@mui/icons-material';

const ProfilePage = ({ user, setUser }) => {
  // 状態管理
  const [editing, setEditing] = useState(false);
  const [formData, setFormData] = useState({
    username: user?.username || '',
    email: user?.email || '',
    full_name: user?.full_name || '',
    bio: user?.bio || '',
  });
  const [loading, setLoading] = useState(false);
  const [message, setMessage] = useState({ type: '', text: '' });

  // フォーム入力の処理
  const handleInputChange = (event) => {
    const { name, value } = event.target;
    setFormData(prev => ({
      ...prev,
      [name]: value
    }));
  };

  // 編集モードの切り替え
  const handleEditToggle = () => {
    if (editing) {
      // キャンセル時は元の値に戻す
      setFormData({
        username: user?.username || '',
        email: user?.email || '',
        full_name: user?.full_name || '',
        bio: user?.bio || '',
      });
    }
    setEditing(!editing);
    setMessage({ type: '', text: '' });
  };

  // プロフィール更新
  const handleSave = async () => {
    setLoading(true);
    setMessage({ type: '', text: '' });

    try {
      // TODO: 実際のAPIコールに置き換え
      // const response = await apiClient.put('/auth/profile', formData);
      
      // モック処理
      setTimeout(() => {
        setUser(prev => ({
          ...prev,
          ...formData
        }));
        setEditing(false);
        setMessage({ type: 'success', text: 'プロフィールを更新しました' });
        setLoading(false);
      }, 1000);
      
    } catch (error) {
      console.error('プロフィール更新エラー:', error);
      setMessage({ type: 'error', text: 'プロフィールの更新に失敗しました' });
      setLoading(false);
    }
  };

  // モック統計データ
  const stats = {
    completedChapters: 3,
    totalStudyTime: 24,
    currentStreak: 7,
    skillScore: 78,
    strengths: ['プロンプトエンジニアリング', 'テキスト分類', '言語モデル'],
    achievements: ['初回ログイン', '最初のチャプター完了', '1週間連続学習']
  };

  return (
    <Box>
      {/* ヘッダー */}
      <Paper sx={{ p: 3, mb: 4, textAlign: 'center' }}>
        <Avatar
          sx={{
            width: 100,
            height: 100,
            mx: 'auto',
            mb: 2,
            bgcolor: 'primary.main',
            fontSize: '2rem'
          }}
        >
          {user?.full_name ? user.full_name.charAt(0) : user?.username?.charAt(0)}
        </Avatar>
        <Typography variant="h4" component="h1" gutterBottom>
          {user?.full_name || user?.username}
        </Typography>
        <Typography variant="body1" color="text.secondary">
          {user?.email}
        </Typography>
      </Paper>

      <Grid container spacing={3}>
        {/* プロフィール情報 */}
        <Grid item xs={12} md={6}>
          <Card>
            <CardContent>
              <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', mb: 3 }}>
                <Typography variant="h6">
                  <Person sx={{ mr: 1, verticalAlign: 'middle' }} />
                  プロフィール情報
                </Typography>
                <Button
                  variant={editing ? "outlined" : "contained"}
                  startIcon={editing ? <Cancel /> : <Edit />}
                  onClick={handleEditToggle}
                  disabled={loading}
                >
                  {editing ? 'キャンセル' : '編集'}
                </Button>
              </Box>

              {message.text && (
                <Alert severity={message.type} sx={{ mb: 2 }}>
                  {message.text}
                </Alert>
              )}

              <Box component="form" sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
                <TextField
                  label="ユーザー名"
                  name="username"
                  value={formData.username}
                  onChange={handleInputChange}
                  disabled={!editing}
                  fullWidth
                  InputProps={{
                    startAdornment: <Person sx={{ mr: 1, color: 'action.active' }} />,
                  }}
                />

                <TextField
                  label="メールアドレス"
                  name="email"
                  type="email"
                  value={formData.email}
                  onChange={handleInputChange}
                  disabled={!editing}
                  fullWidth
                  InputProps={{
                    startAdornment: <Email sx={{ mr: 1, color: 'action.active' }} />,
                  }}
                />

                <TextField
                  label="フルネーム"
                  name="full_name"
                  value={formData.full_name}
                  onChange={handleInputChange}
                  disabled={!editing}
                  fullWidth
                />

                <TextField
                  label="自己紹介"
                  name="bio"
                  value={formData.bio}
                  onChange={handleInputChange}
                  disabled={!editing}
                  fullWidth
                  multiline
                  rows={3}
                  placeholder="あなたについて教えてください..."
                />

                {editing && (
                  <Button
                    variant="contained"
                    startIcon={<Save />}
                    onClick={handleSave}
                    disabled={loading}
                    sx={{ mt: 2 }}
                  >
                    {loading ? '保存中...' : '保存'}
                  </Button>
                )}
              </Box>
            </CardContent>
          </Card>
        </Grid>

        {/* 学習統計 */}
        <Grid item xs={12} md={6}>
          <Card sx={{ mb: 3 }}>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                <TrendingUp sx={{ mr: 1, verticalAlign: 'middle' }} />
                学習統計
              </Typography>
              
              <Grid container spacing={2}>
                <Grid item xs={6}>
                  <Box sx={{ textAlign: 'center' }}>
                    <Typography variant="h4" color="primary.main">
                      {stats.completedChapters}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      完了チャプター
                    </Typography>
                  </Box>
                </Grid>
                <Grid item xs={6}>
                  <Box sx={{ textAlign: 'center' }}>
                    <Typography variant="h4" color="success.main">
                      {stats.totalStudyTime}h
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      総学習時間
                    </Typography>
                  </Box>
                </Grid>
                <Grid item xs={6}>
                  <Box sx={{ textAlign: 'center' }}>
                    <Typography variant="h4" color="warning.main">
                      {stats.currentStreak}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      連続学習日数
                    </Typography>
                  </Box>
                </Grid>
                <Grid item xs={6}>
                  <Box sx={{ textAlign: 'center' }}>
                    <Typography variant="h4" color="info.main">
                      {stats.skillScore}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      技能スコア
                    </Typography>
                  </Box>
                </Grid>
              </Grid>
            </CardContent>
          </Card>

          {/* 技能と実績 */}
          <Card>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                <Assessment sx={{ mr: 1, verticalAlign: 'middle' }} />
                技能と実績
              </Typography>
              
              <Box sx={{ mb: 3 }}>
                <Typography variant="body2" color="text.secondary" gutterBottom>
                  強み
                </Typography>
                <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 1 }}>
                  {stats.strengths.map((strength, index) => (
                    <Chip 
                      key={index} 
                      label={strength} 
                      color="success" 
                      size="small" 
                    />
                  ))}
                </Box>
              </Box>

              <Divider sx={{ my: 2 }} />

              <Box>
                <Typography variant="body2" color="text.secondary" gutterBottom>
                  獲得した実績
                </Typography>
                <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 1 }}>
                  {stats.achievements.map((achievement, index) => (
                    <Chip 
                      key={index} 
                      label={achievement} 
                      color="primary" 
                      size="small"
                      variant="outlined"
                    />
                  ))}
                </Box>
              </Box>
            </CardContent>
          </Card>
        </Grid>

        {/* アカウント設定 */}
        <Grid item xs={12}>
          <Card>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                アカウント設定
              </Typography>
              
              <Box sx={{ display: 'flex', gap: 2, flexWrap: 'wrap' }}>
                <Button variant="outlined" color="warning">
                  パスワード変更
                </Button>
                <Button variant="outlined" color="info">
                  通知設定
                </Button>
                <Button variant="outlined" color="error">
                  アカウント削除
                </Button>
              </Box>
              
              <Typography variant="body2" color="text.secondary" sx={{ mt: 2 }}>
                アカウント作成日: {new Date(user?.created_at || Date.now()).toLocaleDateString('ja-JP')}
              </Typography>
            </CardContent>
          </Card>
        </Grid>
      </Grid>
    </Box>
  );
};

export default ProfilePage;

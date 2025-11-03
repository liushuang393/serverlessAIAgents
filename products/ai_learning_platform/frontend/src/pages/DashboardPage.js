// frontend/src/pages/DashboardPage.js
// ダッシュボードページコンポーネント - ユーザーの学習状況概要
import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  Box,
  Typography,
  Grid,
  Card,
  CardContent,
  CardActions,
  Button,
  LinearProgress,
  Paper,
  List,
  ListItem,
  ListItemText,
  ListItemIcon,
  Chip,
  Avatar,
} from '@mui/material';
import {
  Assessment,
  School,
  TrendingUp,
  PlayArrow,
  CheckCircle,
  Schedule,
  Star,
  BookmarkBorder,
} from '@mui/icons-material';
import LoadingSpinner from '../components/Common/LoadingSpinner';

const DashboardPage = ({ user }) => {
  const navigate = useNavigate();

  // 状態管理
  const [loading, setLoading] = useState(true);
  const [dashboardData, setDashboardData] = useState({
    skillProfile: null,
    recentProgress: [],
    recommendedChapters: [],
    stats: {
      completedChapters: 0,
      totalChapters: 12,
      currentStreak: 0,
      totalStudyTime: 0,
    }
  });

  // ダッシュボードデータを取得
  useEffect(() => {
    const fetchDashboardData = async () => {
      try {
        // TODO: 実際のAPIコールに置き換え
        // const response = await apiClient.get('/dashboard');
        
        // モックデータ
        setTimeout(() => {
          setDashboardData({
            skillProfile: {
              overall_score: 75,
              strengths: ['プロンプトエンジニアリング', 'テキスト分類'],
              weaknesses: ['ファインチューニング', 'マルチモーダルモデル'],
              recommendations: [
                {
                  chapter_id: 11,
                  title: 'Fine-tuning Representation Models',
                  reason: '分類モデルの微調整技術を強化する必要があります'
                },
                {
                  chapter_id: 9,
                  title: 'Multimodal Large Language Models',
                  reason: 'マルチモーダルモデルの理解を深めることを推奨します'
                }
              ]
            },
            recentProgress: [
              {
                id: 1,
                title: 'Introduction to Language Models',
                progress: 100,
                completed_at: '2024-01-15'
              },
              {
                id: 2,
                title: 'Tokens and Embeddings',
                progress: 60,
                last_accessed: '2024-01-20'
              },
              {
                id: 3,
                title: 'Looking Inside Transformer LLMs',
                progress: 30,
                last_accessed: '2024-01-18'
              }
            ],
            recommendedChapters: [
              {
                id: 4,
                title: 'Text Classification',
                description: 'テキスト分類タスクとその応用を習得する',
                estimated_time: '2週間'
              },
              {
                id: 6,
                title: 'Prompt Engineering',
                description: '効果的なプロンプト設計技術を学習する',
                estimated_time: '1週間'
              }
            ],
            stats: {
              completedChapters: 1,
              totalChapters: 12,
              currentStreak: 5,
              totalStudyTime: 24
            }
          });
          setLoading(false);
        }, 1000);
        
      } catch (error) {
        console.error('ダッシュボードデータ取得エラー:', error);
        setLoading(false);
      }
    };

    fetchDashboardData();
  }, []);

  // ローディング中の表示
  if (loading) {
    return <LoadingSpinner message="ダッシュボードを読み込み中..." />;
  }

  const { skillProfile, recentProgress, recommendedChapters, stats } = dashboardData;

  return (
    <Box>
      {/* ウェルカムセクション */}
      <Paper sx={{ p: 3, mb: 4, background: 'linear-gradient(135deg, #1976d2 0%, #42a5f5 100%)', color: 'white' }}>
        <Box sx={{ display: 'flex', alignItems: 'center', mb: 2 }}>
          <Avatar sx={{ mr: 2, bgcolor: 'rgba(255, 255, 255, 0.2)' }}>
            {user?.full_name ? user.full_name.charAt(0) : user?.username?.charAt(0)}
          </Avatar>
          <Box>
            <Typography variant="h4" component="h1">
              おかえりなさい、{user?.full_name || user?.username}さん
            </Typography>
            <Typography variant="h6" sx={{ opacity: 0.9 }}>
              今日も学習を続けましょう！
            </Typography>
          </Box>
        </Box>
      </Paper>

      <Grid container spacing={3}>
        {/* 統計カード */}
        <Grid item xs={12} md={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <School color="primary" sx={{ fontSize: 40, mb: 1 }} />
              <Typography variant="h4" component="div">
                {stats.completedChapters}/{stats.totalChapters}
              </Typography>
              <Typography color="text.secondary">
                完了チャプター
              </Typography>
            </CardContent>
          </Card>
        </Grid>

        <Grid item xs={12} md={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <TrendingUp color="primary" sx={{ fontSize: 40, mb: 1 }} />
              <Typography variant="h4" component="div">
                {stats.currentStreak}
              </Typography>
              <Typography color="text.secondary">
                連続学習日数
              </Typography>
            </CardContent>
          </Card>
        </Grid>

        <Grid item xs={12} md={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <Schedule color="primary" sx={{ fontSize: 40, mb: 1 }} />
              <Typography variant="h4" component="div">
                {stats.totalStudyTime}h
              </Typography>
              <Typography color="text.secondary">
                総学習時間
              </Typography>
            </CardContent>
          </Card>
        </Grid>

        <Grid item xs={12} md={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <Star color="primary" sx={{ fontSize: 40, mb: 1 }} />
              <Typography variant="h4" component="div">
                {skillProfile?.overall_score || 0}
              </Typography>
              <Typography color="text.secondary">
                技能スコア
              </Typography>
            </CardContent>
          </Card>
        </Grid>

        {/* 技能プロフィール */}
        {skillProfile && (
          <Grid item xs={12} md={6}>
            <Card sx={{ height: '100%' }}>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  <Assessment sx={{ mr: 1, verticalAlign: 'middle' }} />
                  技能プロフィール
                </Typography>
                
                <Box sx={{ mb: 2 }}>
                  <Typography variant="body2" color="text.secondary" gutterBottom>
                    総合スコア
                  </Typography>
                  <LinearProgress 
                    variant="determinate" 
                    value={skillProfile.overall_score} 
                    sx={{ height: 8, borderRadius: 4 }}
                  />
                  <Typography variant="body2" sx={{ mt: 0.5 }}>
                    {skillProfile.overall_score}/100
                  </Typography>
                </Box>

                <Box sx={{ mb: 2 }}>
                  <Typography variant="body2" color="text.secondary" gutterBottom>
                    強み
                  </Typography>
                  <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 1 }}>
                    {skillProfile.strengths.map((strength, index) => (
                      <Chip 
                        key={index} 
                        label={strength} 
                        color="success" 
                        size="small" 
                      />
                    ))}
                  </Box>
                </Box>

                <Box>
                  <Typography variant="body2" color="text.secondary" gutterBottom>
                    改善点
                  </Typography>
                  <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 1 }}>
                    {skillProfile.weaknesses.map((weakness, index) => (
                      <Chip 
                        key={index} 
                        label={weakness} 
                        color="warning" 
                        size="small" 
                      />
                    ))}
                  </Box>
                </Box>
              </CardContent>
              <CardActions>
                <Button 
                  size="small" 
                  onClick={() => navigate('/skill-assessment')}
                >
                  再診断する
                </Button>
              </CardActions>
            </Card>
          </Grid>
        )}

        {/* 学習進捗 */}
        <Grid item xs={12} md={6}>
          <Card sx={{ height: '100%' }}>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                <BookmarkBorder sx={{ mr: 1, verticalAlign: 'middle' }} />
                最近の学習進捗
              </Typography>
              
              <List>
                {recentProgress.map((item) => (
                  <ListItem key={item.id} sx={{ px: 0 }}>
                    <ListItemIcon>
                      {item.progress === 100 ? (
                        <CheckCircle color="success" />
                      ) : (
                        <PlayArrow color="primary" />
                      )}
                    </ListItemIcon>
                    <ListItemText
                      primary={item.title}
                      secondary={
                        <Box>
                          <LinearProgress 
                            variant="determinate" 
                            value={item.progress} 
                            sx={{ mt: 1, mb: 0.5 }}
                          />
                          <Typography variant="caption">
                            {item.progress}% 完了
                          </Typography>
                        </Box>
                      }
                    />
                  </ListItem>
                ))}
              </List>
            </CardContent>
            <CardActions>
              <Button 
                size="small" 
                onClick={() => navigate('/learning-path')}
              >
                全ての進捗を見る
              </Button>
            </CardActions>
          </Card>
        </Grid>

        {/* 推奨学習コンテンツ */}
        <Grid item xs={12}>
          <Card>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                <Star sx={{ mr: 1, verticalAlign: 'middle' }} />
                あなたにおすすめの学習コンテンツ
              </Typography>
              
              <Grid container spacing={2}>
                {recommendedChapters.map((chapter) => (
                  <Grid item xs={12} md={6} key={chapter.id}>
                    <Card variant="outlined">
                      <CardContent>
                        <Typography variant="h6" component="h3">
                          {chapter.title}
                        </Typography>
                        <Typography variant="body2" color="text.secondary" sx={{ mb: 1 }}>
                          {chapter.description}
                        </Typography>
                        <Chip 
                          label={`推定学習時間: ${chapter.estimated_time}`} 
                          size="small" 
                          color="info"
                        />
                      </CardContent>
                      <CardActions>
                        <Button 
                          size="small" 
                          variant="contained"
                          onClick={() => navigate(`/chapter/${chapter.id}`)}
                        >
                          学習開始
                        </Button>
                      </CardActions>
                    </Card>
                  </Grid>
                ))}
              </Grid>
            </CardContent>
          </Card>
        </Grid>
      </Grid>
    </Box>
  );
};

export default DashboardPage;

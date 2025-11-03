// frontend/src/pages/LearningPathPage.js
// 学習パスページコンポーネント - 学習コンテンツ一覧と進捗
import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  Box,
  Typography,
  Card,
  CardContent,
  CardActions,
  Button,
  LinearProgress,
  Grid,
  Chip,
  Paper,
  List,
  ListItem,
  ListItemIcon,
  ListItemText,
  Avatar,
} from '@mui/material';
import {
  School,
  PlayArrow,
  CheckCircle,
  Lock,
  Schedule,
  MenuBook,
  VideoLibrary,
  Quiz,
  Psychology,
} from '@mui/icons-material';
import LoadingSpinner from '../components/Common/LoadingSpinner';

const LearningPathPage = () => {
  const navigate = useNavigate();

  // 状態管理
  const [loading, setLoading] = useState(true);
  const [chapters, setChapters] = useState([]);
  const [userProgress, setUserProgress] = useState({});

  // データを取得
  useEffect(() => {
    const fetchLearningPath = async () => {
      try {
        // TODO: 実際のAPIコールに置き換え
        // const [chaptersResponse, progressResponse] = await Promise.all([
        //   apiClient.get('/learning/chapters'),
        //   apiClient.get('/learning/progress')
        // ]);
        
        // モックデータ
        setTimeout(() => {
          const mockChapters = [
            {
              id: 1,
              title: 'Introduction to Language Models',
              description: '言語モデルの基本概念と応用について理解する',
              order_index: 1,
              estimated_time: '1週間',
              difficulty: 'beginner',
              contents: [
                { type: 'markdown', title: '言語モデルとは' },
                { type: 'video', title: '実例で学ぶ言語モデル' },
                { type: 'quiz', title: '理解度チェック' }
              ]
            },
            {
              id: 2,
              title: 'Tokens and Embeddings',
              description: 'トークン化とベクトル表現の基礎知識を学習する',
              order_index: 2,
              estimated_time: '1週間',
              difficulty: 'beginner',
              contents: [
                { type: 'markdown', title: 'トークン化の基礎' },
                { type: 'markdown', title: 'ベクトル表現' },
                { type: 'simulation', title: '実践演習' }
              ]
            },
            {
              id: 3,
              title: 'Looking Inside Transformer LLMs',
              description: 'Transformerアーキテクチャと大規模言語モデルの動作原理を深く理解する',
              order_index: 3,
              estimated_time: '2週間',
              difficulty: 'intermediate',
              contents: [
                { type: 'markdown', title: 'Transformerアーキテクチャ' },
                { type: 'video', title: 'Attention機構の解説' },
                { type: 'quiz', title: '理解度テスト' }
              ]
            },
            {
              id: 4,
              title: 'Text Classification',
              description: 'テキスト分類タスクとその応用を習得する',
              order_index: 4,
              estimated_time: '2週間',
              difficulty: 'intermediate',
              contents: [
                { type: 'markdown', title: '分類手法の概要' },
                { type: 'simulation', title: '実装演習' },
                { type: 'quiz', title: '応用問題' }
              ]
            },
            {
              id: 5,
              title: 'Text Clustering and Topic Modeling',
              description: 'テキストクラスタリングとトピックモデリング技術を学習する',
              order_index: 5,
              estimated_time: '2週間',
              difficulty: 'intermediate',
              contents: [
                { type: 'markdown', title: 'クラスタリング手法' },
                { type: 'markdown', title: 'トピックモデリング' },
                { type: 'simulation', title: '実践プロジェクト' }
              ]
            },
            {
              id: 6,
              title: 'Prompt Engineering',
              description: '効果的なプロンプト設計技術を学習する',
              order_index: 6,
              estimated_time: '1週間',
              difficulty: 'beginner',
              contents: [
                { type: 'markdown', title: 'プロンプト設計の基礎' },
                { type: 'video', title: '実例とベストプラクティス' },
                { type: 'simulation', title: 'プロンプト作成演習' }
              ]
            }
          ];

          const mockProgress = {
            1: { status: 'completed', progress: 100 },
            2: { status: 'in_progress', progress: 60 },
            3: { status: 'in_progress', progress: 30 },
            4: { status: 'not_started', progress: 0 },
            5: { status: 'not_started', progress: 0 },
            6: { status: 'not_started', progress: 0 }
          };

          setChapters(mockChapters);
          setUserProgress(mockProgress);
          setLoading(false);
        }, 1000);
        
      } catch (error) {
        console.error('学習パスデータ取得エラー:', error);
        setLoading(false);
      }
    };

    fetchLearningPath();
  }, []);

  // 難易度に応じた色を取得
  const getDifficultyColor = (difficulty) => {
    switch (difficulty) {
      case 'beginner': return 'success';
      case 'intermediate': return 'warning';
      case 'advanced': return 'error';
      default: return 'default';
    }
  };

  // 進捗状況に応じたアイコンを取得
  const getProgressIcon = (chapterId) => {
    const progress = userProgress[chapterId];
    if (!progress) return <Lock color="disabled" />;
    
    switch (progress.status) {
      case 'completed': return <CheckCircle color="success" />;
      case 'in_progress': return <PlayArrow color="primary" />;
      case 'not_started': return <School color="action" />;
      default: return <Lock color="disabled" />;
    }
  };

  // コンテンツタイプのアイコンを取得
  const getContentIcon = (type) => {
    switch (type) {
      case 'markdown': return <MenuBook />;
      case 'video': return <VideoLibrary />;
      case 'quiz': return <Quiz />;
      case 'simulation': return <Psychology />;
      default: return <MenuBook />;
    }
  };

  // チャプターが利用可能かチェック
  const isChapterAvailable = (chapterId) => {
    // 最初のチャプターは常に利用可能
    if (chapterId === 1) return true;
    
    // 前のチャプターが完了している場合のみ利用可能
    const previousChapter = chapterId - 1;
    const previousProgress = userProgress[previousChapter];
    return previousProgress && previousProgress.status === 'completed';
  };

  // ローディング中の表示
  if (loading) {
    return <LoadingSpinner message="学習パスを読み込み中..." />;
  }

  return (
    <Box>
      {/* ヘッダー */}
      <Paper sx={{ p: 3, mb: 4, background: 'linear-gradient(135deg, #1976d2 0%, #42a5f5 100%)', color: 'white' }}>
        <Typography variant="h4" component="h1" gutterBottom>
          <School sx={{ mr: 2, verticalAlign: 'middle' }} />
          学習パス
        </Typography>
        <Typography variant="h6">
          体系的なカリキュラムでAI・機械学習を効率的に学習しましょう
        </Typography>
      </Paper>

      {/* 進捗概要 */}
      <Card sx={{ mb: 4 }}>
        <CardContent>
          <Typography variant="h6" gutterBottom>
            学習進捗概要
          </Typography>
          <Grid container spacing={2}>
            <Grid item xs={12} sm={4}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h4" color="success.main">
                  {Object.values(userProgress).filter(p => p.status === 'completed').length}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  完了チャプター
                </Typography>
              </Box>
            </Grid>
            <Grid item xs={12} sm={4}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h4" color="primary.main">
                  {Object.values(userProgress).filter(p => p.status === 'in_progress').length}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  学習中チャプター
                </Typography>
              </Box>
            </Grid>
            <Grid item xs={12} sm={4}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h4" color="text.secondary">
                  {chapters.length}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  総チャプター数
                </Typography>
              </Box>
            </Grid>
          </Grid>
        </CardContent>
      </Card>

      {/* チャプター一覧 */}
      <Grid container spacing={3}>
        {chapters.map((chapter) => {
          const progress = userProgress[chapter.id] || { status: 'not_started', progress: 0 };
          const isAvailable = isChapterAvailable(chapter.id);
          
          return (
            <Grid item xs={12} md={6} key={chapter.id}>
              <Card 
                sx={{ 
                  height: '100%',
                  display: 'flex',
                  flexDirection: 'column',
                  opacity: isAvailable ? 1 : 0.6,
                  transition: 'transform 0.2s',
                  '&:hover': {
                    transform: isAvailable ? 'translateY(-4px)' : 'none',
                    boxShadow: isAvailable ? 4 : 1,
                  },
                }}
              >
                <CardContent sx={{ flexGrow: 1 }}>
                  <Box sx={{ display: 'flex', alignItems: 'center', mb: 2 }}>
                    <Avatar sx={{ mr: 2, bgcolor: 'primary.main' }}>
                      {chapter.order_index}
                    </Avatar>
                    <Box sx={{ flexGrow: 1 }}>
                      <Typography variant="h6" component="h3">
                        {chapter.title}
                      </Typography>
                      <Box sx={{ display: 'flex', gap: 1, mt: 1 }}>
                        <Chip 
                          label={chapter.difficulty} 
                          color={getDifficultyColor(chapter.difficulty)}
                          size="small"
                        />
                        <Chip 
                          label={chapter.estimated_time} 
                          icon={<Schedule />}
                          size="small"
                          variant="outlined"
                        />
                      </Box>
                    </Box>
                    {getProgressIcon(chapter.id)}
                  </Box>

                  <Typography variant="body2" color="text.secondary" sx={{ mb: 2 }}>
                    {chapter.description}
                  </Typography>

                  {/* 進捗バー */}
                  <Box sx={{ mb: 2 }}>
                    <Box sx={{ display: 'flex', justifyContent: 'space-between', mb: 1 }}>
                      <Typography variant="body2">進捗</Typography>
                      <Typography variant="body2">{progress.progress}%</Typography>
                    </Box>
                    <LinearProgress 
                      variant="determinate" 
                      value={progress.progress} 
                      sx={{ height: 6, borderRadius: 3 }}
                    />
                  </Box>

                  {/* コンテンツ一覧 */}
                  <Typography variant="body2" color="text.secondary" gutterBottom>
                    学習内容:
                  </Typography>
                  <List dense>
                    {chapter.contents.slice(0, 3).map((content, index) => (
                      <ListItem key={index} sx={{ py: 0.5, px: 0 }}>
                        <ListItemIcon sx={{ minWidth: 32 }}>
                          {getContentIcon(content.type)}
                        </ListItemIcon>
                        <ListItemText 
                          primary={content.title}
                          primaryTypographyProps={{ variant: 'body2' }}
                        />
                      </ListItem>
                    ))}
                  </List>
                </CardContent>

                <CardActions>
                  <Button
                    variant={progress.status === 'not_started' ? 'contained' : 'outlined'}
                    fullWidth
                    disabled={!isAvailable}
                    onClick={() => navigate(`/chapter/${chapter.id}`)}
                    startIcon={
                      !isAvailable ? <Lock /> :
                      progress.status === 'completed' ? <CheckCircle /> :
                      progress.status === 'in_progress' ? <PlayArrow /> :
                      <School />
                    }
                  >
                    {!isAvailable ? 'ロック中' :
                     progress.status === 'completed' ? '復習する' :
                     progress.status === 'in_progress' ? '続きから学習' :
                     '学習開始'}
                  </Button>
                </CardActions>
              </Card>
            </Grid>
          );
        })}
      </Grid>
    </Box>
  );
};

export default LearningPathPage;

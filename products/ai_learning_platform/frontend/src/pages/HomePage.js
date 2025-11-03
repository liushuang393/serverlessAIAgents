// frontend/src/pages/HomePage.js
// ホームページコンポーネント - ランディングページ
import React from 'react';
import { useNavigate } from 'react-router-dom';
import {
  Box,
  Typography,
  Button,
  Container,
  Grid,
  Card,
  CardContent,
  CardActions,
  Paper,
  List,
  ListItem,
  ListItemIcon,
  ListItemText,
} from '@mui/material';
import {
  School,
  Assessment,
  TrendingUp,
  PersonalVideo,
  CheckCircle,
  Star,
  Psychology,
  Timeline,
} from '@mui/icons-material';

const HomePage = () => {
  const navigate = useNavigate();

  // 機能紹介データ
  const features = [
    {
      icon: <Assessment color="primary" sx={{ fontSize: 40 }} />,
      title: '技能診断',
      description: 'AIがあなたの現在のスキルレベルを正確に評価し、強みと改善点を明確にします。',
      action: () => navigate('/skill-assessment'),
      buttonText: '診断を開始'
    },
    {
      icon: <School color="primary" sx={{ fontSize: 40 }} />,
      title: '個人化学習パス',
      description: '診断結果に基づいて、あなたに最適な学習コンテンツとスケジュールを提案します。',
      action: () => navigate('/learning-path'),
      buttonText: '学習パスを見る'
    },
    {
      icon: <PersonalVideo color="primary" sx={{ fontSize: 40 }} />,
      title: 'インタラクティブ学習',
      description: '動画、クイズ、実践演習を組み合わせた効果的な学習体験を提供します。',
      action: () => navigate('/dashboard'),
      buttonText: '学習を開始'
    },
    {
      icon: <TrendingUp color="primary" sx={{ fontSize: 40 }} />,
      title: '進捗追跡',
      description: '学習の進捗を可視化し、目標達成までの道のりを明確にします。',
      action: () => navigate('/dashboard'),
      buttonText: '進捗を確認'
    }
  ];

  // 学習内容
  const learningTopics = [
    '言語モデルの基礎',
    'トークンと埋め込み',
    'Transformerアーキテクチャ',
    'テキスト分類',
    'プロンプトエンジニアリング',
    'セマンティック検索とRAG',
    'マルチモーダルモデル',
    'ファインチューニング'
  ];

  return (
    <Box>
      {/* ヒーローセクション */}
      <Paper
        sx={{
          background: 'linear-gradient(135deg, #1976d2 0%, #42a5f5 100%)',
          color: 'white',
          py: 8,
          mb: 6,
          borderRadius: 0,
        }}
      >
        <Container maxWidth="lg">
          <Grid container spacing={4} alignItems="center">
            <Grid item xs={12} md={6}>
              <Typography variant="h2" component="h1" gutterBottom>
                AI・機械学習を
                <br />
                効率的に学習
              </Typography>
              <Typography variant="h5" sx={{ mb: 4, opacity: 0.9 }}>
                個人化された学習体験で、あなたのAIスキルを次のレベルへ
              </Typography>
              <Box sx={{ display: 'flex', gap: 2, flexWrap: 'wrap' }}>
                <Button
                  variant="contained"
                  size="large"
                  sx={{
                    backgroundColor: 'white',
                    color: 'primary.main',
                    '&:hover': {
                      backgroundColor: 'rgba(255, 255, 255, 0.9)',
                    },
                  }}
                  onClick={() => navigate('/register')}
                >
                  今すぐ始める
                </Button>
                <Button
                  variant="outlined"
                  size="large"
                  sx={{
                    borderColor: 'white',
                    color: 'white',
                    '&:hover': {
                      borderColor: 'white',
                      backgroundColor: 'rgba(255, 255, 255, 0.1)',
                    },
                  }}
                  onClick={() => navigate('/skill-assessment')}
                >
                  技能診断を試す
                </Button>
              </Box>
            </Grid>
            <Grid item xs={12} md={6}>
              <Box
                sx={{
                  display: 'flex',
                  justifyContent: 'center',
                  alignItems: 'center',
                  height: 300,
                }}
              >
                <Psychology sx={{ fontSize: 200, opacity: 0.8 }} />
              </Box>
            </Grid>
          </Grid>
        </Container>
      </Paper>

      <Container maxWidth="lg">
        {/* 機能紹介セクション */}
        <Box sx={{ mb: 8 }}>
          <Typography variant="h3" component="h2" textAlign="center" gutterBottom>
            プラットフォームの特徴
          </Typography>
          <Typography variant="h6" textAlign="center" color="text.secondary" sx={{ mb: 6 }}>
            AIを活用した最新の学習システムで、効率的にスキルアップ
          </Typography>
          
          <Grid container spacing={4}>
            {features.map((feature, index) => (
              <Grid item xs={12} md={6} key={index}>
                <Card
                  sx={{
                    height: '100%',
                    display: 'flex',
                    flexDirection: 'column',
                    transition: 'transform 0.2s',
                    '&:hover': {
                      transform: 'translateY(-4px)',
                      boxShadow: 4,
                    },
                  }}
                >
                  <CardContent sx={{ flexGrow: 1 }}>
                    <Box sx={{ display: 'flex', alignItems: 'center', mb: 2 }}>
                      {feature.icon}
                      <Typography variant="h5" component="h3" sx={{ ml: 2 }}>
                        {feature.title}
                      </Typography>
                    </Box>
                    <Typography variant="body1" color="text.secondary">
                      {feature.description}
                    </Typography>
                  </CardContent>
                  <CardActions>
                    <Button
                      size="small"
                      variant="outlined"
                      onClick={feature.action}
                      startIcon={<Star />}
                    >
                      {feature.buttonText}
                    </Button>
                  </CardActions>
                </Card>
              </Grid>
            ))}
          </Grid>
        </Box>

        {/* 学習内容セクション */}
        <Box sx={{ mb: 8 }}>
          <Grid container spacing={4}>
            <Grid item xs={12} md={6}>
              <Typography variant="h4" component="h2" gutterBottom>
                学習内容
              </Typography>
              <Typography variant="body1" color="text.secondary" sx={{ mb: 3 }}>
                最新のAI・機械学習技術を体系的に学習できるカリキュラムを提供しています。
                基礎から応用まで、段階的にスキルを身につけることができます。
              </Typography>
              <List>
                {learningTopics.map((topic, index) => (
                  <ListItem key={index} sx={{ py: 0.5 }}>
                    <ListItemIcon>
                      <CheckCircle color="primary" />
                    </ListItemIcon>
                    <ListItemText primary={topic} />
                  </ListItem>
                ))}
              </List>
            </Grid>
            <Grid item xs={12} md={6}>
              <Box
                sx={{
                  display: 'flex',
                  justifyContent: 'center',
                  alignItems: 'center',
                  height: '100%',
                  minHeight: 400,
                }}
              >
                <Timeline sx={{ fontSize: 150, color: 'primary.main', opacity: 0.7 }} />
              </Box>
            </Grid>
          </Grid>
        </Box>

        {/* CTA セクション */}
        <Paper
          sx={{
            textAlign: 'center',
            py: 6,
            px: 4,
            mb: 4,
            background: 'linear-gradient(135deg, #f5f5f5 0%, #e0e0e0 100%)',
          }}
        >
          <Typography variant="h4" component="h2" gutterBottom>
            今すぐ学習を始めませんか？
          </Typography>
          <Typography variant="h6" color="text.secondary" sx={{ mb: 4 }}>
            無料で技能診断を受けて、あなたに最適な学習パスを見つけましょう
          </Typography>
          <Box sx={{ display: 'flex', gap: 2, justifyContent: 'center', flexWrap: 'wrap' }}>
            <Button
              variant="contained"
              size="large"
              onClick={() => navigate('/register')}
            >
              アカウント作成
            </Button>
            <Button
              variant="outlined"
              size="large"
              onClick={() => navigate('/skill-assessment')}
            >
              技能診断を開始
            </Button>
          </Box>
        </Paper>
      </Container>
    </Box>
  );
};

export default HomePage;

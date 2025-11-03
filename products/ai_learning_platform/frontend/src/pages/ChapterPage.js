// frontend/src/pages/ChapterPage.js
// チャプター詳細ページコンポーネント - 学習コンテンツの表示
import React, { useState, useEffect } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import {
  Box,
  Typography,
  Card,
  CardContent,
  Button,
  Paper,
  Tabs,
  Tab,
  List,
  ListItem,
  ListItemIcon,
  ListItemText,
  LinearProgress,
  Chip,
  Alert,
} from '@mui/material';
import {
  MenuBook,
  VideoLibrary,
  Quiz,
  Psychology,
  CheckCircle,
  PlayArrow,
  NavigateNext,
  NavigateBefore,
} from '@mui/icons-material';
import ReactMarkdown from 'react-markdown';
import LoadingSpinner from '../components/Common/LoadingSpinner';

const ChapterPage = () => {
  const { chapterId } = useParams();
  const navigate = useNavigate();

  // 状態管理
  const [loading, setLoading] = useState(true);
  const [chapter, setChapter] = useState(null);
  const [contents, setContents] = useState([]);
  const [currentContentIndex, setCurrentContentIndex] = useState(0);
  const [progress, setProgress] = useState({ status: 'not_started', progress: 0 });
  const [error, setError] = useState('');

  // データを取得
  useEffect(() => {
    const fetchChapterData = async () => {
      try {
        // TODO: 実際のAPIコールに置き換え
        // const response = await apiClient.get(`/learning/chapters/${chapterId}`);
        
        // モックデータ
        setTimeout(() => {
          const mockChapter = {
            id: parseInt(chapterId),
            title: 'Introduction to Language Models',
            description: '言語モデルの基本概念と応用について理解する',
            order_index: 1,
            estimated_time: '1週間',
            difficulty: 'beginner'
          };

          const mockContents = [
            {
              id: 1,
              title: '言語モデルとは',
              content_type: 'markdown',
              content: `# 言語モデルとは

言語モデル（Language Model）は、自然言語の確率分布を学習するモデルです。

## 基本概念

言語モデルは以下の目的で使用されます：

1. **テキスト生成**: 与えられた文脈に基づいて、次に来る単語や文章を予測
2. **言語理解**: テキストの意味や構造を理解
3. **翻訳**: ある言語から別の言語への変換

## 種類

### 統計的言語モデル
- N-gramモデル
- 隠れマルコフモデル

### ニューラル言語モデル
- RNNベースモデル
- Transformerベースモデル（GPT、BERT等）

## 応用例

- チャットボット
- 機械翻訳
- 文書要約
- 質問応答システム

言語モデルは現代のAI技術の基盤となっており、多くのアプリケーションで活用されています。`
            },
            {
              id: 2,
              title: '実例で学ぶ言語モデル',
              content_type: 'video',
              content: 'https://example.com/video/language-models-intro'
            },
            {
              id: 3,
              title: '理解度チェック',
              content_type: 'quiz',
              content: {
                questions: [
                  {
                    question: '言語モデルの主な目的は何ですか？',
                    options: ['画像認識', 'テキスト生成と理解', '音声認識', 'データベース管理'],
                    correct: 1
                  },
                  {
                    question: 'Transformerベースの言語モデルの例はどれですか？',
                    options: ['CNN', 'GPT', 'SVM', 'KNN'],
                    correct: 1
                  }
                ]
              }
            }
          ];

          setChapter(mockChapter);
          setContents(mockContents);
          setProgress({ status: 'in_progress', progress: 33 });
          setLoading(false);
        }, 1000);
        
      } catch (error) {
        console.error('チャプターデータ取得エラー:', error);
        setError('チャプターデータの取得に失敗しました');
        setLoading(false);
      }
    };

    fetchChapterData();
  }, [chapterId]);

  // コンテンツタイプに応じたアイコンを取得
  const getContentIcon = (type) => {
    switch (type) {
      case 'markdown': return <MenuBook />;
      case 'video': return <VideoLibrary />;
      case 'quiz': return <Quiz />;
      case 'simulation': return <Psychology />;
      default: return <MenuBook />;
    }
  };

  // 進捗を更新
  const updateProgress = async (contentId, status) => {
    try {
      // TODO: 実際のAPIコールに置き換え
      // await apiClient.put(`/learning/progress/${contentId}`, { status });
      
      // 進捗を計算
      const completedCount = currentContentIndex + 1;
      const newProgress = Math.round((completedCount / contents.length) * 100);
      
      setProgress({
        status: newProgress === 100 ? 'completed' : 'in_progress',
        progress: newProgress
      });
    } catch (error) {
      console.error('進捗更新エラー:', error);
    }
  };

  // 次のコンテンツへ
  const handleNext = () => {
    if (currentContentIndex < contents.length - 1) {
      setCurrentContentIndex(currentContentIndex + 1);
      updateProgress(contents[currentContentIndex].id, 'completed');
    }
  };

  // 前のコンテンツへ
  const handlePrevious = () => {
    if (currentContentIndex > 0) {
      setCurrentContentIndex(currentContentIndex - 1);
    }
  };

  // コンテンツをレンダリング
  const renderContent = (content) => {
    switch (content.content_type) {
      case 'markdown':
        return (
          <Box sx={{ '& h1': { color: 'primary.main' }, '& h2': { color: 'primary.main' } }}>
            <ReactMarkdown>{content.content}</ReactMarkdown>
          </Box>
        );
      
      case 'video':
        return (
          <Box sx={{ textAlign: 'center', py: 4 }}>
            <VideoLibrary sx={{ fontSize: 80, color: 'primary.main', mb: 2 }} />
            <Typography variant="h6" gutterBottom>
              動画コンテンツ
            </Typography>
            <Typography variant="body2" color="text.secondary" sx={{ mb: 2 }}>
              {content.content}
            </Typography>
            <Button variant="contained" startIcon={<PlayArrow />}>
              動画を再生
            </Button>
          </Box>
        );
      
      case 'quiz':
        return (
          <Box>
            <Typography variant="h6" gutterBottom>
              理解度チェック
            </Typography>
            {content.content.questions.map((q, index) => (
              <Card key={index} sx={{ mb: 2 }}>
                <CardContent>
                  <Typography variant="body1" gutterBottom>
                    {index + 1}. {q.question}
                  </Typography>
                  <List>
                    {q.options.map((option, optIndex) => (
                      <ListItem key={optIndex} button>
                        <ListItemText primary={`${String.fromCharCode(65 + optIndex)}. ${option}`} />
                      </ListItem>
                    ))}
                  </List>
                </CardContent>
              </Card>
            ))}
          </Box>
        );
      
      default:
        return (
          <Typography variant="body1">
            このコンテンツタイプはまだサポートされていません。
          </Typography>
        );
    }
  };

  // ローディング中の表示
  if (loading) {
    return <LoadingSpinner message="チャプターを読み込み中..." />;
  }

  // エラー表示
  if (error) {
    return (
      <Alert severity="error" sx={{ mt: 2 }}>
        {error}
      </Alert>
    );
  }

  const currentContent = contents[currentContentIndex];

  return (
    <Box>
      {/* ヘッダー */}
      <Paper sx={{ p: 3, mb: 4 }}>
        <Typography variant="h4" component="h1" gutterBottom>
          {chapter.title}
        </Typography>
        <Typography variant="body1" color="text.secondary" sx={{ mb: 2 }}>
          {chapter.description}
        </Typography>
        
        <Box sx={{ display: 'flex', gap: 1, mb: 2 }}>
          <Chip label={chapter.difficulty} color="primary" size="small" />
          <Chip label={chapter.estimated_time} variant="outlined" size="small" />
        </Box>

        {/* 進捗バー */}
        <Box>
          <Box sx={{ display: 'flex', justifyContent: 'space-between', mb: 1 }}>
            <Typography variant="body2">学習進捗</Typography>
            <Typography variant="body2">{progress.progress}%</Typography>
          </Box>
          <LinearProgress 
            variant="determinate" 
            value={progress.progress} 
            sx={{ height: 8, borderRadius: 4 }}
          />
        </Box>
      </Paper>

      <Box sx={{ display: 'flex', gap: 3 }}>
        {/* サイドバー - コンテンツ一覧 */}
        <Paper sx={{ width: 300, p: 2, height: 'fit-content' }}>
          <Typography variant="h6" gutterBottom>
            学習内容
          </Typography>
          <List>
            {contents.map((content, index) => (
              <ListItem
                key={content.id}
                button
                selected={index === currentContentIndex}
                onClick={() => setCurrentContentIndex(index)}
              >
                <ListItemIcon>
                  {index < currentContentIndex ? (
                    <CheckCircle color="success" />
                  ) : (
                    getContentIcon(content.content_type)
                  )}
                </ListItemIcon>
                <ListItemText 
                  primary={content.title}
                  primaryTypographyProps={{ 
                    variant: 'body2',
                    color: index <= currentContentIndex ? 'primary' : 'text.secondary'
                  }}
                />
              </ListItem>
            ))}
          </List>
        </Paper>

        {/* メインコンテンツ */}
        <Box sx={{ flexGrow: 1 }}>
          <Card>
            <CardContent>
              <Box sx={{ display: 'flex', alignItems: 'center', mb: 3 }}>
                {getContentIcon(currentContent.content_type)}
                <Typography variant="h5" component="h2" sx={{ ml: 1 }}>
                  {currentContent.title}
                </Typography>
              </Box>
              
              {renderContent(currentContent)}
            </CardContent>
          </Card>

          {/* ナビゲーションボタン */}
          <Box sx={{ display: 'flex', justifyContent: 'space-between', mt: 3 }}>
            <Button
              variant="outlined"
              startIcon={<NavigateBefore />}
              onClick={handlePrevious}
              disabled={currentContentIndex === 0}
            >
              前のコンテンツ
            </Button>

            <Button
              variant="outlined"
              onClick={() => navigate('/learning-path')}
            >
              学習パスに戻る
            </Button>

            {currentContentIndex === contents.length - 1 ? (
              <Button
                variant="contained"
                startIcon={<CheckCircle />}
                onClick={() => {
                  updateProgress(currentContent.id, 'completed');
                  navigate('/learning-path');
                }}
              >
                チャプター完了
              </Button>
            ) : (
              <Button
                variant="contained"
                endIcon={<NavigateNext />}
                onClick={handleNext}
              >
                次のコンテンツ
              </Button>
            )}
          </Box>
        </Box>
      </Box>
    </Box>
  );
};

export default ChapterPage;

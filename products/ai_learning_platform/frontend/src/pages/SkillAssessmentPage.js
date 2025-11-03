// frontend/src/pages/SkillAssessmentPage.js
// 技能評価ページコンポーネント - 診断テストの実施
import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  Box,
  Typography,
  Card,
  CardContent,
  Button,
  Radio,
  RadioGroup,
  FormControlLabel,
  FormControl,
  FormLabel,
  Paper,
  LinearProgress,
  Alert,
  Chip,
  Grid,
} from '@mui/material';
import {
  Assessment,
  NavigateNext,
  NavigateBefore,
  CheckCircle,
  Psychology,
} from '@mui/icons-material';
import LoadingSpinner from '../components/Common/LoadingSpinner';

const SkillAssessmentPage = () => {
  const navigate = useNavigate();

  // 状態管理
  const [loading, setLoading] = useState(true);
  const [submitting, setSubmitting] = useState(false);
  const [questions, setQuestions] = useState([]);
  const [currentQuestionIndex, setCurrentQuestionIndex] = useState(0);
  const [answers, setAnswers] = useState({});
  const [assessmentResult, setAssessmentResult] = useState(null);
  const [error, setError] = useState('');

  // 質問データを取得
  useEffect(() => {
    const fetchQuestions = async () => {
      try {
        // TODO: 実際のAPIコールに置き換え
        // const response = await apiClient.get('/skill-assessment/questions');
        
        // モックデータ
        setTimeout(() => {
          setQuestions([
            {
              id: 1,
              category: 'Prompt Engineering',
              question: '効果的なプロンプトの特徴として最も重要なのはどれですか？',
              options: {
                A: '明確で具体的な指示',
                B: '曖昧で抽象的な表現',
                C: '可能な限り短い文章',
                D: '専門用語を多用する'
              }
            },
            {
              id: 2,
              category: 'Text Classification',
              question: 'テキスト分類において、特徴量エンジニアリングの目的は何ですか？',
              options: {
                A: 'データ量を増やすため',
                B: 'モデルの性能を向上させるため',
                C: '処理時間を長くするため',
                D: 'メモリ使用量を増やすため'
              }
            },
            {
              id: 3,
              category: 'Language Models',
              question: 'Transformerアーキテクチャの主要な特徴は何ですか？',
              options: {
                A: 'RNNベースの構造',
                B: 'CNNベースの構造',
                C: 'Attention機構',
                D: '決定木ベースの構造'
              }
            },
            {
              id: 4,
              category: 'Fine-tuning',
              question: 'ファインチューニングの主な目的は何ですか？',
              options: {
                A: 'モデルサイズを小さくする',
                B: '特定のタスクに適応させる',
                C: '学習時間を短縮する',
                D: 'メモリ使用量を削減する'
              }
            },
            {
              id: 5,
              category: 'Multimodal',
              question: 'マルチモーダルモデルの特徴は何ですか？',
              options: {
                A: '単一のデータタイプのみ処理',
                B: '複数のデータタイプを統合処理',
                C: 'テキストのみ処理',
                D: '画像のみ処理'
              }
            }
          ]);
          setLoading(false);
        }, 1000);
        
      } catch (error) {
        console.error('質問データ取得エラー:', error);
        setError('質問データの取得に失敗しました');
        setLoading(false);
      }
    };

    fetchQuestions();
  }, []);

  // 回答の処理
  const handleAnswerChange = (questionId, selectedOption) => {
    setAnswers(prev => ({
      ...prev,
      [questionId]: selectedOption
    }));
  };

  // 次の質問へ
  const handleNext = () => {
    if (currentQuestionIndex < questions.length - 1) {
      setCurrentQuestionIndex(currentQuestionIndex + 1);
    }
  };

  // 前の質問へ
  const handlePrevious = () => {
    if (currentQuestionIndex > 0) {
      setCurrentQuestionIndex(currentQuestionIndex - 1);
    }
  };

  // 評価結果の提出
  const handleSubmit = async () => {
    setSubmitting(true);
    setError('');

    try {
      // 回答データを整理
      const submissionData = {
        answers: Object.entries(answers).map(([questionId, selectedOption]) => ({
          question_id: parseInt(questionId),
          selected_option: selectedOption
        }))
      };

      // TODO: 実際のAPIコールに置き換え
      // const response = await apiClient.post('/skill-assessment/submit', submissionData);
      
      // モック結果
      setTimeout(() => {
        setAssessmentResult({
          overall_score: 78,
          strengths: ['プロンプトエンジニアリング', '言語モデル'],
          weaknesses: ['ファインチューニング', 'マルチモーダルモデル'],
          recommendations: [
            {
              chapter_id: 11,
              title: 'Fine-tuning Representation Models',
              reason: 'ファインチューニング技術の理解を深める必要があります'
            },
            {
              chapter_id: 9,
              title: 'Multimodal Large Language Models',
              reason: 'マルチモーダルモデルの基礎知識を習得することを推奨します'
            }
          ],
          detailed_analysis: 'あなたは基本的なAI概念をよく理解していますが、より高度な技術について学習することで、さらなるスキル向上が期待できます。'
        });
        setSubmitting(false);
      }, 2000);
      
    } catch (error) {
      console.error('評価提出エラー:', error);
      setError('評価の提出に失敗しました');
      setSubmitting(false);
    }
  };

  // ローディング中の表示
  if (loading) {
    return <LoadingSpinner message="技能評価を準備中..." />;
  }

  // 結果表示
  if (assessmentResult) {
    return (
      <Box>
        <Paper sx={{ p: 3, mb: 4, textAlign: 'center', background: 'linear-gradient(135deg, #4caf50 0%, #81c784 100%)', color: 'white' }}>
          <CheckCircle sx={{ fontSize: 60, mb: 2 }} />
          <Typography variant="h4" component="h1" gutterBottom>
            技能評価完了！
          </Typography>
          <Typography variant="h6">
            あなたの総合スコア: {assessmentResult.overall_score}/100
          </Typography>
        </Paper>

        <Grid container spacing={3}>
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom color="success.main">
                  <CheckCircle sx={{ mr: 1, verticalAlign: 'middle' }} />
                  強み
                </Typography>
                <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 1, mb: 2 }}>
                  {assessmentResult.strengths.map((strength, index) => (
                    <Chip key={index} label={strength} color="success" />
                  ))}
                </Box>
              </CardContent>
            </Card>
          </Grid>

          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom color="warning.main">
                  <Psychology sx={{ mr: 1, verticalAlign: 'middle' }} />
                  改善点
                </Typography>
                <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 1, mb: 2 }}>
                  {assessmentResult.weaknesses.map((weakness, index) => (
                    <Chip key={index} label={weakness} color="warning" />
                  ))}
                </Box>
              </CardContent>
            </Card>
          </Grid>

          <Grid item xs={12}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  詳細分析
                </Typography>
                <Typography variant="body1" sx={{ mb: 3 }}>
                  {assessmentResult.detailed_analysis}
                </Typography>

                <Typography variant="h6" gutterBottom>
                  推奨学習コンテンツ
                </Typography>
                {assessmentResult.recommendations.map((rec, index) => (
                  <Card key={index} variant="outlined" sx={{ mb: 2 }}>
                    <CardContent>
                      <Typography variant="h6" component="h3">
                        {rec.title}
                      </Typography>
                      <Typography variant="body2" color="text.secondary">
                        {rec.reason}
                      </Typography>
                      <Button 
                        variant="outlined" 
                        size="small" 
                        sx={{ mt: 1 }}
                        onClick={() => navigate(`/chapter/${rec.chapter_id}`)}
                      >
                        学習開始
                      </Button>
                    </CardContent>
                  </Card>
                ))}
              </CardContent>
            </Card>
          </Grid>
        </Grid>

        <Box sx={{ textAlign: 'center', mt: 4 }}>
          <Button 
            variant="contained" 
            size="large"
            onClick={() => navigate('/dashboard')}
          >
            ダッシュボードに戻る
          </Button>
        </Box>
      </Box>
    );
  }

  // 質問表示
  const currentQuestion = questions[currentQuestionIndex];
  const progress = ((currentQuestionIndex + 1) / questions.length) * 100;
  const isLastQuestion = currentQuestionIndex === questions.length - 1;
  const allQuestionsAnswered = questions.every(q => answers[q.id]);

  return (
    <Box>
      {/* ヘッダー */}
      <Paper sx={{ p: 3, mb: 4 }}>
        <Typography variant="h4" component="h1" gutterBottom>
          <Assessment sx={{ mr: 2, verticalAlign: 'middle' }} />
          技能評価テスト
        </Typography>
        <Typography variant="body1" color="text.secondary">
          あなたのAI・機械学習スキルを評価します。各質問に最適だと思う回答を選択してください。
        </Typography>
      </Paper>

      {/* 進捗バー */}
      <Box sx={{ mb: 4 }}>
        <Box sx={{ display: 'flex', justifyContent: 'space-between', mb: 1 }}>
          <Typography variant="body2">
            質問 {currentQuestionIndex + 1} / {questions.length}
          </Typography>
          <Typography variant="body2">
            {Math.round(progress)}% 完了
          </Typography>
        </Box>
        <LinearProgress variant="determinate" value={progress} sx={{ height: 8, borderRadius: 4 }} />
      </Box>

      {/* エラーメッセージ */}
      {error && (
        <Alert severity="error" sx={{ mb: 3 }}>
          {error}
        </Alert>
      )}

      {/* 質問カード */}
      <Card sx={{ mb: 4 }}>
        <CardContent>
          <Box sx={{ mb: 2 }}>
            <Chip label={currentQuestion.category} color="primary" size="small" />
          </Box>
          
          <Typography variant="h6" component="h2" gutterBottom>
            {currentQuestion.question}
          </Typography>

          <FormControl component="fieldset" sx={{ width: '100%', mt: 3 }}>
            <RadioGroup
              value={answers[currentQuestion.id] || ''}
              onChange={(e) => handleAnswerChange(currentQuestion.id, e.target.value)}
            >
              {Object.entries(currentQuestion.options).map(([key, value]) => (
                <FormControlLabel
                  key={key}
                  value={key}
                  control={<Radio />}
                  label={`${key}. ${value}`}
                  sx={{ mb: 1 }}
                />
              ))}
            </RadioGroup>
          </FormControl>
        </CardContent>
      </Card>

      {/* ナビゲーションボタン */}
      <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
        <Button
          variant="outlined"
          startIcon={<NavigateBefore />}
          onClick={handlePrevious}
          disabled={currentQuestionIndex === 0}
        >
          前の質問
        </Button>

        <Box sx={{ display: 'flex', gap: 1 }}>
          {questions.map((_, index) => (
            <Box
              key={index}
              sx={{
                width: 12,
                height: 12,
                borderRadius: '50%',
                backgroundColor: answers[questions[index].id] 
                  ? 'primary.main' 
                  : index === currentQuestionIndex 
                    ? 'warning.main' 
                    : 'grey.300',
                cursor: 'pointer'
              }}
              onClick={() => setCurrentQuestionIndex(index)}
            />
          ))}
        </Box>

        {isLastQuestion ? (
          <Button
            variant="contained"
            onClick={handleSubmit}
            disabled={!allQuestionsAnswered || submitting}
            startIcon={submitting ? <LoadingSpinner /> : <CheckCircle />}
          >
            {submitting ? '評価中...' : '評価完了'}
          </Button>
        ) : (
          <Button
            variant="contained"
            endIcon={<NavigateNext />}
            onClick={handleNext}
          >
            次の質問
          </Button>
        )}
      </Box>
    </Box>
  );
};

export default SkillAssessmentPage;

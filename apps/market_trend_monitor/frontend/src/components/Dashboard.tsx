/**
 * ダッシュボードコンポーネント.
 * 
 * 目的: トレンドグラフと最新ニュースを表示
 * I/O:
 *   - Input: なし (ストアから取得)
 *   - Output: ダッシュボードUI
 */

import React, { useEffect } from 'react';
import {
  Box,
  Grid,
  Paper,
  Typography,
  CircularProgress,
  Alert,
  Stack,
  Chip,
} from '@mui/material';
import { useAppStore } from '@/store/useAppStore';
import TrendChart from './TrendChart';
import TrendList from './TrendList';

const Dashboard: React.FC = () => {
  const { trends, loading, error, fetchTrends } = useAppStore();

  useEffect(() => {
    fetchTrends();
  }, [fetchTrends]);

  if (loading && trends.length === 0) {
    return (
      <Box
        display="flex"
        justifyContent="center"
        alignItems="center"
        minHeight="400px"
      >
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Box p={3}>
        <Alert severity="error">{error}</Alert>
      </Box>
    );
  }

  const totalArticles = trends.reduce((sum, t) => sum + t.articles_count, 0);
  const averageScore =
    trends.length > 0
      ? trends.reduce((sum, t) => sum + t.score, 0) / trends.length
      : 0;
  const topTopics = trends.slice(0, 4).map((trend) => trend.topic);

  return (
    <Box p={3}>
      <Paper
        sx={{
          p: { xs: 3, md: 4 },
          mb: 3,
          background:
            'linear-gradient(135deg, rgba(79,70,229,0.9), rgba(124,58,237,0.85))',
          color: '#fff',
          border: '1px solid rgba(255,255,255,0.12)',
          boxShadow: 'var(--shadow-elev)',
          position: 'relative',
          overflow: 'hidden',
        }}
      >
        <Box
          sx={{
            position: 'absolute',
            inset: 0,
            backgroundImage:
              'linear-gradient(rgba(255,255,255,0.08) 1px, transparent 1px), linear-gradient(90deg, rgba(255,255,255,0.08) 1px, transparent 1px)',
            backgroundSize: '36px 36px',
            opacity: 0.2,
            zIndex: 0,
            pointerEvents: 'none',
          }}
        />
        <Box
          sx={{
            position: 'absolute',
            right: -40,
            top: -60,
            width: 200,
            height: 200,
            borderRadius: '50%',
            background: 'radial-gradient(circle, rgba(255,255,255,0.4), transparent 70%)',
            filter: 'blur(4px)',
            zIndex: 0,
            pointerEvents: 'none',
          }}
        />
        <Stack spacing={1} maxWidth={560} sx={{ position: 'relative', zIndex: 1 }}>
          <Typography variant="h3">市場動向ダッシュボード</Typography>
          <Typography variant="body1" sx={{ opacity: 0.8 }}>
            マーケットの兆候を一望し、意思決定の速度を上げるためのリアルタイム監視。
          </Typography>
          <Stack direction="row" spacing={1} flexWrap="wrap" mt={1}>
            {topTopics.length === 0 ? (
              <Chip
                label="新しいデータを収集中"
                size="small"
                sx={{
                  color: '#fff',
                  borderColor: 'rgba(255,255,255,0.4)',
                  background: 'rgba(255,255,255,0.1)',
                }}
                variant="outlined"
              />
            ) : (
              topTopics.map((topic) => (
                <Chip
                  key={topic}
                  label={topic}
                  size="small"
                  sx={{
                    color: '#fff',
                    borderColor: 'rgba(255,255,255,0.3)',
                    background: 'rgba(255,255,255,0.12)',
                  }}
                  variant="outlined"
                />
              ))
            )}
          </Stack>
        </Stack>
      </Paper>

      <Grid container spacing={3}>
        <Grid item xs={12} md={4}>
          <Paper
            sx={{
              p: 3,
              minHeight: 160,
              backgroundImage:
                'linear-gradient(135deg, rgba(99,102,241,0.12), rgba(18,18,26,0.9))',
            }}
          >
            <Typography variant="overline" color="text.secondary">
              トレンド数
            </Typography>
            <Typography variant="h3">{trends.length}</Typography>
            <Typography variant="body2" color="text.secondary">
              直近の重要シグナルを集計
            </Typography>
          </Paper>
        </Grid>
        <Grid item xs={12} md={4}>
          <Paper
            sx={{
              p: 3,
              minHeight: 160,
              backgroundImage:
                'linear-gradient(135deg, rgba(168,85,247,0.12), rgba(18,18,26,0.9))',
            }}
          >
            <Typography variant="overline" color="text.secondary">
              記事総数
            </Typography>
            <Typography variant="h3">{totalArticles}</Typography>
            <Typography variant="body2" color="text.secondary">
              複数ソースを統合した証拠母数
            </Typography>
          </Paper>
        </Grid>
        <Grid item xs={12} md={4}>
          <Paper
            sx={{
              p: 3,
              minHeight: 160,
              backgroundImage:
                'linear-gradient(135deg, rgba(14,165,233,0.12), rgba(18,18,26,0.9))',
            }}
          >
            <Typography variant="overline" color="text.secondary">
              平均スコア
            </Typography>
            <Typography variant="h3">{averageScore.toFixed(2)}</Typography>
            <Typography variant="body2" color="text.secondary">
              信号強度のベースライン
            </Typography>
          </Paper>
        </Grid>

        {/* トレンドグラフ */}
        <Grid item xs={12} lg={8}>
          <Paper
            sx={{
              p: 3,
              height: '420px',
              border: '1px solid rgba(99,102,241,0.2)',
              backgroundImage:
                'linear-gradient(180deg, rgba(18,18,26,0.95), rgba(10,10,15,0.95))',
            }}
          >
            <Typography variant="h6" gutterBottom>
              トレンド推移
            </Typography>
            <TrendChart trends={trends} />
          </Paper>
        </Grid>

        {/* 統計情報 */}
        <Grid item xs={12} lg={4}>
          <Paper
            sx={{
              p: 3,
              height: '420px',
              border: '1px solid rgba(168,85,247,0.2)',
              backgroundImage:
                'linear-gradient(180deg, rgba(18,18,26,0.95), rgba(10,10,15,0.95))',
            }}
          >
            <Typography variant="h6" gutterBottom>
              インサイト要約
            </Typography>
            <Stack spacing={2} mt={2}>
              <Box>
                <Typography variant="body2" color="text.secondary">
                  注目テーマ
                </Typography>
                <Typography variant="h6">
                  {topTopics[0] || 'データ待機中'}
                </Typography>
              </Box>
              <Box>
                <Typography variant="body2" color="text.secondary">
                  主要ソース
                </Typography>
                <Typography variant="h6">
                  {trends.length > 0 ? 'News / GitHub / arXiv' : '未集計'}
                </Typography>
              </Box>
              <Box>
                <Typography variant="body2" color="text.secondary">
                  シグナルステータス
                </Typography>
                <Typography variant="h6">
                  {averageScore > 0.65 ? '強い兆候あり' : '通常監視'}
                </Typography>
              </Box>
            </Stack>
          </Paper>
        </Grid>

        {/* トレンド一覧 */}
        <Grid item xs={12}>
          <Paper
            sx={{
              p: 3,
              border: '1px solid rgba(255,255,255,0.06)',
              backgroundImage:
                'linear-gradient(180deg, rgba(18,18,26,0.96), rgba(10,10,15,0.96))',
            }}
          >
            <Typography variant="h6" gutterBottom>
              最新トレンド
            </Typography>
            <TrendList trends={trends.slice(0, 10)} />
          </Paper>
        </Grid>
      </Grid>
    </Box>
  );
};

export default Dashboard;

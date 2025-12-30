/**
 * ダッシュボードコンポーネント.
 * 
 * 目的: トレンドグラフと最新ニュースを表示
 * I/O:
 *   - Input: なし (ストアから取得)
 *   - Output: ダッシュボードUI
 */

import { useEffect } from 'react';
import {
  Box,
  Grid,
  Paper,
  Typography,
  CircularProgress,
  Alert,
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

  return (
    <Box p={3}>
      <Typography variant="h4" gutterBottom>
        市場動向ダッシュボード
      </Typography>

      <Grid container spacing={3}>
        {/* トレンドグラフ */}
        <Grid item xs={12} lg={8}>
          <Paper sx={{ p: 2, height: '400px' }}>
            <Typography variant="h6" gutterBottom>
              トレンド推移
            </Typography>
            <TrendChart trends={trends} />
          </Paper>
        </Grid>

        {/* 統計情報 */}
        <Grid item xs={12} lg={4}>
          <Paper sx={{ p: 2, height: '400px' }}>
            <Typography variant="h6" gutterBottom>
              統計情報
            </Typography>
            <Box mt={2}>
              <Typography variant="body1">
                総トレンド数: {trends.length}
              </Typography>
              <Typography variant="body1" mt={1}>
                総記事数:{' '}
                {trends.reduce((sum, t) => sum + t.articles_count, 0)}
              </Typography>
              <Typography variant="body1" mt={1}>
                平均スコア:{' '}
                {trends.length > 0
                  ? (
                      trends.reduce((sum, t) => sum + t.score, 0) /
                      trends.length
                    ).toFixed(2)
                  : 0}
              </Typography>
            </Box>
          </Paper>
        </Grid>

        {/* トレンド一覧 */}
        <Grid item xs={12}>
          <Paper sx={{ p: 2 }}>
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


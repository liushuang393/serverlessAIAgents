/**
 * Prediction Tracker コンポーネント.
 *
 * Phase 13: 予測一覧 + レビューフォーム + Brier Score可視化
 */
import { useState, useEffect, useCallback } from 'react';
import {
  Box,
  Card,
  CardContent,
  Typography,
  Button,
  Chip,
  LinearProgress,
  Alert,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  Select,
  MenuItem,
  FormControl,
  InputLabel,
  Grid,
  Tooltip,
} from '@mui/material';
import { apiClient } from '../api/client';
import { useI18n } from '../i18n';

interface PredictionItem {
  id: string;
  statement: string;
  target_date: string;
  confidence: number;
  status: string;
  created_at: string;
  metadata: Record<string, unknown>;
}

interface CalibrationBin {
  bin_range: string;
  avg_confidence: number;
  avg_outcome: number;
  count: number;
}

interface PredictionBootstrapResponse {
  status: string;
  message: string;
  created_count: number;
  skipped_count: number;
  source_trends: number;
}

const statusColors: Record<string, string> = {
  pending: '#ff9800',
  correct: '#4caf50',
  partial: '#2196f3',
  incorrect: '#f44336',
};

export default function PredictionTracker() {
  const { t } = useI18n();
  const [predictions, setPredictions] = useState<PredictionItem[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [brierScore, setBrierScore] = useState<number | null>(null);
  const [calibrationBins, setCalibrationBins] = useState<CalibrationBin[]>([]);
  const [reviewDialog, setReviewDialog] = useState<string | null>(null);
  const [reviewOutcome, setReviewOutcome] = useState('correct');
  const [reviewNotes, setReviewNotes] = useState('');
  const [actualOutcome, setActualOutcome] = useState('');
  const [notice, setNotice] = useState<string | null>(null);

  const fetchPredictions = useCallback(async () => {
    setLoading(true);
    try {
      const [predResp, calResp] = await Promise.allSettled([
        apiClient.get<{ predictions: PredictionItem[]; total: number }>('/predictions'),
        apiClient.get<{ brier_score: number | null; calibration_bins: CalibrationBin[] }>('/predictions/calibration'),
      ]);
      if (predResp.status === 'fulfilled') {
        setPredictions(predResp.value.data.predictions || []);
        setError(null);
      } else {
        setPredictions([]);
        setError(t('pred.fetch_error'));
      }
      if (calResp.status === 'fulfilled') {
        setBrierScore(calResp.value.data.brier_score ?? null);
        setCalibrationBins(calResp.value.data.calibration_bins || []);
      } else {
        setBrierScore(null);
        setCalibrationBins([]);
      }
    } catch {
      setPredictions([]);
      setBrierScore(null);
      setCalibrationBins([]);
      setError(t('pred.fetch_error'));
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchPredictions();
  }, [fetchPredictions]);

  const handleReview = async () => {
    if (!reviewDialog || !actualOutcome) return;
    try {
      await apiClient.post(`/predictions/${reviewDialog}/review`, {
        actual_outcome: actualOutcome,
        outcome: reviewOutcome,
        notes: reviewNotes,
      });
      setReviewDialog(null);
      setActualOutcome('');
      setReviewNotes('');
      fetchPredictions();
    } catch {
      setError(t('pred.review_submit_error'));
    }
  };

  const handleBootstrapPredictions = async () => {
    try {
      setLoading(true);
      const resp = await apiClient.post<PredictionBootstrapResponse>(
        '/predictions/bootstrap',
        {
          horizon_days: 30,
          limit: 8,
        },
      );
      setNotice(resp.data.message || t('pred.auto_generated'));
      setError(null);
      await fetchPredictions();
    } catch {
      setError(t('pred.auto_generate_error'));
    } finally {
      setLoading(false);
    }
  };

  return (
    <Box>
      <Box
        sx={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'space-between',
          flexWrap: 'wrap',
          gap: 1,
          mb: 2,
        }}
      >
        <Box>
          <Typography variant="h4" gutterBottom>
            {t('pred.title')}
          </Typography>
          <Typography variant="body2" color="text.secondary" sx={{ mb: 3 }}>
            {t('pred.subtitle')}
          </Typography>
        </Box>
        <Button
          variant="outlined"
          onClick={handleBootstrapPredictions}
          disabled={loading}
        >
          {t('pred.generate_btn')}
        </Button>
      </Box>

      {loading && <LinearProgress sx={{ mb: 2 }} />}
      {error && <Alert severity="error" sx={{ mb: 2 }}>{error}</Alert>}
      {notice && (
        <Alert severity="info" sx={{ mb: 2 }} onClose={() => setNotice(null)}>
          {notice}
        </Alert>
      )}

      <Grid container spacing={2} sx={{ mb: 3 }}>
        <Grid item xs={4}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <Typography variant="subtitle2" color="text.secondary">
                {t('pred.brier_title')}
              </Typography>
              <Typography variant="h3" color={brierScore !== null && brierScore < 0.25 ? 'success.main' : 'warning.main'}>
                {brierScore !== null ? brierScore.toFixed(4) : 'N/A'}
              </Typography>
              <Typography variant="caption" color="text.secondary">
                {t('pred.brier_desc')}
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={4}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <Typography variant="subtitle2" color="text.secondary">
                {t('pred.total_predictions')}
              </Typography>
              <Typography variant="h3">{predictions.length}</Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={4}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <Typography variant="subtitle2" color="text.secondary">
                {t('pred.pending_count')}
              </Typography>
              <Typography variant="h3" color="warning.main">
                {predictions.filter((p) => p.status === 'pending').length}
              </Typography>
            </CardContent>
          </Card>
        </Grid>
      </Grid>

      {calibrationBins.length > 0 && (
        <Card sx={{ mb: 3 }}>
          <CardContent>
            <Typography variant="subtitle1" gutterBottom>
              {t('pred.calibration_chart')}
            </Typography>
            <Box sx={{ display: 'flex', alignItems: 'end', gap: 1, height: 100 }}>
              {calibrationBins.map((bin) => (
                <Box key={bin.bin_range} sx={{ flex: 1, textAlign: 'center' }}>
                  <Box
                    sx={{
                      height: `${bin.avg_outcome * 100}%`,
                      minHeight: 4,
                      bgcolor: Math.abs(bin.avg_confidence - bin.avg_outcome) < 0.15
                        ? 'success.main'
                        : 'warning.main',
                      borderRadius: 1,
                      mb: 0.5,
                    }}
                  />
                  <Typography variant="caption">{bin.bin_range}</Typography>
                </Box>
              ))}
            </Box>
          </CardContent>
        </Card>
      )}

      {!loading && predictions.length === 0 && (
        <Alert severity="warning" sx={{ mb: 2 }}>
          {t('pred.no_predictions')}
        </Alert>
      )}

      <TableContainer component={Paper}>
        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell>{t('pred.table_statement')}</TableCell>
              <TableCell>{t('pred.table_confidence')}</TableCell>
              <TableCell>{t('pred.table_target_date')}</TableCell>
              <TableCell>{t('pred.table_status')}</TableCell>
              <TableCell>{t('pred.table_action')}</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {predictions.length === 0 && (
              <TableRow>
                <TableCell colSpan={5}>
                  <Typography variant="body2" color="text.secondary">
                    {t('pred.no_data')}
                  </Typography>
                </TableCell>
              </TableRow>
            )}
            {predictions.map((pred) => (
              <TableRow key={pred.id}>
                <TableCell>
                  <Typography variant="body2" sx={{ maxWidth: 600 }}>
                    {pred.statement}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                    <LinearProgress
                      variant="determinate"
                      value={pred.confidence * 100}
                      sx={{ width: 50, height: 8 }}
                    />
                    <Typography variant="caption" sx={{ whiteSpace: 'nowrap' }}>
                      {(pred.confidence * 100).toFixed(0)}%
                    </Typography>
                  </Box>
                </TableCell>
                <TableCell>
                  <Typography variant="caption">{pred.target_date}</Typography>
                </TableCell>
                <TableCell>
                  <Tooltip
                    arrow
                    title={
                      {
                        pending: t('pred.status_pending_tip'),
                        partial: t('pred.status_partial_tip'),
                        correct: t('pred.status_correct_tip'),
                      }[pred.status] || t('pred.status_incorrect_tip')
                    }
                  >
                    <Chip
                      label={
                        {
                          pending: t('pred.status_pending'),
                          partial: t('pred.status_partial'),
                          correct: t('pred.status_correct'),
                        }[pred.status] || t('pred.status_incorrect')
                      }
                      size="small"
                      sx={{ bgcolor: statusColors[pred.status] || '#666', color: 'white', cursor: 'help' }}
                    />
                  </Tooltip>
                </TableCell>
                <TableCell>
                  {pred.status === 'pending' && (
                    <Button
                      size="small"
                      variant="outlined"
                      onClick={() => setReviewDialog(pred.id)}
                    >
                      {t('pred.review_btn')}
                    </Button>
                  )}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>

      <Dialog open={!!reviewDialog} onClose={() => setReviewDialog(null)} maxWidth="sm" fullWidth>
        <DialogTitle>{t('pred.review_dialog_title')}</DialogTitle>
        <DialogContent>
          <TextField
            fullWidth
            label={t('pred.review_actual')}
            value={actualOutcome}
            onChange={(e) => setActualOutcome(e.target.value)}
            sx={{ mt: 1, mb: 2 }}
          />
          <FormControl fullWidth sx={{ mb: 2 }}>
            <InputLabel>{t('pred.review_judgment')}</InputLabel>
            <Select
              value={reviewOutcome}
              label={t('pred.review_judgment')}
              onChange={(e) => setReviewOutcome(e.target.value)}
            >
              <MenuItem value="correct">{t('pred.review_correct')}</MenuItem>
              <MenuItem value="partial">{t('pred.review_partial')}</MenuItem>
              <MenuItem value="incorrect">{t('pred.review_incorrect')}</MenuItem>
              <MenuItem value="unknown">{t('pred.review_unknown')}</MenuItem>
            </Select>
          </FormControl>
          <TextField
            fullWidth
            label={t('pred.review_notes')}
            value={reviewNotes}
            onChange={(e) => setReviewNotes(e.target.value)}
            multiline
            rows={2}
          />
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setReviewDialog(null)}>{t('pred.review_cancel')}</Button>
          <Button onClick={handleReview} variant="contained" disabled={!actualOutcome}>
            {t('pred.review_save')}
          </Button>
        </DialogActions>
      </Dialog>
    </Box>
  );
}

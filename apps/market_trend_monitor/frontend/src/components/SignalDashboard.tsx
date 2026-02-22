/**
 * Signal Dashboard コンポーネント.
 *
 * Phase 13: 5軸レーダーチャート + グレード分布
 */
import { useState, useEffect, useCallback } from 'react';
import {
  Box,
  Card,
  CardContent,
  Typography,
  Grid,
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
  Tooltip,
} from '@mui/material';
import { apiClient } from '../api/client';
import { useI18n } from '../i18n';

interface SignalScore {
  reliability: number;
  leading: number;
  relevance: number;
  actionability: number;
  convergence: number;
  total: number;
  grade: string;
}

interface SignalItem {
  id: string;
  trend_id: string;
  score: SignalScore;
  grade: string;
  evaluated_at: string;
  metadata: Record<string, unknown>;
}

interface SignalsResponse {
  signals: SignalItem[];
  total: number;
}

interface SignalDashboardResponse {
  grade_distribution?: Record<string, number>;
  [key: string]: unknown;
}

const gradeColors: Record<string, string> = {
  A: '#4caf50',
  B: '#2196f3',
  C: '#ff9800',
  D: '#9e9e9e',
};

function RadarChart({ score }: { score: SignalScore }) {
  const axes = ['reliability', 'leading', 'relevance', 'actionability', 'convergence'] as const;
  const size = 160;
  const center = size / 2;
  const radius = size * 0.38;

  const points = axes.map((axis, i) => {
    const angle = (Math.PI * 2 * i) / axes.length - Math.PI / 2;
    const value = score[axis];
    return {
      x: center + Math.cos(angle) * radius * value,
      y: center + Math.sin(angle) * radius * value,
      labelX: center + Math.cos(angle) * (radius + 18),
      labelY: center + Math.sin(angle) * (radius + 18),
      axis,
      value,
    };
  });

  const pathD = points.map((p, i) => `${i === 0 ? 'M' : 'L'} ${p.x} ${p.y}`).join(' ') + ' Z';

  return (
    <svg width={size} height={size} viewBox={`-6 0 ${size} ${size}`}>
      {[0.25, 0.5, 0.75, 1].map((scale) => (
        <polygon
          key={scale}
          points={axes
            .map((_, i) => {
              const angle = (Math.PI * 2 * i) / axes.length - Math.PI / 2;
              return `${center + Math.cos(angle) * radius * scale},${center + Math.sin(angle) * radius * scale}`;
            })
            .join(' ')}
          fill="none"
          stroke="rgba(255,255,255,0.1)"
          strokeWidth="1"
        />
      ))}
      <path d={pathD} fill={`${gradeColors[score.grade]}33`} stroke={gradeColors[score.grade]} strokeWidth="2" />
      {points.map((p) => (
        <g key={p.axis}>
          <circle cx={p.x} cy={p.y} r="3" fill={gradeColors[score.grade]} />
          <text
            x={p.labelX}
            y={p.labelY}
            textAnchor="middle"
            dominantBaseline="middle"
            fontSize="8"
            fill="rgba(255,255,255,0.7)"
          >
            {p.axis.slice(0, 3).toUpperCase()}
          </text>
        </g>
      ))}
    </svg>
  );
}

export default function SignalDashboard() {
  const { t } = useI18n();
  const [signals, setSignals] = useState<SignalItem[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [stats, setStats] = useState<Record<string, unknown>>({});

  const axisLabels: Record<string, string> = {
    reliability: 'Reliability',
    leading: 'Leading',
    relevance: 'Relevance',
    actionability: 'Actionability',
    convergence: 'Convergence',
  };

  const fetchSignals = useCallback(async () => {
    setLoading(true);
    try {
      const [signalResp, dashResp] = await Promise.allSettled([
        apiClient.get<SignalsResponse>('/signals'),
        apiClient.get<SignalDashboardResponse>('/signals/dashboard'),
      ]);
      if (signalResp.status === 'fulfilled') {
        setSignals(signalResp.value.data.signals || []);
        setError(null);
      } else {
        setSignals([]);
        setError(t('signal.fetch_error'));
      }
      if (dashResp.status === 'fulfilled') {
        setStats(dashResp.value.data || {});
      } else {
        setStats({});
      }
    } catch {
      setSignals([]);
      setStats({});
      setError(t('signal.fetch_error'));
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchSignals();
  }, [fetchSignals]);

  const gradeDist = (stats as Record<string, Record<string, number>>).grade_distribution || {};

  return (
    <Box>
      <Typography variant="h4" gutterBottom>
        {t('signal.title')}
      </Typography>
      <Typography variant="body2" color="text.secondary" sx={{ mb: 3 }}>
        {t('signal.subtitle')}
      </Typography>

      {loading && <LinearProgress sx={{ mb: 2 }} />}
      {error && <Alert severity="error" sx={{ mb: 2 }}>{error}</Alert>}

      <Grid container spacing={2} sx={{ mb: 3 }}>
        {['A', 'B', 'C', 'D'].map((grade) => (
          <Grid item xs={3} key={grade}>
            <Tooltip
              arrow
              title={
                grade === 'A' ? t('signal.grade_a_tip') :
                  grade === 'B' ? t('signal.grade_b_tip') :
                    grade === 'C' ? t('signal.grade_c_tip') :
                      t('signal.grade_d_tip')
              }
            >
              <Card sx={{ cursor: 'help' }}>
                <CardContent sx={{ textAlign: 'center', py: 1 }}>
                  <Chip
                    label={`${t('signal.grade_prefix')} ${grade}`}
                    sx={{ bgcolor: gradeColors[grade], color: 'white', fontWeight: 'bold', mb: 1 }}
                  />
                  <Typography variant="h4">{(gradeDist[grade] as number) || 0}</Typography>
                </CardContent>
              </Card>
            </Tooltip>
          </Grid>
        ))}
      </Grid>

      <TableContainer component={Paper}>
        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell>{t('signal.table_trend')}</TableCell>
              <TableCell>{t('signal.table_grade')}</TableCell>
              <TableCell>{t('signal.table_score')}</TableCell>
              <TableCell>{t('signal.table_radar')}</TableCell>
              <TableCell>{t('signal.table_detail')}</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {signals.map((signal) => (
              <TableRow key={signal.id}>
                <TableCell>
                  <Typography variant="body2">
                    {(signal.metadata?.trend_topic as string) || signal.trend_id}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Tooltip
                    arrow
                    title={
                      signal.grade === 'A' ? t('signal.grade_a_row_tip') :
                        signal.grade === 'B' ? t('signal.grade_b_row_tip') :
                          signal.grade === 'C' ? t('signal.grade_c_row_tip') :
                            t('signal.grade_d_row_tip')
                    }
                  >
                    <Chip
                      label={signal.grade}
                      size="small"
                      sx={{ bgcolor: gradeColors[signal.grade], color: 'white', fontWeight: 'bold', cursor: 'help' }}
                    />
                  </Tooltip>
                </TableCell>
                <TableCell>
                  <Typography variant="body2" fontWeight="bold">
                    {signal.score.total.toFixed(2)}
                  </Typography>
                </TableCell>
                <TableCell>
                  <RadarChart score={signal.score} />
                </TableCell>
                <TableCell>
                  <Box>
                    {Object.entries(axisLabels).map(([key, label]) => {
                      const tips: Record<string, string> = {
                        reliability: t('signal.axis_reliability_tip'),
                        leading: t('signal.axis_leading_tip'),
                        relevance: t('signal.axis_relevance_tip'),
                        actionability: t('signal.axis_actionability_tip'),
                        convergence: t('signal.axis_convergence_tip'),
                      };
                      return (
                        <Box key={key} sx={{ display: 'flex', alignItems: 'center', gap: 0.5 }}>
                          <Tooltip title={tips[key]} placement="left" arrow>
                            <Typography variant="caption" sx={{ width: 90, cursor: 'help' }}>{label}</Typography>
                          </Tooltip>
                          <LinearProgress
                            variant="determinate"
                            value={signal.score[key as keyof SignalScore] as number * 100}
                            sx={{ width: 60, height: 6 }}
                          />
                          <Typography variant="caption">
                            {(signal.score[key as keyof SignalScore] as number).toFixed(2)}
                          </Typography>
                        </Box>
                      );
                    })}
                  </Box>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
    </Box>
  );
}

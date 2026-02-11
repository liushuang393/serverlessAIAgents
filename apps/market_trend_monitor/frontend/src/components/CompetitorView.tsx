/**
 * Competitor View コンポーネント.
 *
 * Phase 13: 競合マップ + 脅威/機会マトリクス
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
  Button,
  TextField,
  Stack,
} from '@mui/material';
import { apiClient } from '../api/client';

interface CompetitorProfile {
  name: string;
  focus_areas: string[];
  recent_activities: string[];
  market_position: string;
  threat_level: number;
  opportunity_level: number;
  last_updated: string;
  metadata: Record<string, unknown>;
}

interface CompetitorListResponse {
  competitors: CompetitorProfile[];
  total: number;
  detected_count?: number;
  watchlist_count?: number;
  detected_competitors?: string[];
  undetected_watchlist?: string[];
  auto_discovered?: boolean;
  source_articles?: number;
  scope?: string;
}

interface CompetitorConfigResponse {
  competitors: string[];
  watchlist_count: number;
}

interface CompetitorDiscoverResponse {
  status: string;
  message: string;
  competitors: CompetitorProfile[];
  total: number;
  detected_count: number;
  watchlist_count?: number;
  detected_competitors?: string[];
  undetected_watchlist?: string[];
  source_articles?: number;
  focused_collected_articles?: number;
  scope?: string;
}

const positionColors: Record<string, string> = {
  leader: '#f44336',
  challenger: '#ff9800',
  follower: '#2196f3',
  niche: '#9e9e9e',
};

const positionLabels: Record<string, string> = {
  leader: 'Leader',
  challenger: 'Challenger',
  follower: 'Follower',
  niche: 'Niche',
};

function ThreatOpportunityChart({ competitors }: { competitors: CompetitorProfile[] }) {
  const size = 280;
  const padding = 40;
  const plotSize = size - padding * 2;

  return (
    <svg width={size} height={size} viewBox={`0 0 ${size} ${size}`}>
      {/* Grid lines */}
      {[0, 0.25, 0.5, 0.75, 1].map((v) => (
        <g key={v}>
          <line
            x1={padding}
            y1={padding + plotSize * (1 - v)}
            x2={padding + plotSize}
            y2={padding + plotSize * (1 - v)}
            stroke="rgba(255,255,255,0.08)"
            strokeWidth="1"
          />
          <line
            x1={padding + plotSize * v}
            y1={padding}
            x2={padding + plotSize * v}
            y2={padding + plotSize}
            stroke="rgba(255,255,255,0.08)"
            strokeWidth="1"
          />
        </g>
      ))}
      {/* Axis labels */}
      <text x={size / 2} y={size - 4} textAnchor="middle" fontSize="10" fill="rgba(255,255,255,0.6)">
        Threat Level →
      </text>
      <text
        x={8}
        y={size / 2}
        textAnchor="middle"
        fontSize="10"
        fill="rgba(255,255,255,0.6)"
        transform={`rotate(-90, 8, ${size / 2})`}
      >
        Opportunity →
      </text>
      {/* Data points */}
      {competitors.map((c) => {
        const cx = padding + c.threat_level * plotSize;
        const cy = padding + (1 - c.opportunity_level) * plotSize;
        const color = positionColors[c.market_position] || '#666';
        return (
          <g key={c.name}>
            <circle cx={cx} cy={cy} r="6" fill={color} opacity="0.8" />
            <text
              x={cx}
              y={cy - 10}
              textAnchor="middle"
              fontSize="8"
              fill="rgba(255,255,255,0.8)"
            >
              {c.name.length > 10 ? c.name.slice(0, 10) + '...' : c.name}
            </text>
          </g>
        );
      })}
      {/* Quadrant labels */}
      <text x={padding + 8} y={padding + 14} fontSize="8" fill="rgba(255,255,255,0.3)">
        Low Risk / High Opp
      </text>
      <text x={padding + plotSize - 8} y={padding + 14} textAnchor="end" fontSize="8" fill="rgba(255,255,255,0.3)">
        High Risk / High Opp
      </text>
      <text x={padding + 8} y={padding + plotSize - 4} fontSize="8" fill="rgba(255,255,255,0.3)">
        Low Risk / Low Opp
      </text>
      <text x={padding + plotSize - 8} y={padding + plotSize - 4} textAnchor="end" fontSize="8" fill="rgba(255,255,255,0.3)">
        High Risk / Low Opp
      </text>
    </svg>
  );
}

export default function CompetitorView() {
  const [competitors, setCompetitors] = useState<CompetitorProfile[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [watchlist, setWatchlist] = useState<string[]>([]);
  const [watchlistInput, setWatchlistInput] = useState('');
  const [notice, setNotice] = useState<string | null>(null);
  const [detectedCount, setDetectedCount] = useState(0);
  const [watchlistCount, setWatchlistCount] = useState(0);
  const [sourceArticles, setSourceArticles] = useState(0);
  const [undetectedWatchlist, setUndetectedWatchlist] = useState<string[]>([]);

  const persistWatchlist = async (nextWatchlist: string[]) => {
    try {
      const resp = await apiClient.put<CompetitorConfigResponse>('/competitors/config', {
        competitors: nextWatchlist,
      });
      const persisted = resp.data.competitors || [];
      setWatchlist(persisted);
      setWatchlistCount(resp.data.watchlist_count || persisted.length);
      setNotice('競合ウォッチリストを保存しました');
      setError(null);
    } catch {
      setError('競合ウォッチリストの保存に失敗しました');
    }
  };

  const fetchCompetitors = useCallback(async () => {
    setLoading(true);
    try {
      const [competitorResp, configResp] = await Promise.allSettled([
        apiClient.get<CompetitorListResponse>('/competitors'),
        apiClient.get<CompetitorConfigResponse>('/competitors/config'),
      ]);

      if (competitorResp.status === 'fulfilled') {
        const payload = competitorResp.value.data;
        setCompetitors(payload.competitors || []);
        setDetectedCount(payload.detected_count || 0);
        setWatchlistCount(payload.watchlist_count || 0);
        setSourceArticles(payload.source_articles || 0);
        setUndetectedWatchlist(payload.undetected_watchlist || []);
      } else {
        setCompetitors([]);
        setDetectedCount(0);
        setWatchlistCount(0);
        setSourceArticles(0);
        setUndetectedWatchlist([]);
      }

      if (configResp.status === 'fulfilled') {
        const items = configResp.value.data.competitors || [];
        setWatchlist(items);
        setWatchlistCount(items.length);
      } else {
        setWatchlist([]);
      }

      setError(null);
    } catch {
      setError('競合データの取得に失敗しました');
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchCompetitors();
  }, [fetchCompetitors]);

  const detectedCompetitors = competitors.filter(
    (c) => Number(c.metadata?.article_count || 0) > 0,
  );

  const positionDist = detectedCompetitors.reduce<Record<string, number>>((acc, c) => {
    acc[c.market_position] = (acc[c.market_position] || 0) + 1;
    return acc;
  }, {});

  const highThreats = detectedCompetitors.filter((c) => c.threat_level >= 0.7);
  const highOpportunities = detectedCompetitors.filter((c) => c.opportunity_level >= 0.6);

  const addWatchlistCompetitor = async () => {
    const normalized = watchlistInput.trim();
    if (!normalized) {
      return;
    }
    if (watchlist.some((item) => item.toLowerCase() === normalized.toLowerCase())) {
      setWatchlistInput('');
      return;
    }
    const nextWatchlist = [...watchlist, normalized];
    setWatchlist(nextWatchlist);
    setWatchlistCount(nextWatchlist.length);
    setWatchlistInput('');
    await persistWatchlist(nextWatchlist);
  };

  const removeWatchlistCompetitor = async (name: string) => {
    const nextWatchlist = watchlist.filter((item) => item !== name);
    setWatchlist(nextWatchlist);
    setWatchlistCount(nextWatchlist.length);
    await persistWatchlist(nextWatchlist);
  };

  const saveWatchlist = async () => {
    await persistWatchlist(watchlist);
  };

  const autoDiscover = async () => {
    try {
      setLoading(true);
      const resp = await apiClient.post<CompetitorDiscoverResponse>(
        '/competitors/discover',
        {
          include_unmatched: false,
          limit: 400,
          refresh_with_watchlist: true,
          max_focus_keywords: 24,
        },
      );
      const payload = resp.data;
      setCompetitors(payload.competitors || []);
      setDetectedCount(payload.detected_count || 0);
      setWatchlistCount(payload.watchlist_count || watchlist.length);
      setSourceArticles(payload.source_articles || 0);
      setUndetectedWatchlist(payload.undetected_watchlist || []);
      setNotice(payload.message || 'Auto discovery completed');
      setError(null);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'unknown error';
      setError(`最新記事からの競合自動発見に失敗しました: ${message}`);
    } finally {
      setLoading(false);
    }
  };

  return (
    <Box>
      <Typography variant="h5" gutterBottom>
        Competitor Landscape
      </Typography>
      <Typography variant="body2" color="text.secondary" sx={{ mb: 2 }}>
        Market positioning, threat assessment, and opportunity analysis
      </Typography>

      {loading && <LinearProgress sx={{ mb: 2 }} />}
      {error && <Alert severity="error" sx={{ mb: 2 }}>{error}</Alert>}
      {notice && (
        <Alert severity="info" sx={{ mb: 2 }} onClose={() => setNotice(null)}>
          {notice}
        </Alert>
      )}

      <Card sx={{ mb: 2 }}>
        <CardContent>
          <Typography variant="subtitle1" gutterBottom>
            発見コントロール
          </Typography>
          <Stack
            direction={{ xs: 'column', md: 'row' }}
            spacing={1}
            sx={{ mb: 1, flexWrap: 'wrap' }}
          >
            <Button
              variant="contained"
              onClick={autoDiscover}
              disabled={loading}
              sx={{ whiteSpace: 'nowrap' }}
            >
              最新記事から自動発見
            </Button>
            <Button
              variant="outlined"
              onClick={fetchCompetitors}
              disabled={loading}
              sx={{ whiteSpace: 'nowrap' }}
            >
              更新
            </Button>
            <Button
              variant="outlined"
              onClick={saveWatchlist}
              disabled={loading}
              sx={{ whiteSpace: 'nowrap' }}
            >
              ウォッチリスト保存
            </Button>
          </Stack>
          <Box sx={{ display: 'flex', gap: 1, mb: 1 }}>
            <TextField
              size="small"
              fullWidth
              label="競合企業を追加"
              value={watchlistInput}
              onChange={(e) => setWatchlistInput(e.target.value)}
              onKeyDown={(e) => {
                if (e.key === 'Enter') {
                  void addWatchlistCompetitor();
                }
              }}
            />
            <Button
              variant="outlined"
              onClick={() => void addWatchlistCompetitor()}
              sx={{ whiteSpace: 'nowrap', flexShrink: 0, minWidth: 80 }}
            >
              追加
            </Button>
          </Box>
          <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
            {watchlist.map((name) => (
              <Chip
                key={name}
                label={name}
                onDelete={() => void removeWatchlistCompetitor(name)}
                sx={{
                  maxWidth: '100%',
                  height: 'auto',
                  alignItems: 'flex-start',
                  '& .MuiChip-label': {
                    display: 'block',
                    whiteSpace: 'normal',
                    lineHeight: 1.3,
                    py: 0.5,
                  },
                }}
              />
            ))}
          </Box>
          <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mt: 1 }}>
            Detected {detectedCount}/{watchlistCount || watchlist.length} competitor(s) in {sourceArticles} collected articles.
          </Typography>
          {undetectedWatchlist.length > 0 && (
            <Typography variant="caption" color="warning.main" sx={{ display: 'block', mt: 0.5 }}>
              Not detected yet: {undetectedWatchlist.slice(0, 8).join(', ')}
              {undetectedWatchlist.length > 8 ? ' ...' : ''}
            </Typography>
          )}
        </CardContent>
      </Card>

      {!loading && detectedCompetitors.length === 0 && (
        <Alert severity="warning" sx={{ mb: 2 }}>
          No competitor mentions detected yet. Run data collection first, then click
          {' '}Auto Discover.
        </Alert>
      )}

      <Grid container spacing={2} sx={{ mb: 3 }}>
        <Grid item xs={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center', py: 1 }}>
              <Typography variant="subtitle2" color="text.secondary">
                Tracked
              </Typography>
              <Typography variant="h3">{detectedCompetitors.length}</Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center', py: 1 }}>
              <Typography variant="subtitle2" color="text.secondary">
                High Threat
              </Typography>
              <Typography variant="h3" color="error.main">
                {highThreats.length}
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center', py: 1 }}>
              <Typography variant="subtitle2" color="text.secondary">
                Opportunities
              </Typography>
              <Typography variant="h3" color="success.main">
                {highOpportunities.length}
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center', py: 1 }}>
              <Typography variant="subtitle2" color="text.secondary">
                Leaders
              </Typography>
              <Typography variant="h3" color="warning.main">
                {positionDist.leader || 0}
              </Typography>
            </CardContent>
          </Card>
        </Grid>
      </Grid>

      <Grid container spacing={2} sx={{ mb: 3 }}>
        <Grid item xs={5}>
          <Card>
            <CardContent>
              <Typography variant="subtitle1" gutterBottom>
                Threat / Opportunity Matrix
              </Typography>
              <Box sx={{ display: 'flex', justifyContent: 'center' }}>
                <ThreatOpportunityChart competitors={detectedCompetitors} />
              </Box>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={7}>
          <Card>
            <CardContent>
              <Typography variant="subtitle1" gutterBottom>
                Position Distribution
              </Typography>
              {['leader', 'challenger', 'follower', 'niche'].map((pos) => (
                <Box key={pos} sx={{ display: 'flex', alignItems: 'center', gap: 1, mb: 1 }}>
                  <Chip
                    label={positionLabels[pos]}
                    size="small"
                    sx={{
                      bgcolor: positionColors[pos],
                      color: 'white',
                      fontWeight: 'bold',
                      width: 100,
                    }}
                  />
                  <LinearProgress
                    variant="determinate"
                    value={
                      detectedCompetitors.length > 0
                        ? ((positionDist[pos] || 0) / detectedCompetitors.length) * 100
                        : 0
                    }
                    sx={{ flex: 1, height: 8 }}
                  />
                  <Typography variant="body2" sx={{ width: 24, textAlign: 'right' }}>
                    {positionDist[pos] || 0}
                  </Typography>
                </Box>
              ))}
            </CardContent>
          </Card>
        </Grid>
      </Grid>

      <TableContainer component={Paper}>
        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell>Competitor</TableCell>
              <TableCell>Position</TableCell>
              <TableCell>Threat</TableCell>
              <TableCell>Opportunity</TableCell>
              <TableCell>Focus Areas</TableCell>
              <TableCell>Activities</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {competitors.length === 0 && (
              <TableRow>
                <TableCell colSpan={6}>
                  <Typography variant="body2" color="text.secondary">
                    No competitor profile yet
                  </Typography>
                </TableCell>
              </TableRow>
            )}
            {competitors.map((c) => (
              <TableRow key={c.name}>
                <TableCell>
                  <Typography variant="body2" fontWeight="bold">
                    {c.name}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Chip
                    label={positionLabels[c.market_position] || c.market_position}
                    size="small"
                    sx={{
                      bgcolor: positionColors[c.market_position] || '#666',
                      color: 'white',
                      fontWeight: 'bold',
                    }}
                  />
                </TableCell>
                <TableCell>
                  <Box sx={{ display: 'flex', alignItems: 'center', gap: 0.5 }}>
                    <LinearProgress
                      variant="determinate"
                      value={c.threat_level * 100}
                      color={c.threat_level >= 0.7 ? 'error' : 'primary'}
                      sx={{ width: 50, height: 6 }}
                    />
                    <Typography variant="caption">
                      {(c.threat_level * 100).toFixed(0)}%
                    </Typography>
                  </Box>
                </TableCell>
                <TableCell>
                  <Box sx={{ display: 'flex', alignItems: 'center', gap: 0.5 }}>
                    <LinearProgress
                      variant="determinate"
                      value={c.opportunity_level * 100}
                      color="success"
                      sx={{ width: 50, height: 6 }}
                    />
                    <Typography variant="caption">
                      {(c.opportunity_level * 100).toFixed(0)}%
                    </Typography>
                  </Box>
                </TableCell>
                <TableCell>
                  <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                    {c.focus_areas.slice(0, 3).map((area, i) => (
                      <Chip key={i} label={area} size="small" variant="outlined" />
                    ))}
                  </Box>
                </TableCell>
                <TableCell>
                  <Typography variant="caption" color="text.secondary" sx={{ maxWidth: 200 }}>
                    {Number(c.metadata?.article_count || 0) <= 0
                      ? 'No mention in recent data'
                      : c.recent_activities.length > 0
                        ? c.recent_activities[0].slice(0, 60) + '...'
                        : 'No recent activity'}
                  </Typography>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
    </Box>
  );
}

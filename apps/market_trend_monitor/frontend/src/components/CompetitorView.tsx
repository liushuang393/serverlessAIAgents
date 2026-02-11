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

  const fetchCompetitors = useCallback(async () => {
    setLoading(true);
    try {
      const resp = await apiClient.get('/competitors');
      setCompetitors(resp.data.competitors || []);
      setError(null);
    } catch (e) {
      setError('Failed to load competitor data');
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchCompetitors();
  }, [fetchCompetitors]);

  const positionDist = competitors.reduce<Record<string, number>>((acc, c) => {
    acc[c.market_position] = (acc[c.market_position] || 0) + 1;
    return acc;
  }, {});

  const highThreats = competitors.filter((c) => c.threat_level >= 0.7);
  const highOpportunities = competitors.filter((c) => c.opportunity_level >= 0.6);

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

      <Grid container spacing={2} sx={{ mb: 3 }}>
        <Grid item xs={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center', py: 1 }}>
              <Typography variant="subtitle2" color="text.secondary">
                Tracked
              </Typography>
              <Typography variant="h3">{competitors.length}</Typography>
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
                <ThreatOpportunityChart competitors={competitors} />
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
                      competitors.length > 0
                        ? ((positionDist[pos] || 0) / competitors.length) * 100
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
                    {c.recent_activities.length > 0
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

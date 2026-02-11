/**
 * Evidence Viewer コンポーネント.
 *
 * Phase 13: 証拠一覧 + 検索 + チェーン表示
 */
import { Fragment, useState, useEffect, useCallback } from 'react';
import {
  Box,
  Card,
  CardContent,
  Typography,
  TextField,
  Chip,
  LinearProgress,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  IconButton,
  Collapse,
  InputAdornment,
  Alert,
} from '@mui/material';
import SearchIcon from '@mui/icons-material/Search';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ExpandLessIcon from '@mui/icons-material/ExpandLess';
import LinkIcon from '@mui/icons-material/Link';
import { apiClient } from '../api/client';

interface EvidenceItem {
  id: string;
  source_type: string;
  url: string;
  title: string;
  collected_at: string;
  reliability_score: number;
  extracted_data: Record<string, unknown>;
}

const sourceColors: Record<string, string> = {
  arxiv: '#e91e63',
  github: '#4caf50',
  news: '#2196f3',
  rss: '#ff9800',
  stackoverflow: '#f48024',
  devto: '#0a0a0a',
};

export default function EvidenceViewer() {
  const [evidences, setEvidences] = useState<EvidenceItem[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [search, setSearch] = useState('');
  const [expandedId, setExpandedId] = useState<string | null>(null);

  const fetchEvidences = useCallback(async () => {
    setLoading(true);
    try {
      const resp = await apiClient.get<{ evidences: EvidenceItem[]; total: number }>('/evidence');
      setEvidences(resp.data.evidences || []);
      setError(null);
    } catch (e) {
      setError('Failed to load evidence data');
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchEvidences();
  }, [fetchEvidences]);

  const filtered = evidences.filter((e) => {
    if (!search) return true;
    const q = search.toLowerCase();
    return (
      e.title.toLowerCase().includes(q) ||
      e.source_type.toLowerCase().includes(q) ||
      e.url.toLowerCase().includes(q)
    );
  });

  const getReliabilityColor = (score: number) => {
    if (score >= 0.8) return '#4caf50';
    if (score >= 0.6) return '#ff9800';
    return '#f44336';
  };

  return (
    <Box>
      <Typography variant="h5" gutterBottom>
        Evidence Ledger
      </Typography>
      <Typography variant="body2" color="text.secondary" sx={{ mb: 2 }}>
        Collected evidence from all sources with reliability scoring
      </Typography>

      <TextField
        fullWidth
        size="small"
        placeholder="Search by title, source, or URL..."
        value={search}
        onChange={(e) => setSearch(e.target.value)}
        sx={{ mb: 2 }}
        InputProps={{
          startAdornment: (
            <InputAdornment position="start">
              <SearchIcon />
            </InputAdornment>
          ),
        }}
      />

      {loading && <LinearProgress sx={{ mb: 2 }} />}
      {error && (
        <Alert severity="error" sx={{ mb: 2 }}>
          {error}
        </Alert>
      )}

      <TableContainer component={Paper}>
        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell />
              <TableCell>Title</TableCell>
              <TableCell>Source</TableCell>
              <TableCell>Reliability</TableCell>
              <TableCell>Collected</TableCell>
              <TableCell>Link</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {filtered.map((evidence) => (
              <Fragment key={evidence.id}>
                <TableRow
                  hover
                  onClick={() =>
                    setExpandedId(expandedId === evidence.id ? null : evidence.id)
                  }
                  sx={{ cursor: 'pointer' }}
                >
                  <TableCell>
                    <IconButton size="small">
                      {expandedId === evidence.id ? (
                        <ExpandLessIcon />
                      ) : (
                        <ExpandMoreIcon />
                      )}
                    </IconButton>
                  </TableCell>
                  <TableCell>
                    <Typography variant="body2" noWrap sx={{ maxWidth: 300 }}>
                      {evidence.title}
                    </Typography>
                  </TableCell>
                  <TableCell>
                    <Chip
                      label={evidence.source_type}
                      size="small"
                      sx={{
                        bgcolor: sourceColors[evidence.source_type] || '#666',
                        color: 'white',
                      }}
                    />
                  </TableCell>
                  <TableCell>
                    <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                      <LinearProgress
                        variant="determinate"
                        value={evidence.reliability_score * 100}
                        sx={{
                          width: 60,
                          '& .MuiLinearProgress-bar': {
                            bgcolor: getReliabilityColor(evidence.reliability_score),
                          },
                        }}
                      />
                      <Typography variant="caption">
                        {(evidence.reliability_score * 100).toFixed(0)}%
                      </Typography>
                    </Box>
                  </TableCell>
                  <TableCell>
                    <Typography variant="caption">
                      {new Date(evidence.collected_at).toLocaleDateString()}
                    </Typography>
                  </TableCell>
                  <TableCell>
                    <IconButton
                      size="small"
                      href={evidence.url}
                      target="_blank"
                      rel="noopener noreferrer"
                      onClick={(e) => e.stopPropagation()}
                    >
                      <LinkIcon fontSize="small" />
                    </IconButton>
                  </TableCell>
                </TableRow>
                <TableRow key={`${evidence.id}-detail`}>
                  <TableCell colSpan={6} sx={{ py: 0 }}>
                    <Collapse in={expandedId === evidence.id}>
                      <Card variant="outlined" sx={{ m: 1 }}>
                        <CardContent>
                          <Typography variant="subtitle2">Keywords</Typography>
                          <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5, mb: 1 }}>
                            {((evidence.extracted_data?.keywords as string[]) || []).map(
                              (kw: string, i: number) => (
                                <Chip key={i} label={kw} size="small" variant="outlined" />
                              ),
                            )}
                          </Box>
                          <Typography variant="subtitle2">Content Preview</Typography>
                          <Typography variant="body2" color="text.secondary">
                            {String(evidence.extracted_data?.content || '').slice(0, 300)}...
                          </Typography>
                        </CardContent>
                      </Card>
                    </Collapse>
                  </TableCell>
                </TableRow>
              </Fragment>
            ))}
          </TableBody>
        </Table>
      </TableContainer>

      <Typography variant="caption" color="text.secondary" sx={{ mt: 1, display: 'block' }}>
        Showing {filtered.length} of {evidences.length} evidence items
      </Typography>
    </Box>
  );
}

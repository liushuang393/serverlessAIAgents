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
  Divider,
  Tooltip,
} from '@mui/material';
import SearchIcon from '@mui/icons-material/Search';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ExpandLessIcon from '@mui/icons-material/ExpandLess';
import LinkIcon from '@mui/icons-material/Link';
import { apiClient } from '../api/client';
import { useI18n } from '../i18n';

interface EvidenceItem {
  id: string;
  source_type: string;
  url: string;
  title: string;
  collected_at: string;
  reliability_score: number;
  extracted_data: Record<string, unknown>;
}

interface GuardBlocker {
  code: string;
  message: string;
  current: number;
  required: number;
}

interface GuardSummary {
  total_evidence: number;
  claims_count: number;
  avg_reliability: number;
  source_diversity: number;
  citation_ready_count: number;
  citation_ready_ratio: number;
  fresh_count: number;
  fresh_ratio: number;
}

interface EvidenceDiagnostic {
  evidence_id: string;
  traceability_score: number;
  citation_ready: boolean;
  freshness_days: number;
  is_fresh: boolean;
  content_length: number;
  quote_preview: string;
}

interface ClaimDiagnostic {
  claim_id: string;
  status: string;
  coverage_score: number;
}

interface GroundingGuardPayload {
  status: string;
  summary: GuardSummary;
  blockers: GuardBlocker[];
  actions: string[];
  evidence_diagnostics: EvidenceDiagnostic[];
  claim_diagnostics: ClaimDiagnostic[];
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
  const { t } = useI18n();
  const [evidences, setEvidences] = useState<EvidenceItem[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [search, setSearch] = useState('');
  const [expandedId, setExpandedId] = useState<string | null>(null);
  const [guard, setGuard] = useState<GroundingGuardPayload | null>(null);

  const fetchEvidences = useCallback(async () => {
    setLoading(true);
    try {
      const [evidenceResp, guardResp] = await Promise.allSettled([
        apiClient.get<{ evidences: EvidenceItem[]; total: number }>('/evidence'),
        apiClient.get<GroundingGuardPayload>('/evidence/grounding'),
      ]);

      if (evidenceResp.status === 'fulfilled') {
        setEvidences(evidenceResp.value.data.evidences || []);
        setError(null);
      } else {
        setEvidences([]);
        setError(t('evidence.fetch_error'));
      }

      if (guardResp.status === 'fulfilled') {
        setGuard(guardResp.value.data);
      } else {
        setGuard(null);
      }
    } catch {
      setEvidences([]);
      setGuard(null);
      setError(t('evidence.fetch_error'));
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

  const diagnosticsById = (guard?.evidence_diagnostics || []).reduce<Record<string, EvidenceDiagnostic>>(
    (acc, item) => {
      acc[item.evidence_id] = item;
      return acc;
    },
    {},
  );

  const supportedClaims = (guard?.claim_diagnostics || []).filter(
    (claim) => claim.status === 'supported',
  ).length;

  return (
    <Box>
      <Typography variant="h4" gutterBottom>
        {t('evidence.title')}
      </Typography>
      <Typography variant="body2" color="text.secondary" sx={{ mb: 3 }}>
        {t('evidence.subtitle')}
      </Typography>

      {guard && (
        <Card sx={{ mb: 2, borderColor: 'rgba(255,255,255,0.14)' }}>
          <CardContent>
            <Box
              sx={{
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'space-between',
                flexWrap: 'wrap',
                gap: 1,
                mb: 1,
              }}
            >
              <Typography variant="subtitle1">{t('evidence.guard_title')}</Typography>
              <Chip
                size="small"
                color={guard.status === 'ready' ? 'success' : 'warning'}
                label={guard.status === 'ready' ? t('evidence.guard_ready') : t('evidence.guard_needs_more')}
              />
            </Box>
            <Box sx={{ display: 'flex', gap: 1, flexWrap: 'wrap', mb: 1 }}>
              <Tooltip title={t('evidence.avg_reliability_tooltip')} arrow>
                <Chip
                  size="small"
                  variant="outlined"
                  label={`${t('evidence.avg_reliability')} ${(guard.summary.avg_reliability * 100).toFixed(0)}%`}
                  sx={{ cursor: 'help' }}
                />
              </Tooltip>
              <Tooltip title={t('evidence.citation_ready_tooltip')} arrow>
                <Chip
                  size="small"
                  variant="outlined"
                  label={`${t('evidence.citation_ready')} ${(guard.summary.citation_ready_ratio * 100).toFixed(0)}%`}
                  sx={{ cursor: 'help' }}
                />
              </Tooltip>
              <Tooltip title={t('evidence.freshness_tooltip')} arrow>
                <Chip
                  size="small"
                  variant="outlined"
                  label={`${t('evidence.freshness')} ${(guard.summary.fresh_ratio * 100).toFixed(0)}%`}
                  sx={{ cursor: 'help' }}
                />
              </Tooltip>
              <Tooltip title={t('evidence.source_diversity_tooltip')} arrow>
                <Chip
                  size="small"
                  variant="outlined"
                  label={`${t('evidence.source_diversity')} ${guard.summary.source_diversity}`}
                  sx={{ cursor: 'help' }}
                />
              </Tooltip>
              <Tooltip title={t('evidence.proven_claims_tooltip')} arrow>
                <Chip
                  size="small"
                  variant="outlined"
                  label={`${t('evidence.proven_claims')} ${supportedClaims}/${guard.summary.claims_count}`}
                  sx={{ cursor: 'help' }}
                />
              </Tooltip>
            </Box>
            {guard.blockers.length > 0 && (
              <Alert severity="warning" sx={{ mb: 1 }}>
                {guard.blockers.map((blocker) => (
                  <Typography key={blocker.code} variant="body2">
                    {blocker.message} ({blocker.current} / {blocker.required})
                  </Typography>
                ))}
              </Alert>
            )}
            <Divider sx={{ my: 1, borderColor: 'rgba(255,255,255,0.08)' }} />
            <Typography variant="caption" color="text.secondary">
              {guard.actions.join(' / ') || 'No additional action required.'}
            </Typography>
          </CardContent>
        </Card>
      )}

      <TextField
        fullWidth
        size="small"
        placeholder={t('evidence.search_placeholder')}
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
              <TableCell>{t('evidence.table_title')}</TableCell>
              <TableCell>{t('evidence.table_source')}</TableCell>
              <TableCell>{t('evidence.table_reliability')}</TableCell>
              <TableCell>{t('evidence.table_collected')}</TableCell>
              <TableCell>{t('evidence.table_link')}</TableCell>
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
                    <Typography variant="body2" sx={{ maxWidth: 500 }}>
                      {evidence.title}
                    </Typography>
                    {diagnosticsById[evidence.id] && (
                      <Box sx={{ mt: 0.5, display: 'flex', gap: 0.5, flexWrap: 'wrap' }}>
                        <Chip
                          size="small"
                          label={
                            diagnosticsById[evidence.id].citation_ready
                              ? t('evidence.citation_ready_badge')
                              : t('evidence.tracing_gap')
                          }
                          color={diagnosticsById[evidence.id].citation_ready ? 'success' : 'warning'}
                        />
                        <Chip
                          size="small"
                          variant="outlined"
                          label={`${t('evidence.tracing_rate')} ${(diagnosticsById[evidence.id].traceability_score * 100).toFixed(0)}%`}
                        />
                      </Box>
                    )}
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
                      <Typography variant="caption" sx={{ whiteSpace: 'nowrap' }}>
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
                          <Typography variant="subtitle2">{t('evidence.keywords')}</Typography>
                          <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5, mb: 1 }}>
                            {((evidence.extracted_data?.keywords as string[]) || []).map(
                              (kw: string, i: number) => (
                                <Chip key={i} label={kw} size="small" variant="outlined" />
                              ),
                            )}
                          </Box>
                          <Typography variant="subtitle2">{t('evidence.content_preview')}</Typography>
                          <Typography variant="body2" color="text.secondary">
                            {String(evidence.extracted_data?.content || '').slice(0, 300)}...
                          </Typography>
                          {diagnosticsById[evidence.id]?.quote_preview && (
                            <>
                              <Typography variant="subtitle2" sx={{ mt: 1 }}>
                                {t('evidence.quote_preview')}
                              </Typography>
                              <Typography variant="body2" color="text.secondary">
                                {diagnosticsById[evidence.id].quote_preview}
                              </Typography>
                            </>
                          )}
                          {diagnosticsById[evidence.id] && (
                            <Box sx={{ display: 'flex', gap: 0.5, mt: 1, flexWrap: 'wrap' }}>
                              <Chip
                                size="small"
                                variant="outlined"
                                label={`${t('evidence.freshness_days')} ${diagnosticsById[evidence.id].freshness_days}日`}
                              />
                              <Chip
                                size="small"
                                color={diagnosticsById[evidence.id].is_fresh ? 'success' : 'default'}
                                label={
                                  diagnosticsById[evidence.id].is_fresh
                                    ? t('evidence.fresh')
                                    : t('evidence.needs_refresh')
                                }
                              />
                              <Chip
                                size="small"
                                variant="outlined"
                                label={`${t('evidence.char_count')} ${diagnosticsById[evidence.id].content_length}`}
                              />
                            </Box>
                          )}
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
        {t('evidence.showing_count').replaceAll('{total}', String(evidences.length)).replaceAll('{filtered}', String(filtered.length))}
      </Typography>
    </Box>
  );
}

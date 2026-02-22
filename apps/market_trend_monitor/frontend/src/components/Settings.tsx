/**
 * 設定画面コンポーネント.
 * 
 * 目的: キーワードとデータソースの設定
 * I/O:
 *   - Input: ユーザー入力
 *   - Output: 設定UI
 */

import React, { useEffect, useState } from 'react';
import {
  Box,
  Paper,
  Typography,
  TextField,
  Button,
  Chip,
  Stack,
  FormGroup,
  FormControlLabel,
  Checkbox,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  Alert,
  Snackbar,
  IconButton,
  Collapse,
} from '@mui/material';
import { ExpandMore, ChevronRight } from '@mui/icons-material';
import { apiClient } from '@/api/client';
import { useAppStore } from '@/store/useAppStore';
import { SourceType } from '@/types';
import { useI18n } from '../i18n';

type CompetitorConfigResponse = {
  competitors: string[];
  competitor_aliases?: Record<string, string[]>;
};

function toAliasInputMap(
  competitors: string[],
  competitorAliases: Record<string, string[]>,
): Record<string, string> {
  const output: Record<string, string> = {};
  competitors.forEach((name) => {
    const aliases = competitorAliases[name] || [];
    const normalized = aliases.filter(
      (alias) => alias.trim() && alias.trim().toLowerCase() !== name.toLowerCase(),
    );
    output[name] = normalized.join(', ');
  });
  return output;
}

function parseAliasInput(raw: string): string[] {
  const deduped: string[] = [];
  const seen = new Set<string>();
  raw
    .split(',')
    .map((item) => item.trim())
    .filter(Boolean)
    .forEach((alias) => {
      const key = alias.toLowerCase();
      if (!seen.has(key)) {
        seen.add(key);
        deduped.push(alias);
      }
    });
  return deduped;
}

const Settings: React.FC = () => {
  const { t } = useI18n();
  const {
    keywords,
    sources,
    collectionWindowDays,
    updateKeywords,
    updateSources,
    updateCollectionWindowDays,
    collectData,
    loading,
  } = useAppStore();

  const [keywordInput, setKeywordInput] = useState('');
  const [localKeywords, setLocalKeywords] = useState<string[]>(keywords);
  const [localSources, setLocalSources] = useState<SourceType[]>(sources);
  const [localCollectionWindowDays, setLocalCollectionWindowDays] = useState<number>(collectionWindowDays);
  const [showSuccess, setShowSuccess] = useState(false);
  const [noticeMessage, setNoticeMessage] = useState('');
  const [noticeSeverity, setNoticeSeverity] = useState<'success' | 'warning' | 'error'>('success');
  const [localCompetitors, setLocalCompetitors] = useState<string[]>([]);
  const [competitorInput, setCompetitorInput] = useState('');
  const [localCompetitorAliases, setLocalCompetitorAliases] = useState<Record<string, string>>({});
  const [aliasEditorOpen, setAliasEditorOpen] = useState(false);

  useEffect(() => {
    const fetchCompetitorConfig = async () => {
      try {
        const response = await apiClient.get<CompetitorConfigResponse>('/competitors/config');
        const competitors = response.data.competitors || [];
        const aliases = response.data.competitor_aliases || {};
        setLocalCompetitors(competitors);
        setLocalCompetitorAliases(toAliasInputMap(competitors, aliases));
      } catch {
        setLocalCompetitors([]);
        setLocalCompetitorAliases({});
      }
    };

    fetchCompetitorConfig();
  }, []);

  const handleAddKeyword = () => {
    if (keywordInput.trim() && !localKeywords.includes(keywordInput.trim())) {
      setLocalKeywords([...localKeywords, keywordInput.trim()]);
      setKeywordInput('');
    }
  };

  const handleDeleteKeyword = (keyword: string) => {
    setLocalKeywords(localKeywords.filter((k) => k !== keyword));
  };

  const handleSourceChange = (source: SourceType, checked: boolean) => {
    if (checked) {
      setLocalSources([...localSources, source]);
    } else {
      setLocalSources(localSources.filter((s) => s !== source));
    }
  };

  const handleSave = () => {
    updateKeywords(localKeywords);
    updateSources(localSources);
    updateCollectionWindowDays(localCollectionWindowDays as 7 | 30 | 90);
    setNoticeSeverity('success');
    setNoticeMessage(t('stg.saved_msg'));
    setShowSuccess(true);
  };

  const persistCompetitorConfig = async (
    nextCompetitors: string[],
    nextAliasInputMap: Record<string, string>,
  ) => {
    const competitorAliases = nextCompetitors.reduce<Record<string, string[]>>((acc, name) => {
      const aliases = parseAliasInput(nextAliasInputMap[name] || '');
      if (aliases.length > 0) {
        acc[name] = aliases;
      }
      return acc;
    }, {});

    const response = await apiClient.put<CompetitorConfigResponse>(
      '/competitors/config',
      {
        competitors: nextCompetitors,
        competitor_aliases: competitorAliases,
      },
    );
    const competitors = response.data.competitors || [];
    const aliases = response.data.competitor_aliases || {};
    setLocalCompetitors(competitors);
    setLocalCompetitorAliases(toAliasInputMap(competitors, aliases));
  };

  const handleAddCompetitor = async () => {
    const normalized = competitorInput.trim();
    if (!normalized) {
      return;
    }
    if (localCompetitors.some((item) => item.toLowerCase() === normalized.toLowerCase())) {
      setCompetitorInput('');
      return;
    }
    const nextCompetitors = [...localCompetitors, normalized];
    const nextAliasInputMap = { ...localCompetitorAliases, [normalized]: '' };
    setLocalCompetitors(nextCompetitors);
    setLocalCompetitorAliases(nextAliasInputMap);
    setCompetitorInput('');
    try {
      await persistCompetitorConfig(nextCompetitors, nextAliasInputMap);
      setNoticeSeverity('success');
      setNoticeMessage(t('stg.watchlist_saved'));
    } catch {
      setNoticeSeverity('error');
      setNoticeMessage(t('stg.watchlist_save_error'));
    }
    setShowSuccess(true);
  };

  const handleDeleteCompetitor = async (name: string) => {
    const nextCompetitors = localCompetitors.filter((item) => item !== name);
    const nextAliasInputMap = { ...localCompetitorAliases };
    delete nextAliasInputMap[name];
    setLocalCompetitors(nextCompetitors);
    setLocalCompetitorAliases(nextAliasInputMap);
    try {
      await persistCompetitorConfig(nextCompetitors, nextAliasInputMap);
      setNoticeSeverity('success');
      setNoticeMessage(t('stg.watchlist_saved'));
    } catch {
      setNoticeSeverity('error');
      setNoticeMessage(t('stg.watchlist_save_error'));
    }
    setShowSuccess(true);
  };

  const handleAliasInputChange = (name: string, value: string) => {
    setLocalCompetitorAliases((prev) => ({
      ...prev,
      [name]: value,
    }));
  };

  const handleSaveCompetitors = async () => {
    try {
      await persistCompetitorConfig(localCompetitors, localCompetitorAliases);
      setNoticeSeverity('success');
      setNoticeMessage(t('stg.watchlist_saved'));
      setShowSuccess(true);
    } catch {
      setNoticeSeverity('error');
      setNoticeMessage(t('stg.watchlist_save_error'));
      setShowSuccess(true);
    }
  };

  const handleCollect = async () => {
    const response = await collectData(
      localKeywords,
      localSources,
      localCollectionWindowDays as 7 | 30 | 90,
    );
    if (!response) {
      setNoticeSeverity('error');
      setNoticeMessage(t('stg.collect_error'));
      setShowSuccess(true);
      return;
    }

    if (response.status === 'empty') {
      setNoticeSeverity('warning');
      setNoticeMessage(t('stg.collect_empty'));
      setShowSuccess(true);
      return;
    }

    setNoticeSeverity('success');
    setNoticeMessage(
      t('stg.collect_success')
        .replaceAll('{days}', String(localCollectionWindowDays))
        .replaceAll('{articles}', String(response.articles_count))
        .replaceAll('{trends}', String(response.trends_count))
    );
    setShowSuccess(true);
  };

  return (
    <Box p={3}>
      <Paper
        sx={{
          p: { xs: 3, md: 4 },
          mb: 3,
          background:
            'linear-gradient(135deg, rgba(99,102,241,0.2), rgba(168,85,247,0.12))',
        }}
      >
        <Typography variant="h4" gutterBottom>
          {t('stg.title')}
        </Typography>
        <Typography variant="body2" color="text.secondary" sx={{ mb: 3 }}>
          {t('stg.subtitle')}
        </Typography>
      </Paper>

      <Stack spacing={3}>
        {/* キーワード設定 */}
        <Paper
          sx={{
            p: 3,
            backgroundImage:
              'linear-gradient(135deg, rgba(99,102,241,0.08), rgba(18,18,26,0.95))',
            border: '1px solid rgba(99,102,241,0.2)',
          }}
        >
          <Typography variant="h6" gutterBottom>
            {t('stg.keywords_title')}
          </Typography>
          <Box display="flex" gap={2} mb={2}>
            <TextField
              fullWidth
              label={t('stg.add_keyword_label')}
              value={keywordInput}
              onChange={(e) => setKeywordInput(e.target.value)}
              onKeyPress={(e) => {
                if (e.key === 'Enter') {
                  handleAddKeyword();
                }
              }}
              sx={{
                '& .MuiInputBase-root': {
                  backgroundColor: '#0f1117',
                },
              }}
            />
            <Button
              variant="contained"
              onClick={handleAddKeyword}
              sx={{ whiteSpace: 'nowrap', flexShrink: 0, minWidth: '80px' }}
            >
              {t('stg.add_btn')}
            </Button>
          </Box>
          <Stack direction="row" spacing={1} flexWrap="wrap" gap={1}>
            {localKeywords.map((keyword) => (
              <Chip
                key={keyword}
                label={keyword}
                onDelete={() => handleDeleteKeyword(keyword)}
                sx={{ backgroundColor: 'rgba(99, 102, 241, 0.18)', color: '#e2e8f0' }}
              />
            ))}
          </Stack>
        </Paper>

        {/* データソース設定 */}
        <Paper
          sx={{
            p: 3,
            backgroundImage:
              'linear-gradient(135deg, rgba(168,85,247,0.08), rgba(18,18,26,0.95))',
            border: '1px solid rgba(168,85,247,0.2)',
          }}
        >
          <Typography variant="h6" gutterBottom>
            {t('stg.sources_title')}
          </Typography>
          <FormGroup>
            {Object.values(SourceType).map((source) => (
              <FormControlLabel
                key={source}
                control={
                  <Checkbox
                    checked={localSources.includes(source)}
                    onChange={(e) => handleSourceChange(source, e.target.checked)}
                  />
                }
                label={source.toUpperCase()}
              />
            ))}
          </FormGroup>
          <Box mt={2}>
            <FormControl sx={{ minWidth: 220 }}>
              <InputLabel id="collection-window-label">{t('stg.collection_period')}</InputLabel>
              <Select
                labelId="collection-window-label"
                value={localCollectionWindowDays}
                label={t('stg.collection_period')}
                onChange={(e) => setLocalCollectionWindowDays(Number(e.target.value))}
              >
                <MenuItem value={7}>{t('stg.period_7d')}</MenuItem>
                <MenuItem value={30}>{t('stg.period_30d')}</MenuItem>
                <MenuItem value={90}>{t('stg.period_90d')}</MenuItem>
              </Select>
            </FormControl>
          </Box>
        </Paper>

        {/* 競合ウォッチリスト設定 */}
        <Paper
          sx={{
            p: 3,
            backgroundImage:
              'linear-gradient(135deg, rgba(59,130,246,0.08), rgba(18,18,26,0.95))',
            border: '1px solid rgba(59,130,246,0.2)',
          }}
        >
          <Typography variant="h6" gutterBottom>
            {t('stg.competitor_title')}
          </Typography>
          <Box display="flex" gap={2} mb={2}>
            <TextField
              fullWidth
              label={t('stg.add_competitor_label')}
              value={competitorInput}
              onChange={(e) => setCompetitorInput(e.target.value)}
              onKeyDown={(e) => {
                if (e.key === 'Enter') {
                  void handleAddCompetitor();
                }
              }}
              sx={{
                '& .MuiInputBase-root': {
                  backgroundColor: '#0f1117',
                },
              }}
            />
            <Button
              variant="outlined"
              onClick={() => void handleAddCompetitor()}
              sx={{ whiteSpace: 'nowrap', flexShrink: 0, minWidth: '80px' }}
            >
              {t('stg.add_btn')}
            </Button>
            <Button
              variant="contained"
              onClick={handleSaveCompetitors}
              sx={{ whiteSpace: 'nowrap', flexShrink: 0, minWidth: '100px' }}
            >
              {t('stg.save_btn')}
            </Button>
          </Box>
          <Box display="flex" flexWrap="wrap" gap={1}>
            {localCompetitors.map((name) => (
              <Chip
                key={name}
                label={name}
                onDelete={() => void handleDeleteCompetitor(name)}
                sx={{
                  backgroundColor: 'rgba(59, 130, 246, 0.18)',
                  color: '#dbeafe',
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
          {localCompetitors.length > 0 ? (
            <Box mt={3}>
              <Box display="flex" alignItems="center" justifyContent="space-between">
                <Typography variant="subtitle2" color="text.secondary">
                  {t('stg.alias_title')}
                </Typography>
                <IconButton
                  size="small"
                  onClick={() => setAliasEditorOpen((prev) => !prev)}
                  aria-label={t('stg.alias_toggle_label')}
                >
                  {aliasEditorOpen ? <ExpandMore fontSize="small" /> : <ChevronRight fontSize="small" />}
                </IconButton>
              </Box>
              <Collapse in={aliasEditorOpen} timeout="auto" unmountOnExit>
                <Box mt={1} display="flex" flexDirection="column" gap={2}>
                  {localCompetitors.map((name) => (
                    <TextField
                      key={`${name}-aliases`}
                      fullWidth
                      label={`${name} ${t('stg.alias_suffix')}`}
                      placeholder={t('stg.alias_placeholder')}
                      value={localCompetitorAliases[name] || ''}
                      onChange={(e) => handleAliasInputChange(name, e.target.value)}
                      sx={{
                        '& .MuiInputBase-root': {
                          backgroundColor: '#0f1117',
                        },
                      }}
                    />
                  ))}
                </Box>
              </Collapse>
            </Box>
          ) : null}
        </Paper>

        {/* アクションボタン */}
        <Box display="flex" gap={2} flexWrap="wrap">
          <Button
            variant="contained"
            onClick={handleSave}
            sx={{ whiteSpace: 'nowrap', flexShrink: 0 }}
          >
            {t('stg.save_settings')}
          </Button>
          <Button
            variant="outlined"
            onClick={handleCollect}
            disabled={loading || localKeywords.length === 0}
            sx={{ whiteSpace: 'nowrap', flexShrink: 0 }}
          >
            {loading ? t('stg.collecting') : t('stg.collect_btn')}
          </Button>
        </Box>
      </Stack>

      <Snackbar
        open={showSuccess}
        autoHideDuration={3000}
        onClose={() => setShowSuccess(false)}
      >
        <Alert severity={noticeSeverity}>{noticeMessage}</Alert>
      </Snackbar>
    </Box>
  );
};

export default Settings;

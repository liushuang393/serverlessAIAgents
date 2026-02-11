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
  const [noticeMessage, setNoticeMessage] = useState('設定を保存しました');
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
    setNoticeMessage('設定を保存しました');
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
      setNoticeMessage('競合ウォッチリストを保存しました');
    } catch {
      setNoticeSeverity('error');
      setNoticeMessage('競合ウォッチリスト保存に失敗しました');
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
      setNoticeMessage('競合ウォッチリストを保存しました');
    } catch {
      setNoticeSeverity('error');
      setNoticeMessage('競合ウォッチリスト保存に失敗しました');
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
      setNoticeMessage('競合ウォッチリストを保存しました');
      setShowSuccess(true);
    } catch {
      setNoticeSeverity('error');
      setNoticeMessage('競合ウォッチリスト保存に失敗しました');
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
      setNoticeMessage('データ収集に失敗しました。APIログを確認してください');
      setShowSuccess(true);
      return;
    }

    if (response.status === 'empty') {
      setNoticeSeverity('warning');
      setNoticeMessage('データ収集は完了しましたが、該当データがありませんでした');
      setShowSuccess(true);
      return;
    }

    setNoticeSeverity('success');
    setNoticeMessage(
      `データ収集完了（直近${localCollectionWindowDays}日）: 記事 ${response.articles_count} 件 / トレンド ${response.trends_count} 件`
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
          設定
        </Typography>
        <Typography variant="body2" color="text.secondary">
          監視対象と収集頻度を調整して、重要シグナルの感度を最適化します。
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
            監視キーワード
          </Typography>
          <Box display="flex" gap={2} mb={2}>
            <TextField
              fullWidth
              label="キーワードを追加"
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
              追加
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
            データソース
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
              <InputLabel id="collection-window-label">収集期間</InputLabel>
              <Select
                labelId="collection-window-label"
                value={localCollectionWindowDays}
                label="収集期間"
                onChange={(e) => setLocalCollectionWindowDays(Number(e.target.value))}
              >
                <MenuItem value={7}>直近7日</MenuItem>
                <MenuItem value={30}>直近30日</MenuItem>
                <MenuItem value={90}>直近90日</MenuItem>
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
            競合ウォッチリスト
          </Typography>
          <Box display="flex" gap={2} mb={2}>
            <TextField
              fullWidth
              label="競合企業を追加"
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
              追加
            </Button>
            <Button
              variant="contained"
              onClick={handleSaveCompetitors}
              sx={{ whiteSpace: 'nowrap', flexShrink: 0, minWidth: '100px' }}
            >
              保存
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
                  別名辞書（カンマ区切り）
                </Typography>
                <IconButton
                  size="small"
                  onClick={() => setAliasEditorOpen((prev) => !prev)}
                  aria-label="別名辞書の折りたたみ切り替え"
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
                      label={`${name} の別名`}
                      placeholder="例: International Business Machines, IBM Corp"
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
            設定を保存
          </Button>
          <Button
            variant="outlined"
            onClick={handleCollect}
            disabled={loading || localKeywords.length === 0}
            sx={{ whiteSpace: 'nowrap', flexShrink: 0 }}
          >
            {loading ? 'データ収集中...' : 'データ収集を実行'}
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

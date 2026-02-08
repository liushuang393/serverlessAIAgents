/**
 * 設定画面コンポーネント.
 * 
 * 目的: キーワードとデータソースの設定
 * I/O:
 *   - Input: ユーザー入力
 *   - Output: 設定UI
 */

import React, { useState } from 'react';
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
  Alert,
  Snackbar,
} from '@mui/material';
import { useAppStore } from '@/store/useAppStore';
import { SourceType } from '@/types';

const Settings: React.FC = () => {
  const {
    keywords,
    sources,
    updateKeywords,
    updateSources,
    collectData,
    loading,
  } = useAppStore();

  const [keywordInput, setKeywordInput] = useState('');
  const [localKeywords, setLocalKeywords] = useState<string[]>(keywords);
  const [localSources, setLocalSources] = useState<SourceType[]>(sources);
  const [showSuccess, setShowSuccess] = useState(false);

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
    setShowSuccess(true);
  };

  const handleCollect = async () => {
    await collectData(localKeywords, localSources);
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
            <Button variant="contained" onClick={handleAddKeyword}>
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
        </Paper>

        {/* アクションボタン */}
        <Box display="flex" gap={2} flexWrap="wrap">
          <Button variant="contained" onClick={handleSave}>
            設定を保存
          </Button>
          <Button
            variant="outlined"
            onClick={handleCollect}
            disabled={loading || localKeywords.length === 0}
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
        <Alert severity="success">設定を保存しました</Alert>
      </Snackbar>
    </Box>
  );
};

export default Settings;

/**
 * トレンドリストコンポーネント.
 * 
 * 目的: トレンド一覧を表形式で表示
 * I/O:
 *   - Input: trends (Trend[])
 *   - Output: テーブルUI
 */

import React from 'react';
import {
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Chip,
  Box,
  Typography,
  Stack,
  Tooltip,
} from '@mui/material';
import type { Trend, SentimentType } from '@/types';
import { format } from 'date-fns';
import { useI18n } from '../i18n';

interface TrendListProps {
  trends: Trend[];
}

const getSentimentColor = (
  sentiment: SentimentType
): 'success' | 'default' | 'error' => {
  switch (sentiment) {
    case 'positive':
      return 'success';
    case 'neutral':
      return 'default';
    case 'negative':
      return 'error';
    default:
      return 'default';
  }
};

const formatGrowthLabel = (trend: Trend): string => {
  const growthState =
    typeof trend.metadata?.growth_state === 'string'
      ? trend.metadata.growth_state
      : '';

  if (growthState === 'new') {
    return 'NEW';
  }
  if (growthState === 'insufficient_history' || growthState === 'no_signal') {
    return 'N/A';
  }
  return `${(trend.growth_rate * 100).toFixed(1)}%`;
};

const TrendList: React.FC<TrendListProps> = ({ trends }) => {
  const { t } = useI18n();

  const resolveGrowthHint = (trend: Trend): string => {
    const explanation =
      typeof trend.metadata?.growth_explanation === 'string'
        ? trend.metadata.growth_explanation
        : '';
    return explanation || t('trend_list.growth_hint_default');
  };

  if (trends.length === 0) {
    return (
      <Box sx={{ p: 4, textAlign: 'center', color: 'text.secondary' }}>
        <Typography variant="body1">{t('trend_list.no_data')}</Typography>
        <Typography variant="body2">{t('trend_list.no_data_hint')}</Typography>
      </Box>
    );
  }

  return (
    <TableContainer
      sx={{
        borderRadius: 2,
        border: '1px solid rgba(255,255,255,0.06)',
        backgroundColor: '#0f1117',
      }}
    >
      <Table
        sx={{
          '& .MuiTableCell-head': {
            color: '#cbd5f5',
            borderBottom: '1px solid rgba(255,255,255,0.08)',
          },
          '& .MuiTableCell-body': {
            color: '#e2e8f0',
            borderBottom: '1px solid rgba(255,255,255,0.04)',
          },
        }}
      >
        <TableHead>
          <TableRow>
            <TableCell sx={{ fontWeight: 600 }}>{t('trend_list.topic')}</TableCell>
            <TableCell align="right" sx={{ fontWeight: 600 }}>
              {t('trend_list.score')}
            </TableCell>
            <TableCell align="right" sx={{ fontWeight: 600 }}>
              {t('trend_list.articles')}
            </TableCell>
            <TableCell sx={{ fontWeight: 600 }}>{t('trend_list.sentiment')}</TableCell>
            <TableCell align="right" sx={{ fontWeight: 600 }}>
              {t('trend_list.growth_rate')}
            </TableCell>
            <TableCell sx={{ fontWeight: 600 }}>{t('trend_list.created_at')}</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {trends.map((trend) => (
            <TableRow
              key={trend.id}
              hover
              sx={{
                '&:nth-of-type(odd)': {
                  backgroundColor: 'rgba(255, 255, 255, 0.02)',
                },
                '&:hover': {
                  backgroundColor: 'rgba(99, 102, 241, 0.08)',
                },
              }}
            >
              <TableCell>
                <Stack direction="row" spacing={1} alignItems="center">
                  {trend.topic}
                  {!!trend.metadata?.is_breakthrough && (
                    <Chip
                      label="BREAKTHROUGH"
                      size="small"
                      color="secondary"
                      sx={{
                        fontSize: '0.65rem',
                        height: '18px',
                        background: 'linear-gradient(90deg, #f43f5e, #fb7185)',
                        fontWeight: 700,
                      }}
                    />
                  )}
                </Stack>
              </TableCell>
              <TableCell align="right">{trend.score.toFixed(2)}</TableCell>
              <TableCell align="right">{trend.articles_count}</TableCell>
              <TableCell>
                <Chip
                  label={trend.sentiment}
                  color={getSentimentColor(trend.sentiment)}
                  size="small"
                  variant="outlined"
                  sx={{ borderColor: 'rgba(255,255,255,0.2)' }}
                />
              </TableCell>
              <TableCell align="right">
                <Tooltip title={resolveGrowthHint(trend)}>
                  <span>{formatGrowthLabel(trend)}</span>
                </Tooltip>
              </TableCell>
              <TableCell>
                {format(new Date(trend.created_at), 'yyyy/MM/dd HH:mm')}
              </TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </TableContainer>
  );
};

export default TrendList;

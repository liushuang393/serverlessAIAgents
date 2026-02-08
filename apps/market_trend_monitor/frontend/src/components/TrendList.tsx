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
} from '@mui/material';
import type { Trend, SentimentType } from '@/types';
import { format } from 'date-fns';

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

const TrendList: React.FC<TrendListProps> = ({ trends }) => {
  if (trends.length === 0) {
    return <div>データがありません</div>;
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
            <TableCell sx={{ fontWeight: 600 }}>トピック</TableCell>
            <TableCell align="right" sx={{ fontWeight: 600 }}>
              スコア
            </TableCell>
            <TableCell align="right" sx={{ fontWeight: 600 }}>
              記事数
            </TableCell>
            <TableCell sx={{ fontWeight: 600 }}>センチメント</TableCell>
            <TableCell align="right" sx={{ fontWeight: 600 }}>
              成長率
            </TableCell>
            <TableCell sx={{ fontWeight: 600 }}>作成日時</TableCell>
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
              <TableCell>{trend.topic}</TableCell>
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
                {(trend.growth_rate * 100).toFixed(1)}%
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

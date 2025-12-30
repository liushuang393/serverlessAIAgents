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
    <TableContainer>
      <Table>
        <TableHead>
          <TableRow>
            <TableCell>トピック</TableCell>
            <TableCell align="right">スコア</TableCell>
            <TableCell align="right">記事数</TableCell>
            <TableCell>センチメント</TableCell>
            <TableCell align="right">成長率</TableCell>
            <TableCell>作成日時</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {trends.map((trend) => (
            <TableRow key={trend.id} hover>
              <TableCell>{trend.topic}</TableCell>
              <TableCell align="right">{trend.score.toFixed(2)}</TableCell>
              <TableCell align="right">{trend.articles_count}</TableCell>
              <TableCell>
                <Chip
                  label={trend.sentiment}
                  color={getSentimentColor(trend.sentiment)}
                  size="small"
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


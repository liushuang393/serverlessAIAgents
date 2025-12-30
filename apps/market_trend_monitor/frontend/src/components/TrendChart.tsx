/**
 * トレンドチャートコンポーネント.
 * 
 * 目的: トレンドスコアの推移を可視化
 * I/O:
 *   - Input: trends (Trend[])
 *   - Output: チャートUI
 */

import React from 'react';
import {
  LineChart,
  Line,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer,
} from 'recharts';
import type { Trend } from '@/types';
import { format } from 'date-fns';

interface TrendChartProps {
  trends: Trend[];
}

const TrendChart: React.FC<TrendChartProps> = ({ trends }) => {
  // トレンドデータをチャート用に変換
  const chartData = trends
    .slice(0, 20)
    .map((trend) => ({
      name: trend.topic.substring(0, 15) + '...',
      score: trend.score,
      growth: trend.growth_rate * 100,
      date: format(new Date(trend.created_at), 'MM/dd'),
    }))
    .reverse();

  if (chartData.length === 0) {
    return (
      <div
        style={{
          display: 'flex',
          justifyContent: 'center',
          alignItems: 'center',
          height: '100%',
        }}
      >
        データがありません
      </div>
    );
  }

  return (
    <ResponsiveContainer width="100%" height="90%">
      <LineChart data={chartData}>
        <CartesianGrid strokeDasharray="3 3" />
        <XAxis dataKey="date" />
        <YAxis />
        <Tooltip />
        <Legend />
        <Line
          type="monotone"
          dataKey="score"
          stroke="#8884d8"
          name="スコア"
          strokeWidth={2}
        />
        <Line
          type="monotone"
          dataKey="growth"
          stroke="#82ca9d"
          name="成長率(%)"
          strokeWidth={2}
        />
      </LineChart>
    </ResponsiveContainer>
  );
};

export default TrendChart;


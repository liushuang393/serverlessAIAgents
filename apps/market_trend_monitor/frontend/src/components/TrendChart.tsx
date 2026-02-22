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
import { useI18n } from '../i18n';

interface TrendChartProps {
  trends: Trend[];
}

const TrendChart: React.FC<TrendChartProps> = ({ trends }) => {
  const { t } = useI18n();
  // トレンドデータを日付ごとに集約
  const aggregatedData = trends.reduce((acc: Record<string, { score: number; growth: number; count: number }>, trend) => {
    const dateStr = format(new Date(trend.created_at), 'MM/dd');
    if (!acc[dateStr]) {
      acc[dateStr] = { score: 0, growth: 0, count: 0 };
    }
    acc[dateStr].score += trend.score;
    acc[dateStr].growth += trend.growth_rate * 100;
    acc[dateStr].count += 1;
    return acc;
  }, {});

  const chartData = Object.entries(aggregatedData)
    .map(([date, values]) => ({
      date,
      score: values.score / values.count,
      growth: values.growth / values.count,
    }))
    .sort((a, b) => a.date.localeCompare(b.date));

  if (chartData.length === 0) {
    return (
      <div
        style={{
          display: 'flex',
          justifyContent: 'center',
          alignItems: 'center',
          height: '100%',
          color: '#94a3b8',
          fontSize: '0.9rem',
        }}
      >
        {t('chart.no_data')}
      </div>
    );
  }

  return (
    <ResponsiveContainer width="100%" height="90%">
      <LineChart data={chartData}>
        <CartesianGrid strokeDasharray="4 4" stroke="rgba(148, 163, 184, 0.2)" />
        <XAxis dataKey="date" stroke="#94a3b8" />
        <YAxis stroke="#94a3b8" />
        <Tooltip
          contentStyle={{
            borderRadius: 12,
            border: '1px solid rgba(255, 255, 255, 0.12)',
            background: '#0f1117',
            boxShadow: '0 12px 24px rgba(0, 0, 0, 0.45)',
            color: '#f8fafc',
          }}
        />
        <Legend />
        <Line
          type="monotone"
          dataKey="score"
          stroke="#818cf8"
          name={t('chart.score_label')}
          strokeWidth={2}
          dot={{ r: 2 }}
        />
        <Line
          type="monotone"
          dataKey="growth"
          stroke="#a855f7"
          name={t('chart.growth_label')}
          strokeWidth={2}
          dot={{ r: 2 }}
        />
      </LineChart>
    </ResponsiveContainer>
  );
};

export default TrendChart;

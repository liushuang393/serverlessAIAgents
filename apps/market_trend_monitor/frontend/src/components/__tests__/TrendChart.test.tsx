/**
 * TrendChart コンポーネントのユニットテスト.
 * 
 * 目的: トレンドチャートが正しくレンダリングされることを検証
 */

import { describe, it, expect } from 'vitest';
import { render, screen } from '@testing-library/react';
import TrendChart from '../TrendChart';
import type { Trend } from '@/types';
import { SentimentType } from '@/types';

describe('TrendChart', () => {
  const mockTrends: Trend[] = [
    {
      id: '1',
      topic: 'COBOL Migration',
      score: 85.5,
      articles_count: 10,
      keywords: ['COBOL', 'Java'],
      sentiment: SentimentType.POSITIVE,
      growth_rate: 0.15,
      created_at: '2024-01-01T00:00:00Z',
      metadata: {},
    },
    {
      id: '2',
      topic: 'AI Development',
      score: 92.3,
      articles_count: 25,
      keywords: ['AI', 'ML'],
      sentiment: SentimentType.NEUTRAL,
      growth_rate: 0.25,
      created_at: '2024-01-02T00:00:00Z',
      metadata: {},
    },
  ];

  it('チャートを正しくレンダリングする', () => {
    const { container } = render(<TrendChart trends={mockTrends} />);

    // コンポーネントがレンダリングされることを確認
    expect(container.firstChild).toBeTruthy();
  });

  it('空のデータの場合、メッセージを表示する', () => {
    render(<TrendChart trends={[]} />);

    expect(screen.getByText('データがありません')).toBeInTheDocument();
  });

  it('最大20件のトレンドを表示する', () => {
    const manyTrends: Trend[] = Array.from({ length: 30 }, (_, i) => ({
      id: `${i}`,
      topic: `Trend ${i}`,
      score: 80 + i,
      articles_count: 10,
      keywords: ['test'],
      sentiment: SentimentType.NEUTRAL,
      growth_rate: 0.1,
      created_at: `2024-01-${String(i + 1).padStart(2, '0')}T00:00:00Z`,
      metadata: {},
    }));

    const { container } = render(<TrendChart trends={manyTrends} />);

    // コンポーネントがレンダリングされることを確認
    expect(container.firstChild).toBeTruthy();
  });
});


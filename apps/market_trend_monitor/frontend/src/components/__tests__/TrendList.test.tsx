/**
 * TrendList コンポーネントのユニットテスト.
 * 
 * 目的: トレンドリストが正しくレンダリングされることを検証
 */

import { describe, it, expect } from 'vitest';
import { render, screen } from '@testing-library/react';
import TrendList from '../TrendList';
import type { Trend } from '@/types';
import { SentimentType } from '@/types';

describe('TrendList', () => {
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

  it('トレンドリストを正しくレンダリングする', () => {
    render(<TrendList trends={mockTrends} />);

    expect(screen.getByText('COBOL Migration')).toBeInTheDocument();
    expect(screen.getByText('AI Development')).toBeInTheDocument();
  });

  it('スコアを正しく表示する', () => {
    render(<TrendList trends={mockTrends} />);

    expect(screen.getByText('85.50')).toBeInTheDocument();
    expect(screen.getByText('92.30')).toBeInTheDocument();
  });

  it('記事数を正しく表示する', () => {
    render(<TrendList trends={mockTrends} />);

    expect(screen.getByText('10')).toBeInTheDocument();
    expect(screen.getByText('25')).toBeInTheDocument();
  });

  it('成長率を正しく表示する', () => {
    render(<TrendList trends={mockTrends} />);

    expect(screen.getByText('15.0%')).toBeInTheDocument();
    expect(screen.getByText('25.0%')).toBeInTheDocument();
  });

  it('空のリストの場合、メッセージを表示する', () => {
    render(<TrendList trends={[]} />);

    expect(screen.getByText('データがありません')).toBeInTheDocument();
  });

  it('センチメントチップを表示する', () => {
    render(<TrendList trends={mockTrends} />);

    const positiveChips = screen.getAllByText('positive');
    const neutralChips = screen.getAllByText('neutral');

    expect(positiveChips.length).toBeGreaterThan(0);
    expect(neutralChips.length).toBeGreaterThan(0);
  });
});


/**
 * アプリケーションストアのユニットテスト.
 * 
 * 目的: Zustand ストアの状態管理が正しく動作することを検証
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { renderHook, act } from '@testing-library/react';
import { useAppStore } from '../useAppStore';
import { apiClient } from '@/api/client';
import type { SourceType } from '@/types';
import { SentimentType, NotificationPriority } from '@/types';

// API クライアントをモック
vi.mock('@/api/client', () => ({
  apiClient: {
    getTrends: vi.fn(),
    getReports: vi.fn(),
    collect: vi.fn(),
  },
}));

describe('useAppStore', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    // ストアをリセット
    useAppStore.setState({
      trends: [],
      reports: [],
      notifications: [],
      loading: false,
      error: null,
      keywords: ['COBOL', 'Java migration', 'AI'],
      sources: [],
    });
  });

  it('初期状態が正しい', () => {
    const { result } = renderHook(() => useAppStore());

    expect(result.current.trends).toEqual([]);
    expect(result.current.reports).toEqual([]);
    expect(result.current.loading).toBe(false);
    expect(result.current.error).toBe(null);
    expect(result.current.keywords).toEqual(['COBOL', 'Java migration', 'AI']);
  });

  it('キーワードを更新できる', () => {
    const { result } = renderHook(() => useAppStore());

    act(() => {
      result.current.updateKeywords(['New', 'Keywords']);
    });

    expect(result.current.keywords).toEqual(['New', 'Keywords']);
  });

  it('データソースを更新できる', () => {
    const { result } = renderHook(() => useAppStore());

    act(() => {
      result.current.updateSources(['news' as SourceType, 'github' as SourceType]);
    });

    expect(result.current.sources).toEqual(['news', 'github']);
  });

  it('通知を追加できる', () => {
    const { result } = renderHook(() => useAppStore());

    const notification = {
      id: '1',
      title: 'Test',
      message: 'Test message',
      priority: NotificationPriority.HIGH,
      trend_id: 'trend1',
      created_at: '2024-01-01T00:00:00Z',
      read: false,
      metadata: {},
    };

    act(() => {
      result.current.addNotification(notification);
    });

    expect(result.current.notifications).toHaveLength(1);
    expect(result.current.notifications[0].title).toBe('Test');
  });

  it('通知を既読にできる', () => {
    const { result } = renderHook(() => useAppStore());

    const notification = {
      id: '1',
      title: 'Test',
      message: 'Test message',
      priority: NotificationPriority.HIGH,
      trend_id: 'trend1',
      created_at: '2024-01-01T00:00:00Z',
      read: false,
      metadata: {},
    };

    act(() => {
      result.current.addNotification(notification);
      result.current.markNotificationAsRead('1');
    });

    expect(result.current.notifications[0].read).toBe(true);
  });

  it('fetchTrends が成功する', async () => {
    const mockTrends = [
      {
        id: '1',
        topic: 'Test Trend',
        score: 85.5,
        articles_count: 10,
        keywords: ['test'],
        sentiment: SentimentType.POSITIVE,
        growth_rate: 0.15,
        created_at: '2024-01-01T00:00:00Z',
        metadata: {},
      },
    ];

    vi.mocked(apiClient.getTrends).mockResolvedValue({
      trends: mockTrends,
      total: 1,
    });

    const { result } = renderHook(() => useAppStore());

    await act(async () => {
      await result.current.fetchTrends();
    });

    expect(result.current.trends).toEqual(mockTrends);
    expect(result.current.loading).toBe(false);
    expect(result.current.error).toBe(null);
  });
});


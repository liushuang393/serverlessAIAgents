/**
 * 統合テスト (Component Integration Test).
 * 
 * 目的: 複数のコンポーネントが連携して正しく動作することを検証
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { BrowserRouter } from 'react-router-dom';
import Dashboard from '../components/Dashboard';
import Settings from '../components/Settings';
import { useAppStore } from '../store/useAppStore';
import { apiClient } from '../api/client';

// API クライアントをモック
vi.mock('../api/client', () => ({
  apiClient: {
    getTrends: vi.fn(),
    getReports: vi.fn(),
    collect: vi.fn(),
  },
}));

describe('Integration Tests', () => {
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

  describe('Dashboard Integration', () => {
    it('ダッシュボードがマウントされる', () => {
      vi.mocked(apiClient.getTrends).mockResolvedValue({
        trends: [],
        total: 0,
      });

      const { container } = render(
        <BrowserRouter>
          <Dashboard />
        </BrowserRouter>
      );

      expect(container).toBeTruthy();
    });

    it('エラーが発生した場合、エラーメッセージを表示する', async () => {
      vi.mocked(apiClient.getTrends).mockRejectedValue(
        new Error('Network error')
      );

      render(
        <BrowserRouter>
          <Dashboard />
        </BrowserRouter>
      );

      await waitFor(() => {
        expect(screen.getByText(/Network error/)).toBeInTheDocument();
      });
    });
  });

  describe('Settings Integration', () => {
    it('キーワードを追加できる', async () => {
      const user = userEvent.setup();

      render(
        <BrowserRouter>
          <Settings />
        </BrowserRouter>
      );

      // キーワード入力
      const input = screen.getByLabelText('キーワードを追加');
      await user.type(input, 'New Keyword');

      // 追加ボタンをクリック
      const addButton = screen.getByRole('button', { name: '追加' });
      await user.click(addButton);

      // キーワードが表示されることを確認
      expect(screen.getByText('New Keyword')).toBeInTheDocument();
    });

    it('設定を保存できる', async () => {
      const user = userEvent.setup();

      render(
        <BrowserRouter>
          <Settings />
        </BrowserRouter>
      );

      // 保存ボタンをクリック
      const saveButton = screen.getByRole('button', { name: '設定を保存' });
      await user.click(saveButton);

      // 成功メッセージを確認
      await waitFor(() => {
        expect(screen.getByText('設定を保存しました')).toBeInTheDocument();
      });
    });
  });
});


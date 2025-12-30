/**
 * アプリケーション状態管理ストア.
 * 
 * 目的: グローバル状態を一元管理
 * 使用技術: Zustand
 * 注意: 状態の更新は必ずアクションを通して行うこと
 */

import { create } from 'zustand';
import type { Trend, Report, Notification, SourceType } from '@/types';
import { apiClient } from '@/api/client';

interface AppState {
  // データ
  trends: Trend[];
  reports: Report[];
  notifications: Notification[];
  
  // UI状態
  loading: boolean;
  error: string | null;
  
  // 設定
  keywords: string[];
  sources: SourceType[];
  
  // アクション
  setLoading: (loading: boolean) => void;
  setError: (error: string | null) => void;
  
  // データ取得
  fetchTrends: () => Promise<void>;
  fetchReports: () => Promise<void>;
  
  // データ収集
  collectData: (keywords: string[], sources: SourceType[]) => Promise<void>;
  
  // 設定
  updateKeywords: (keywords: string[]) => void;
  updateSources: (sources: SourceType[]) => void;
  
  // 通知
  addNotification: (notification: Notification) => void;
  markNotificationAsRead: (id: string) => void;
  clearNotifications: () => void;
}

export const useAppStore = create<AppState>((set, get) => ({
  // 初期状態
  trends: [],
  reports: [],
  notifications: [],
  loading: false,
  error: null,
  keywords: ['COBOL', 'Java migration', 'AI'],
  sources: [],
  
  // UI状態管理
  setLoading: (loading) => set({ loading }),
  setError: (error) => set({ error }),
  
  // トレンド取得
  fetchTrends: async () => {
    set({ loading: true, error: null });
    try {
      const response = await apiClient.getTrends(50);
      set({ trends: response.trends, loading: false });
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Unknown error';
      set({ error: message, loading: false });
    }
  },
  
  // レポート取得
  fetchReports: async () => {
    set({ loading: true, error: null });
    try {
      const response = await apiClient.getReports(20);
      set({ reports: response.reports, loading: false });
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Unknown error';
      set({ error: message, loading: false });
    }
  },
  
  // データ収集
  collectData: async (keywords, sources) => {
    set({ loading: true, error: null });
    try {
      await apiClient.collect({ keywords, sources });
      // 収集後、トレンドを再取得
      await get().fetchTrends();
      set({ loading: false });
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Unknown error';
      set({ error: message, loading: false });
    }
  },
  
  // 設定更新
  updateKeywords: (keywords) => set({ keywords }),
  updateSources: (sources) => set({ sources }),
  
  // 通知管理
  addNotification: (notification) =>
    set((state) => ({
      notifications: [notification, ...state.notifications],
    })),
  
  markNotificationAsRead: (id) =>
    set((state) => ({
      notifications: state.notifications.map((n) =>
        n.id === id ? { ...n, read: true } : n
      ),
    })),
  
  clearNotifications: () => set({ notifications: [] }),
}));


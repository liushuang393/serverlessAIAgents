/**
 * アプリケーション状態管理ストア.
 *
 * 目的: グローバル状態を一元管理
 * 使用技術: Zustand
 * 注意: 状態の更新は必ずアクションを通して行うこと
 */

import { create } from 'zustand';
import type { CollectResponse, Trend, Report, Notification } from '@/types';
import { SourceType } from '@/types';
import { apiClient } from '@/api/client';

const SETTINGS_STORAGE_KEY = 'market-trend-monitor:settings:v1';
const DEFAULT_KEYWORDS = ['COBOL', 'Java migration', 'AI'];
const SOURCE_VALUES: SourceType[] = [
  SourceType.NEWS,
  SourceType.GITHUB,
  SourceType.ARXIV,
  SourceType.RSS,
];

interface PersistedSettings {
  keywords: string[];
  sources: SourceType[];
}

const loadPersistedSettings = (): PersistedSettings | null => {
  if (typeof window === 'undefined') {
    return null;
  }

  try {
    const raw = window.localStorage.getItem(SETTINGS_STORAGE_KEY);
    if (!raw) {
      return null;
    }

    const parsed = JSON.parse(raw) as {
      keywords?: unknown;
      sources?: unknown;
    };

    const keywords = Array.isArray(parsed.keywords)
      ? parsed.keywords.filter((item): item is string => typeof item === 'string')
      : [];

    const sources = Array.isArray(parsed.sources)
      ? parsed.sources.filter(
          (item): item is SourceType =>
            typeof item === 'string' && SOURCE_VALUES.includes(item as SourceType)
        )
      : [];

    return {
      keywords,
      sources,
    };
  } catch {
    return null;
  }
};

const savePersistedSettings = (settings: PersistedSettings): void => {
  if (typeof window === 'undefined') {
    return;
  }

  window.localStorage.setItem(SETTINGS_STORAGE_KEY, JSON.stringify(settings));
};

const persistedSettings = loadPersistedSettings();

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
  collectData: (
    keywords: string[],
    sources: SourceType[]
  ) => Promise<CollectResponse | null>;

  // 設定
  updateKeywords: (keywords: string[]) => void;
  updateSources: (sources: SourceType[]) => void;

  // 通知
  addNotification: (notification: Notification) => void;
  markNotificationAsRead: (id: string) => void;
  clearNotifications: () => void;
}

export const useAppStore = create<AppState>((set) => ({
  // 初期状態
  trends: [],
  reports: [],
  notifications: [],
  loading: false,
  error: null,
  keywords: persistedSettings?.keywords.length ? persistedSettings.keywords : DEFAULT_KEYWORDS,
  sources: persistedSettings?.sources ?? [],

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
      const response = await apiClient.collect({ keywords, sources });
      const [trendsResponse, reportsResponse] = await Promise.all([
        apiClient.getTrends(50),
        apiClient.getReports(20),
      ]);

      set({
        trends: trendsResponse.trends,
        reports: reportsResponse.reports,
        loading: false,
      });

      return response;
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Unknown error';
      set({ error: message, loading: false });
      return null;
    }
  },

  // 設定更新
  updateKeywords: (keywords) =>
    set((state) => {
      savePersistedSettings({ keywords, sources: state.sources });
      return { keywords };
    }),

  updateSources: (sources) =>
    set((state) => {
      savePersistedSettings({ keywords: state.keywords, sources });
      return { sources };
    }),

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

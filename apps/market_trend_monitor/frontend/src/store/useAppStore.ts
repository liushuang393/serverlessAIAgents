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
const DEFAULT_COLLECTION_WINDOW_DAYS = 7;
const SOURCE_VALUES: SourceType[] = [
  SourceType.NEWS,
  SourceType.GITHUB,
  SourceType.ARXIV,
  SourceType.RSS,
];
const COLLECTION_WINDOWS = [7, 30, 90] as const;
type CollectionWindowDays = (typeof COLLECTION_WINDOWS)[number];

interface PersistedSettings {
  keywords: string[];
  sources: SourceType[];
  collectionWindowDays: CollectionWindowDays;
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
      collectionWindowDays?: unknown;
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
    const collectionWindowDays = COLLECTION_WINDOWS.includes(
      parsed.collectionWindowDays as CollectionWindowDays
    )
      ? (parsed.collectionWindowDays as CollectionWindowDays)
      : DEFAULT_COLLECTION_WINDOW_DAYS;

    return {
      keywords,
      sources,
      collectionWindowDays,
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
  collectionWindowDays: CollectionWindowDays;

  // アクション
  setLoading: (loading: boolean) => void;
  setError: (error: string | null) => void;

  // データ取得
  fetchTrends: () => Promise<void>;
  fetchReports: () => Promise<void>;

  // データ収集
  collectData: (
    keywords: string[],
    sources: SourceType[],
    collectionWindowDays: CollectionWindowDays
  ) => Promise<CollectResponse | null>;

  // 設定
  updateKeywords: (keywords: string[]) => void;
  updateSources: (sources: SourceType[]) => void;
  updateCollectionWindowDays: (days: CollectionWindowDays) => void;

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
  collectionWindowDays: persistedSettings?.collectionWindowDays ?? DEFAULT_COLLECTION_WINDOW_DAYS,

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
  collectData: async (keywords, sources, collectionWindowDays) => {
    set({ loading: true, error: null });
    try {
      const now = new Date();
      const start = new Date(now);
      start.setDate(start.getDate() - Math.max(collectionWindowDays, 1));
      const response = await apiClient.collect({
        keywords,
        sources,
        date_range: {
          start: start.toISOString(),
          end: now.toISOString(),
        },
      });
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
      savePersistedSettings({
        keywords,
        sources: state.sources,
        collectionWindowDays: state.collectionWindowDays,
      });
      return { keywords };
    }),

  updateSources: (sources) =>
    set((state) => {
      savePersistedSettings({
        keywords: state.keywords,
        sources,
        collectionWindowDays: state.collectionWindowDays,
      });
      return { sources };
    }),

  updateCollectionWindowDays: (days) =>
    set((state) => {
      savePersistedSettings({
        keywords: state.keywords,
        sources: state.sources,
        collectionWindowDays: days,
      });
      return { collectionWindowDays: days };
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

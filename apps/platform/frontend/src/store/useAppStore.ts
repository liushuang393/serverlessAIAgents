/**
 * AgentFlow Platform - Zustand ストア.
 *
 * App 一覧・サマリー・詳細のグローバル状態管理。
 */

import { create } from 'zustand';
import type {
  AggregatedAgent,
  AppDetail,
  AppListItem,
  AppSummaryResponse,
  HealthCheckResult,
  RAGOverviewResponse,
  SkillCategoryGroup,
  SkillInfo,
  TagInfo,
} from '@/types';
import {
  fetchAgents,
  fetchAppDetail,
  fetchAppHealth,
  fetchApps,
  fetchRAGOverview,
  fetchSkills,
  fetchSkillsGrouped,
  fetchSkillTags,
  fetchSummary,
  refreshApps,
  searchSkills,
} from '@/api/client';

interface LoadAppsOptions {
  waitForHealth?: boolean;
  silent?: boolean;
}

interface LoadAppDetailOptions {
  waitForHealth?: boolean;
}

interface AppState {
  /** App 一覧 */
  apps: AppListItem[];
  /** App 総数 */
  totalApps: number;
  /** サマリー */
  summary: AppSummaryResponse | null;
  /** 選択中の App 詳細 */
  selectedApp: AppDetail | null;
  /** ヘルスチェック結果キャッシュ */
  healthCache: Record<string, HealthCheckResult>;

  /* --- Phase 3: Agent --- */
  agents: AggregatedAgent[];

  /* --- Phase 3: Skill --- */
  skills: SkillInfo[];
  skillTags: TagInfo[];
  skillGroups: SkillCategoryGroup[];

  /* --- Phase 3: RAG --- */
  ragOverview: RAGOverviewResponse | null;

  /** ローディング状態 */
  loading: boolean;
  /** エラーメッセージ */
  error: string | null;

  /** App 一覧をロード */
  loadApps: (options?: LoadAppsOptions) => Promise<void>;
  /** サマリーをロード */
  loadSummary: () => Promise<void>;
  /** App 詳細をロード */
  loadAppDetail: (name: string, options?: LoadAppDetailOptions) => Promise<void>;
  /** ヘルスチェック実行 */
  checkHealth: (name: string) => Promise<void>;
  /** App 一覧を再スキャン */
  refresh: () => Promise<void>;

  /* --- Phase 3 アクション --- */
  loadAgents: () => Promise<void>;
  loadSkills: () => Promise<void>;
  loadSkillsGrouped: () => Promise<void>;
  searchSkillsByTag: (tag: string) => Promise<void>;
  loadRAGOverview: () => Promise<void>;

  /** エラーをクリア */
  clearError: () => void;
}

export const useAppStore = create<AppState>((set) => ({
  apps: [],
  totalApps: 0,
  summary: null,
  selectedApp: null,
  healthCache: {},
  agents: [],
  skills: [],
  skillTags: [],
  skillGroups: [],
  ragOverview: null,
  loading: false,
  error: null,

  loadApps: async (options) => {
    const waitForHealth = options?.waitForHealth ?? false;
    const silent = options?.silent ?? false;
    if (silent) {
      set({ error: null });
    } else {
      set({ loading: true, error: null });
    }
    try {
      const res = await fetchApps({ waitForHealth });
      if (silent) {
        set((state) => ({ apps: res.apps, totalApps: res.total, loading: state.loading }));
      } else {
        set({ apps: res.apps, totalApps: res.total, loading: false });
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : 'App 一覧の取得に失敗';
      if (silent) {
        set({ error: message });
      } else {
        set({ error: message, loading: false });
      }
    }
  },

  loadSummary: async () => {
    try {
      const res = await fetchSummary();
      set({ summary: res });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'サマリーの取得に失敗';
      set({ error: message });
    }
  },

  loadAppDetail: async (name: string, options) => {
    set({ loading: true, error: null, selectedApp: null });
    try {
      const detail = await fetchAppDetail(name, {
        waitForHealth: options?.waitForHealth ?? false,
      });
      set({ selectedApp: detail, loading: false });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'App 詳細の取得に失敗';
      set({ error: message, loading: false });
    }
  },

  checkHealth: async (name: string) => {
    try {
      const result = await fetchAppHealth(name);
      set((state) => ({
        healthCache: { ...state.healthCache, [name]: result },
      }));
    } catch {
      /* ヘルスチェック失敗は静かに無視 */
    }
  },

  refresh: async () => {
    set({ loading: true, error: null });
    try {
      await refreshApps();
      const res = await fetchApps({ waitForHealth: true });
      const summary = await fetchSummary();
      set({
        apps: res.apps,
        totalApps: res.total,
        summary,
        loading: false,
      });
    } catch (err) {
      const message = err instanceof Error ? err.message : '再スキャンに失敗';
      set({ error: message, loading: false });
    }
  },

  /* --- Phase 3: Agent --- */
  loadAgents: async () => {
    set({ loading: true, error: null });
    try {
      const agentsRes = await fetchAgents();
      set({
        agents: agentsRes.agents,
        loading: false,
      });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Agent 一覧の取得に失敗';
      set({ error: message, loading: false });
    }
  },

  /* --- Phase 3: Skill --- */
  loadSkills: async () => {
    set({ loading: true, error: null });
    try {
      const [skillsRes, tagsRes] = await Promise.all([
        fetchSkills(),
        fetchSkillTags(),
      ]);
      set({
        skills: skillsRes.skills,
        skillTags: tagsRes.tags,
        loading: false,
      });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Skill 一覧の取得に失敗';
      set({ error: message, loading: false });
    }
  },

  loadSkillsGrouped: async () => {
    set({ loading: true, error: null });
    try {
      const res = await fetchSkillsGrouped();
      set({ skillGroups: res.groups, loading: false });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'カテゴリ別 Skill 一覧の取得に失敗';
      set({ error: message, loading: false });
    }
  },

  searchSkillsByTag: async (tag: string) => {
    set({ loading: true, error: null });
    try {
      const res = await searchSkills(tag);
      set({ skills: res.skills, loading: false });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Skill 検索に失敗';
      set({ error: message, loading: false });
    }
  },

  /* --- Phase 3: RAG --- */
  loadRAGOverview: async () => {
    set({ loading: true, error: null });
    try {
      const overview = await fetchRAGOverview();
      set({ ragOverview: overview, loading: false });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'RAG 情報の取得に失敗';
      set({ error: message, loading: false });
    }
  },

  clearError: () => set({ error: null }),
}));


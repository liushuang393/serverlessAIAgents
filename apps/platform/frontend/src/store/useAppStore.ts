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
  CapabilityTag,
  HealthCheckResult,
  RAGOverviewResponse,
  SkillInfo,
  TagInfo,
} from '@/types';
import {
  fetchAgents,
  fetchAppDetail,
  fetchAppHealth,
  fetchApps,
  fetchCapabilities,
  fetchRAGOverview,
  fetchSkills,
  fetchSkillTags,
  fetchSummary,
  refreshApps,
  searchAgents,
  searchSkills,
} from '@/api/client';

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
  capabilities: CapabilityTag[];

  /* --- Phase 3: Skill --- */
  skills: SkillInfo[];
  skillTags: TagInfo[];

  /* --- Phase 3: RAG --- */
  ragOverview: RAGOverviewResponse | null;

  /** ローディング状態 */
  loading: boolean;
  /** エラーメッセージ */
  error: string | null;

  /** App 一覧をロード */
  loadApps: () => Promise<void>;
  /** サマリーをロード */
  loadSummary: () => Promise<void>;
  /** App 詳細をロード */
  loadAppDetail: (name: string) => Promise<void>;
  /** ヘルスチェック実行 */
  checkHealth: (name: string) => Promise<void>;
  /** App 一覧を再スキャン */
  refresh: () => Promise<void>;

  /* --- Phase 3 アクション --- */
  loadAgents: () => Promise<void>;
  searchAgentsByCapability: (cap: string) => Promise<void>;
  loadSkills: () => Promise<void>;
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
  capabilities: [],
  skills: [],
  skillTags: [],
  ragOverview: null,
  loading: false,
  error: null,

  loadApps: async () => {
    set({ loading: true, error: null });
    try {
      const res = await fetchApps();
      set({ apps: res.apps, totalApps: res.total, loading: false });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'App 一覧の取得に失敗';
      set({ error: message, loading: false });
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

  loadAppDetail: async (name: string) => {
    set({ loading: true, error: null, selectedApp: null });
    try {
      const detail = await fetchAppDetail(name);
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
      const res = await fetchApps();
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
      const [agentsRes, capsRes] = await Promise.all([
        fetchAgents(),
        fetchCapabilities(),
      ]);
      set({
        agents: agentsRes.agents,
        capabilities: capsRes.capabilities,
        loading: false,
      });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Agent 一覧の取得に失敗';
      set({ error: message, loading: false });
    }
  },

  searchAgentsByCapability: async (cap: string) => {
    set({ loading: true, error: null });
    try {
      const res = await searchAgents(cap);
      set({ agents: res.agents, loading: false });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Agent 検索に失敗';
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


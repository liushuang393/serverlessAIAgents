/**
 * AgentFlow Platform - API クライアント.
 *
 * バックエンド /api/apps/* とのHTTP通信。
 * Vite dev server のプロキシ経由で API にアクセスする。
 */

import axios from 'axios';
import type {
  AgentListResponse,
  AgentStatsResponse,
  AppRAGConfig,
  AppRAGConfigPatchRequest,
  AgentsByAppResponse,
  AppActionResponse,
  AppCreateOptionsResponse,
  AppCreateRequest,
  AppCreateResponse,
  AppDetail,
  AppListResponse,
  AppSummaryResponse,
  CapabilitiesResponse,
  HealthCheckResult,
  MCPConfigResponse,
  MCPLazyLoadingConfig,
  MCPServerConfig,
  PortConflictReport,
  RAGOverviewResponse,
  RAGPattern,
  RAGStatsResponse,
  RetrievalMethod,
  RefreshResponse,
  SkillInfo,
  SkillListResponse,
  SkillStatsResponse,
  TagsResponse,
} from '@/types';

/** axios インスタンス（通常 API 用） */
const api = axios.create({
  baseURL: '/api',
  timeout: 15000,
  headers: { 'Content-Type': 'application/json' },
});

/**
 * 長時間操作用 axios インスタンス.
 *
 * docker compose build 等は数分かかるため、タイムアウトを 5 分に設定。
 */
const longRunningApi = axios.create({
  baseURL: '/api',
  timeout: 300_000,
  headers: { 'Content-Type': 'application/json' },
});

/** App 一覧を取得 */
export async function fetchApps(): Promise<AppListResponse> {
  const { data } = await api.get<AppListResponse>('/apps');
  return data;
}

/** App 概要統計を取得 */
export async function fetchSummary(): Promise<AppSummaryResponse> {
  const { data } = await api.get<AppSummaryResponse>('/apps/summary');
  return data;
}

/** App 詳細を取得 */
export async function fetchAppDetail(appName: string): Promise<AppDetail> {
  const { data } = await api.get<AppDetail>(`/apps/${appName}`);
  return data;
}

/** App ヘルスチェック */
export async function fetchAppHealth(
  appName: string,
): Promise<HealthCheckResult> {
  const { data } = await api.get<HealthCheckResult>(
    `/apps/${appName}/health`,
  );
  return data;
}

/** App publish（docker compose up -d --build） — 長時間操作 */
export async function publishApp(appName: string): Promise<AppActionResponse> {
  const { data } = await longRunningApi.post<AppActionResponse>(`/apps/${appName}/publish`);
  return data;
}

/** App start（docker compose up -d） — 長時間操作 */
export async function startApp(appName: string): Promise<AppActionResponse> {
  const { data } = await longRunningApi.post<AppActionResponse>(`/apps/${appName}/start`);
  return data;
}

/** App stop（docker compose down） — 長時間操作 */
export async function stopApp(appName: string): Promise<AppActionResponse> {
  const { data } = await longRunningApi.post<AppActionResponse>(`/apps/${appName}/stop`);
  return data;
}

/** App 一覧を再スキャン */
export async function refreshApps(): Promise<RefreshResponse> {
  const { data } = await api.post<RefreshResponse>('/apps/refresh');
  return data;
}

/** App 作成オプション取得 */
export async function fetchAppCreateOptions(): Promise<AppCreateOptionsResponse> {
  const { data } = await api.get<AppCreateOptionsResponse>('/apps/create/options');
  return data;
}

/** 新規 App 作成 */
export async function createApp(request: AppCreateRequest): Promise<AppCreateResponse> {
  const { data } = await api.post<AppCreateResponse>('/apps/create', request);
  return data;
}

/** ポート重複レポート */
export async function fetchPortConflicts(): Promise<PortConflictReport> {
  const { data } = await api.get<PortConflictReport>('/apps/ports/conflicts');
  return data;
}

/** 重複ポート再割当 */
export async function rebalancePorts(
  dryRun = true,
): Promise<{
  dry_run: boolean;
  updates?: Record<string, Record<string, number>>;
  updated_apps?: string[];
  total_updates: number;
}> {
  const { data } = await api.post('/apps/ports/rebalance', { dry_run: dryRun });
  return data;
}

/* ============================================================
 * Phase 3: Agent API
 * ============================================================ */

/** 全 Agent 一覧 */
export async function fetchAgents(): Promise<AgentListResponse> {
  const { data } = await api.get<AgentListResponse>('/agents');
  return data;
}

/** Agent 統計 */
export async function fetchAgentStats(): Promise<AgentStatsResponse> {
  const { data } = await api.get<AgentStatsResponse>('/agents/stats');
  return data;
}

/** Capability タグ一覧 */
export async function fetchCapabilities(): Promise<CapabilitiesResponse> {
  const { data } = await api.get<CapabilitiesResponse>('/agents/capabilities');
  return data;
}

/** App 別 Agent グルーピング */
export async function fetchAgentsByApp(): Promise<AgentsByAppResponse> {
  const { data } = await api.get<AgentsByAppResponse>('/agents/by-app');
  return data;
}

/** Capability 検索 */
export async function searchAgents(capability: string): Promise<AgentListResponse> {
  const { data } = await api.get<AgentListResponse>('/agents/search', {
    params: { capability },
  });
  return data;
}

/* ============================================================
 * Phase 3: Skill API
 * ============================================================ */

/** 全 Skill 一覧 */
export async function fetchSkills(): Promise<SkillListResponse> {
  const { data } = await api.get<SkillListResponse>('/skills');
  return data;
}

/** Skill 統計 */
export async function fetchSkillStats(): Promise<SkillStatsResponse> {
  const { data } = await api.get<SkillStatsResponse>('/skills/stats');
  return data;
}

/** タグ一覧 */
export async function fetchSkillTags(): Promise<TagsResponse> {
  const { data } = await api.get<TagsResponse>('/skills/tags');
  return data;
}

/** タグ検索 */
export async function searchSkills(tag: string): Promise<SkillListResponse> {
  const { data } = await api.get<SkillListResponse>('/skills/search', {
    params: { tag },
  });
  return data;
}

/** Skill 詳細 */
export async function fetchSkillDetail(name: string): Promise<SkillInfo> {
  const { data } = await api.get<SkillInfo>(`/skills/${name}`);
  return data;
}

/* ============================================================
 * Phase 3: RAG API
 * ============================================================ */

/** RAG 概要 */
export async function fetchRAGOverview(): Promise<RAGOverviewResponse> {
  const { data } = await api.get<RAGOverviewResponse>('/rag/overview');
  return data;
}

/** RAG 統計 */
export async function fetchRAGStats(): Promise<RAGStatsResponse> {
  const { data } = await api.get<RAGStatsResponse>('/rag/stats');
  return data;
}

/** RAG 検索方式一覧 */
export async function fetchRAGRetrievalMethods(): Promise<{ methods: RetrievalMethod[]; total: number }> {
  const { data } = await api.get<{ methods: RetrievalMethod[]; total: number }>('/rag/retrieval-methods');
  return data;
}

/** RAG パターン一覧 */
export async function fetchRAGPatterns(): Promise<{ patterns: RAGPattern[]; total: number }> {
  const { data } = await api.get<{ patterns: RAGPattern[]; total: number }>('/rag/patterns');
  return data;
}

/** 全 App の RAG 設定一覧 */
export async function fetchAppRAGConfigs(): Promise<{ apps: AppRAGConfig[]; total: number }> {
  const { data } = await api.get<{ apps: AppRAGConfig[]; total: number }>('/rag/apps/configs');
  return data;
}

/** App 単位 RAG 設定 */
export async function fetchAppRAGConfig(appName: string): Promise<AppRAGConfig> {
  const { data } = await api.get<AppRAGConfig>(`/rag/apps/${appName}/config`);
  return data;
}

/** App 単位 RAG 設定更新 */
export async function patchAppRAGConfig(
  appName: string,
  patch: AppRAGConfigPatchRequest,
): Promise<AppRAGConfig> {
  const { data } = await api.patch<AppRAGConfig>(`/rag/apps/${appName}/config`, patch);
  return data;
}

/* ============================================================
 * MCP API
 * ============================================================ */

/** MCP 設定全体 */
export async function fetchMCPConfig(): Promise<MCPConfigResponse> {
  const { data } = await api.get<MCPConfigResponse>('/mcp/config');
  return data;
}

/** MCP サーバー追加/更新 */
export async function upsertMCPServer(
  server: MCPServerConfig,
): Promise<{ success: boolean; server: MCPServerConfig }> {
  const { data } = await api.post<{ success: boolean; server: MCPServerConfig }>(
    '/mcp/servers',
    server,
  );
  return data;
}

/** MCP サーバー削除 */
export async function deleteMCPServer(name: string): Promise<{ success: boolean; deleted: string }> {
  const { data } = await api.delete<{ success: boolean; deleted: string }>(`/mcp/servers/${name}`);
  return data;
}

/** MCP lazy_loading 更新 */
export async function patchMCPLazyLoading(
  patch: Partial<MCPLazyLoadingConfig>,
): Promise<{ success: boolean; lazy_loading: MCPLazyLoadingConfig }> {
  const { data } = await api.patch<{ success: boolean; lazy_loading: MCPLazyLoadingConfig }>(
    '/mcp/lazy-loading',
    patch,
  );
  return data;
}

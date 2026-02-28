/**
 * AgentFlow Platform - API クライアント.
 *
 * バックエンド /api/studios/framework/apps/* とのHTTP通信。
 * Vite dev server のプロキシ経由で API にアクセスする。
 */

import axios from 'axios';
import type {
  AgentListResponse,
  AgentStatsResponse,
  AgentsByAppResponse,
  AgentsByBusinessBaseResponse,
  AgentsByPatternResponse,
  AppRAGConfig,
  AppRAGConfigPatchRequest,
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
  ManifestMigrationReport,
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
  timeout: 60_000,
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

const inflightRequests = new Map<string, Promise<unknown>>();
const FRAMEWORK_APPS_BASE = '/studios/framework/apps';

async function withInflightDedup<T>(
  key: string,
  request: () => Promise<T>,
): Promise<T> {
  const existing = inflightRequests.get(key) as Promise<T> | undefined;
  if (existing) {
    return existing;
  }

  const next = request().finally(() => {
    inflightRequests.delete(key);
  });
  inflightRequests.set(key, next);
  return next;
}

/** App 一覧取得オプション */
export interface FetchAppsOptions {
  waitForHealth?: boolean;
  includeRuntime?: boolean;
}

/** App 一覧を取得 */
export async function fetchApps(
  options: FetchAppsOptions = {},
): Promise<AppListResponse> {
  const waitForHealth = options.waitForHealth ?? false;
  const includeRuntime = options.includeRuntime ?? false;
  const key = `${FRAMEWORK_APPS_BASE}?wait_for_health=${waitForHealth}&include_runtime=${includeRuntime}`;
  return withInflightDedup(key, async () => {
    const { data } = await api.get<AppListResponse>(FRAMEWORK_APPS_BASE, {
      params: {
        wait_for_health: waitForHealth,
        include_runtime: includeRuntime,
      },
    });
    return data;
  });
}

/** App 概要統計を取得 */
export async function fetchSummary(): Promise<AppSummaryResponse> {
  return withInflightDedup(`${FRAMEWORK_APPS_BASE}/summary`, async () => {
    const { data } = await api.get<AppSummaryResponse>(`${FRAMEWORK_APPS_BASE}/summary`);
    return data;
  });
}

/** App 詳細を取得 */
export async function fetchAppDetail(appName: string): Promise<AppDetail> {
  const { data } = await api.get<AppDetail>(`${FRAMEWORK_APPS_BASE}/${appName}`);
  return data;
}

/** App ヘルスチェック */
export async function fetchAppHealth(
  appName: string,
): Promise<HealthCheckResult> {
  const { data } = await api.get<HealthCheckResult>(
    `${FRAMEWORK_APPS_BASE}/${appName}/health`,
  );
  return data;
}

/** App publish（docker compose up -d --build） — 長時間操作 */
export async function publishApp(appName: string): Promise<AppActionResponse> {
  const { data } = await longRunningApi.post<AppActionResponse>(`${FRAMEWORK_APPS_BASE}/${appName}/publish`);
  return data;
}

/** App start（docker compose up -d） — 長時間操作 */
export async function startApp(appName: string): Promise<AppActionResponse> {
  const { data } = await longRunningApi.post<AppActionResponse>(`${FRAMEWORK_APPS_BASE}/${appName}/start`);
  return data;
}

/** App stop（docker compose down） — 長時間操作 */
export async function stopApp(appName: string): Promise<AppActionResponse> {
  const { data } = await longRunningApi.post<AppActionResponse>(`${FRAMEWORK_APPS_BASE}/${appName}/stop`);
  return data;
}

/** App restart（stop -> start） — 長時間操作 */
export async function restartApp(appName: string): Promise<AppActionResponse> {
  const { data } = await longRunningApi.post<AppActionResponse>(`${FRAMEWORK_APPS_BASE}/${appName}/restart`);
  return data;
}

/** App ローカル開発起動（バックエンド・フロントエンドをローカルで起動） */
export async function localStartApp(appName: string): Promise<AppActionResponse> {
  const { data } = await longRunningApi.post<AppActionResponse>(`${FRAMEWORK_APPS_BASE}/${appName}/local-start`);
  return data;
}

/** App CLI ステータス */
export async function fetchAppCLIStatus(
  appName: string,
): Promise<{ app_name: string; status: Record<string, unknown> }> {
  const { data } = await api.get<{ app_name: string; status: Record<string, unknown> }>(
    `${FRAMEWORK_APPS_BASE}/${appName}/cli/status`,
  );
  return data;
}

/** App CLI セットアップ */
export async function setupAppCLI(
  appName: string,
): Promise<{ app_name: string; setup: Record<string, unknown> }> {
  const { data } = await longRunningApi.post<{ app_name: string; setup: Record<string, unknown> }>(
    `${FRAMEWORK_APPS_BASE}/${appName}/cli/setup`,
  );
  return data;
}

/** App 一覧を再スキャン */
export async function refreshApps(): Promise<RefreshResponse> {
  const { data } = await api.post<RefreshResponse>(`${FRAMEWORK_APPS_BASE}/refresh`);
  return data;
}

/** app_config マニフェスト標準化 */
export async function migrateManifests(dryRun = true): Promise<ManifestMigrationReport> {
  const { data } = await api.post<ManifestMigrationReport>(`${FRAMEWORK_APPS_BASE}/migrate-manifests`, {
    dry_run: dryRun,
  });
  return data;
}

/** App 作成オプション取得 */
export async function fetchAppCreateOptions(): Promise<AppCreateOptionsResponse> {
  const { data } = await api.get<AppCreateOptionsResponse>(`${FRAMEWORK_APPS_BASE}/create/options`);
  return data;
}

/** 新規 App 作成 */
export async function createApp(request: AppCreateRequest): Promise<AppCreateResponse> {
  const { data } = await api.post<AppCreateResponse>(`${FRAMEWORK_APPS_BASE}/create`, request);
  return data;
}

/** ポート重複レポート */
export async function fetchPortConflicts(): Promise<PortConflictReport> {
  return withInflightDedup(`${FRAMEWORK_APPS_BASE}/ports/conflicts`, async () => {
    const { data } = await api.get<PortConflictReport>(`${FRAMEWORK_APPS_BASE}/ports/conflicts`);
    return data;
  });
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
  const { data } = await api.post(`${FRAMEWORK_APPS_BASE}/ports/rebalance`, { dry_run: dryRun });
  return data;
}

/* ============================================================
 * Phase 3: Agent API
 * ============================================================ */

/** 全 Agent 一覧 */
export async function fetchAgents(): Promise<AgentListResponse> {
  const { data } = await api.get<AgentListResponse>('/studios/framework/agents');
  return data;
}

/** Agent 統計 */
export async function fetchAgentStats(): Promise<AgentStatsResponse> {
  const { data } = await api.get<AgentStatsResponse>('/studios/framework/agents/stats');
  return data;
}

/** Capability タグ一覧 */
export async function fetchCapabilities(): Promise<CapabilitiesResponse> {
  const { data } = await api.get<CapabilitiesResponse>('/studios/framework/agents/capabilities');
  return data;
}

/** App 別 Agent グルーピング */
export async function fetchAgentsByApp(): Promise<AgentsByAppResponse> {
  const { data } = await api.get<AgentsByAppResponse>('/studios/framework/agents/by-app');
  return data;
}

/** Capability 検索 */
export async function searchAgents(capability: string): Promise<AgentListResponse> {
  const { data } = await api.get<AgentListResponse>('/studios/framework/agents/search', {
    params: { capability },
  });
  return data;
}

/** Agent pattern 別グルーピング */
export async function fetchAgentsByPattern(): Promise<AgentsByPatternResponse> {
  const { data } = await api.get<AgentsByPatternResponse>('/studios/framework/agents/by-pattern');
  return data;
}

/** 業務基盤別 Agent グルーピング */
export async function fetchAgentsByBusinessBase(): Promise<AgentsByBusinessBaseResponse> {
  const { data } = await api.get<AgentsByBusinessBaseResponse>('/studios/framework/agents/by-business-base');
  return data;
}

/* ============================================================
 * Phase 3: Skill API
 * ============================================================ */

/** 全 Skill 一覧 */
export async function fetchSkills(): Promise<SkillListResponse> {
  const { data } = await api.get<SkillListResponse>('/studios/framework/skills');
  return data;
}

/** Skill 統計 */
export async function fetchSkillStats(): Promise<SkillStatsResponse> {
  const { data } = await api.get<SkillStatsResponse>('/studios/framework/skills/stats');
  return data;
}

/** タグ一覧 */
export async function fetchSkillTags(): Promise<TagsResponse> {
  const { data } = await api.get<TagsResponse>('/studios/framework/skills/tags');
  return data;
}

/** タグ検索 */
export async function searchSkills(tag: string): Promise<SkillListResponse> {
  const { data } = await api.get<SkillListResponse>('/studios/framework/skills/search', {
    params: { tag },
  });
  return data;
}

/** Skill 詳細 */
export async function fetchSkillDetail(name: string): Promise<SkillInfo> {
  const { data } = await api.get<SkillInfo>(`/studios/framework/skills/${name}`);
  return data;
}

/* ============================================================
 * Phase 3: RAG API
 * ============================================================ */

/** RAG 概要 */
export async function fetchRAGOverview(): Promise<RAGOverviewResponse> {
  const { data } = await api.get<RAGOverviewResponse>('/studios/framework/rag/overview');
  return data;
}

/** RAG 統計 */
export async function fetchRAGStats(): Promise<RAGStatsResponse> {
  const { data } = await api.get<RAGStatsResponse>('/studios/framework/rag/stats');
  return data;
}

/** RAG 検索方式一覧 */
export async function fetchRAGRetrievalMethods(): Promise<{ methods: RetrievalMethod[]; total: number }> {
  const { data } = await api.get<{ methods: RetrievalMethod[]; total: number }>('/studios/framework/rag/retrieval-methods');
  return data;
}

/** RAG パターン一覧 */
export async function fetchRAGPatterns(): Promise<{ patterns: RAGPattern[]; total: number }> {
  const { data } = await api.get<{ patterns: RAGPattern[]; total: number }>('/studios/framework/rag/patterns');
  return data;
}

/** 全 App の RAG 設定一覧 */
export async function fetchAppRAGConfigs(): Promise<{ apps: AppRAGConfig[]; total: number }> {
  const { data } = await api.get<{ apps: AppRAGConfig[]; total: number }>('/studios/framework/rag/apps/configs');
  return data;
}

/** App 単位 RAG 設定 */
export async function fetchAppRAGConfig(appName: string): Promise<AppRAGConfig> {
  const { data } = await api.get<AppRAGConfig>(
    `/studios/framework/rag/apps/${appName}/config`,
  );
  return data;
}

/** App 単位 RAG 設定更新 */
export async function patchAppRAGConfig(
  appName: string,
  patch: AppRAGConfigPatchRequest,
): Promise<AppRAGConfig> {
  const { data } = await api.patch<AppRAGConfig>(
    `/studios/framework/rag/apps/${appName}/config`,
    patch,
  );
  return data;
}

/* ============================================================
 * MCP API
 * ============================================================ */

/** MCP 設定全体 */
export async function fetchMCPConfig(): Promise<MCPConfigResponse> {
  const { data } = await api.get<MCPConfigResponse>('/studios/framework/mcp/config');
  return data;
}

/** MCP サーバー追加/更新 */
export async function upsertMCPServer(
  server: MCPServerConfig,
): Promise<{ success: boolean; server: MCPServerConfig }> {
  const { data } = await api.post<{ success: boolean; server: MCPServerConfig }>(
    '/studios/framework/mcp/servers',
    server,
  );
  return data;
}

/** MCP サーバー削除 */
export async function deleteMCPServer(name: string): Promise<{ success: boolean; deleted: string }> {
  const { data } = await api.delete<{ success: boolean; deleted: string }>(
    `/studios/framework/mcp/servers/${name}`,
  );
  return data;
}

/** MCP lazy_loading 更新 */
export async function patchMCPLazyLoading(
  patch: Partial<MCPLazyLoadingConfig>,
): Promise<{ success: boolean; lazy_loading: MCPLazyLoadingConfig }> {
  const { data } = await api.patch<{ success: boolean; lazy_loading: MCPLazyLoadingConfig }>(
    '/studios/framework/mcp/lazy-loading',
    patch,
  );
  return data;
}

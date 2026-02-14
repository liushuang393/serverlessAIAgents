/**
 * AgentFlow Platform - 型定義.
 *
 * バックエンド /api/apps/* のレスポンスに対応する TypeScript 型。
 */

/** App ヘルスステータス */
export type AppStatus = 'healthy' | 'unhealthy' | 'unknown' | 'stopped';

/** ポート設定 */
export interface PortsConfig {
  api: number | null;
  frontend: number | null;
  db: number | null;
  redis: number | null;
}

/** Agent メタデータ */
export interface AgentInfo {
  name: string;
  module: string | null;
  capabilities: string[];
}

/** エントリーポイント設定 */
export interface EntryPointsConfig {
  api_module: string | null;
  health: string | null;
}

/** 依存設定 */
export interface DependenciesConfig {
  database: string | null;
  redis: boolean;
  external: string[];
}

/** App 一覧アイテム（GET /api/apps 用） */
export interface AppListItem {
  name: string;
  display_name: string;
  version: string;
  icon: string;
  status: AppStatus;
  ports: PortsConfig;
  agent_count: number;
  tags: string[];
  urls?: {
    backend: string | null;
    frontend: string | null;
    health: string | null;
    database: string | null;
  };
  visibility?: {
    mode: 'private' | 'public' | 'tenant_allowlist';
    tenants: string[];
  };
}

/** App 詳細（GET /api/apps/{name} 用） */
export interface AppDetail {
  name: string;
  display_name: string;
  description: string;
  version: string;
  icon: string;
  status: AppStatus;
  ports: PortsConfig;
  entry_points: EntryPointsConfig;
  agents: AgentInfo[];
  services: Record<string, unknown>;
  dependencies: DependenciesConfig;
  visibility?: {
    mode: 'private' | 'public' | 'tenant_allowlist';
    tenants: string[];
  };
  blueprint?: {
    engine_pattern: string;
    flow_pattern: string | null;
    system_prompt: string;
    llm_provider?: string | null;
    llm_base_url?: string | null;
    llm_api_key_env?: string | null;
    default_model: string | null;
    default_skills: string[];
    vector_db_provider?: string | null;
    vector_db_url?: string | null;
    vector_db_collection?: string | null;
    vector_db_api_key_env?: string | null;
    mcp_servers: string[];
  };
  tags: string[];
  urls?: {
    backend: string | null;
    frontend: string | null;
    health: string | null;
    database: string | null;
  };
}

/** ヘルスチェック結果（GET /api/apps/{name}/health 用） */
export interface HealthCheckAttempt {
  url: string;
  http_status?: number;
  response_time_ms?: number;
  payload_state?: string;
  error?: string;
  message?: string;
}

export interface DockerServiceStatus {
  service: string;
  state: string;
  health: string;
  running: boolean;
  published_ports: number[];
}

export interface DockerRuntimeDetails {
  detected: boolean;
  available: boolean;
  command?: string;
  error?: string;
  compose_files?: string[];
  backend_service?: string | null;
  backend_running?: boolean;
  backend_published_ports?: number[];
  services?: DockerServiceStatus[];
}

export interface HealthCheckDetails {
  checked_url?: string;
  http_status?: number;
  payload_state?: string | null;
  payload?: unknown;
  attempts?: HealthCheckAttempt[];
  candidate_urls?: string[];
  health_paths?: string[];
  docker?: DockerRuntimeDetails;
}

export interface HealthCheckResult {
  app_name: string;
  status: AppStatus;
  response_time_ms: number;
  checked_at: string;
  error?: string;
  details?: HealthCheckDetails;
}

/** App 操作結果（publish/start/stop） */
export interface AppActionResponse {
  app_name: string;
  action: 'publish' | 'start' | 'stop' | string;
  success: boolean;
  command: string;
  cwd: string;
  return_code: number | null;
  stdout?: string;
  stderr?: string;
  error?: string;
  health?: HealthCheckResult;
}

/** App 一覧レスポンス */
export interface AppListResponse {
  apps: AppListItem[];
  total: number;
}

/** サマリーレスポンス */
export interface AppSummaryResponse {
  total_apps: number;
  total_agents: number;
  apps: Array<{
    name: string;
    display_name: string;
    agent_count: number;
    has_api: boolean;
  }>;
}

/** リフレッシュレスポンス */
export interface RefreshResponse {
  discovered: number;
  new: string[];
  removed: string[];
  unchanged: string[];
  errors: Record<string, string>;
}

/* ============================================================
 * Phase 3: Agent / Skill / RAG 型定義
 * ============================================================ */

/** 集約済み Agent（GET /api/agents 用） */
export interface AggregatedAgent {
  name: string;
  app_name: string;
  app_display_name: string;
  app_icon: string;
  module: string | null;
  capabilities: string[];
}

/** Agent 一覧レスポンス */
export interface AgentListResponse {
  agents: AggregatedAgent[];
  total: number;
}

/** Agent 統計レスポンス */
export interface AgentStatsResponse {
  total_agents: number;
  total_apps_with_agents: number;
  total_capabilities: number;
}

/** Capability タグ情報 */
export interface CapabilityTag {
  tag: string;
  count: number;
  apps: string[];
}

/** Capabilities レスポンス */
export interface CapabilitiesResponse {
  capabilities: CapabilityTag[];
  total: number;
}

/** Agent by-app グループ */
export interface AgentGroup {
  app_name: string;
  display_name: string;
  icon: string;
  agents: AggregatedAgent[];
}

/** Agent by-app レスポンス */
export interface AgentsByAppResponse {
  groups: AgentGroup[];
  total_apps: number;
}

/** Skill 情報 */
export interface SkillInfo {
  name: string;
  label: string;
  version: string;
  description: string;
  author: string;
  tags: string[];
  triggers: string[];
  path: string;
}

/** Skill 一覧レスポンス */
export interface SkillListResponse {
  skills: SkillInfo[];
  total: number;
}

/** Skill 統計レスポンス */
export interface SkillStatsResponse {
  total_skills: number;
  total_tags: number;
  total_triggers: number;
}

/** タグ情報 */
export interface TagInfo {
  tag: string;
  count: number;
}

/** タグ一覧レスポンス */
export interface TagsResponse {
  tags: TagInfo[];
  total: number;
}

/** RAG チャンキング戦略 */
export interface ChunkStrategy {
  name: string;
  label: string;
  description: string;
}

/** RAG リランカー */
export interface Reranker {
  name: string;
  label: string;
  description: string;
}

/** RAG 検索方式 */
export interface RetrievalMethod {
  name: string;
  label: string;
  description: string;
}

/** RAG 推奨パターン */
export interface RAGPattern {
  name: string;
  label: string;
  description: string;
  config: {
    chunk_strategy: string;
    chunk_size: number;
    chunk_overlap: number;
    retrieval_method: string;
    reranker: string | null;
    top_k: number;
    score_threshold: number | null;
  };
}

/** RAG データソース */
export interface RAGDataSource {
  type: string;
  uri: string;
  label?: string;
  enabled?: boolean;
  schedule?: string | null;
  options?: Record<string, unknown>;
}

/** App 単位 RAG 設定 */
export interface AppRAGConfig {
  app_name: string;
  display_name: string;
  icon: string;
  config_path: string;
  rag: {
    enabled: boolean;
    pattern: string | null;
    vector_provider: string | null;
    vector_url: string | null;
    vector_collection: string | null;
    embedding_model: string | null;
    chunk_strategy: string;
    chunk_size: number;
    chunk_overlap: number;
    retrieval_method: string;
    reranker: string | null;
    top_k: number;
    score_threshold: number | null;
    indexing_schedule: string | null;
    data_sources: RAGDataSource[];
  };
}

/** RAG 設定更新 */
export interface AppRAGConfigPatchRequest {
  enabled?: boolean;
  pattern?: string | null;
  vector_provider?: string | null;
  vector_url?: string | null;
  vector_collection?: string | null;
  embedding_model?: string | null;
  chunk_strategy?: string;
  chunk_size?: number;
  chunk_overlap?: number;
  retrieval_method?: string;
  reranker?: string | null;
  top_k?: number;
  score_threshold?: number | null;
  indexing_schedule?: string | null;
  data_sources?: RAGDataSource[];
}

/** RAG 使用 App */
export interface RAGAppInfo {
  app_name: string;
  display_name: string;
  icon: string;
  rag_details: string[];
}

/** RAG 概要レスポンス */
export interface RAGOverviewResponse {
  description: string;
  chunk_strategies: ChunkStrategy[];
  rerankers: Reranker[];
  retrieval_methods?: RetrievalMethod[];
  patterns?: RAGPattern[];
  apps_using_rag: RAGAppInfo[];
  stats: RAGStatsResponse;
}

/** RAG 統計レスポンス */
export interface RAGStatsResponse {
  total_strategies: number;
  total_rerankers: number;
  total_apps_using_rag: number;
}

/* ============================================================
 * Provisioning / MCP 型定義
 * ============================================================ */

export type EnginePattern =
  | 'simple'
  | 'flow'
  | 'pipeline'
  | 'coordinator'
  | 'deep_agent';

export type DatabaseKind = 'none' | 'sqlite' | 'postgresql';
export type VectorDatabaseKind =
  | 'none'
  | 'qdrant'
  | 'pinecone'
  | 'weaviate'
  | 'pgvector'
  | 'milvus';
export type LLMProviderKind =
  | 'auto'
  | 'openai'
  | 'anthropic'
  | 'gemini'
  | 'azure_openai'
  | 'ollama'
  | 'openrouter'
  | 'custom';

export interface AgentBlueprintInput {
  name: string;
  role: string;
  prompt: string;
  capabilities: string[];
}

export interface AppCreateRequest {
  name: string;
  display_name: string;
  description: string;
  icon: string;
  engine_pattern: EnginePattern;
  flow_pattern: string | null;
  system_prompt: string;
  database: DatabaseKind;
  vector_database: VectorDatabaseKind;
  frontend_enabled: boolean;
  redis_enabled: boolean;
  rag_enabled: boolean;
  llm_provider: LLMProviderKind;
  default_model: string | null;
  llm_base_url: string | null;
  llm_api_key: string | null;
  llm_api_key_env: string | null;
  vector_db_url: string | null;
  vector_db_collection: string | null;
  vector_db_api_key: string | null;
  vector_db_api_key_env: string | null;
  write_framework_env: boolean;
  framework_env_file: string;
  default_skills: string[];
  mcp_servers: string[];
  tenant_visibility_mode: 'private' | 'public' | 'tenant_allowlist';
  tenant_ids: string[];
  agents: AgentBlueprintInput[];
}

export interface AppCreateResponse {
  success: boolean;
  app_name: string;
  app_dir: string;
  config_path: string;
  ports: PortsConfig;
  files_created: string[];
  framework_env_file?: string | null;
  framework_env_updated_keys?: string[];
  app_config: Record<string, unknown>;
}

export interface AppCreateOptionsResponse {
  engine_patterns: Array<{
    value: EnginePattern;
    label: string;
    description: string;
  }>;
  database_options: Array<{
    value: DatabaseKind;
    label: string;
  }>;
  vector_database_options: Array<{
    value: VectorDatabaseKind;
    label: string;
  }>;
  llm_provider_options: Array<{
    value: LLMProviderKind;
    label: string;
  }>;
  visibility_modes: Array<{
    value: 'private' | 'public' | 'tenant_allowlist';
    label: string;
  }>;
}

export interface PortConflictItem {
  port_type: 'api' | 'frontend' | 'db' | 'redis';
  port: number;
  apps: string[];
}

export interface PortConflictReport {
  has_conflicts: boolean;
  conflicts: PortConflictItem[];
}

export interface MCPServerConfig {
  name: string;
  command: string;
  args: string[];
  env: Record<string, string>;
  enabled: boolean;
  description: string;
}

export interface MCPLazyLoadingConfig {
  enabled: boolean;
  threshold: number;
  auto_load_on_call: boolean;
  cache_session: boolean;
}

export interface MCPConfigResponse {
  config_path: string;
  config: {
    servers: MCPServerConfig[];
    lazy_loading: MCPLazyLoadingConfig;
  };
}

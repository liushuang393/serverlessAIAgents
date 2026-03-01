/**
 * AgentFlow Platform - 型定義.
 *
 * バックエンド /api/studios/framework/apps/* のレスポンスに対応する TypeScript 型。
 */

/** App ヘルスステータス */
export type AppStatus = 'healthy' | 'unhealthy' | 'unknown' | 'stopped';

/** ランタイムURL */
export interface RuntimeUrls {
  backend: string | null;
  frontend: string | null;
  health: string | null;
  database: string | null;
}

/** DB 接続情報 */
export interface RuntimeDatabase {
  kind: string | null;
  url: string | null;
  host: string | null;
  port: number | null;
  name: string | null;
  user: string | null;
  password: string | null;
  password_env: string | null;
  note: string | null;
}

/** 実行コマンド情報 */
export interface RuntimeCommands {
  backend_dev: string | null;
  frontend_dev: string | null;
  publish: string | null;
  start: string | null;
  stop: string | null;
}

export interface RuntimeCLIAuth {
  status: string[] | null;
  api_key_env: string | null;
  api_key_login: string[] | null;
  interactive_login: string[] | null;
}

export interface RuntimeCLITool {
  executable: string | null;
  install_commands: string[][];
  auth: RuntimeCLIAuth;
  diagnostic_mode: 'read_only' | 'plan';
  diagnostic_command: string[] | null;
}

export interface RuntimeCLIConfig {
  preferred: Array<'codex' | 'claude'>;
  codex: RuntimeCLITool;
  claude: RuntimeCLITool;
}

/** ランタイム設定 */
export interface RuntimeConfig {
  urls: RuntimeUrls;
  database: RuntimeDatabase;
  commands: RuntimeCommands;
  cli: RuntimeCLIConfig;
}

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

/** App 一覧アイテム（GET /api/studios/framework/apps 用） */
export interface AppListItem {
  name: string;
  display_name: string;
  description?: string;
  version: string;
  icon: string;
  status: AppStatus;
  ports: PortsConfig;
  agent_count: number;
  tags: string[];
  urls?: RuntimeUrls;
  runtime?: RuntimeConfig;
  visibility?: {
    mode: 'private' | 'public' | 'tenant_allowlist';
    tenants: string[];
  };
  business_base: BusinessBaseKind;
}

/** App 詳細（GET /api/studios/framework/apps/{name} 用） */
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
  urls?: RuntimeUrls;
  runtime?: RuntimeConfig;
}

/** ヘルスチェック結果（GET /api/studios/framework/apps/{name}/health 用） */
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
  components?: {
    frontend?: {
      required: boolean;
      healthy: boolean;
      status: AppStatus | 'skipped';
      message?: string;
      port?: number | null;
    };
    backend?: {
      required: boolean;
      healthy: boolean;
      status: AppStatus | 'skipped';
      message?: string;
    };
    database?: {
      required: boolean;
      healthy: boolean;
      status: AppStatus | 'skipped';
      message?: string;
      kind?: string | null;
      port?: number | null;
    };
  };
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
  command_source: string;
  cwd: string;
  return_code: number | null;
  stdout?: string;
  stderr?: string;
  error?: string;
  health?: HealthCheckResult;
  diagnostic?: ActionDiagnostic;
}

export interface ActionDiagnostic {
  tool?: string | null;
  setup?: {
    ready: boolean;
    available_tools: string[];
    authenticated_tools: string[];
  };
  summary?: string;
  recommendations?: string[];
  raw_output?: string;
  command_source?: string;
  diagnostic_success?: boolean;
  diagnostic_command?: string | null;
  diagnostic_error?: string | null;
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

/** 集約済み Agent（GET /api/studios/framework/agents 用） */
export interface CanonicalCapability {
  id: string;
  domain: string;
  task: string;
  qualifier: string | null;
  label: string;
  aliases: string[];
}

export interface AggregatedAgent {
  name: string;
  app_name: string;
  app_display_name: string;
  app_icon: string;
  module: string | null;
  capabilities: CanonicalCapability[];
  capabilities_legacy: string[];
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
  id: string;
  domain: string;
  task: string;
  qualifier: string | null;
  label: string;
  aliases: string[];
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

/** Agent by-pattern グループ */
export interface AgentPatternGroup {
  pattern: string;
  count: number;
  agents: AggregatedAgent[];
}

/** Agent by-pattern レスポンス */
export interface AgentsByPatternResponse {
  groups: AgentPatternGroup[];
  total_groups: number;
}

/** Agent by-business-base グループ */
export interface AgentBusinessBaseGroup {
  business_base: string;
  count: number;
  agents: AggregatedAgent[];
}

/** Agent by-business-base レスポンス */
export interface AgentsByBusinessBaseResponse {
  groups: AgentBusinessBaseGroup[];
  total_groups: number;
}

/** Skill 情報 */
export interface SkillInfo {
  name: string;
  label: string;
  version: string;
  description: string;
  author: string;
  tags: string[];
  tags_legacy?: string[];
  triggers: string[];
  requirements?: string[];
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
  id: string;
  type: string;
  uri?: string | null;
  label?: string;
  enabled?: boolean;
  schedule?: string | null;
  options?: Record<string, unknown>;
}

/** App の DB 自動検出ヒント */
export interface RAGDatabaseHint {
  available: boolean;
  kind: string | null;
  uri: string | null;
  host: string | null;
  port: number | null;
  database: string | null;
  user: string | null;
  source: string;
  sample_uri: string;
  sample_label: string;
  message: string;
}

/** RAG Data Source 向け DB 種別 */
export interface RAGDatabaseTypeOption {
  name: string;
  label: string;
  dialect: string;
  connection_kind: 'network' | 'file';
  default_port: number | null;
  sample_uri: string;
}

/** RAG Vector Provider 選択肢 */
export interface RAGVectorProviderOption {
  name: string;
  label: string;
}

/** App 単位 RAG 設定 */
export interface AppRAGConfig {
  app_name: string;
  display_name: string;
  icon: string;
  config_path: string;
  db_hint?: RAGDatabaseHint;
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

export interface AppRAGHotApplyResult {
  mode: 'hot';
  applied: boolean;
  subscriber_count: number;
}

export interface AppRAGConfigUpdateResponse extends AppRAGConfig {
  contracts_rag?: Record<string, unknown>;
  config_version?: string;
  updated_at?: string;
  hot_apply?: AppRAGHotApplyResult;
}

export interface RAGIngestRunSummary {
  run_id: string;
  status: string;
  trigger_mode?: string;
  dry_run: boolean;
  started_at: string | null;
  finished_at: string | null;
  duration_ms: number;
  summary?: Record<string, unknown>;
}

export interface RAGIngestRunsResponse {
  total: number;
  runs: RAGIngestRunSummary[];
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
  database_types?: RAGDatabaseTypeOption[];
  vector_providers?: RAGVectorProviderOption[];
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
export type ProductLineKind = 'migration' | 'faq' | 'assistant' | 'framework';
export type SurfaceProfileKind = 'business' | 'developer' | 'operator';
export type AuditProfileKind = 'business' | 'developer';
export type SecurityModeKind = 'read_only' | 'approval_required' | 'autonomous';
export type RiskLevelKind = 'low' | 'medium' | 'high';
export type EvolutionScopeLevel = 'tenant_app' | 'tenant_product_line' | 'global_verified';
export type EvolutionValidatorBackend = 'redis_stream' | 'none';
export type BusinessBaseKind =
  | 'platform'
  | 'knowledge'
  | 'reasoning'
  | 'interaction'
  | 'integration'
  | 'operations'
  | 'governance'
  | 'media'
  | 'custom';

export interface AgentBlueprintInput {
  name: string;
  role: string;
  prompt: string;
  capabilities: string[];
}

export interface PluginBindingInput {
  id: string;
  version: string;
  config: Record<string, unknown>;
}

export interface EvolutionValidatorQueueInput {
  backend: EvolutionValidatorBackend;
  redis_url: string | null;
  stream_key: string;
  consumer_group: string;
  max_retries: number;
}

export interface EvolutionRetrievalInput {
  high_confidence_skip_threshold: number;
  high_complexity_threshold: number;
  low_confidence_threshold: number;
}

export interface EvolutionSuspicionInput {
  max_age_days: number;
  failure_streak_threshold: number;
  performance_drop_ratio: number;
}

export interface EvolutionConfigInput {
  enabled: boolean;
  strategy_service_url: string | null;
  validator_queue: EvolutionValidatorQueueInput;
  scope_policy: EvolutionScopeLevel[];
  retrieval: EvolutionRetrievalInput;
  suspicion: EvolutionSuspicionInput;
}

export interface AppCreateRequest {
  name: string;
  display_name: string;
  description: string;
  icon: string;
  business_base: BusinessBaseKind;
  product_line: ProductLineKind;
  surface_profile: SurfaceProfileKind;
  audit_profile: AuditProfileKind;
  security_mode: SecurityModeKind | null;
  evolution: EvolutionConfigInput | null;
  plugin_bindings: PluginBindingInput[];
  template: string | null;
  data_sources: string[];
  permission_scopes: string[];
  risk_level: RiskLevelKind | null;
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
  surface_profile?: SurfaceProfileKind;
  templates?: Array<{
    id: string;
    studio: ProductLineKind;
    label: string;
    description: string;
  }>;
  data_source_options?: Array<{
    value: string;
    label: string;
  }>;
  permission_scopes?: Array<{
    value: string;
    label: string;
  }>;
  risk_levels?: Array<{
    value: RiskLevelKind;
    label: string;
  }>;
  security_modes?: Array<{
    value: SecurityModeKind;
    label: string;
  }>;
  engine_patterns?: Array<{
    value: EnginePattern;
    label: string;
    description: string;
  }>;
  database_options?: Array<{
    value: DatabaseKind;
    label: string;
  }>;
  vector_database_options?: Array<{
    value: VectorDatabaseKind;
    label: string;
  }>;
  llm_provider_options?: Array<{
    value: LLMProviderKind;
    label: string;
  }>;
  visibility_modes?: Array<{
    value: 'private' | 'public' | 'tenant_allowlist';
    label: string;
  }>;
  evolution_scope_options?: Array<{
    value: EvolutionScopeLevel;
    label: string;
  }>;
  evolution_validator_backends?: Array<{
    value: EvolutionValidatorBackend;
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

export interface ManifestMigrationItem {
  name: string;
  updated_fields: string[];
}

export interface ManifestMigrationReport {
  total: number;
  changed: number;
  unchanged: number;
  dry_run: boolean;
  apps: ManifestMigrationItem[];
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

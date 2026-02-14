import { useEffect, useMemo, useState } from 'react';
import { createApp, fetchAppCreateOptions } from '@/api/client';
import type {
  AppCreateOptionsResponse,
  AppCreateRequest,
  AppCreateResponse,
  DatabaseKind,
  EnginePattern,
  LLMProviderKind,
  VectorDatabaseKind,
} from '@/types';

interface Props {
  open: boolean;
  onClose: () => void;
  onCreated: (created: AppCreateResponse) => void;
}

function splitCSV(value: string): string[] {
  return value
    .split(',')
    .map((v) => v.trim())
    .filter((v) => v.length > 0);
}

export function AppCreateModal({ open, onClose, onCreated }: Props) {
  const [options, setOptions] = useState<AppCreateOptionsResponse | null>(null);
  const [loadingOptions, setLoadingOptions] = useState(false);
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const [name, setName] = useState('');
  const [displayName, setDisplayName] = useState('');
  const [description, setDescription] = useState('');
  const [icon, setIcon] = useState('📦');
  const [enginePattern, setEnginePattern] = useState<EnginePattern>('flow');
  const [flowPattern, setFlowPattern] = useState('');
  const [database, setDatabase] = useState<DatabaseKind>('postgresql');
  const [vectorDatabase, setVectorDatabase] = useState<VectorDatabaseKind>('none');
  const [frontendEnabled, setFrontendEnabled] = useState(true);
  const [redisEnabled, setRedisEnabled] = useState(false);
  const [ragEnabled, setRagEnabled] = useState(false);
  const [llmProvider, setLlmProvider] = useState<LLMProviderKind>('auto');
  const [defaultModel, setDefaultModel] = useState('');
  const [llmBaseUrl, setLlmBaseUrl] = useState('');
  const [llmApiKey, setLlmApiKey] = useState('');
  const [llmApiKeyEnv, setLlmApiKeyEnv] = useState('');
  const [vectorDbUrl, setVectorDbUrl] = useState('');
  const [vectorDbCollection, setVectorDbCollection] = useState('');
  const [vectorDbApiKey, setVectorDbApiKey] = useState('');
  const [vectorDbApiKeyEnv, setVectorDbApiKeyEnv] = useState('');
  const [writeFrameworkEnv, setWriteFrameworkEnv] = useState(true);
  const [frameworkEnvFile, setFrameworkEnvFile] = useState('.env');
  const [skillsInput, setSkillsInput] = useState('');
  const [mcpServersInput, setMcpServersInput] = useState('');
  const [visibilityMode, setVisibilityMode] = useState<'private' | 'public' | 'tenant_allowlist'>('private');
  const [tenantIdsInput, setTenantIdsInput] = useState('');
  const [agentName, setAgentName] = useState('PrimaryAgent');
  const [agentRole, setAgentRole] = useState('specialist');
  const [agentCapabilitiesInput, setAgentCapabilitiesInput] = useState('assistant');
  const [agentPrompt, setAgentPrompt] = useState('ユーザーの要求を理解し、実行可能な回答を返してください。');
  const [systemPrompt, setSystemPrompt] = useState('あなたは業務特化の AI アシスタントです。');

  const disabledSubmit = useMemo(() => {
    return submitting || name.trim() === '' || displayName.trim() === '';
  }, [submitting, name, displayName]);

  useEffect(() => {
    if (!open) {
      return;
    }
    setError(null);
    setLoadingOptions(true);
    fetchAppCreateOptions()
      .then((res) => {
        setOptions(res);
      })
      .catch((err) => {
        const msg = err instanceof Error ? err.message : '作成オプションの取得に失敗しました';
        setError(msg);
      })
      .finally(() => {
        setLoadingOptions(false);
      });
  }, [open]);

  if (!open) {
    return null;
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError(null);
    setSubmitting(true);

    const request: AppCreateRequest = {
      name: name.trim(),
      display_name: displayName.trim(),
      description: description.trim(),
      icon: icon.trim() || '📦',
      engine_pattern: enginePattern,
      flow_pattern: flowPattern.trim() || null,
      system_prompt: systemPrompt.trim(),
      database,
      vector_database: vectorDatabase,
      frontend_enabled: frontendEnabled,
      redis_enabled: redisEnabled,
      rag_enabled: ragEnabled,
      llm_provider: llmProvider,
      default_model: defaultModel.trim() || null,
      llm_base_url: llmBaseUrl.trim() || null,
      llm_api_key: llmApiKey.trim() || null,
      llm_api_key_env: llmApiKeyEnv.trim() || null,
      vector_db_url: vectorDbUrl.trim() || null,
      vector_db_collection: vectorDbCollection.trim() || null,
      vector_db_api_key: vectorDbApiKey.trim() || null,
      vector_db_api_key_env: vectorDbApiKeyEnv.trim() || null,
      write_framework_env: writeFrameworkEnv,
      framework_env_file: frameworkEnvFile.trim() || '.env',
      default_skills: splitCSV(skillsInput),
      mcp_servers: splitCSV(mcpServersInput),
      tenant_visibility_mode: visibilityMode,
      tenant_ids: splitCSV(tenantIdsInput),
      agents: [
        {
          name: agentName.trim() || 'PrimaryAgent',
          role: agentRole.trim() || 'specialist',
          prompt: agentPrompt.trim(),
          capabilities: splitCSV(agentCapabilitiesInput),
        },
      ],
    };

    try {
      const created = await createApp(request);
      onCreated(created);
      onClose();
    } catch (err) {
      const msg = err instanceof Error ? err.message : 'App 作成に失敗しました';
      setError(msg);
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <div className="fixed inset-0 bg-slate-950 z-50 flex items-center justify-center p-4">
      <div className="w-full max-w-4xl max-h-[90vh] -translate-y-[10px] overflow-y-auto bg-slate-900 border border-slate-700 rounded-2xl p-6 shadow-2xl">
        <div className="flex items-start justify-between mb-4">
          <div>
            <h2 className="text-xl font-bold text-slate-100">APPを追加</h2>
            <p className="text-xs text-slate-500 mt-1">
              AgentFlow パターン・MCP・DB 設定をまとめて初期化します
            </p>
          </div>
          <button
            type="button"
            onClick={onClose}
            className="text-slate-400 hover:text-slate-200 text-sm"
          >
            閉じる
          </button>
        </div>

        {error && (
          <div className="mb-4 bg-red-500/10 border border-red-500/20 text-red-300 rounded-lg px-3 py-2 text-sm">
            {error}
          </div>
        )}

        <form onSubmit={handleSubmit} className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Field label="App 名 (snake_case)">
              <input
                value={name}
                onChange={(e) => setName(e.target.value)}
                className="input"
                placeholder="sales_assistant"
              />
            </Field>
            <Field label="表示名">
              <input
                value={displayName}
                onChange={(e) => setDisplayName(e.target.value)}
                className="input"
                placeholder="営業アシスタント"
              />
            </Field>
            <Field label="アイコン">
              <input value={icon} onChange={(e) => setIcon(e.target.value)} className="input" />
            </Field>
            <Field label="Engine Pattern">
              <select
                value={enginePattern}
                onChange={(e) => setEnginePattern(e.target.value as EnginePattern)}
                className="input"
              >
                {(options?.engine_patterns ?? []).map((item) => (
                  <option key={item.value} value={item.value}>
                    {item.label}
                  </option>
                ))}
              </select>
            </Field>
          </div>

          <Field label="説明">
            <input
              value={description}
              onChange={(e) => setDescription(e.target.value)}
              className="input"
              placeholder="業務ユースケースの説明"
            />
          </Field>

          <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
            <Field label="Flow Pattern (任意)">
              <input
                value={flowPattern}
                onChange={(e) => setFlowPattern(e.target.value)}
                className="input"
                placeholder="customer-support-flow"
              />
            </Field>
            <Field label="RDB">
              <select
                value={database}
                onChange={(e) => setDatabase(e.target.value as DatabaseKind)}
                className="input"
              >
                {(options?.database_options ?? []).map((item) => (
                  <option key={item.value} value={item.value}>
                    {item.label}
                  </option>
                ))}
              </select>
            </Field>
            <Field label="Vector DB">
              <select
                value={vectorDatabase}
                onChange={(e) => setVectorDatabase(e.target.value as VectorDatabaseKind)}
                className="input"
              >
                {(options?.vector_database_options ?? []).map((item) => (
                  <option key={item.value} value={item.value}>
                    {item.label}
                  </option>
                ))}
              </select>
            </Field>
            <Field label="LLM Provider">
              <select
                value={llmProvider}
                onChange={(e) => setLlmProvider(e.target.value as LLMProviderKind)}
                className="input"
              >
                {(options?.llm_provider_options ?? []).map((item) => (
                  <option key={item.value} value={item.value}>
                    {item.label}
                  </option>
                ))}
              </select>
            </Field>
            <Field label="Visibility">
              <select
                value={visibilityMode}
                onChange={(e) => setVisibilityMode(e.target.value as 'private' | 'public' | 'tenant_allowlist')}
                className="input"
              >
                {(options?.visibility_modes ?? []).map((item) => (
                  <option key={item.value} value={item.value}>
                    {item.label}
                  </option>
                ))}
              </select>
            </Field>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <Toggle label="Frontend" checked={frontendEnabled} onChange={setFrontendEnabled} />
            <Toggle label="Redis" checked={redisEnabled} onChange={setRedisEnabled} />
            <Toggle label="RAG" checked={ragEnabled} onChange={setRagEnabled} />
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Field label="Default Model">
              <input
                value={defaultModel}
                onChange={(e) => setDefaultModel(e.target.value)}
                className="input"
                placeholder="gpt-4.1-mini"
              />
            </Field>
            <Field label="LLM Base URL (任意)">
              <input
                value={llmBaseUrl}
                onChange={(e) => setLlmBaseUrl(e.target.value)}
                className="input"
                placeholder="https://api.openai.com/v1"
              />
            </Field>
            <Field label="LLM API Key (任意)">
              <input
                type="password"
                value={llmApiKey}
                onChange={(e) => setLlmApiKey(e.target.value)}
                className="input"
                placeholder="sk-..."
              />
            </Field>
            <Field label="LLM API Key ENV 名 (任意)">
              <input
                value={llmApiKeyEnv}
                onChange={(e) => setLlmApiKeyEnv(e.target.value.toUpperCase())}
                className="input"
                placeholder="OPENAI_API_KEY"
              />
            </Field>
            <Field label="Vector DB URL (任意)">
              <input
                value={vectorDbUrl}
                onChange={(e) => setVectorDbUrl(e.target.value)}
                className="input"
                placeholder="http://localhost:6333"
              />
            </Field>
            <Field label="Vector Collection (任意)">
              <input
                value={vectorDbCollection}
                onChange={(e) => setVectorDbCollection(e.target.value)}
                className="input"
                placeholder={`${name || 'app'}_knowledge`}
              />
            </Field>
            <Field label="Vector DB API Key (任意)">
              <input
                type="password"
                value={vectorDbApiKey}
                onChange={(e) => setVectorDbApiKey(e.target.value)}
                className="input"
                placeholder="vector-key"
              />
            </Field>
            <Field label="Vector DB API Key ENV 名 (任意)">
              <input
                value={vectorDbApiKeyEnv}
                onChange={(e) => setVectorDbApiKeyEnv(e.target.value.toUpperCase())}
                className="input"
                placeholder="QDRANT_API_KEY"
              />
            </Field>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Field label="Default Skills (CSV)">
              <input
                value={skillsInput}
                onChange={(e) => setSkillsInput(e.target.value)}
                className="input"
                placeholder="rag,conversation_export"
              />
            </Field>
            <Field label="MCP Servers (CSV)">
              <input
                value={mcpServersInput}
                onChange={(e) => setMcpServersInput(e.target.value)}
                className="input"
                placeholder="filesystem,git"
              />
            </Field>
            <Field label="Tenant IDs (CSV)">
              <input
                value={tenantIdsInput}
                onChange={(e) => setTenantIdsInput(e.target.value)}
                className="input"
                placeholder="tenant-a,tenant-b"
              />
            </Field>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Toggle
              label="Framework .env に反映"
              checked={writeFrameworkEnv}
              onChange={setWriteFrameworkEnv}
            />
            <Field label="Framework env ファイル">
              <input
                value={frameworkEnvFile}
                onChange={(e) => setFrameworkEnvFile(e.target.value)}
                className="input"
                placeholder=".env"
              />
            </Field>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <Field label="Primary Agent 名">
              <input
                value={agentName}
                onChange={(e) => setAgentName(e.target.value)}
                className="input"
              />
            </Field>
            <Field label="Primary Agent ロール">
              <input
                value={agentRole}
                onChange={(e) => setAgentRole(e.target.value)}
                className="input"
              />
            </Field>
            <Field label="Primary Agent Capability (CSV)">
              <input
                value={agentCapabilitiesInput}
                onChange={(e) => setAgentCapabilitiesInput(e.target.value)}
                className="input"
              />
            </Field>
          </div>

          <Field label="System Prompt">
            <textarea
              value={systemPrompt}
              onChange={(e) => setSystemPrompt(e.target.value)}
              className="input min-h-24"
            />
          </Field>

          <Field label="Primary Agent Prompt">
            <textarea
              value={agentPrompt}
              onChange={(e) => setAgentPrompt(e.target.value)}
              className="input min-h-24"
            />
          </Field>

          <div className="pt-2 flex items-center justify-end gap-3">
            <button
              type="button"
              onClick={onClose}
              className="px-4 py-2 rounded-lg border border-slate-700 text-slate-300 hover:bg-slate-800"
            >
              キャンセル
            </button>
            <button
              type="submit"
              disabled={disabledSubmit || loadingOptions}
              className="px-4 py-2 rounded-lg bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 text-white"
            >
              {submitting ? '作成中...' : '作成する'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}

function Field({ label, children }: { label: string; children: React.ReactNode }) {
  return (
    <label className="block space-y-1.5">
      <span className="text-xs text-slate-400">{label}</span>
      {children}
    </label>
  );
}

function Toggle({
  label,
  checked,
  onChange,
}: {
  label: string;
  checked: boolean;
  onChange: (value: boolean) => void;
}) {
  return (
    <label className="flex items-center justify-between rounded-lg border border-slate-700 bg-slate-950/40 px-3 py-2">
      <span className="text-sm text-slate-300">{label}</span>
      <button
        type="button"
        onClick={() => onChange(!checked)}
        className={`w-10 h-6 rounded-full transition-colors ${checked ? 'bg-indigo-600' : 'bg-slate-700'}`}
      >
        <span
          className={`block w-4 h-4 bg-white rounded-full mt-1 transition-transform ${checked ? 'translate-x-5' : 'translate-x-1'}`}
        />
      </button>
    </label>
  );
}

/**
 * RAGOverview - RAG 機能概要 + App 単位設定管理.
 */

import { useEffect, useMemo, useState, type ReactNode } from 'react';
import {
  fetchAppRAGConfigs,
  fetchRAGPatterns,
  patchAppRAGConfig,
} from '@/api/client';
import type {
  AppRAGConfig,
  RAGDataSource,
  RAGDatabaseTypeOption,
  RAGDatabaseHint,
  RAGPattern,
  RAGVectorProviderOption,
} from '@/types';
import { useAppStore } from '@/store/useAppStore';

type DataSourceType = 'web' | 'file' | 'database' | 'api' | 's3';

interface DataSourceTypeOption {
  value: DataSourceType;
  label: string;
  description: string;
  placeholder: string;
}

interface DataSourceDraft {
  id: string;
  type: DataSourceType;
  uri: string;
  label: string;
  enabled: boolean;
  schedule: string;
  options: Record<string, unknown>;
}

interface RAGFormState {
  enabled: boolean;
  pattern: string;
  vector_provider: string;
  vector_url: string;
  vector_collection: string;
  embedding_model: string;
  chunk_strategy: string;
  chunk_size: number;
  chunk_overlap: number;
  retrieval_method: string;
  reranker: string;
  top_k: number;
  score_threshold: string;
  indexing_schedule: string;
  data_sources: DataSourceDraft[];
}

const DEFAULT_DATABASE_TYPE_OPTIONS: RAGDatabaseTypeOption[] = [
  {
    name: 'postgresql',
    label: 'PostgreSQL',
    dialect: 'postgresql',
    connection_kind: 'network',
    default_port: 5432,
    sample_uri: 'postgresql+asyncpg://user:password@localhost:5432/app_db',
  },
  {
    name: 'mysql',
    label: 'MySQL',
    dialect: 'mysql',
    connection_kind: 'network',
    default_port: 3306,
    sample_uri: 'mysql+aiomysql://user:password@localhost:3306/app_db',
  },
  {
    name: 'sqlite',
    label: 'SQLite',
    dialect: 'sqlite',
    connection_kind: 'file',
    default_port: null,
    sample_uri: 'sqlite+aiosqlite:///./app.db',
  },
  {
    name: 'mssql',
    label: 'SQL Server',
    dialect: 'mssql',
    connection_kind: 'network',
    default_port: 1433,
    sample_uri: 'mssql+pyodbc://user:password@localhost:1433/app_db?driver=ODBC+Driver+18+for+SQL+Server',
  },
];

const DEFAULT_VECTOR_PROVIDER_OPTIONS: RAGVectorProviderOption[] = [
  { name: 'qdrant', label: 'Qdrant' },
  { name: 'pinecone', label: 'Pinecone' },
  { name: 'weaviate', label: 'Weaviate' },
  { name: 'pgvector', label: 'PostgreSQL (pgvector)' },
  { name: 'milvus', label: 'Milvus' },
];

const DATA_SOURCE_TYPE_OPTIONS: DataSourceTypeOption[] = [
  {
    value: 'web',
    label: 'Web',
    description: 'クロール対象のページ URL を登録',
    placeholder: 'https://example.com/docs',
  },
  {
    value: 'file',
    label: 'File',
    description: 'ローカルファイルや共有パスを登録',
    placeholder: '/data/knowledge/faq.md',
  },
  {
    value: 'database',
    label: 'Database',
    description: 'SQL DB 接続情報と対象テーブル/クエリを登録',
    placeholder: 'postgresql+asyncpg://user:password@localhost:5432/app_db',
  },
  {
    value: 'api',
    label: 'API',
    description: '外部 API の取得先を登録',
    placeholder: 'https://api.example.com/v1/faq',
  },
  {
    value: 's3',
    label: 'S3',
    description: 'オブジェクトストレージを登録',
    placeholder: 's3://bucket/path',
  },
];

function createSourceId(): string {
  return `source-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
}

function normalizeDatabaseKind(value: string | null | undefined): string {
  const kind = String(value ?? '').trim().toLowerCase();
  if (!kind) {
    return '';
  }
  if (kind.includes('postgres')) {
    return 'postgresql';
  }
  if (kind.includes('sqlite')) {
    return 'sqlite';
  }
  if (kind.includes('mysql')) {
    return 'mysql';
  }
  if (kind.includes('mssql') || kind.includes('sqlserver') || kind.includes('sql_server')) {
    return 'mssql';
  }
  return kind;
}

function normalizeSourceType(value: string | null | undefined): DataSourceType {
  const type = String(value ?? '').trim().toLowerCase();
  if (type === 'file') return 'file';
  if (type === 'database' || type === 'db' || type === 'sql') return 'database';
  if (type === 'api') return 'api';
  if (type === 's3') return 's3';
  return 'web';
}

function cleanText(value: unknown): string {
  if (typeof value === 'string') {
    return value.trim();
  }
  if (typeof value === 'number' && Number.isFinite(value)) {
    return String(value);
  }
  return '';
}

function readOption(options: Record<string, unknown>, key: string): string {
  return cleanText(options[key]);
}

function compactOptions(options: Record<string, unknown>): Record<string, unknown> {
  const entries = Object.entries(options)
    .map(([key, value]) => [key, cleanText(value)] as const)
    .filter(([, value]) => value.length > 0);
  return Object.fromEntries(entries);
}

function createEmptySource(type: DataSourceType = 'web'): DataSourceDraft {
  return {
    id: createSourceId(),
    type,
    uri: '',
    label: '',
    enabled: true,
    schedule: '',
    options: {},
  };
}

function buildDatabaseSourceFromHint(
  appName: string,
  dbHint: RAGDatabaseHint | undefined,
): DataSourceDraft {
  const kind = normalizeDatabaseKind(dbHint?.kind) || 'postgresql';
  const uri = cleanText(dbHint?.uri) || cleanText(dbHint?.sample_uri);
  const label = cleanText(dbHint?.sample_label) || `${appName} SQL source`;
  const options: Record<string, unknown> = {
    dialect: kind,
    database_type: kind,
  };
  const databaseName = cleanText(dbHint?.database);
  const host = cleanText(dbHint?.host);
  const user = cleanText(dbHint?.user);
  if (databaseName) options.database = databaseName;
  if (host) options.host = host;
  if (user) options.user = user;
  if (typeof dbHint?.port === 'number') options.port = String(dbHint.port);
  return {
    id: createSourceId(),
    type: 'database',
    uri,
    label,
    enabled: true,
    schedule: '',
    options,
  };
}

function draftFromSource(source: RAGDataSource): DataSourceDraft {
  return {
    id: createSourceId(),
    type: normalizeSourceType(source.type),
    uri: cleanText(source.uri),
    label: cleanText(source.label ?? ''),
    enabled: source.enabled ?? true,
    schedule: cleanText(source.schedule ?? ''),
    options: typeof source.options === 'object' && source.options !== null ? source.options : {},
  };
}

function sourceToPayload(source: DataSourceDraft): RAGDataSource | null {
  const uri = cleanText(source.uri);
  if (!uri) {
    return null;
  }
  const label = cleanText(source.label);
  const schedule = cleanText(source.schedule);
  return {
    type: normalizeSourceType(source.type),
    uri,
    label,
    enabled: source.enabled,
    schedule: schedule || null,
    options: compactOptions(source.options),
  };
}

function sourcePlaceholder(type: DataSourceType, dbHint?: RAGDatabaseHint): string {
  if (type === 'database' && dbHint) {
    return cleanText(dbHint.uri) || cleanText(dbHint.sample_uri);
  }
  const option = DATA_SOURCE_TYPE_OPTIONS.find((item) => item.value === type);
  return option?.placeholder ?? '';
}

function findDatabaseTypeOption(
  options: RAGDatabaseTypeOption[],
  value: string,
  dbHint?: RAGDatabaseHint,
): RAGDatabaseTypeOption {
  const normalized = normalizeDatabaseKind(value);
  if (normalized) {
    const found = options.find((item) => item.name === normalized || item.dialect === normalized);
    if (found) {
      return found;
    }
  }
  const fallback = options[0] ?? DEFAULT_DATABASE_TYPE_OPTIONS[0];
  const hintKind = normalizeDatabaseKind(dbHint?.kind);
  if (hintKind) {
    const hinted = options.find((item) => item.name === hintKind || item.dialect === hintKind);
    if (hinted) {
      return hinted;
    }
  }
  if (fallback) {
    return fallback;
  }
  return {
    name: 'postgresql',
    label: 'PostgreSQL',
    dialect: 'postgresql',
    connection_kind: 'network',
    default_port: 5432,
    sample_uri: cleanText(dbHint?.sample_uri) || 'postgresql+asyncpg://user:password@localhost:5432/app_db',
  };
}

function toForm(config: AppRAGConfig): RAGFormState {
  const baseSources = config.rag.data_sources.map(draftFromSource);
  const dbHint = config.db_hint;
  const hasDatabaseSource = baseSources.some((source) => source.type === 'database');
  const dataSources =
    dbHint?.available && !hasDatabaseSource
      ? [buildDatabaseSourceFromHint(config.app_name, dbHint), ...baseSources]
      : baseSources;

  return {
    enabled: config.rag.enabled,
    pattern: config.rag.pattern ?? '',
    vector_provider: config.rag.vector_provider ?? '',
    vector_url: config.rag.vector_url ?? '',
    vector_collection: config.rag.vector_collection ?? '',
    embedding_model: config.rag.embedding_model ?? '',
    chunk_strategy: config.rag.chunk_strategy,
    chunk_size: config.rag.chunk_size,
    chunk_overlap: config.rag.chunk_overlap,
    retrieval_method: config.rag.retrieval_method,
    reranker: config.rag.reranker ?? '',
    top_k: config.rag.top_k,
    score_threshold:
      config.rag.score_threshold === null || config.rag.score_threshold === undefined
        ? ''
        : String(config.rag.score_threshold),
    indexing_schedule: config.rag.indexing_schedule ?? '',
    data_sources: dataSources,
  };
}

export function RAGOverview() {
  const { ragOverview, loading, error, loadRAGOverview, clearError } = useAppStore();
  const [ragConfigs, setRagConfigs] = useState<AppRAGConfig[]>([]);
  const [patterns, setPatterns] = useState<RAGPattern[]>([]);
  const [selectedApp, setSelectedApp] = useState('');
  const [form, setForm] = useState<RAGFormState | null>(null);
  const [saving, setSaving] = useState(false);
  const [managerLoading, setManagerLoading] = useState(false);
  const [managerError, setManagerError] = useState<string | null>(null);
  const [managerMessage, setManagerMessage] = useState<string | null>(null);

  const selectedConfig = useMemo(
    () => ragConfigs.find((item) => item.app_name === selectedApp) ?? null,
    [ragConfigs, selectedApp],
  );
  const databaseTypeOptions = useMemo(() => {
    const apiOptions = ragOverview?.database_types ?? [];
    const base = apiOptions.length > 0 ? apiOptions : DEFAULT_DATABASE_TYPE_OPTIONS;
    const normalizedBase = base.map((option) => ({
      ...option,
      name: normalizeDatabaseKind(option.name) || option.name,
      dialect: normalizeDatabaseKind(option.dialect) || option.dialect,
    }));
    const hintKind = normalizeDatabaseKind(selectedConfig?.db_hint?.kind);
    if (!hintKind) {
      return normalizedBase;
    }
    if (normalizedBase.some((option) => option.name === hintKind || option.dialect === hintKind)) {
      return normalizedBase;
    }
    const inferredConnectionKind: 'network' | 'file' = hintKind === 'sqlite' ? 'file' : 'network';
    return [
      ...normalizedBase,
      {
        name: hintKind,
        label: hintKind.toUpperCase(),
        dialect: hintKind,
        connection_kind: inferredConnectionKind,
        default_port: null,
        sample_uri:
          cleanText(selectedConfig?.db_hint?.sample_uri) ||
          'postgresql+asyncpg://user:password@localhost:5432/app_db',
      },
    ];
  }, [ragOverview?.database_types, selectedConfig?.db_hint]);
  const vectorProviderOptions = useMemo(() => {
    const apiOptions = ragOverview?.vector_providers ?? [];
    if (apiOptions.length > 0) {
      return apiOptions;
    }
    return DEFAULT_VECTOR_PROVIDER_OPTIONS;
  }, [ragOverview?.vector_providers]);

  useEffect(() => {
    loadRAGOverview();
    void loadManager();
  }, [loadRAGOverview]);

  useEffect(() => {
    if (!selectedConfig) {
      return;
    }
    setForm(toForm(selectedConfig));
  }, [selectedConfig]);

  const loadManager = async () => {
    setManagerLoading(true);
    setManagerError(null);
    try {
      const [configs, patternRes] = await Promise.all([
        fetchAppRAGConfigs(),
        fetchRAGPatterns(),
      ]);
      setRagConfigs(configs.apps);
      setPatterns(patternRes.patterns);

      if (!selectedApp && configs.apps.length > 0) {
        setSelectedApp(configs.apps[0].app_name);
      } else if (selectedApp && !configs.apps.some((item) => item.app_name === selectedApp)) {
        setSelectedApp(configs.apps[0]?.app_name ?? '');
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : 'RAG 設定一覧の取得に失敗しました';
      setManagerError(message);
    } finally {
      setManagerLoading(false);
    }
  };

  const applyPattern = (patternName: string) => {
    if (!form) {
      return;
    }
    const pattern = patterns.find((item) => item.name === patternName);
    if (!pattern) {
      setForm({ ...form, pattern: patternName });
      return;
    }
    setForm({
      ...form,
      pattern: patternName,
      chunk_strategy: pattern.config.chunk_strategy,
      chunk_size: pattern.config.chunk_size,
      chunk_overlap: pattern.config.chunk_overlap,
      retrieval_method: pattern.config.retrieval_method,
      reranker: pattern.config.reranker ?? '',
      top_k: pattern.config.top_k,
      score_threshold:
        pattern.config.score_threshold === null || pattern.config.score_threshold === undefined
          ? ''
          : String(pattern.config.score_threshold),
    });
  };

  const updateSource = (sourceId: string, patch: Partial<DataSourceDraft>) => {
    setForm((current) => {
      if (!current) {
        return current;
      }
      return {
        ...current,
        data_sources: current.data_sources.map((source) =>
          source.id === sourceId ? { ...source, ...patch } : source,
        ),
      };
    });
  };

  const updateSourceOption = (sourceId: string, key: string, value: string) => {
    setForm((current) => {
      if (!current) {
        return current;
      }
      return {
        ...current,
        data_sources: current.data_sources.map((source) =>
          source.id === sourceId
            ? {
              ...source,
              options: {
                ...source.options,
                [key]: value,
              },
            }
            : source,
        ),
      };
    });
  };

  const addSource = () => {
    setForm((current) => {
      if (!current) {
        return current;
      }
      return {
        ...current,
        data_sources: [...current.data_sources, createEmptySource()],
      };
    });
  };

  const removeSource = (sourceId: string) => {
    setForm((current) => {
      if (!current) {
        return current;
      }
      return {
        ...current,
        data_sources: current.data_sources.filter((source) => source.id !== sourceId),
      };
    });
  };

  const applyDetectedDatabaseSource = () => {
    const seeded = buildDatabaseSourceFromHint(
      selectedConfig?.app_name ?? 'app',
      selectedConfig?.db_hint,
    );
    const seededKind = normalizeDatabaseKind(readOption(seeded.options, 'database_type'));
    const seededType = findDatabaseTypeOption(databaseTypeOptions, seededKind, selectedConfig?.db_hint);
    const seededOptions: Record<string, unknown> = {
      ...seeded.options,
      database_type: seededType.name,
      dialect: seededType.dialect,
    };
    const seededUri = cleanText(seeded.uri) || seededType.sample_uri;
    setForm((current) => {
      if (!current) {
        return current;
      }
      const currentDbIndex = current.data_sources.findIndex((source) => source.type === 'database');
      if (currentDbIndex >= 0) {
        const nextSources = [...current.data_sources];
        const existing = nextSources[currentDbIndex];
        nextSources[currentDbIndex] = {
          ...existing,
          uri: cleanText(existing.uri) || seededUri,
          label: cleanText(existing.label) || seeded.label,
          options: {
            ...seededOptions,
            ...existing.options,
          },
        };
        return {
          ...current,
          data_sources: nextSources,
        };
      }
      return {
        ...current,
        data_sources: [
          {
            ...seeded,
            uri: seededUri,
            options: seededOptions,
          },
          ...current.data_sources,
        ],
      };
    });
  };

  const handleDatabaseTypeChange = (source: DataSourceDraft, nextType: string) => {
    const nextOption = findDatabaseTypeOption(databaseTypeOptions, nextType, selectedConfig?.db_hint);
    const currentTypeName = readOption(source.options, 'database_type') || readOption(source.options, 'dialect');
    const currentOption = findDatabaseTypeOption(databaseTypeOptions, currentTypeName, selectedConfig?.db_hint);
    const uriText = cleanText(source.uri);
    const shouldUpdateUri = !uriText || uriText === currentOption.sample_uri;
    updateSource(source.id, {
      uri: shouldUpdateUri ? nextOption.sample_uri : source.uri,
      options: {
        ...source.options,
        database_type: nextOption.name,
        dialect: nextOption.dialect,
      },
    });
  };

  const handleSourceTypeChange = (source: DataSourceDraft, nextType: DataSourceType) => {
    const patch: Partial<DataSourceDraft> = {
      type: nextType,
      options: { ...source.options },
    };
    if (nextType === 'database') {
      const seeded = buildDatabaseSourceFromHint(selectedConfig?.app_name ?? 'app', selectedConfig?.db_hint);
      const seededTypeName =
        readOption(source.options, 'database_type') ||
        readOption(source.options, 'dialect') ||
        readOption(seeded.options, 'database_type');
      const seededType = findDatabaseTypeOption(databaseTypeOptions, seededTypeName, selectedConfig?.db_hint);
      if (!cleanText(source.uri)) {
        patch.uri = cleanText(seeded.uri) || seededType.sample_uri;
      }
      if (!cleanText(source.label)) {
        patch.label = seeded.label;
      }
      patch.options = {
        ...seeded.options,
        ...source.options,
        database_type: seededType.name,
        dialect: seededType.dialect,
      };
    } else if (!cleanText(source.uri)) {
      patch.uri = sourcePlaceholder(nextType, selectedConfig?.db_hint);
    }
    updateSource(source.id, patch);
  };

  const saveConfig = async () => {
    if (!selectedConfig || !form) {
      return;
    }
    setSaving(true);
    setManagerError(null);
    setManagerMessage(null);
    try {
      await patchAppRAGConfig(selectedConfig.app_name, {
        enabled: form.enabled,
        pattern: form.pattern || null,
        vector_provider: form.vector_provider || null,
        vector_url: form.vector_url || null,
        vector_collection: form.vector_collection || null,
        embedding_model: form.embedding_model || null,
        chunk_strategy: form.chunk_strategy,
        chunk_size: form.chunk_size,
        chunk_overlap: form.chunk_overlap,
        retrieval_method: form.retrieval_method,
        reranker: form.reranker || null,
        top_k: form.top_k,
        score_threshold: form.score_threshold === '' ? null : Number(form.score_threshold),
        indexing_schedule: form.indexing_schedule || null,
        data_sources: form.data_sources
          .map(sourceToPayload)
          .filter((source): source is RAGDataSource => source !== null),
      });
      await Promise.all([loadManager(), loadRAGOverview()]);
      setManagerMessage('RAG 設定を保存しました。');
    } catch (err) {
      const message = err instanceof Error ? err.message : 'RAG 設定の保存に失敗しました';
      setManagerError(message);
    } finally {
      setSaving(false);
    }
  };

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-slate-100">RAG Overview</h1>
        <p className="text-sm text-slate-500 mt-1">
          Retrieval-Augmented Generation 機能概要と App 単位設定
        </p>
      </div>

      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button onClick={clearError} className="text-red-400 hover:text-red-300 text-xs">✕</button>
        </div>
      )}

      {(managerError || managerMessage) && (
        <div
          className={`rounded-lg p-4 text-sm ${
            managerError
              ? 'bg-red-500/10 border border-red-500/20 text-red-300'
              : 'bg-emerald-500/10 border border-emerald-500/20 text-emerald-300'
          }`}
        >
          {managerError ?? managerMessage}
        </div>
      )}

      {loading && (
        <div className="flex justify-center py-12">
          <div className="w-10 h-10 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin" />
        </div>
      )}

      {ragOverview && !loading && (
        <>
          <div className="grid grid-cols-1 sm:grid-cols-3 gap-4">
            <StatCard icon="📄" label="Chunking Strategies" value={ragOverview.stats.total_strategies} color="indigo" />
            <StatCard icon="🔀" label="Rerankers" value={ragOverview.stats.total_rerankers} color="cyan" />
            <StatCard icon="📦" label="Apps Using RAG" value={ragOverview.stats.total_apps_using_rag} color="amber" />
          </div>

          <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5">
            <p className="text-sm text-slate-300 leading-relaxed">{ragOverview.description}</p>
          </div>

          <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
            <div className="flex items-center justify-between gap-3">
              <div>
                <h2 className="text-sm font-semibold text-slate-200">RAG Config Studio</h2>
                <p className="text-xs text-slate-500 mt-1">
                  App ごとにデータソース・分割方式・検索方式・再ランク設定を管理します
                </p>
              </div>
              <button
                onClick={() => void loadManager()}
                className="px-3 py-1.5 rounded-md bg-slate-800 hover:bg-slate-700 text-slate-300 text-xs"
              >
                Reload
              </button>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
              <div className="space-y-2">
                {managerLoading && (
                  <p className="text-xs text-slate-500">読み込み中...</p>
                )}
                {!managerLoading && ragConfigs.map((item) => (
                  <button
                    key={item.app_name}
                    onClick={() => setSelectedApp(item.app_name)}
                    className={`w-full text-left rounded-lg border px-3 py-2 transition-colors ${
                      selectedApp === item.app_name
                        ? 'border-indigo-500/50 bg-indigo-500/10'
                        : 'border-slate-700 bg-slate-950/40 hover:bg-slate-800/60'
                    }`}
                  >
                    <p className="text-sm text-slate-200 flex items-center gap-2">
                      <span>{item.icon}</span>
                      <span>{item.display_name}</span>
                    </p>
                    <p className="text-xs text-slate-500 mt-1">{item.app_name}</p>
                    <p className="text-xs mt-1 text-slate-400">
                      {item.rag.enabled ? `RAG ON · ${item.rag.retrieval_method}` : 'RAG OFF'}
                    </p>
                  </button>
                ))}
              </div>

              <div className="lg:col-span-2">
                {!form || !selectedConfig ? (
                  <div className="h-full rounded-lg border border-slate-700 bg-slate-950/40 p-4 text-xs text-slate-500">
                    編集対象の App を選択してください。
                  </div>
                ) : (
                  <div className="rounded-lg border border-slate-700 bg-slate-950/40 p-4 space-y-4">
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                      <Toggle
                        label="RAG 有効化"
                        checked={form.enabled}
                        onChange={(value) => setForm({ ...form, enabled: value })}
                      />
                      <Field label="パターン">
                        <select
                          value={form.pattern}
                          onChange={(e) => applyPattern(e.target.value)}
                          className="input"
                        >
                          <option value="">(手動設定)</option>
                          {patterns.map((pattern) => (
                            <option key={pattern.name} value={pattern.name}>
                              {pattern.label}
                            </option>
                          ))}
                        </select>
                      </Field>
                      <Field label="Vector Provider">
                        <select
                          value={form.vector_provider}
                          onChange={(e) => setForm({ ...form, vector_provider: e.target.value })}
                          className="input"
                        >
                          <option value="">(auto)</option>
                          {vectorProviderOptions.map((provider) => (
                            <option key={provider.name} value={provider.name}>
                              {provider.label}
                            </option>
                          ))}
                        </select>
                      </Field>
                      <Field label="Vector URL">
                        <input
                          value={form.vector_url}
                          onChange={(e) => setForm({ ...form, vector_url: e.target.value })}
                          className="input"
                          placeholder="http://localhost:6333"
                        />
                      </Field>
                      <Field label="Collection">
                        <input
                          value={form.vector_collection}
                          onChange={(e) => setForm({ ...form, vector_collection: e.target.value })}
                          className="input"
                          placeholder={`${selectedConfig.app_name}_knowledge`}
                        />
                      </Field>
                      <Field label="Embedding Model">
                        <input
                          value={form.embedding_model}
                          onChange={(e) => setForm({ ...form, embedding_model: e.target.value })}
                          className="input"
                          placeholder="text-embedding-3-small"
                        />
                      </Field>
                      <Field label="Chunk Strategy">
                        <select
                          value={form.chunk_strategy}
                          onChange={(e) => setForm({ ...form, chunk_strategy: e.target.value })}
                          className="input"
                        >
                          {ragOverview.chunk_strategies.map((strategy) => (
                            <option key={strategy.name} value={strategy.name}>
                              {strategy.label}
                            </option>
                          ))}
                        </select>
                      </Field>
                      <Field label="Retrieval Method">
                        <select
                          value={form.retrieval_method}
                          onChange={(e) => setForm({ ...form, retrieval_method: e.target.value })}
                          className="input"
                        >
                          {(ragOverview.retrieval_methods ?? []).map((method) => (
                            <option key={method.name} value={method.name}>
                              {method.label}
                            </option>
                          ))}
                        </select>
                      </Field>
                      <Field label="Chunk Size">
                        <input
                          type="number"
                          value={form.chunk_size}
                          onChange={(e) => setForm({ ...form, chunk_size: Number(e.target.value) || 800 })}
                          className="input"
                        />
                      </Field>
                      <Field label="Chunk Overlap">
                        <input
                          type="number"
                          value={form.chunk_overlap}
                          onChange={(e) => setForm({ ...form, chunk_overlap: Number(e.target.value) || 120 })}
                          className="input"
                        />
                      </Field>
                      <Field label="Reranker">
                        <select
                          value={form.reranker}
                          onChange={(e) => setForm({ ...form, reranker: e.target.value })}
                          className="input"
                        >
                          <option value="">none</option>
                          {ragOverview.rerankers.map((reranker) => (
                            <option key={reranker.name} value={reranker.name}>
                              {reranker.label}
                            </option>
                          ))}
                        </select>
                      </Field>
                      <Field label="Top K">
                        <input
                          type="number"
                          value={form.top_k}
                          onChange={(e) => setForm({ ...form, top_k: Number(e.target.value) || 5 })}
                          className="input"
                        />
                      </Field>
                      <Field label="Score Threshold (0-1)">
                        <input
                          type="number"
                          step="0.01"
                          value={form.score_threshold}
                          onChange={(e) => setForm({ ...form, score_threshold: e.target.value })}
                          className="input"
                          placeholder="0.2"
                        />
                      </Field>
                      <Field label="Indexing Schedule">
                        <input
                          value={form.indexing_schedule}
                          onChange={(e) => setForm({ ...form, indexing_schedule: e.target.value })}
                          className="input"
                          placeholder="0 */6 * * *"
                        />
                      </Field>
                    </div>

                    <div className="space-y-3 rounded-lg border border-slate-700/80 bg-slate-950/30 p-3.5">
                      <div className="flex flex-wrap items-start justify-between gap-3">
                        <div>
                          <p className="text-xs font-medium text-slate-300">Data Sources</p>
                          <p className="text-[11px] text-slate-500 mt-1">
                            タイプを選択すると必要項目を展開します。FAQ など DB 情報がある App は自動補完できます。
                          </p>
                        </div>
                        <div className="flex items-center gap-2">
                          <button
                            type="button"
                            onClick={addSource}
                            className="px-2.5 py-1.5 rounded-md border border-slate-700 text-slate-300 hover:bg-slate-800 text-[11px]"
                          >
                            Add Source
                          </button>
                          {selectedConfig.db_hint && (
                            <button
                              type="button"
                              onClick={applyDetectedDatabaseSource}
                              className="px-2.5 py-1.5 rounded-md border border-cyan-700/60 text-cyan-300 hover:bg-cyan-500/10 text-[11px]"
                            >
                              {selectedConfig.db_hint.available ? 'Auto Set DB' : 'Insert DB Sample'}
                            </button>
                          )}
                        </div>
                      </div>

                      <DBHintBanner dbHint={selectedConfig.db_hint} />

                      {form.data_sources.length === 0 && (
                        <div className="rounded-md border border-dashed border-slate-700 p-3 text-xs text-slate-500">
                          Data Source が未設定です。`Add Source` で追加してください。
                        </div>
                      )}

                      {form.data_sources.map((source) => (
                        <div key={source.id} className="rounded-md border border-slate-700 bg-slate-950/60 p-3 space-y-3">
                          <div className="grid grid-cols-1 md:grid-cols-4 gap-2.5">
                            <Field label="Type">
                              <select
                                value={source.type}
                                onChange={(e) => handleSourceTypeChange(source, normalizeSourceType(e.target.value))}
                                className="input"
                              >
                                {DATA_SOURCE_TYPE_OPTIONS.map((option) => (
                                  <option key={option.value} value={option.value}>
                                    {option.label}
                                  </option>
                                ))}
                              </select>
                            </Field>
                            <Field label="URI / Path">
                              <input
                                value={source.uri}
                                onChange={(e) => updateSource(source.id, { uri: e.target.value })}
                                className="input"
                                placeholder={
                                  source.type === 'database'
                                    ? findDatabaseTypeOption(
                                      databaseTypeOptions,
                                      readOption(source.options, 'database_type') ||
                                      readOption(source.options, 'dialect') ||
                                      cleanText(selectedConfig.db_hint?.kind),
                                      selectedConfig.db_hint,
                                    ).sample_uri
                                    : sourcePlaceholder(source.type, selectedConfig.db_hint)
                                }
                              />
                            </Field>
                            <Field label="Label">
                              <input
                                value={source.label}
                                onChange={(e) => updateSource(source.id, { label: e.target.value })}
                                className="input"
                                placeholder={`${source.type} source`}
                              />
                            </Field>
                            <Field label="Schedule (optional)">
                              <input
                                value={source.schedule}
                                onChange={(e) => updateSource(source.id, { schedule: e.target.value })}
                                className="input"
                                placeholder="0 */6 * * *"
                              />
                            </Field>
                          </div>

                          <p className="text-[11px] text-slate-500">
                            {DATA_SOURCE_TYPE_OPTIONS.find((option) => option.value === source.type)?.description}
                          </p>

                          {source.type === 'database' && (() => {
                            const dbTypeName =
                              readOption(source.options, 'database_type') ||
                              readOption(source.options, 'dialect') ||
                              cleanText(selectedConfig.db_hint?.kind);
                            const activeDbType = findDatabaseTypeOption(
                              databaseTypeOptions,
                              dbTypeName,
                              selectedConfig.db_hint,
                            );
                            return (
                              <div className="space-y-2.5">
                                <div className="grid grid-cols-1 md:grid-cols-4 gap-2.5">
                                  <Field label="Database Type">
                                    <select
                                      value={activeDbType.name}
                                      onChange={(e) => handleDatabaseTypeChange(source, e.target.value)}
                                      className="input"
                                    >
                                      {databaseTypeOptions.map((option) => (
                                        <option key={option.name} value={option.name}>
                                          {option.label}
                                        </option>
                                      ))}
                                    </select>
                                  </Field>
                                  <Field label="Dialect">
                                    <select
                                      value={readOption(source.options, 'dialect') || activeDbType.dialect}
                                      onChange={(e) => handleDatabaseTypeChange(source, e.target.value)}
                                      className="input"
                                    >
                                      {databaseTypeOptions.map((option) => (
                                        <option key={option.name} value={option.dialect}>
                                          {option.label}
                                        </option>
                                      ))}
                                    </select>
                                  </Field>
                                  <Field label="Schema">
                                    <input
                                      value={readOption(source.options, 'schema')}
                                      onChange={(e) => updateSourceOption(source.id, 'schema', e.target.value)}
                                      className="input"
                                      placeholder="public"
                                    />
                                  </Field>
                                  <Field label="Read Mode">
                                    <select
                                      value={readOption(source.options, 'read_mode') || 'table'}
                                      onChange={(e) => updateSourceOption(source.id, 'read_mode', e.target.value)}
                                      className="input"
                                    >
                                      <option value="table">table</option>
                                      <option value="query">query</option>
                                    </select>
                                  </Field>
                                </div>

                                {activeDbType.connection_kind === 'network' && (
                                  <div className="grid grid-cols-1 md:grid-cols-4 gap-2.5">
                                    <Field label="Host (optional)">
                                      <input
                                        value={readOption(source.options, 'host')}
                                        onChange={(e) => updateSourceOption(source.id, 'host', e.target.value)}
                                        className="input"
                                        placeholder={cleanText(selectedConfig.db_hint?.host) || 'localhost'}
                                      />
                                    </Field>
                                    <Field label="Port (optional)">
                                      <input
                                        value={readOption(source.options, 'port')}
                                        onChange={(e) => updateSourceOption(source.id, 'port', e.target.value)}
                                        className="input"
                                        placeholder={String(activeDbType.default_port ?? '')}
                                      />
                                    </Field>
                                    <Field label="Database (optional)">
                                      <input
                                        value={readOption(source.options, 'database')}
                                        onChange={(e) => updateSourceOption(source.id, 'database', e.target.value)}
                                        className="input"
                                        placeholder={cleanText(selectedConfig.db_hint?.database) || 'app_db'}
                                      />
                                    </Field>
                                    <Field label="User (optional)">
                                      <input
                                        value={readOption(source.options, 'user')}
                                        onChange={(e) => updateSourceOption(source.id, 'user', e.target.value)}
                                        className="input"
                                        placeholder={cleanText(selectedConfig.db_hint?.user) || 'app_user'}
                                      />
                                    </Field>
                                  </div>
                                )}

                                {activeDbType.connection_kind === 'file' && (
                                  <p className="text-[11px] text-slate-500">
                                    File DB は URI にファイルパスを指定してください。例: `{activeDbType.sample_uri}`
                                  </p>
                                )}

                                <div className="grid grid-cols-1 md:grid-cols-4 gap-2.5">
                                  <Field label="Table">
                                    <input
                                      value={readOption(source.options, 'table')}
                                      onChange={(e) => updateSourceOption(source.id, 'table', e.target.value)}
                                      className="input"
                                      placeholder="faq_entries"
                                    />
                                  </Field>
                                  <div className="md:col-span-3">
                                    <Field label="Query (optional)">
                                      <textarea
                                        value={readOption(source.options, 'query')}
                                        onChange={(e) => updateSourceOption(source.id, 'query', e.target.value)}
                                        className="input min-h-20"
                                        placeholder="SELECT id, question, answer, updated_at FROM faq_entries"
                                      />
                                    </Field>
                                  </div>
                                </div>
                              </div>
                            );
                          })()}

                          {source.type === 'api' && (
                            <div className="grid grid-cols-1 md:grid-cols-3 gap-2.5">
                              <Field label="Method">
                                <select
                                  value={readOption(source.options, 'method') || 'GET'}
                                  onChange={(e) => updateSourceOption(source.id, 'method', e.target.value)}
                                  className="input"
                                >
                                  <option value="GET">GET</option>
                                  <option value="POST">POST</option>
                                </select>
                              </Field>
                              <Field label="Auth Header (optional)">
                                <input
                                  value={readOption(source.options, 'auth_header')}
                                  onChange={(e) => updateSourceOption(source.id, 'auth_header', e.target.value)}
                                  className="input"
                                  placeholder="Bearer ${API_TOKEN}"
                                />
                              </Field>
                              <Field label="JSON Path (optional)">
                                <input
                                  value={readOption(source.options, 'json_path')}
                                  onChange={(e) => updateSourceOption(source.id, 'json_path', e.target.value)}
                                  className="input"
                                  placeholder="data.items"
                                />
                              </Field>
                            </div>
                          )}

                          {source.type === 'file' && (
                            <div className="grid grid-cols-1 md:grid-cols-2 gap-2.5">
                              <Field label="Glob Pattern (optional)">
                                <input
                                  value={readOption(source.options, 'glob')}
                                  onChange={(e) => updateSourceOption(source.id, 'glob', e.target.value)}
                                  className="input"
                                  placeholder="**/*.md"
                                />
                              </Field>
                              <Field label="Encoding (optional)">
                                <input
                                  value={readOption(source.options, 'encoding')}
                                  onChange={(e) => updateSourceOption(source.id, 'encoding', e.target.value)}
                                  className="input"
                                  placeholder="utf-8"
                                />
                              </Field>
                            </div>
                          )}

                          {source.type === 's3' && (
                            <div className="grid grid-cols-1 md:grid-cols-3 gap-2.5">
                              <Field label="Region">
                                <input
                                  value={readOption(source.options, 'region')}
                                  onChange={(e) => updateSourceOption(source.id, 'region', e.target.value)}
                                  className="input"
                                  placeholder="ap-northeast-1"
                                />
                              </Field>
                              <Field label="Prefix (optional)">
                                <input
                                  value={readOption(source.options, 'prefix')}
                                  onChange={(e) => updateSourceOption(source.id, 'prefix', e.target.value)}
                                  className="input"
                                  placeholder="faq/"
                                />
                              </Field>
                              <Field label="File Filter (optional)">
                                <input
                                  value={readOption(source.options, 'filter')}
                                  onChange={(e) => updateSourceOption(source.id, 'filter', e.target.value)}
                                  className="input"
                                  placeholder="*.json"
                                />
                              </Field>
                            </div>
                          )}

                          <div className="flex items-center justify-between">
                            <label className="inline-flex items-center gap-2 text-xs text-slate-300">
                              <input
                                type="checkbox"
                                checked={source.enabled}
                                onChange={(e) => updateSource(source.id, { enabled: e.target.checked })}
                                className="rounded border-slate-600 bg-slate-900"
                              />
                              Enabled
                            </label>
                            <button
                              type="button"
                              onClick={() => removeSource(source.id)}
                              className="text-[11px] px-2 py-1 rounded border border-red-500/40 text-red-300 hover:bg-red-500/10"
                            >
                              Remove
                            </button>
                          </div>
                        </div>
                      ))}
                    </div>

                    <div className="flex items-center justify-end gap-2 pt-1">
                      <button
                        onClick={() => setForm(toForm(selectedConfig))}
                        className="px-3 py-1.5 rounded-md border border-slate-700 text-slate-300 hover:bg-slate-800 text-xs"
                      >
                        Reset
                      </button>
                      <button
                        onClick={() => void saveConfig()}
                        disabled={saving}
                        className="px-3 py-1.5 rounded-md bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 text-white text-xs"
                      >
                        {saving ? 'Saving...' : 'Save RAG Config'}
                      </button>
                    </div>
                  </div>
                )}
              </div>
            </div>
          </div>

          <div className="bg-slate-900/50 border border-slate-800 rounded-xl overflow-hidden">
            <div className="px-5 py-3.5 border-b border-slate-800">
              <h2 className="text-sm font-semibold text-slate-200">📦 Apps Using RAG</h2>
            </div>
            <div className="divide-y divide-slate-800/50">
              {ragOverview.apps_using_rag.map((app) => (
                <div key={app.app_name} className="px-5 py-3.5 flex items-center gap-4">
                  <span className="text-2xl">{app.icon}</span>
                  <div className="flex-1 min-w-0">
                    <p className="text-sm font-medium text-slate-200">{app.display_name}</p>
                    <p className="text-xs text-slate-500">{app.app_name}</p>
                  </div>
                  <div className="flex flex-wrap gap-1.5">
                    {app.rag_details.map((detail) => (
                      <span key={detail} className="px-2 py-0.5 bg-amber-500/10 text-amber-400 text-[10px] rounded-full">
                        {detail}
                      </span>
                    ))}
                  </div>
                </div>
              ))}
              {ragOverview.apps_using_rag.length === 0 && (
                <p className="px-5 py-8 text-center text-sm text-slate-500">No apps using RAG</p>
              )}
            </div>
          </div>
        </>
      )}
    </div>
  );
}

function DBHintBanner({ dbHint }: { dbHint: RAGDatabaseHint | undefined }) {
  if (!dbHint) {
    return null;
  }
  if (dbHint.available) {
    const hostPort = cleanText(dbHint.host)
      ? `${cleanText(dbHint.host)}${typeof dbHint.port === 'number' ? `:${dbHint.port}` : ''}`
      : '';
    const dbName = cleanText(dbHint.database);
    return (
      <div className="rounded-md border border-emerald-500/30 bg-emerald-500/10 p-2.5 text-[11px] text-emerald-200">
        Detected DB: `{dbHint.kind ?? 'unknown'}` {dbName ? `(${dbName})` : ''}{hostPort ? ` @ ${hostPort}` : ''} · source: {dbHint.source}
      </div>
    );
  }
  return (
    <div className="rounded-md border border-amber-500/30 bg-amber-500/10 p-2.5 text-[11px] text-amber-100">
      DB 情報が app_config から見つかりません。サンプル URI: `{dbHint.sample_uri}`
    </div>
  );
}

function Field({ label, children }: { label: string; children: ReactNode }) {
  return (
    <label className="block space-y-1">
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

function StatCard({
  icon,
  label,
  value,
  color,
}: {
  icon: string;
  label: string;
  value: number;
  color: 'indigo' | 'cyan' | 'amber';
}) {
  const borderColor = {
    indigo: 'border-indigo-500/20',
    cyan: 'border-cyan-500/20',
    amber: 'border-amber-500/20',
  }[color];
  const valueColor = {
    indigo: 'text-indigo-400',
    cyan: 'text-cyan-400',
    amber: 'text-amber-400',
  }[color];
  return (
    <div className={`bg-slate-900/50 border ${borderColor} rounded-xl p-5`}>
      <div className="flex items-center gap-3 mb-2">
        <span className="text-xl">{icon}</span>
        <span className="text-xs text-slate-500 uppercase tracking-wider">{label}</span>
      </div>
      <p className={`text-3xl font-bold ${valueColor}`}>{value}</p>
    </div>
  );
}

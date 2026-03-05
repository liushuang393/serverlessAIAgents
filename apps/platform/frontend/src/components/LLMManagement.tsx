/**
 * LLM Management - Gateway/provider/engine/registry policy console.
 */

import axios from 'axios';
import { useEffect, useMemo, useState } from 'react';
import {
  fetchLLMCatalog,
  fetchLLMDiagnostics,
  fetchLLMEngineStatus,
  fetchLLMManagementOverview,
  fetchOpenAPIPaths,
  reloadLLMManagementConfig,
  setupAndSwitchLLM,
  switchLLM,
  updateLLMInferenceEngines,
  updateLLMModels,
  updateLLMProviders,
  updateLLMRegistry,
  updateLLMRoutingPolicy,
} from '@/api/client';
import type {
  LLMBackendKind,
  LLMCatalogResponse,
  LLMDiagnosticsResponse,
  LLMEngineRuntimeStatus,
  LLMInferenceEngineConfigItem,
  LLMManagementOverviewResponse,
  LLMModelConfigItem,
  LLMPreflightReport,
  LLMProviderConfigItem,
  LLMManagementProviderKind,
  LLMProviderRuntimeStatus,
  LLMRoutingPolicyConfig,
  LLMSwitchResponse,
} from '@/types';
import { useI18n } from '@/i18n';

type ErrorCategory = 'route_missing' | 'validation' | 'install' | 'health' | 'network' | null;

const prettify = (value: unknown): string => JSON.stringify(value, null, 2);
const DEFAULT_ROLES = 'reasoning,coding,cheap,local';

const EMPTY_CATALOG: LLMCatalogResponse = {
  providers: [],
  backends: [],
  models: [],
  generated_at: '',
};

const parseRoles = (raw: string): string[] => {
  const roles = raw
    .split(',')
    .map((item) => item.trim().toLowerCase())
    .filter(Boolean);
  return roles.length > 0 ? roles : ['reasoning'];
};

const classifyWorkflowFailure = (
  preflight: LLMPreflightReport | null,
  switchResult: LLMSwitchResponse | null,
): ErrorCategory => {
  if (preflight) {
    if (preflight.steps.some((step) => step.phase === 'install' && step.status === 'failed')) {
      return 'install';
    }
    if (preflight.steps.some((step) => step.phase === 'health' && step.status === 'failed')) {
      return 'health';
    }
  }
  if (switchResult?.message.toLowerCase().includes('validation')) {
    return 'validation';
  }
  return 'network';
};

export function LLMManagement() {
  const { t } = useI18n();
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [errorCategory, setErrorCategory] = useState<ErrorCategory>(null);
  const [message, setMessage] = useState<string | null>(null);

  const [overview, setOverview] = useState<LLMManagementOverviewResponse | null>(null);
  const [providerRuntime, setProviderRuntime] = useState<LLMProviderRuntimeStatus[]>([]);
  const [engineRuntime, setEngineRuntime] = useState<LLMEngineRuntimeStatus[]>([]);
  const [catalog, setCatalog] = useState<LLMCatalogResponse>(EMPTY_CATALOG);
  const [diagnostics, setDiagnostics] = useState<LLMDiagnosticsResponse | null>(null);
  const [advancedMode, setAdvancedMode] = useState(false);
  const [openapiHasLLMRoutes, setOpenapiHasLLMRoutes] = useState<boolean | null>(null);

  const [providersDraft, setProvidersDraft] = useState('[]');
  const [enginesDraft, setEnginesDraft] = useState('[]');
  const [modelsDraft, setModelsDraft] = useState('[]');
  const [registryDraft, setRegistryDraft] = useState('{}');
  const [routingDraft, setRoutingDraft] = useState('{}');

  const [switchProvider, setSwitchProvider] = useState<LLMManagementProviderKind>('openai');
  const [switchModel, setSwitchModel] = useState('');
  const [switchBackend, setSwitchBackend] = useState<LLMBackendKind>('none');
  const [switchRoles, setSwitchRoles] = useState(DEFAULT_ROLES);
  const [autoSetup, setAutoSetup] = useState(true);
  const [autoInstall, setAutoInstall] = useState(true);
  const [autoStart, setAutoStart] = useState(true);
  const [healthCheck, setHealthCheck] = useState(true);
  const [validateRuntime, setValidateRuntime] = useState(true);

  const [lastPreflight, setLastPreflight] = useState<LLMPreflightReport | null>(null);
  const [lastSwitch, setLastSwitch] = useState<LLMSwitchResponse | null>(null);

  const diagnoseMissingRoute = async () => {
    const hints: string[] = [];
    try {
      const paths = await fetchOpenAPIPaths();
      const llmPaths = paths.filter((path) => path.startsWith('/api/studios/framework/llm'));
      const hasRoutes = llmPaths.length > 0;
      setOpenapiHasLLMRoutes(hasRoutes);
      if (!hasRoutes) {
        hints.push('Backend OpenAPI missing /api/studios/framework/llm routes.');
      }
    } catch (diagnoseErr) {
      const reason = diagnoseErr instanceof Error ? diagnoseErr.message : 'unknown';
      hints.push(`Failed to inspect /openapi.json: ${reason}`);
      setOpenapiHasLLMRoutes(null);
    }

    try {
      const payload = await fetchLLMDiagnostics();
      setDiagnostics(payload);
      hints.push(...payload.hints);
    } catch {
      // diagnostics endpoint is unavailable when llm router is missing in stale backend processes
    }

    const combined = hints.length > 0
      ? hints.join(' ')
      : 'LLM routes are unavailable. Restart backend and verify /openapi.json includes /api/studios/framework/llm/*.';
    setErrorCategory('route_missing');
    setError(combined);
  };

  const load = async () => {
    setLoading(true);
    setError(null);
    setErrorCategory(null);
    try {
      const [overviewPayload, catalogPayload] = await Promise.all([
        fetchLLMManagementOverview(),
        fetchLLMCatalog().catch(() => EMPTY_CATALOG),
      ]);
      setCatalog(catalogPayload);
      setOverview(overviewPayload);
      setProvidersDraft(prettify(overviewPayload.providers));
      setEnginesDraft(prettify(overviewPayload.inference_engines));
      setModelsDraft(prettify(overviewPayload.models));
      setRegistryDraft(prettify(overviewPayload.registry));
      setRoutingDraft(prettify(overviewPayload.routing_policy));
      setProviderRuntime(overviewPayload.providers_runtime);
      setSwitchProvider((current) => {
        const allowed = new Set(catalogPayload.providers.map((item) => item.name));
        return allowed.has(current) ? current : (catalogPayload.providers[0]?.name ?? 'openai');
      });

      const preferredModel =
        catalogPayload.providers.find((item) => item.name === switchProvider)?.recommended_models[0] ??
        catalogPayload.models.find((item) => item.provider === switchProvider)?.model ??
        '';
      if (!switchModel && preferredModel) {
        setSwitchModel(preferredModel);
      }

      const engineStatus = await fetchLLMEngineStatus();
      setEngineRuntime(engineStatus.engine_status);
    } catch (err) {
      if (axios.isAxiosError(err) && err.response?.status === 404) {
        await diagnoseMissingRoute();
      } else {
        const text = err instanceof Error ? err.message : t('llm_mgmt.error_load');
        setErrorCategory('network');
        setError(text);
      }
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    void load();
  }, []);

  const runtimeSummary = useMemo(() => {
    const availableProviders = providerRuntime.filter((item) => item.status === 'available').length;
    const availableEngines = engineRuntime.filter((item) => item.status === 'available').length;
    return {
      providers: `${availableProviders}/${providerRuntime.length}`,
      engines: `${availableEngines}/${engineRuntime.length}`,
      totalCost: overview?.cost_summary.total_cost_usd ?? 0,
      budgetExceeded: overview?.cost_summary.budget_exceeded ?? false,
    };
  }, [engineRuntime, overview, providerRuntime]);

  const providerModelHints = useMemo(() => {
    if (catalog.models.length === 0) {
      return [];
    }
    return catalog.models
      .filter((item) => item.provider === switchProvider)
      .map((item) => item.model)
      .slice(0, 20);
  }, [catalog.models, switchProvider]);

  const parseJson = <T,>(label: string, raw: string): T => {
    try {
      return JSON.parse(raw) as T;
    } catch (err) {
      const details = err instanceof Error ? err.message : 'invalid json';
      const wrapped = new Error(`${label}: ${details}`) as Error & { cause?: unknown };
      wrapped.cause = err;
      throw wrapped;
    }
  };

  const withSave = async (fn: () => Promise<void>) => {
    setSaving(true);
    setError(null);
    setErrorCategory(null);
    setMessage(null);
    try {
      await fn();
      setMessage(t('llm_mgmt.saved'));
      await load();
    } catch (err) {
      if (axios.isAxiosError(err) && (err.response?.status === 400 || err.response?.status === 422)) {
        setErrorCategory('validation');
      } else {
        setErrorCategory('network');
      }
      setError(err instanceof Error ? err.message : t('llm_mgmt.error_save'));
    } finally {
      setSaving(false);
    }
  };

  const saveProviders = async () => withSave(async () => {
    const payload = parseJson<LLMProviderConfigItem[]>(t('llm_mgmt.section_providers'), providersDraft);
    await updateLLMProviders(payload);
  });

  const saveEngines = async () => withSave(async () => {
    const payload = parseJson<LLMInferenceEngineConfigItem[]>(t('llm_mgmt.section_engines'), enginesDraft);
    await updateLLMInferenceEngines(payload);
  });

  const saveModels = async () => withSave(async () => {
    const payload = parseJson<LLMModelConfigItem[]>(t('llm_mgmt.section_models'), modelsDraft);
    await updateLLMModels(payload);
  });

  const saveRegistry = async () => withSave(async () => {
    const payload = parseJson<Record<string, string>>(t('llm_mgmt.section_registry'), registryDraft);
    await updateLLMRegistry(payload);
  });

  const saveRouting = async () => withSave(async () => {
    const payload = parseJson<LLMRoutingPolicyConfig>(t('llm_mgmt.section_routing'), routingDraft);
    await updateLLMRoutingPolicy(payload);
  });

  const handleReload = async () => {
    setSaving(true);
    setError(null);
    setErrorCategory(null);
    setMessage(null);
    try {
      await reloadLLMManagementConfig();
      setMessage(t('llm_mgmt.reloaded'));
      await load();
    } catch (err) {
      setErrorCategory('network');
      setError(err instanceof Error ? err.message : t('llm_mgmt.error_reload'));
    } finally {
      setSaving(false);
    }
  };

  const handleSetupAndSwitch = async () => {
    setSaving(true);
    setError(null);
    setErrorCategory(null);
    setMessage(null);
    setLastPreflight(null);
    setLastSwitch(null);

    const roles = parseRoles(switchRoles);
    const backend = switchBackend;

    try {
      if (autoSetup) {
        const response = await setupAndSwitchLLM({
          preflight: {
            providers: [switchProvider],
            backends: backend === 'none' ? [] : [backend],
            auto_install: autoInstall,
            auto_start: autoStart,
            health_check: healthCheck,
            dry_run: false,
          },
          switch: {
            provider: switchProvider,
            model: switchModel.trim(),
            backend,
            roles,
            model_alias: null,
            auto_enable_provider: true,
            update_fallback_chain: true,
            validate_runtime: validateRuntime,
          },
        });
        setLastPreflight(response.preflight);
        setLastSwitch(response.switch);
        if (!response.success || !response.switch?.success) {
          const category = classifyWorkflowFailure(response.preflight, response.switch);
          setErrorCategory(category);
          setError(response.switch?.message ?? response.message);
          return;
        }
      } else {
        const response = await switchLLM({
          provider: switchProvider,
          model: switchModel.trim(),
          backend,
          roles,
          model_alias: null,
          auto_enable_provider: true,
          update_fallback_chain: true,
          validate_runtime: validateRuntime,
        });
        setLastSwitch(response);
        if (!response.success) {
          setErrorCategory(classifyWorkflowFailure(null, response));
          setError(response.message);
          return;
        }
      }

      setMessage('Switch completed successfully.');
      await load();
    } catch (err) {
      if (axios.isAxiosError(err) && err.response?.status === 404) {
        await diagnoseMissingRoute();
      } else if (axios.isAxiosError(err) && (err.response?.status === 400 || err.response?.status === 422)) {
        setErrorCategory('validation');
        setError(err.message);
      } else {
        setErrorCategory('network');
        setError(err instanceof Error ? err.message : 'Switch failed');
      }
    } finally {
      setSaving(false);
    }
  };

  const Section = (
    props: {
      title: string;
      description: string;
      value: string;
      onChange: (next: string) => void;
      onSave: () => Promise<void>;
      helper?: string;
    },
  ) => {
    return (
      <section className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-3">
        <div>
          <h2 className="text-sm font-semibold text-slate-100">{props.title}</h2>
          <p className="text-xs text-slate-500 mt-1">{props.description}</p>
          {props.helper && <p className="text-[11px] text-slate-400 mt-1 break-all">{props.helper}</p>}
        </div>
        <textarea
          value={props.value}
          onChange={(event) => props.onChange(event.target.value)}
          className="w-full h-56 bg-slate-950 border border-slate-700 rounded-lg p-3 text-xs text-slate-200 font-mono"
          spellCheck={false}
        />
        <div className="flex justify-end">
          <button
            disabled={saving}
            onClick={() => void props.onSave()}
            className="px-4 py-2 bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 disabled:cursor-not-allowed text-white text-sm font-medium rounded-lg transition-colors"
          >
            {saving ? t('llm_mgmt.saving') : t('common.save')}
          </button>
        </div>
      </section>
    );
  };

  if (loading) {
    return (
      <div className="p-6 max-w-6xl mx-auto">
        <p className="text-sm text-slate-400">{t('common.loading')}</p>
      </div>
    );
  }

  const errorStyle =
    errorCategory === 'route_missing'
      ? 'bg-amber-500/10 border-amber-500/30 text-amber-200'
      : 'bg-red-500/10 border-red-500/30 text-red-300';

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      <div className="flex items-start justify-between gap-4">
        <div>
          <h1 className="text-2xl font-bold text-slate-100">{t('llm_mgmt.title')}</h1>
          <p className="text-sm text-slate-500 mt-1">{t('llm_mgmt.subtitle')}</p>
        </div>
        <div className="flex items-center gap-2">
          <button
            disabled={saving}
            onClick={() => setAdvancedMode((prev) => !prev)}
            data-testid="llm-advanced-toggle"
            className="px-4 py-2 bg-slate-800 hover:bg-slate-700 text-slate-100 text-sm rounded-lg"
          >
            {advancedMode ? 'Hide Advanced Mode' : 'Show Advanced Mode'}
          </button>
          <button
            disabled={saving}
            onClick={() => void handleReload()}
            className="px-4 py-2 bg-slate-800 hover:bg-slate-700 disabled:opacity-50 disabled:cursor-not-allowed text-slate-100 text-sm rounded-lg"
          >
            {saving ? t('llm_mgmt.saving') : t('llm_mgmt.reload')}
          </button>
        </div>
      </div>

      {error && (
        <div
          data-testid={errorCategory === 'route_missing' ? 'llm-route-missing-diagnostic' : 'llm-error-banner'}
          className={`border rounded-lg p-3 text-sm ${errorStyle}`}
        >
          <p>{error}</p>
          {errorCategory === 'route_missing' && (
            <ul className="mt-2 text-xs list-disc list-inside space-y-1">
              <li>1) Restart Platform backend process.</li>
              <li>2) Confirm backend OpenAPI has `/api/studios/framework/llm/*` paths.</li>
              <li>3) Reload this page after backend restart.</li>
              {openapiHasLLMRoutes === true && <li>OpenAPI has LLM routes; check frontend proxy/backend port mismatch.</li>}
            </ul>
          )}
        </div>
      )}
      {message && (
        <div
          data-testid="llm-setup-switch-success"
          className="bg-emerald-500/10 border border-emerald-500/30 rounded-lg p-3 text-sm text-emerald-300"
        >
          {message}
        </div>
      )}

      <section className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-3" data-testid="llm-switch-panel">
        <h2 className="text-sm font-semibold text-slate-100">Quick Setup & Switch</h2>
        <p className="text-xs text-slate-500">
          Select provider/model/backend, then run setup and atomic switch in one action.
        </p>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
          <label className="text-xs text-slate-300 space-y-1">
            <span>Provider</span>
            <select
              data-testid="llm-switch-provider"
              value={switchProvider}
              onChange={(event) => setSwitchProvider(event.target.value as LLMManagementProviderKind)}
              className="w-full bg-slate-950 border border-slate-700 rounded px-3 py-2 text-sm"
            >
              {catalog.providers.map((provider) => (
                <option key={provider.name} value={provider.name}>
                  {provider.name}
                </option>
              ))}
            </select>
          </label>
          <label className="text-xs text-slate-300 space-y-1">
            <span>Model</span>
            <input
              data-testid="llm-switch-model"
              value={switchModel}
              onChange={(event) => setSwitchModel(event.target.value)}
              list="llm-model-hints"
              className="w-full bg-slate-950 border border-slate-700 rounded px-3 py-2 text-sm"
              placeholder="gpt-4o-mini / claude-sonnet-4 / gemini-2.0-flash"
            />
            <datalist id="llm-model-hints">
              {providerModelHints.map((model) => (
                <option key={model} value={model} />
              ))}
            </datalist>
          </label>
          <label className="text-xs text-slate-300 space-y-1">
            <span>Backend</span>
            <select
              data-testid="llm-switch-backend"
              value={switchBackend}
              onChange={(event) => setSwitchBackend(event.target.value as LLMBackendKind)}
              className="w-full bg-slate-950 border border-slate-700 rounded px-3 py-2 text-sm"
            >
              <option value="none">none</option>
              {catalog.backends.map((backend) => (
                <option key={backend.name} value={backend.name}>
                  {backend.name}
                </option>
              ))}
            </select>
          </label>
        </div>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
          <label className="text-xs text-slate-300 space-y-1">
            <span>Roles (comma separated)</span>
            <input
              data-testid="llm-switch-roles"
              value={switchRoles}
              onChange={(event) => setSwitchRoles(event.target.value)}
              className="w-full bg-slate-950 border border-slate-700 rounded px-3 py-2 text-sm"
            />
          </label>
          <label className="text-xs text-slate-300 space-y-1">
            <span>Mode</span>
            <select
              value={autoSetup ? 'setup_switch' : 'switch_only'}
              onChange={(event) => setAutoSetup(event.target.value === 'setup_switch')}
              className="w-full bg-slate-950 border border-slate-700 rounded px-3 py-2 text-sm"
            >
              <option value="setup_switch">Setup + Switch</option>
              <option value="switch_only">Switch only</option>
            </select>
          </label>
        </div>
        <div className="grid grid-cols-2 md:grid-cols-4 gap-3 text-xs text-slate-300">
          <label className="inline-flex items-center gap-2">
            <input type="checkbox" checked={autoInstall} onChange={(event) => setAutoInstall(event.target.checked)} disabled={!autoSetup} />
            auto_install
          </label>
          <label className="inline-flex items-center gap-2">
            <input type="checkbox" checked={autoStart} onChange={(event) => setAutoStart(event.target.checked)} disabled={!autoSetup} />
            auto_start
          </label>
          <label className="inline-flex items-center gap-2">
            <input type="checkbox" checked={healthCheck} onChange={(event) => setHealthCheck(event.target.checked)} disabled={!autoSetup} />
            health_check
          </label>
          <label className="inline-flex items-center gap-2">
            <input type="checkbox" checked={validateRuntime} onChange={(event) => setValidateRuntime(event.target.checked)} />
            validate_runtime
          </label>
        </div>
        <div className="flex justify-end">
          <button
            disabled={saving || !switchModel.trim()}
            onClick={() => void handleSetupAndSwitch()}
            data-testid="llm-setup-switch-button"
            className="px-4 py-2 bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 disabled:cursor-not-allowed text-white text-sm font-medium rounded-lg transition-colors"
          >
            {saving ? 'Running...' : autoSetup ? 'Setup and Switch' : 'Switch'}
          </button>
        </div>
      </section>

      {(lastPreflight || lastSwitch || diagnostics) && (
        <section className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-3">
          <h2 className="text-sm font-semibold text-slate-100">Latest Result</h2>
          {diagnostics && (
            <div className="text-xs text-slate-300">
              <p>Route count: {diagnostics.route_count}</p>
              <p>Config version: {diagnostics.config_version ?? 'N/A'}</p>
              <p>Has LLM routes: {diagnostics.has_llm_routes ? 'yes' : 'no'}</p>
            </div>
          )}
          {lastPreflight && (
            <div>
              <p className="text-xs text-slate-300 mb-2">Preflight: {lastPreflight.summary}</p>
              <div className="space-y-1">
                {lastPreflight.steps.map((step, index) => (
                  <div key={`${step.target}-${step.phase}-${index}`} className="text-xs text-slate-400">
                    [{step.status}] {step.category}.{step.phase} ({step.target}) - {step.message}
                  </div>
                ))}
              </div>
            </div>
          )}
          {lastSwitch && (
            <div>
              <p className="text-xs text-slate-300">Switch: {lastSwitch.message}</p>
              <p className="text-xs text-slate-400">Applied alias: {lastSwitch.applied_alias ?? 'N/A'}</p>
              <p className="text-xs text-slate-400">Rolled back: {lastSwitch.rolled_back ? 'yes' : 'no'}</p>
            </div>
          )}
        </section>
      )}

      <section className="bg-slate-900/50 border border-slate-800 rounded-xl p-5">
        <h2 className="text-sm font-semibold text-slate-100">{t('llm_mgmt.runtime')}</h2>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-3 mt-3">
          <div className="bg-slate-950/70 border border-slate-800 rounded-lg p-3">
            <p className="text-[10px] uppercase tracking-wider text-slate-500">{t('llm_mgmt.providers_available')}</p>
            <p className="text-lg text-slate-100 font-semibold">{runtimeSummary.providers}</p>
          </div>
          <div className="bg-slate-950/70 border border-slate-800 rounded-lg p-3">
            <p className="text-[10px] uppercase tracking-wider text-slate-500">{t('llm_mgmt.engines_available')}</p>
            <p className="text-lg text-slate-100 font-semibold">{runtimeSummary.engines}</p>
          </div>
          <div className="bg-slate-950/70 border border-slate-800 rounded-lg p-3">
            <p className="text-[10px] uppercase tracking-wider text-slate-500">{t('llm_mgmt.total_cost')}</p>
            <p className="text-lg text-slate-100 font-semibold">${runtimeSummary.totalCost.toFixed(4)}</p>
            {runtimeSummary.budgetExceeded && (
              <p className="text-xs text-amber-300 mt-1">{t('llm_mgmt.budget_exceeded')}</p>
            )}
          </div>
        </div>
      </section>

      {advancedMode && (
        <>
          {Section({
            title: t('llm_mgmt.section_providers'),
            description: t('llm_mgmt.section_providers_desc'),
            value: providersDraft,
            onChange: setProvidersDraft,
            onSave: saveProviders,
            helper: t('llm_mgmt.provider_runtime_hint') + ` ${prettify(providerRuntime)}`,
          })}

          {Section({
            title: t('llm_mgmt.section_engines'),
            description: t('llm_mgmt.section_engines_desc'),
            value: enginesDraft,
            onChange: setEnginesDraft,
            onSave: saveEngines,
            helper: t('llm_mgmt.engine_runtime_hint') + ` ${prettify(engineRuntime)}`,
          })}

          {Section({
            title: t('llm_mgmt.section_models'),
            description: t('llm_mgmt.section_models_desc'),
            value: modelsDraft,
            onChange: setModelsDraft,
            onSave: saveModels,
          })}

          {Section({
            title: t('llm_mgmt.section_registry'),
            description: t('llm_mgmt.section_registry_desc'),
            value: registryDraft,
            onChange: setRegistryDraft,
            onSave: saveRegistry,
          })}

          {Section({
            title: t('llm_mgmt.section_routing'),
            description: t('llm_mgmt.section_routing_desc'),
            value: routingDraft,
            onChange: setRoutingDraft,
            onSave: saveRouting,
          })}
        </>
      )}
    </div>
  );
}

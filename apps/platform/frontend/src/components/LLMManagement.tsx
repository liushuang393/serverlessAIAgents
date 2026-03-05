/**
 * LLM Management - Gateway/provider/engine/registry policy console.
 */

import { useEffect, useMemo, useState } from 'react';
import {
  fetchLLMEngineStatus,
  fetchLLMManagementOverview,
  reloadLLMManagementConfig,
  updateLLMInferenceEngines,
  updateLLMModels,
  updateLLMProviders,
  updateLLMRegistry,
  updateLLMRoutingPolicy,
} from '@/api/client';
import type {
  LLMEngineRuntimeStatus,
  LLMInferenceEngineConfigItem,
  LLMManagementOverviewResponse,
  LLMModelConfigItem,
  LLMProviderConfigItem,
  LLMProviderRuntimeStatus,
  LLMRoutingPolicyConfig,
} from '@/types';
import { useI18n } from '@/i18n';

const prettify = (value: unknown): string => JSON.stringify(value, null, 2);

export function LLMManagement() {
  const { t } = useI18n();
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [message, setMessage] = useState<string | null>(null);

  const [overview, setOverview] = useState<LLMManagementOverviewResponse | null>(null);
  const [providerRuntime, setProviderRuntime] = useState<LLMProviderRuntimeStatus[]>([]);
  const [engineRuntime, setEngineRuntime] = useState<LLMEngineRuntimeStatus[]>([]);

  const [providersDraft, setProvidersDraft] = useState('[]');
  const [enginesDraft, setEnginesDraft] = useState('[]');
  const [modelsDraft, setModelsDraft] = useState('[]');
  const [registryDraft, setRegistryDraft] = useState('{}');
  const [routingDraft, setRoutingDraft] = useState('{}');

  const load = async () => {
    setLoading(true);
    setError(null);
    try {
      const payload = await fetchLLMManagementOverview();
      setOverview(payload);
      setProvidersDraft(prettify(payload.providers));
      setEnginesDraft(prettify(payload.inference_engines));
      setModelsDraft(prettify(payload.models));
      setRegistryDraft(prettify(payload.registry));
      setRoutingDraft(prettify(payload.routing_policy));
      setProviderRuntime(payload.providers_runtime);
      const engineStatus = await fetchLLMEngineStatus();
      setEngineRuntime(engineStatus.engine_status);
    } catch (err) {
      const text = err instanceof Error ? err.message : t('llm_mgmt.error_load');
      setError(text);
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

  const saveProviders = async () => {
    setSaving(true);
    setError(null);
    setMessage(null);
    try {
      const payload = parseJson<LLMProviderConfigItem[]>(t('llm_mgmt.section_providers'), providersDraft);
      await updateLLMProviders(payload);
      setMessage(t('llm_mgmt.saved'));
      await load();
    } catch (err) {
      setError(err instanceof Error ? err.message : t('llm_mgmt.error_save'));
    } finally {
      setSaving(false);
    }
  };

  const saveEngines = async () => {
    setSaving(true);
    setError(null);
    setMessage(null);
    try {
      const payload = parseJson<LLMInferenceEngineConfigItem[]>(t('llm_mgmt.section_engines'), enginesDraft);
      await updateLLMInferenceEngines(payload);
      setMessage(t('llm_mgmt.saved'));
      await load();
    } catch (err) {
      setError(err instanceof Error ? err.message : t('llm_mgmt.error_save'));
    } finally {
      setSaving(false);
    }
  };

  const saveModels = async () => {
    setSaving(true);
    setError(null);
    setMessage(null);
    try {
      const payload = parseJson<LLMModelConfigItem[]>(t('llm_mgmt.section_models'), modelsDraft);
      await updateLLMModels(payload);
      setMessage(t('llm_mgmt.saved'));
      await load();
    } catch (err) {
      setError(err instanceof Error ? err.message : t('llm_mgmt.error_save'));
    } finally {
      setSaving(false);
    }
  };

  const saveRegistry = async () => {
    setSaving(true);
    setError(null);
    setMessage(null);
    try {
      const payload = parseJson<Record<string, string>>(t('llm_mgmt.section_registry'), registryDraft);
      await updateLLMRegistry(payload);
      setMessage(t('llm_mgmt.saved'));
      await load();
    } catch (err) {
      setError(err instanceof Error ? err.message : t('llm_mgmt.error_save'));
    } finally {
      setSaving(false);
    }
  };

  const saveRouting = async () => {
    setSaving(true);
    setError(null);
    setMessage(null);
    try {
      const payload = parseJson<LLMRoutingPolicyConfig>(t('llm_mgmt.section_routing'), routingDraft);
      await updateLLMRoutingPolicy(payload);
      setMessage(t('llm_mgmt.saved'));
      await load();
    } catch (err) {
      setError(err instanceof Error ? err.message : t('llm_mgmt.error_save'));
    } finally {
      setSaving(false);
    }
  };

  const handleReload = async () => {
    setSaving(true);
    setError(null);
    setMessage(null);
    try {
      await reloadLLMManagementConfig();
      setMessage(t('llm_mgmt.reloaded'));
      await load();
    } catch (err) {
      setError(err instanceof Error ? err.message : t('llm_mgmt.error_reload'));
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
          {props.helper && <p className="text-[11px] text-slate-400 mt-1">{props.helper}</p>}
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

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      <div className="flex items-start justify-between gap-4">
        <div>
          <h1 className="text-2xl font-bold text-slate-100">{t('llm_mgmt.title')}</h1>
          <p className="text-sm text-slate-500 mt-1">{t('llm_mgmt.subtitle')}</p>
        </div>
        <button
          disabled={saving}
          onClick={() => void handleReload()}
          className="px-4 py-2 bg-slate-800 hover:bg-slate-700 disabled:opacity-50 disabled:cursor-not-allowed text-slate-100 text-sm rounded-lg"
        >
          {saving ? t('llm_mgmt.saving') : t('llm_mgmt.reload')}
        </button>
      </div>

      {error && (
        <div className="bg-red-500/10 border border-red-500/30 rounded-lg p-3 text-sm text-red-300">
          {error}
        </div>
      )}
      {message && (
        <div className="bg-emerald-500/10 border border-emerald-500/30 rounded-lg p-3 text-sm text-emerald-300">
          {message}
        </div>
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
    </div>
  );
}

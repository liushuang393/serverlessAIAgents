import { useEffect, useState } from 'react';
import {
  deleteMCPServer,
  fetchMCPConfig,
  patchMCPLazyLoading,
  upsertMCPServer,
} from '@/api/client';
import type { MCPLazyLoadingConfig, MCPServerConfig } from '@/types';
import { useI18n } from '../i18n';

const EMPTY_SERVER: MCPServerConfig = {
  name: '',
  command: '',
  args: [],
  env: {},
  enabled: true,
  description: '',
};

export function MCPManager() {
  const { t } = useI18n();
  const [configPath, setConfigPath] = useState('');
  const [servers, setServers] = useState<MCPServerConfig[]>([]);
  const [lazyLoading, setLazyLoading] = useState<MCPLazyLoadingConfig>({
    enabled: true,
    threshold: 10,
    auto_load_on_call: true,
    cache_session: true,
  });

  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const [serverDraft, setServerDraft] = useState<MCPServerConfig>(EMPTY_SERVER);
  const [argsInput, setArgsInput] = useState('');
  const [envInput, setEnvInput] = useState('');

  const load = async () => {
    setLoading(true);
    setError(null);
    try {
      const res = await fetchMCPConfig();
      setConfigPath(res.config_path);
      setServers(res.config.servers);
      setLazyLoading(res.config.lazy_loading);
    } catch (err) {
      const msg = err instanceof Error ? err.message : t('mcp_mgr.load_error');
      setError(msg);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    load();
  }, []);

  const parseArgs = (raw: string): string[] => {
    return raw
      .split(',')
      .map((v) => v.trim())
      .filter(Boolean);
  };

  const parseEnv = (raw: string): Record<string, string> => {
    const map: Record<string, string> = {};
    raw
      .split(',')
      .map((v) => v.trim())
      .filter(Boolean)
      .forEach((item) => {
        const [k, ...rest] = item.split('=');
        const key = k?.trim();
        if (!key) {
          return;
        }
        map[key] = rest.join('=').trim();
      });
    return map;
  };

  const handleSaveServer = async () => {
    setSaving(true);
    setError(null);
    try {
      await upsertMCPServer({
        ...serverDraft,
        args: parseArgs(argsInput),
        env: parseEnv(envInput),
      });
      setServerDraft(EMPTY_SERVER);
      setArgsInput('');
      setEnvInput('');
      await load();
    } catch (err) {
      const msg = err instanceof Error ? err.message : t('mcp_mgr.save_server_error');
      setError(msg);
    } finally {
      setSaving(false);
    }
  };

  const handleDelete = async (name: string) => {
    setSaving(true);
    setError(null);
    try {
      await deleteMCPServer(name);
      await load();
    } catch (err) {
      const msg = err instanceof Error ? err.message : t('mcp_mgr.delete_error');
      setError(msg);
    } finally {
      setSaving(false);
    }
  };

  const handleSaveLazy = async () => {
    setSaving(true);
    setError(null);
    try {
      await patchMCPLazyLoading(lazyLoading);
      await load();
    } catch (err) {
      const msg = err instanceof Error ? err.message : t('mcp_mgr.lazy_error');
      setError(msg);
    } finally {
      setSaving(false);
    }
  };

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-slate-100">{t('mcp_mgr.title')}</h1>
        <p className="text-sm text-slate-500 mt-1">{t('mcp_mgr.subtitle')}</p>
      </div>

      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 text-red-300 text-sm">
          {error}
        </div>
      )}

      <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-4 text-xs text-slate-400">
        {t('mcp_mgr.config_file')} <span className="font-mono text-slate-300">{configPath || t('mcp_mgr.config_loading')}</span>
      </div>

      <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
        <h2 className="text-sm font-semibold text-slate-200">{t('mcp_mgr.lazy_loading')}</h2>
        <div className="grid grid-cols-1 md:grid-cols-4 gap-3">
          <Toggle
            label="enabled"
            checked={lazyLoading.enabled}
            onChange={(value) => setLazyLoading((prev) => ({ ...prev, enabled: value }))}
          />
          <Toggle
            label="auto_load_on_call"
            checked={lazyLoading.auto_load_on_call}
            onChange={(value) =>
              setLazyLoading((prev) => ({ ...prev, auto_load_on_call: value }))
            }
          />
          <Toggle
            label="cache_session"
            checked={lazyLoading.cache_session}
            onChange={(value) => setLazyLoading((prev) => ({ ...prev, cache_session: value }))}
          />
          <label className="block space-y-1.5">
            <span className="text-xs text-slate-400">threshold</span>
            <input
              type="number"
              min={1}
              value={lazyLoading.threshold}
              onChange={(e) =>
                setLazyLoading((prev) => ({
                  ...prev,
                  threshold: Number(e.target.value || 1),
                }))
              }
              className="input"
            />
          </label>
        </div>
        <button
          onClick={handleSaveLazy}
          disabled={loading || saving}
          className="px-4 py-2 rounded-lg bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 text-white text-sm"
        >
          {t('mcp_mgr.save')}
        </button>
      </div>

      <div className="bg-slate-900/50 border border-slate-800 rounded-xl overflow-hidden">
        <div className="px-5 py-3 border-b border-slate-800 flex items-center justify-between">
          <h2 className="text-sm font-semibold text-slate-200">Servers ({servers.length})</h2>
          <button
            onClick={load}
            className="text-xs text-indigo-400 hover:text-indigo-300"
            disabled={loading}
          >
            Reload
          </button>
        </div>
        <div className="divide-y divide-slate-800/50">
          {servers.map((server) => (
            <div key={server.name} className="px-5 py-3 flex items-start justify-between gap-4">
              <div className="min-w-0">
                <p className="text-sm text-slate-200 font-medium">{server.name}</p>
                <p className="text-xs text-slate-500 font-mono break-all">
                  {server.command} {server.args.join(' ')}
                </p>
                {server.description && (
                  <p className="text-xs text-slate-400 mt-1">{server.description}</p>
                )}
              </div>
              <div className="flex items-center gap-2 shrink-0">
                <span
                  className={`px-2 py-0.5 rounded-full text-[10px] ${server.enabled
                      ? 'bg-emerald-500/10 text-emerald-300'
                      : 'bg-slate-700/80 text-slate-300'
                    }`}
                >
                  {server.enabled ? 'enabled' : 'disabled'}
                </span>
                <button
                  onClick={() => handleDelete(server.name)}
                  className="text-xs text-red-300 hover:text-red-200"
                  disabled={saving}
                >
                  削除
                </button>
              </div>
            </div>
          ))}
          {!loading && servers.length === 0 && (
            <p className="px-5 py-8 text-center text-sm text-slate-500">MCP サーバーが未登録です</p>
          )}
        </div>
      </div>

      <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
        <h2 className="text-sm font-semibold text-slate-200">サーバー追加 / 更新</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <Field label="name">
            <input
              value={serverDraft.name}
              onChange={(e) => setServerDraft((prev) => ({ ...prev, name: e.target.value }))}
              className="input"
              placeholder="filesystem"
            />
          </Field>
          <Field label="command">
            <input
              value={serverDraft.command}
              onChange={(e) => setServerDraft((prev) => ({ ...prev, command: e.target.value }))}
              className="input"
              placeholder="npx"
            />
          </Field>
          <Field label="args (CSV)">
            <input
              value={argsInput}
              onChange={(e) => setArgsInput(e.target.value)}
              className="input"
              placeholder="-y,@modelcontextprotocol/server-filesystem,/workspace"
            />
          </Field>
          <Field label="env (CSV: KEY=VALUE)">
            <input
              value={envInput}
              onChange={(e) => setEnvInput(e.target.value)}
              className="input"
              placeholder="NODE_ENV=production"
            />
          </Field>
          <Field label="description">
            <input
              value={serverDraft.description}
              onChange={(e) =>
                setServerDraft((prev) => ({ ...prev, description: e.target.value }))
              }
              className="input"
              placeholder="filesystem tools"
            />
          </Field>
          <Toggle
            label="enabled"
            checked={serverDraft.enabled}
            onChange={(value) => setServerDraft((prev) => ({ ...prev, enabled: value }))}
          />
        </div>
        <button
          onClick={handleSaveServer}
          disabled={saving || serverDraft.name.trim() === '' || serverDraft.command.trim() === ''}
          className="px-4 py-2 rounded-lg bg-emerald-600 hover:bg-emerald-700 disabled:opacity-50 text-white text-sm"
        >
          保存
        </button>
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

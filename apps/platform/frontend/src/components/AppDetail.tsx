/**
 * AppDetail - App 詳細ページ.
 *
 * 選択した App のポート、Agent、依存情報、ヘルスチェックを表示。
 */

import { useEffect, useState } from 'react';
import { Link, useParams } from 'react-router-dom';
import axios from 'axios';
import { publishApp, startApp, stopApp } from '@/api/client';
import { useAppStore } from '@/store/useAppStore';
import type { AppActionResponse, HealthCheckAttempt } from '@/types';
import { AppHealthBadge } from './AppHealthBadge';

export function AppDetail() {
  const { name } = useParams<{ name: string }>();
  const {
    selectedApp,
    healthCache,
    loading,
    error,
    loadAppDetail,
    checkHealth,
  } = useAppStore();
  const [actionLoading, setActionLoading] = useState<'publish' | 'start' | 'stop' | null>(null);
  const [actionResult, setActionResult] = useState<AppActionResponse | null>(null);
  const [actionError, setActionError] = useState<string | null>(null);

  useEffect(() => {
    if (name) {
      const load = async () => {
        await checkHealth(name);
        await loadAppDetail(name);
      };
      void load();
    }
  }, [name, loadAppDetail, checkHealth]);

  const health = name ? healthCache[name] : undefined;
  const healthDetails = health?.details;
  const attempts = (healthDetails?.attempts ?? []) as HealthCheckAttempt[];

  const runAction = async (action: 'publish' | 'start' | 'stop') => {
    if (!name) return;
    setActionLoading(action);
    setActionError(null);
    try {
      const handler = action === 'publish' ? publishApp : action === 'start' ? startApp : stopApp;
      const result = await handler(name);
      setActionResult(result);
      await checkHealth(name);
      await loadAppDetail(name);
    } catch (err: unknown) {
      let message: string;
      if (axios.isAxiosError(err)) {
        if (err.code === 'ECONNABORTED') {
          message = `${action} がタイムアウトしました。バックエンドで処理が継続中の可能性があります。`;
        } else if (err.response?.data?.detail) {
          message = String(err.response.data.detail);
        } else if (err.response?.data?.error) {
          message = String(err.response.data.error);
        } else {
          message = err.message;
        }
      } else if (err instanceof Error) {
        message = err.message;
      } else {
        message = `${action} failed`;
      }
      setActionError(message);
    } finally {
      setActionLoading(null);
    }
  };

  if (loading) {
    return (
      <div className="flex justify-center items-center h-full py-24">
        <div className="w-10 h-10 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-6 max-w-4xl mx-auto">
        <Link to="/apps" className="text-sm text-indigo-400 hover:text-indigo-300 mb-4 inline-block">
          ← Back to Apps
        </Link>
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-6 text-center">
          <p className="text-red-400">{error}</p>
        </div>
      </div>
    );
  }

  if (!selectedApp) {
    return (
      <div className="p-6 max-w-4xl mx-auto text-center py-24">
        <p className="text-slate-500">App not found</p>
      </div>
    );
  }

  const app = selectedApp;

  return (
    <div className="p-6 max-w-4xl mx-auto space-y-6">
      {/* 戻るリンク */}
      <Link to="/apps" className="text-sm text-indigo-400 hover:text-indigo-300">
        ← Back to Apps
      </Link>

      {/* ヘッダー */}
      <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-6">
        <div className="flex items-start gap-4">
          <span className="text-4xl">{app.icon}</span>
          <div className="flex-1">
            <div className="flex items-center gap-3 mb-1">
              <h1 className="text-xl font-bold text-slate-100">{app.display_name}</h1>
              <AppHealthBadge status={app.status} />
            </div>
            <p className="text-xs text-slate-500 mb-2">{app.name} · v{app.version}</p>
            {app.description && (
              <p className="text-sm text-slate-400">{app.description}</p>
            )}
          </div>
          <div className="flex flex-wrap items-center gap-2">
            <button
              onClick={() => runAction('publish')}
              disabled={actionLoading !== null}
              className="px-3 py-1.5 bg-emerald-600 hover:bg-emerald-700 disabled:opacity-50 text-white text-xs rounded-lg transition-colors"
            >
              {actionLoading === 'publish' ? 'Publishing...' : '🚀 Publish'}
            </button>
            <button
              onClick={() => runAction('start')}
              disabled={actionLoading !== null}
              className="px-3 py-1.5 bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 text-white text-xs rounded-lg transition-colors"
            >
              {actionLoading === 'start' ? 'Starting...' : '▶ Start'}
            </button>
            <button
              onClick={() => runAction('stop')}
              disabled={actionLoading !== null}
              className="px-3 py-1.5 bg-rose-600 hover:bg-rose-700 disabled:opacity-50 text-white text-xs rounded-lg transition-colors"
            >
              {actionLoading === 'stop' ? 'Stopping...' : '■ Stop'}
            </button>
            <button
              onClick={() => name && checkHealth(name)}
              className="px-3 py-1.5 bg-slate-800 hover:bg-slate-700 text-slate-300 text-xs rounded-lg transition-colors"
            >
              🔍 Health Check
            </button>
          </div>
        </div>
      </div>

      {(actionResult || actionError) && (
        <div className={`border rounded-xl p-4 ${actionError || actionResult?.success === false ? 'bg-red-500/10 border-red-500/30' : 'bg-emerald-500/10 border-emerald-500/30'}`}>
          {actionError && <p className="text-sm text-red-300">{actionError}</p>}
          {actionResult && (
            <div className="space-y-1">
              <p className={`text-sm ${actionResult.success ? 'text-emerald-300' : 'text-red-300'}`}>
                {actionResult.action.toUpperCase()} {actionResult.success ? 'completed' : 'failed'}
              </p>
              <p className="text-xs text-slate-300 font-mono break-all">{actionResult.command}</p>
              {actionResult.error && (
                <p className="text-xs text-red-300">{actionResult.error}</p>
              )}
            </div>
          )}
        </div>
      )}

      {/* ヘルスチェック結果 */}
      {health && (
        <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5">
          <h2 className="text-sm font-semibold text-slate-200 mb-3">Health Check</h2>
          <div className="grid grid-cols-2 sm:grid-cols-4 gap-4 text-sm">
            <InfoItem label="Status" value={health.status} />
            <InfoItem label="Response" value={health.response_time_ms > 0 ? `${health.response_time_ms.toFixed(1)}ms` : '—'} />
            <InfoItem label="Checked" value={new Date(health.checked_at).toLocaleTimeString('ja-JP')} />
            {health.error && <InfoItem label="Error" value={health.error} isError />}
            {typeof healthDetails?.checked_url === 'string' && (
              <InfoItem label="Checked URL" value={healthDetails.checked_url} />
            )}
            {typeof healthDetails?.http_status === 'number' && (
              <InfoItem label="HTTP" value={String(healthDetails.http_status)} />
            )}
            {healthDetails?.docker && (
              <InfoItem
                label="Docker Backend"
                value={healthDetails.docker.backend_running ? 'running' : 'not running'}
              />
            )}
          </div>
          {attempts.length > 0 && (
            <div className="mt-4 pt-4 border-t border-slate-800">
              <p className="text-[11px] text-slate-500 uppercase tracking-wider mb-2">Attempts</p>
              <div className="space-y-1.5">
                {attempts.slice(0, 8).map((a, index) => (
                  <p key={`${a.url}-${index}`} className="text-xs text-slate-400 font-mono break-all">
                    {a.url}
                    {typeof a.http_status === 'number' ? ` -> HTTP ${a.http_status}` : ''}
                    {a.error ? ` -> ${a.error}` : ''}
                  </p>
                ))}
              </div>
            </div>
          )}
          {healthDetails?.docker?.services && healthDetails.docker.services.length > 0 && (
            <div className="mt-4 pt-4 border-t border-slate-800">
              <p className="text-[11px] text-slate-500 uppercase tracking-wider mb-2">Docker Services</p>
              <div className="space-y-1">
                {healthDetails.docker.services.map((service) => (
                  <p key={service.service} className="text-xs text-slate-400 font-mono">
                    {service.service}: {service.running ? 'running' : service.state}
                    {service.published_ports.length > 0 ? ` (ports: ${service.published_ports.join(',')})` : ''}
                  </p>
                ))}
              </div>
            </div>
          )}
        </div>
      )}

      <Section title="Runtime URLs">
        <div className="space-y-2">
          <RuntimeUrlRow label="Backend" url={app.urls?.backend ?? null} />
          <RuntimeUrlRow label="Frontend" url={app.urls?.frontend ?? null} />
          <RuntimeUrlRow label="Health URL" url={app.urls?.health ?? null} />
          <InfoItem label="Database" value={app.urls?.database ?? 'N/A'} />
          {app.urls?.backend && (
            <InfoItem
              label="Framework Call"
              value={`curl ${app.urls.health ?? `${app.urls.backend}/health`}`}
            />
          )}
          <p className="text-xs text-slate-500">
            DB はローカル Docker 公開ポート前提で表示しています。
          </p>
          </div>
      </Section>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
        {/* ポート */}
        <Section title="Ports">
          <div className="grid grid-cols-2 gap-3">
            <InfoItem label="API" value={app.ports.api ? `:${app.ports.api}` : 'N/A'} />
            <InfoItem label="Frontend" value={app.ports.frontend ? `:${app.ports.frontend}` : 'N/A'} />
            <InfoItem label="Database" value={app.ports.db ? `:${app.ports.db}` : 'N/A'} />
            <InfoItem label="Redis" value={app.ports.redis ? `:${app.ports.redis}` : 'N/A'} />
          </div>
        </Section>

        {/* 依存 */}
        <Section title="Dependencies">
          <div className="space-y-2">
            <InfoItem label="Database" value={app.dependencies.database ?? 'None'} />
            <InfoItem label="Redis" value={app.dependencies.redis ? 'Yes' : 'No'} />
            {app.dependencies.external.length > 0 && (
              <InfoItem label="External" value={app.dependencies.external.join(', ')} />
            )}
          </div>
        </Section>
      </div>

      {app.blueprint && (
        <Section title="Runtime Blueprint">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
            <InfoItem label="Engine Pattern" value={app.blueprint.engine_pattern} />
            <InfoItem label="Flow Pattern" value={app.blueprint.flow_pattern ?? 'N/A'} />
            <InfoItem label="LLM Provider" value={app.blueprint.llm_provider ?? 'auto'} />
            <InfoItem label="Default Model" value={app.blueprint.default_model ?? 'N/A'} />
            <InfoItem label="Vector DB" value={app.blueprint.vector_db_provider ?? 'N/A'} />
            <InfoItem label="Vector Collection" value={app.blueprint.vector_db_collection ?? 'N/A'} />
            {app.blueprint.llm_api_key_env && (
              <InfoItem label="LLM API Key ENV" value={app.blueprint.llm_api_key_env} />
            )}
            {app.blueprint.vector_db_api_key_env && (
              <InfoItem label="Vector API Key ENV" value={app.blueprint.vector_db_api_key_env} />
            )}
          </div>
        </Section>
      )}

      {/* Agents */}
      {app.agents.length > 0 && (
        <Section title={`Agents (${app.agents.length})`}>
          <div className="divide-y divide-slate-800/50">
            {app.agents.map((agent) => (
              <div key={agent.name} className="py-3 first:pt-0 last:pb-0">
                <p className="text-sm font-medium text-slate-200">🤖 {agent.name}</p>
                {agent.module && (
                  <p className="text-xs text-slate-500 mt-0.5 font-mono">{agent.module}</p>
                )}
                {agent.capabilities.length > 0 && (
                  <div className="flex flex-wrap gap-1.5 mt-1.5">
                    {agent.capabilities.map((cap) => (
                      <span key={cap} className="px-2 py-0.5 bg-indigo-500/10 text-indigo-400 text-[10px] rounded-full border border-indigo-500/20">
                        {cap}
                      </span>
                    ))}
                  </div>
                )}
              </div>
            ))}
          </div>
        </Section>
      )}

      {/* タグ */}
      {app.tags.length > 0 && (
        <div className="flex flex-wrap gap-2">
          {app.tags.map((tag) => (
            <span key={tag} className="px-3 py-1 bg-slate-800/80 text-slate-400 text-xs rounded-full">
              {tag}
            </span>
          ))}
        </div>
      )}
    </div>
  );
}

function RuntimeUrlRow({ label, url }: { label: string; url: string | null }) {
  return (
    <div className="flex items-center justify-between gap-3">
      <InfoItem label={label} value={url ?? 'N/A'} />
      {url && (
        <a
          href={url}
          target="_blank"
          rel="noreferrer"
          className="text-xs px-2.5 py-1 rounded-md border border-indigo-500/40 text-indigo-300 hover:bg-indigo-500/10 transition-colors"
        >
          Open
        </a>
      )}
    </div>
  );
}

/** セクションラッパー */
function Section({ title, children }: { title: string; children: React.ReactNode }) {
  return (
    <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-5">
      <h2 className="text-sm font-semibold text-slate-200 mb-3">{title}</h2>
      {children}
    </div>
  );
}

/** 情報表示アイテム */
function InfoItem({
  label,
  value,
  isError = false,
}: {
  label: string;
  value: string;
  isError?: boolean;
}) {
  return (
    <div>
      <p className="text-[10px] text-slate-500 uppercase tracking-wider mb-0.5">
        {label}
      </p>
      <p className={`text-sm ${isError ? 'text-red-400' : 'text-slate-300'}`}>
        {value}
      </p>
    </div>
  );
}

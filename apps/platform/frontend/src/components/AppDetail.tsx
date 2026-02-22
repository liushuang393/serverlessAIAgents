/**
 * AppDetail - App 詳細ページ.
 *
 * 選択した App のポート、Agent、依存情報、ヘルスチェックを表示。
 */

import { useEffect, useState } from 'react';
import { Link, useParams } from 'react-router-dom';
import axios from 'axios';
import { localStartApp, publishApp, startApp, stopApp } from '@/api/client';
import { useAppStore } from '@/store/useAppStore';
import type { AppActionResponse, HealthCheckAttempt, RuntimeCommands } from '@/types';
import { AppHealthBadge } from './AppHealthBadge';
import { useI18n } from '../i18n';

export function AppDetail() {
  const { t } = useI18n();
  const { name } = useParams<{ name: string }>();
  const {
    selectedApp,
    healthCache,
    loading,
    error,
    loadAppDetail,
    checkHealth,
  } = useAppStore();
  const [actionLoading, setActionLoading] = useState<'publish' | 'start' | 'stop' | 'local-start' | null>(null);
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

  const runAction = async (action: 'publish' | 'start' | 'stop' | 'local-start') => {
    if (!name) return;
    setActionLoading(action);
    setActionError(null);
    try {
      // アクションに応じたハンドラを選択
      let handler: (appName: string) => Promise<AppActionResponse>;
      switch (action) {
        case 'publish':
          handler = publishApp;
          break;
        case 'start':
          handler = startApp;
          break;
        case 'stop':
          handler = stopApp;
          break;
        case 'local-start':
          handler = localStartApp;
          break;
      }
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
          {t('app_detail.back')}
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
        <p className="text-slate-500">{t('app_detail.not_found')}</p>
      </div>
    );
  }

  const app = selectedApp;
  const runtime = app.runtime;
  const db = runtime?.database;
  const commands = runtime?.commands;
  const healthPath = app.entry_points.health?.startsWith('/')
    ? app.entry_points.health
    : `/${app.entry_points.health ?? 'health'}`;
  const backendUrl = app.ports.api
    ? `http://localhost:${app.ports.api}`
    : (runtime?.urls.backend ?? app.urls?.backend ?? null);
  const frontendUrl = app.ports.frontend
    ? `http://localhost:${app.ports.frontend}`
    : (runtime?.urls.frontend ?? app.urls?.frontend ?? null);
  const healthUrl = app.ports.api
    ? `http://localhost:${app.ports.api}${healthPath}`
    : (runtime?.urls.health ?? app.urls?.health ?? null);
  const databaseUrl = runtime?.urls.database ?? app.urls?.database ?? db?.url ?? null;
  const healthCurlTarget =
    healthUrl ??
    (backendUrl ? `${backendUrl}/health` : null);

  return (
    <div className="p-6 max-w-4xl mx-auto space-y-6">
      {/* 戻るリンク */}
      <Link to="/apps" className="text-sm text-indigo-400 hover:text-indigo-300">
        {t('app_detail.back')}
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
              {actionLoading === 'publish' ? t('app_detail.publishing') : t('app_detail.publish')}
            </button>
            <button
              onClick={() => runAction('start')}
              disabled={actionLoading !== null}
              className="px-3 py-1.5 bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 text-white text-xs rounded-lg transition-colors"
            >
              {actionLoading === 'start' ? t('app_detail.starting') : t('app_detail.start')}
            </button>
            <button
              onClick={() => runAction('stop')}
              disabled={actionLoading !== null}
              className="px-3 py-1.5 bg-rose-600 hover:bg-rose-700 disabled:opacity-50 text-white text-xs rounded-lg transition-colors"
            >
              {actionLoading === 'stop' ? t('app_detail.stopping') : t('app_detail.stop')}
            </button>
            <button
              onClick={() => runAction('local-start')}
              disabled={actionLoading !== null}
              className="px-3 py-1.5 bg-amber-600 hover:bg-amber-700 disabled:opacity-50 text-white text-xs rounded-lg transition-colors"
            >
              {actionLoading === 'local-start' ? t('app_detail.starting') : t('app_detail.local')}
            </button>
            <button
              onClick={async () => {
                if (!name) return;
                await checkHealth(name);
                await loadAppDetail(name);
              }}
              className="px-3 py-1.5 bg-slate-800 hover:bg-slate-700 text-slate-300 text-xs rounded-lg transition-colors"
            >
              {t('app_detail.health_check_btn')}
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
                {actionResult.action.toUpperCase()} {actionResult.success ? t('app_detail.health_check_completed') : t('app_detail.health_check_failed')}
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
          <h2 className="text-sm font-semibold text-slate-200 mb-3">{t('app_detail.health_check_title')}</h2>
          <div className="grid grid-cols-2 sm:grid-cols-4 gap-4 text-sm">
            <InfoItem label={t('app_detail.hc_status')} value={health.status} />
            <InfoItem label={t('app_detail.hc_response')} value={health.response_time_ms > 0 ? `${health.response_time_ms.toFixed(1)}ms` : '—'} />
            <InfoItem label={t('app_detail.hc_checked')} value={new Date(health.checked_at).toLocaleTimeString('ja-JP')} />
            {health.error && <InfoItem label={t('app_detail.hc_error')} value={health.error} isError />}
            {typeof healthDetails?.checked_url === 'string' && (
              <InfoItem label={t('app_detail.hc_url')} value={healthDetails.checked_url} />
            )}
            {typeof healthDetails?.http_status === 'number' && (
              <InfoItem label={t('app_detail.hc_http').replaceAll('{code}', String(healthDetails.http_status))} value={String(healthDetails.http_status)} />
            )}
            {healthDetails?.docker && (
              <InfoItem
                label={t('app_detail.docker_backend')}
                value={healthDetails.docker.backend_running ? t('app_detail.docker_running') : t('app_detail.docker_not_running')}
              />
            )}
          </div>
          {attempts.length > 0 && (
            <div className="mt-4 pt-4 border-t border-slate-800">
              <p className="text-[11px] text-slate-500 uppercase tracking-wider mb-2">{t('app_detail.docker_attempts')}</p>
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
              <p className="text-[11px] text-slate-500 uppercase tracking-wider mb-2">{t('app_detail.docker_services')}</p>
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

      <Section title={t('app_detail.runtime_urls')}>
        <div className="space-y-2">
          <RuntimeUrlRow label={t('app_detail.backend')} url={backendUrl} />
          <RuntimeUrlRow label={t('app_detail.frontend')} url={frontendUrl} />
          <RuntimeUrlRow label={t('app_detail.hc_url')} url={healthUrl} />
          <RuntimeUrlRow label={t('app_detail.db_connections')} url={databaseUrl} />
          {healthCurlTarget && (
            <InfoItem
              label="Framework Call"
              value={`curl ${healthCurlTarget}`}
            />
          )}
        </div>
      </Section>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
        {/* ポート */}
        <Section title="Ports">
          <div className="grid grid-cols-2 gap-3">
            <InfoItem label="API" value={app.ports.api ? `:${app.ports.api}` : t('app_detail.na')} />
            <InfoItem label={t('app_detail.frontend')} value={app.ports.frontend ? `:${app.ports.frontend}` : t('app_detail.na')} />
            <InfoItem label={t('app_detail.primary_db')} value={app.ports.db ? `:${app.ports.db}` : t('app_detail.na')} />
            <InfoItem label={t('app_detail.redis')} value={app.ports.redis ? `:${app.ports.redis}` : t('app_detail.na')} />
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

      <Section title={t('app_detail.db_connections')}>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
          <InfoItem label="Kind" value={db?.kind ?? app.dependencies.database ?? t('app_detail.na')} />
          <InfoItem label="URL" value={db?.url ?? databaseUrl ?? t('app_detail.na')} />
          <InfoItem label="Host" value={db?.host ?? t('app_detail.na')} />
          <InfoItem label="Port" value={db?.port ? String(db.port) : t('app_detail.na')} />
          <InfoItem label="Name" value={db?.name ?? t('app_detail.na')} />
          <InfoItem label="User" value={db?.user ?? t('app_detail.na')} />
          <InfoItem label="Password" value={db?.password ?? t('app_detail.na')} />
          <InfoItem label="Password ENV" value={db?.password_env ?? t('app_detail.na')} />
          {db?.note && <InfoItem label="Note" value={db.note} />}
        </div>
      </Section>

      <Section title={t('app_detail.startup_commands')}>
        <div className="space-y-2">
          <CommandRow label={t('app_detail.backend_cmd')} command={commands?.backend_dev ?? null} />
          <CommandRow label={t('app_detail.frontend_cmd')} command={commands?.frontend_dev ?? null} />
          <CommandRow label={t('app_detail.publish')} command={commands?.publish ?? null} />
          <CommandRow label={t('app_detail.start')} command={commands?.start ?? null} />
          <CommandRow label={t('app_detail.stop')} command={commands?.stop ?? null} />
          {!hasAnyCommand(commands) && (
            <p className="text-xs text-slate-500">
              <code>app_config.json</code> -&gt; <code>runtime.commands</code> に
              コマンドを設定すると、Platform から一元表示・実行できます。
            </p>
          )}
        </div>
      </Section>

      {app.blueprint && (
        <Section title={t('app_detail.runtime_config')}>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
            <InfoItem label={t('app_detail.engine_pattern')} value={app.blueprint.engine_pattern} />
            <InfoItem label={t('app_detail.flow_pattern')} value={app.blueprint.flow_pattern ?? t('app_detail.na')} />
            <InfoItem label={t('app_detail.llm_provider')} value={app.blueprint.llm_provider ?? 'auto'} />
            <InfoItem label={t('app_detail.default_model')} value={app.blueprint.default_model ?? t('app_detail.na')} />
            <InfoItem label={t('app_detail.vector_db')} value={app.blueprint.vector_db_provider ?? t('app_detail.na')} />
            <InfoItem label="Vector Collection" value={app.blueprint.vector_db_collection ?? t('app_detail.na')} />
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
        <Section title={`${t('app_detail.agent_name')} (${app.agents.length})`}>
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

function hasAnyCommand(commands: RuntimeCommands | undefined): boolean {
  if (!commands) {
    return false;
  }
  return [commands.backend_dev, commands.frontend_dev, commands.publish, commands.start, commands.stop]
    .some((value) => typeof value === 'string' && value.trim().length > 0);
}

function CommandRow({ label, command }: { label: string; command: string | null }) {
  return (
    <div className="flex items-start justify-between gap-3">
      <InfoItem label={label} value={command ?? 'N/A'} />
      {command && (
        <button
          onClick={() => navigator.clipboard.writeText(command).catch(() => {})}
          className="text-xs px-2.5 py-1 rounded-md border border-slate-700 text-slate-300 hover:bg-slate-800 transition-colors"
        >
          Copy
        </button>
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

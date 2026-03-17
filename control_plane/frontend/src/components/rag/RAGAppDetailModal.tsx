/**
 * RAG アプリ詳細モーダル.
 *
 * 選択されたアプリの RAG 設定を読み取り専用で表示。
 * 編集機能は各アプリの管理 UI にリンクする。
 */
import { useCallback, useEffect, useState } from 'react';
import { useI18n } from '../../i18n';

import { fetchAppRAGIngestRuns } from '@/api/client';
import type { AppRAGConfig, RAGIngestRunSummary } from '@/types';

interface Props {
  config: AppRAGConfig;
  onClose: () => void;
}

/** 期間フォーマット */
function fmtDuration(ms: number): string {
  if (ms < 1000) return `${ms}ms`;
  if (ms < 60_000) return `${(ms / 1000).toFixed(1)}s`;
  return `${(ms / 60_000).toFixed(1)}m`;
}

/** セクションヘッダー */
function SectionTitle({ children }: { children: React.ReactNode }) {
  return (
    <h3 className="text-xs font-semibold text-slate-400 uppercase tracking-wider mb-2">
      {children}
    </h3>
  );
}

/** キーバリュー行 */
function KV({ label, value }: { label: string; value: React.ReactNode }) {
  return (
    <div className="flex justify-between py-1.5 border-b border-slate-800/50">
      <span className="text-xs text-slate-500">{label}</span>
      <span className="text-xs text-slate-200">{value ?? '-'}</span>
    </div>
  );
}

/** RAG アプリ詳細モーダル */
export function RAGAppDetailModal({ config, onClose }: Props) {
  const { t } = useI18n();
  const r = config.rag;

  const [ingestRuns, setIngestRuns] = useState<RAGIngestRunSummary[]>([]);
  const [ingestLoading, setIngestLoading] = useState(false);

  const loadIngestRuns = useCallback(async () => {
    setIngestLoading(true);
    try {
      const resp = await fetchAppRAGIngestRuns(config.app_name, 10);
      setIngestRuns(resp.runs);
    } catch {
      setIngestRuns([]);
    } finally {
      setIngestLoading(false);
    }
  }, [config.app_name]);

  useEffect(() => {
    void loadIngestRuns();
  }, [loadIngestRuns]);

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 backdrop-blur-sm">
      <div className="relative w-full max-w-2xl max-h-[85vh] overflow-y-auto rounded-2xl border border-slate-700 bg-slate-900 shadow-2xl">
        {/* ヘッダー */}
        <div className="sticky top-0 z-10 flex items-center justify-between bg-slate-900 border-b border-slate-800 px-6 py-4">
          <div className="flex items-center gap-3">
            <span className="text-xl">{config.icon || '📱'}</span>
            <div>
              <h2 className="text-base font-semibold text-slate-100">
                {config.display_name}
              </h2>
              <p className="text-xs text-slate-500">{config.app_name}</p>
            </div>
          </div>
          <button
            onClick={onClose}
            className="text-slate-400 hover:text-slate-200 text-lg px-2"
          >
            &times;
          </button>
        </div>

        <div className="px-6 py-4 space-y-5">
          {/* RAG ステータス */}
          <div className="flex items-center gap-2">
            <span
              className={`h-2.5 w-2.5 rounded-full ${
                r.enabled ? 'bg-emerald-400' : 'bg-slate-500'
              }`}
            />
            <span className="text-sm text-slate-200">
              {r.enabled
                ? t('rag_dashboard.rag_enabled')
                : t('rag_dashboard.rag_disabled')}
            </span>
          </div>

          {r.enabled && (
            <>
              {/* チャンキング設定 */}
              <section>
                <SectionTitle>
                  {t('rag_dashboard.chunk_settings')}
                </SectionTitle>
                <KV label="Strategy" value={r.chunk_strategy} />
                <KV label="Chunk Size" value={r.chunk_size} />
                <KV label="Chunk Overlap" value={r.chunk_overlap} />
              </section>

              {/* 検索設定 */}
              <section>
                <SectionTitle>
                  {t('rag_dashboard.retrieval_settings')}
                </SectionTitle>
                <KV label="Retrieval Method" value={r.retrieval_method} />
                <KV label="Reranker" value={r.reranker || 'なし'} />
                <KV label="Top-K" value={r.top_k} />
                <KV
                  label="Score Threshold"
                  value={r.score_threshold != null ? r.score_threshold : '-'}
                />
              </section>

              {/* ベクトル DB */}
              <section>
                <SectionTitle>
                  {t('rag_dashboard.vector_db')}
                </SectionTitle>
                <KV label="Provider" value={r.vector_provider || '-'} />
                <KV label="URL" value={r.vector_url || '-'} />
                <KV label="Collection" value={r.vector_collection || '-'} />
                <KV label="Embedding Model" value={r.embedding_model || '-'} />
              </section>

              {/* データソース */}
              {r.data_sources.length > 0 && (
                <section>
                  <SectionTitle>
                    {t('rag_dashboard.data_sources')}{' '}
                    ({r.data_sources.length})
                  </SectionTitle>
                  <div className="space-y-1.5">
                    {r.data_sources.map((ds) => (
                      <div
                        key={ds.id}
                        className="flex items-center gap-2 p-2 rounded-lg bg-slate-800/40 text-xs"
                      >
                        <span
                          className={`h-1.5 w-1.5 rounded-full ${
                            ds.enabled !== false
                              ? 'bg-emerald-400'
                              : 'bg-slate-500'
                          }`}
                        />
                        <span className="text-slate-300 font-mono">
                          {ds.type}
                        </span>
                        <span className="text-slate-500 truncate flex-1">
                          {ds.uri || ds.label || '-'}
                        </span>
                      </div>
                    ))}
                  </div>
                </section>
              )}

              {/* インジェスト履歴 */}
              <section>
                <SectionTitle>
                  {t('rag_dashboard.ingest_history')}
                </SectionTitle>
                {ingestLoading ? (
                  <p className="text-xs text-slate-500">読み込み中...</p>
                ) : ingestRuns.length === 0 ? (
                  <p className="text-xs text-slate-500">履歴なし</p>
                ) : (
                  <div className="space-y-1">
                    {ingestRuns.map((run) => (
                      <div
                        key={run.run_id}
                        className="flex items-center justify-between p-2 rounded-lg bg-slate-800/40 text-xs"
                      >
                        <div className="flex items-center gap-2">
                          <span
                            className={`px-1.5 py-0.5 rounded-full ${
                              run.status === 'completed'
                                ? 'text-emerald-400 bg-emerald-500/10'
                                : run.status === 'failed'
                                  ? 'text-rose-400 bg-rose-500/10'
                                  : run.status === 'running'
                                    ? 'text-blue-400 bg-blue-500/10'
                                    : 'text-slate-400 bg-slate-500/10'
                            }`}
                          >
                            {run.status}
                          </span>
                          <span className="text-slate-500 font-mono">
                            {run.run_id.slice(0, 8)}
                          </span>
                        </div>
                        <div className="flex items-center gap-3 text-slate-500">
                          <span>{fmtDuration(run.duration_ms)}</span>
                          {run.started_at && (
                            <span>
                              {new Date(run.started_at).toLocaleString('ja-JP')}
                            </span>
                          )}
                        </div>
                      </div>
                    ))}
                  </div>
                )}
              </section>
            </>
          )}

          {/* 認証情報 */}
          {config.auth && (
            <section>
              <SectionTitle>
                {t('rag_dashboard.auth_info')}
              </SectionTitle>
              <KV
                label="Auth Mode"
                value={config.auth.mode || 'legacy'}
              />
              <KV
                label="Tenant Claim Key"
                value={config.auth.tenant_claim_key || '-'}
              />
              <KV
                label="SSO"
                value={
                  config.auth.allow_same_tenant_sso ? '有効' : '無効'
                }
              />
            </section>
          )}
        </div>
      </div>
    </div>
  );
}

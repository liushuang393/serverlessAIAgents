/**
 * 設定モーダルコンポーネント.
 *
 * 目的: RAG設定の管理
 * API対接: GET/PUT /api/config/rag
 */

import React, { useState, useEffect, useCallback } from 'react';
import { apiClient } from '../api/client';
import { useI18n } from '../i18n';

/** RAGソース設定 */
interface RAGSourceConfig {
  name: string;
  enabled: boolean;
  directory?: string;
  url?: string;
}

/** Agent RAG設定 */
interface AgentRAGConfig {
  agent_id: string;
  agent_name: string;
  use_rag: boolean;
  rag_sources: RAGSourceConfig[];
  top_k: number;
  min_similarity: number;
}

interface SettingsModalProps {
  isOpen: boolean;
  onClose: () => void;
}

export const SettingsModal: React.FC<SettingsModalProps> = ({ isOpen, onClose }) => {
  const { t } = useI18n();
  const [configs, setConfigs] = useState<AgentRAGConfig[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [selectedAgent, setSelectedAgent] = useState<string>('shu');
  const [isSaving, setIsSaving] = useState(false);

  /** RAG設定を取得 */
  const fetchConfigs = useCallback(async () => {
    setIsLoading(true);
    setError(null);
    try {
      const response = await apiClient.get('/api/config/rag');
      setConfigs(response.data as AgentRAGConfig[]);
    } catch (err) {
      // エラー詳細をUIに表示
      const errorDetail = err instanceof Error ? err.message : t('settings.unknown_error');
      setError(`${t('settings.fetch_error')} ${errorDetail}`);
    } finally {
      setIsLoading(false);
    }
  }, []);

  /** 設定を保存 */
  const saveConfig = useCallback(async (agentId: string, updates: Partial<AgentRAGConfig>) => {
    setIsSaving(true);
    try {
      await apiClient.put(`/api/config/rag/${agentId}`, updates);
      await fetchConfigs();
    } catch (err) {
      // エラー詳細をUIに表示
      const errorDetail = err instanceof Error ? err.message : t('settings.unknown_error');
      setError(`${t('settings.save_error')} ${errorDetail}`);
    } finally {
      setIsSaving(false);
    }
  }, [fetchConfigs]);

  /** RAG使用トグル */
  const toggleRAG = useCallback((agentId: string) => {
    const cfg = configs.find(c => c.agent_id === agentId);
    if (cfg) {
      saveConfig(agentId, { use_rag: !cfg.use_rag });
    }
  }, [configs, saveConfig]);

  /** パラメータ更新 */
  const updateParam = useCallback((agentId: string, key: string, value: number) => {
    saveConfig(agentId, { [key]: value });
  }, [saveConfig]);

  useEffect(() => {
    if (isOpen) {
      fetchConfigs();
    }
  }, [isOpen, fetchConfigs]);

  if (!isOpen) return null;

  const currentConfig = configs.find(c => c.agent_id === selectedAgent);

  return (
    <div className="fixed inset-0 bg-black/60 backdrop-blur-sm flex items-center justify-center z-50">
      <div className="bg-[#12121a] rounded-2xl border border-white/10 w-full max-w-2xl max-h-[80vh] overflow-hidden">
        {/* Header */}
        <div className="flex items-center justify-between px-6 py-4 border-b border-white/5">
          <h2 className="text-lg font-semibold text-white flex items-center gap-2">
            <span aria-hidden="true">⚙️</span> {t('settings.rag_settings')}
          </h2>
          <button
            onClick={onClose}
            className="text-slate-400 hover:text-white transition-colors p-1"
          >
            ✕
          </button>
        </div>

        {/* Content */}
        <div className="p-6 overflow-y-auto max-h-[60vh]">
          {isLoading ? (
            <div className="text-center py-8">
              <div className="w-8 h-8 border-2 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin mx-auto mb-2" />
              <p className="text-slate-400 text-sm">{t('settings.loading')}</p>
            </div>
          ) : error ? (
            <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 text-red-400">
              {error}
            </div>
          ) : (
            <>
              {/* Agent選択タブ */}
              <div className="flex gap-2 mb-6 border-b border-white/5 pb-4">
                {configs.map(cfg => (
                  <button
                    key={cfg.agent_id}
                    onClick={() => setSelectedAgent(cfg.agent_id)}
                    className={`px-4 py-2 rounded-lg text-sm font-medium transition-colors ${
                      selectedAgent === cfg.agent_id
                        ? 'bg-indigo-500/20 text-indigo-400 border border-indigo-500/30'
                        : 'text-slate-400 hover:bg-slate-800'
                    }`}
                  >
                    {cfg.agent_name}
                  </button>
                ))}
              </div>

              {/* 選択中Agent設定 */}
              {currentConfig && (
                <div className="space-y-6">
                  {/* RAG有効/無効 */}
                  <div className="flex items-center justify-between p-4 bg-[#0a0a0f] rounded-xl">
                    <div>
                      <div className="text-white font-medium">{t('settings.use_rag')}</div>
                      <div className="text-sm text-slate-400">{t('settings.rag_desc')}</div>
                    </div>
                    <button
                      onClick={() => toggleRAG(currentConfig.agent_id)}
                      disabled={isSaving}
                      className={`w-14 h-7 rounded-full transition-colors ${
                        currentConfig.use_rag ? 'bg-indigo-500' : 'bg-slate-700'
                      }`}
                    >
                      <div className={`w-5 h-5 rounded-full bg-white transition-transform ${
                        currentConfig.use_rag ? 'translate-x-8' : 'translate-x-1'
                      }`} />
                    </button>
                  </div>

                  {/* RAGパラメータ設定 */}
                  {currentConfig.use_rag && (
                    <>
                      {/* Top K */}
                      <div className="p-4 bg-[#0a0a0f] rounded-xl">
                        <div className="flex justify-between mb-2">
                          <span className="text-white font-medium">{t('settings.top_k_label')}</span>
                          <span className="text-indigo-400">{currentConfig.top_k}</span>
                        </div>
                        <input
                          type="range"
                          min="1"
                          max="10"
                          value={currentConfig.top_k}
                          onChange={(e) => updateParam(currentConfig.agent_id, 'top_k', parseInt(e.target.value))}
                          className="w-full accent-indigo-500"
                        />
                        <div className="flex justify-between text-xs text-slate-500 mt-1">
                          <span>1</span>
                          <span>10</span>
                        </div>
                      </div>

                      {/* Min Similarity */}
                      <div className="p-4 bg-[#0a0a0f] rounded-xl">
                        <div className="flex justify-between mb-2">
                          <span className="text-white font-medium">{t('settings.min_similarity_label')}</span>
                          <span className="text-indigo-400">{(currentConfig.min_similarity * 100).toFixed(0)}%</span>
                        </div>
                        <input
                          type="range"
                          min="0"
                          max="100"
                          value={currentConfig.min_similarity * 100}
                          onChange={(e) => updateParam(currentConfig.agent_id, 'min_similarity', parseInt(e.target.value) / 100)}
                          className="w-full accent-indigo-500"
                        />
                        <div className="flex justify-between text-xs text-slate-500 mt-1">
                          <span>0%</span>
                          <span>100%</span>
                        </div>
                      </div>

                      {/* RAGソース一覧 */}
                      <div className="p-4 bg-[#0a0a0f] rounded-xl">
                        <div className="text-white font-medium mb-3">{t('settings.rag_sources')}</div>
                        <div className="space-y-2">
                          {currentConfig.rag_sources.map((source, idx) => (
                            <div key={idx} className="flex items-center gap-3 p-3 bg-slate-800/50 rounded-lg">
                              <div className={`w-2 h-2 rounded-full ${source.enabled ? 'bg-green-400' : 'bg-slate-500'}`} />
                              <div className="flex-1">
                                <div className="text-sm text-white">{source.name}</div>
                                {source.directory && (
                                  <div className="text-xs text-slate-500">📁 {source.directory}</div>
                                )}
                              </div>
                              <span className={`text-xs px-2 py-1 rounded ${
                                source.enabled ? 'bg-green-500/10 text-green-400' : 'bg-slate-700 text-slate-400'
                              }`}>
                                {source.enabled ? t('settings.enabled') : t('settings.disabled')}
                              </span>
                            </div>
                          ))}
                          {currentConfig.rag_sources.length === 0 && (
                            <div className="text-center py-4 text-slate-500 text-sm">
                              {t('settings.no_rag_sources')}
                            </div>
                          )}
                        </div>
                      </div>
                    </>
                  )}
                </div>
              )}
            </>
          )}
        </div>

        {/* Footer */}
        <div className="px-6 py-4 border-t border-white/5 flex justify-end">
          <button
            onClick={onClose}
            className="px-4 py-2 bg-slate-700 hover:bg-slate-600 text-white rounded-lg transition-colors"
          >
            {t('common.close')}
          </button>
        </div>
      </div>
    </div>
  );
};

export default SettingsModal;

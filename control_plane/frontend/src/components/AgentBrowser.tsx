/**
 * AgentBrowser - 全 App 横断 Agent 一覧.
 *
 * Agent Type フィルタ付きで Agent を表示。
 */

import { useEffect, useState } from "react";
import { useAppStore } from "@/store/useAppStore";
import { useI18n } from "../i18n";

export function AgentBrowser() {
  const { t } = useI18n();
  const { agents, loading, error, loadAgents, clearError } = useAppStore();

  const [selectedType, setSelectedType] = useState<string>("");

  useEffect(() => {
    loadAgents();
  }, [loadAgents]);

  const typeCounts = agents.reduce<Record<string, number>>((acc, agent) => {
    const key = agent.agent_type || "specialist";
    acc[key] = (acc[key] ?? 0) + 1;
    return acc;
  }, {});

  const visibleAgents = selectedType
    ? agents.filter(
        (agent) => (agent.agent_type || "specialist") === selectedType,
      )
    : agents;

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      {/* ヘッダー */}
      <div>
        <h1 className="text-2xl font-bold text-slate-100">
          {t("agent_browser.title")}
        </h1>
        <p className="text-sm text-slate-500 mt-1">
          {t("agent_browser.subtitle")}（{visibleAgents.length}）
        </p>
      </div>

      {/* エラー */}
      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button
            onClick={clearError}
            className="text-red-400 hover:text-red-300 text-xs"
          >
            ✕
          </button>
        </div>
      )}

      {/* Type フィルタ */}
      {Object.keys(typeCounts).length > 0 && (
        <div className="bg-slate-900/50 border border-slate-800 rounded-xl p-4">
          <p className="text-xs text-slate-500 uppercase tracking-wider mb-3">
            {t("agent_browser.filter_type")}
          </p>
          <div className="flex flex-wrap gap-2">
            <button
              onClick={() => setSelectedType("")}
              className={`px-3 py-1.5 text-xs rounded-full transition-colors ${
                selectedType === ""
                  ? "bg-cyan-600 text-white"
                  : "bg-slate-800 text-slate-400 hover:bg-slate-700"
              }`}
            >
              {t("agent_browser.all")}
            </button>
            {Object.entries(typeCounts)
              .sort((a, b) => a[0].localeCompare(b[0]))
              .map(([type, count]) => (
                <button
                  key={type}
                  onClick={() =>
                    setSelectedType(type === selectedType ? "" : type)
                  }
                  className={`px-3 py-1.5 text-xs rounded-full transition-colors ${
                    selectedType === type
                      ? "bg-cyan-600 text-white"
                      : "bg-slate-800 text-slate-400 hover:bg-slate-700"
                  }`}
                >
                  {type}
                  <span className="ml-1.5 opacity-60">({count})</span>
                </button>
              ))}
          </div>
        </div>
      )}

      {/* ローディング */}
      {loading && (
        <div className="flex justify-center py-12">
          <div className="w-10 h-10 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin" />
        </div>
      )}

      {/* Agent 一覧テーブル */}
      {!loading && visibleAgents.length > 0 && (
        <div className="bg-slate-900/50 border border-slate-800 rounded-xl overflow-hidden">
          <div className="px-5 py-3.5 border-b border-slate-800">
            <h2 className="text-sm font-semibold text-slate-200">
              {t("agent_browser.agents_count")}
            </h2>
          </div>
          <div className="divide-y divide-slate-800/50 max-h-[70vh] overflow-y-auto">
            {visibleAgents.map((agent, idx) => (
              <div
                key={`${agent.app_name}-${agent.name}-${idx}`}
                className="px-5 py-3.5 hover:bg-slate-800/30 transition-colors"
              >
                <div className="flex items-center gap-4">
                  <span className="text-2xl">{agent.app_icon}</span>
                  <div className="flex-1 min-w-0">
                    <div className="flex items-center gap-2">
                      <p className="text-sm font-medium text-slate-200">
                        {agent.name}
                      </p>
                      <span className="px-2 py-0.5 bg-cyan-500/10 text-cyan-400 text-[10px] rounded-full">
                        {agent.agent_type || "specialist"}
                      </span>
                    </div>
                    <p className="text-xs text-slate-500">
                      {agent.app_display_name}
                    </p>
                    {agent.module && (
                      <p className="text-[10px] text-slate-600 font-mono mt-0.5">
                        {agent.module}
                      </p>
                    )}
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      )}

      {/* 空状態 */}
      {!loading && visibleAgents.length === 0 && (
        <div className="text-center py-16">
          <p className="text-4xl mb-4">🤖</p>
          <p className="text-slate-400">{t("agent_browser.no_agents")}</p>
        </div>
      )}
    </div>
  );
}

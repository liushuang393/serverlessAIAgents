/**
 * AgentPatterns - Agent タイプ / App テンプレート / Business Base.
 */

import { useEffect, useMemo, useState } from "react";
import {
  fetchAgentTypes,
  fetchAgentsByBusinessBase,
  fetchAgentsByType,
  fetchAppTemplates,
} from "@/api/client";
import type {
  AgentBusinessBaseGroup,
  AgentTypeDefinition,
  AgentTypeGroup,
  AppTemplateInfo,
} from "@/types";
import { useI18n } from "../i18n";

type ViewTab = "types" | "templates" | "business";

const BUSINESS_ICONS: Readonly<Record<string, string>> = {
  platform: "🏗️",
  knowledge: "📚",
  reasoning: "🧠",
  interaction: "💬",
  integration: "🔌",
  operations: "⚙️",
  governance: "🛡️",
  media: "🎨",
  custom: "📦",
};

const BEHAVIOR_KEYS = [
  "observe",
  "reflect",
  "experiment",
  "intent_recognition",
  "decompose",
  "delegate",
] as const;

export function AgentPatterns() {
  const { t } = useI18n();
  const [typeDefinitions, setTypeDefinitions] = useState<AgentTypeDefinition[]>(
    [],
  );
  const [typeGroups, setTypeGroups] = useState<AgentTypeGroup[]>([]);
  const [templateCatalog, setTemplateCatalog] = useState<AppTemplateInfo[]>([]);
  const [businessGroups, setBusinessGroups] = useState<
    AgentBusinessBaseGroup[]
  >([]);
  const [activeTab, setActiveTab] = useState<ViewTab>("types");
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [expanded, setExpanded] = useState<string | null>(null);

  useEffect(() => {
    const load = async () => {
      setLoading(true);
      setError(null);
      try {
        const [types, byType, templates, byBusiness] = await Promise.all([
          fetchAgentTypes(),
          fetchAgentsByType(),
          fetchAppTemplates(),
          fetchAgentsByBusinessBase(),
        ]);
        setTypeDefinitions(types.types);
        setTypeGroups(byType.groups);
        setTemplateCatalog(templates.templates);
        setBusinessGroups(byBusiness.groups);
      } catch (err) {
        const message = err instanceof Error ? err.message : t("pat.no_agents");
        setError(message);
      } finally {
        setLoading(false);
      }
    };
    void load();
  }, []);

  const typeGroupMap = useMemo(() => {
    const map = new Map<string, AgentTypeGroup>();
    for (const group of typeGroups) {
      map.set(group.agent_type, group);
    }
    return map;
  }, [typeGroups]);

  return (
    <div className="p-6 max-w-7xl mx-auto space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-slate-100">{t("pat.title")}</h1>
        <p className="text-sm text-slate-500 mt-1">{t("pat.subtitle")}</p>
      </div>

      {error && (
        <div className="bg-red-500/10 border border-red-500/20 rounded-lg p-4 flex items-center justify-between">
          <span className="text-red-400 text-sm">{error}</span>
          <button
            onClick={() => setError(null)}
            className="text-red-400 hover:text-red-300 text-xs"
          >
            ✕
          </button>
        </div>
      )}

      <div className="flex gap-2">
        <TabButton
          active={activeTab === "types"}
          onClick={() => {
            setActiveTab("types");
            setExpanded(null);
          }}
          label={t("pat.tab_types")}
        />
        <TabButton
          active={activeTab === "templates"}
          onClick={() => {
            setActiveTab("templates");
            setExpanded(null);
          }}
          label={t("pat.tab_templates")}
        />
        <TabButton
          active={activeTab === "business"}
          onClick={() => {
            setActiveTab("business");
            setExpanded(null);
          }}
          label={t("pat.tab_business")}
        />
      </div>

      {loading && (
        <div className="flex justify-center py-16">
          <div className="w-10 h-10 border-4 border-indigo-500/30 border-t-indigo-500 rounded-full animate-spin" />
        </div>
      )}

      {!loading && activeTab === "types" && (
        <div className="space-y-3 max-h-[70vh] overflow-y-auto">
          {typeDefinitions.map((definition) => {
            const group = typeGroupMap.get(definition.agent_type);
            const count = group?.count ?? 0;
            const isOpen = expanded === definition.agent_type;
            return (
              <div
                key={definition.agent_type}
                className="border border-slate-800 rounded-xl bg-slate-900/30"
              >
                <button
                  onClick={() =>
                    setExpanded(isOpen ? null : definition.agent_type)
                  }
                  className="w-full text-left p-4"
                >
                  <div className="flex items-start justify-between gap-3">
                    <div className="min-w-0">
                      <div className="flex items-center gap-2">
                        <h3 className="text-sm font-bold text-slate-200">
                          {definition.label}
                        </h3>
                        <span className="text-[10px] px-1.5 py-0.5 rounded-full bg-cyan-500/10 text-cyan-400">
                          {definition.agent_type}
                        </span>
                      </div>
                      <p className="text-xs text-slate-400 mt-1">
                        {definition.summary}
                      </p>
                      <div className="flex flex-wrap gap-1.5 mt-2">
                        {BEHAVIOR_KEYS.filter(
                          (key) => definition.behaviors[key],
                        ).map((key) => (
                          <span
                            key={key}
                            className="text-[10px] px-2 py-0.5 bg-slate-800/70 text-slate-300 rounded-full"
                          >
                            {t(`pat.behavior_${key}`)}
                          </span>
                        ))}
                      </div>
                    </div>
                    <div className="flex items-center gap-3 shrink-0">
                      <span className="text-xs text-slate-500">
                        {count} agents
                      </span>
                      <span
                        className={`text-slate-500 transition-transform ${isOpen ? "rotate-180" : ""}`}
                      >
                        ▾
                      </span>
                    </div>
                  </div>
                </button>
                {isOpen && (
                  <div className="border-t border-slate-800/60 p-3 space-y-1.5">
                    {(group?.agents ?? []).length === 0 && (
                      <p className="text-xs text-slate-500">
                        {t("pat.no_agents")}
                      </p>
                    )}
                    {(group?.agents ?? []).map((agent) => (
                      <div
                        key={`${agent.app_name}-${agent.name}`}
                        className="flex items-center gap-2 p-2 bg-slate-900/40 rounded-lg"
                      >
                        <span className="text-sm">🤖</span>
                        <p className="text-xs font-medium text-slate-200 flex-1 truncate">
                          {agent.name}
                        </p>
                        <p className="text-[10px] text-slate-500">
                          {agent.app_display_name}
                        </p>
                      </div>
                    ))}
                  </div>
                )}
              </div>
            );
          })}
        </div>
      )}

      {!loading && activeTab === "templates" && (
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4 max-h-[70vh] overflow-y-auto">
          {templateCatalog.map((template) => (
            <div
              key={template.app_template}
              className="border border-slate-800 rounded-xl bg-slate-900/30 p-4 space-y-3"
            >
              <div className="flex items-start justify-between gap-3">
                <div>
                  <h3 className="text-sm font-bold text-slate-100">
                    {template.label}
                  </h3>
                  <p className="text-xs text-slate-400 mt-1">
                    {template.description}
                  </p>
                </div>
                <span className="text-[10px] px-2 py-0.5 rounded-full bg-indigo-500/10 text-indigo-300">
                  {template.count}
                </span>
              </div>
              <div className="text-[11px] text-slate-500 font-mono bg-slate-950/60 border border-slate-800 rounded px-2 py-1">
                app_template: {template.app_template}
              </div>
              <div className="space-y-1">
                {template.apps.slice(0, 5).map((app) => (
                  <div
                    key={app.name}
                    className="flex items-center gap-2 text-xs text-slate-300"
                  >
                    <span>{app.icon || "📦"}</span>
                    <span className="truncate">{app.display_name}</span>
                  </div>
                ))}
                {template.count === 0 && (
                  <p className="text-xs text-slate-500">{t("pat.no_agents")}</p>
                )}
              </div>
            </div>
          ))}
        </div>
      )}

      {!loading && activeTab === "business" && (
        <div className="space-y-3 max-h-[70vh] overflow-y-auto">
          {businessGroups.map((group) => {
            const isOpen = expanded === group.business_base;
            return (
              <div
                key={group.business_base}
                className="border border-slate-800 rounded-xl bg-slate-900/30"
              >
                <button
                  onClick={() =>
                    setExpanded(isOpen ? null : group.business_base)
                  }
                  className="w-full text-left p-4"
                >
                  <div className="flex items-center gap-3">
                    <span className="text-xl">
                      {BUSINESS_ICONS[group.business_base] ?? "📦"}
                    </span>
                    <div className="flex-1">
                      <h3 className="text-sm font-bold text-slate-200">
                        {group.business_base}
                      </h3>
                    </div>
                    <span className="text-xs text-slate-500">
                      {group.count} agents
                    </span>
                    <span
                      className={`text-slate-500 transition-transform ${isOpen ? "rotate-180" : ""}`}
                    >
                      ▾
                    </span>
                  </div>
                </button>
                {isOpen && (
                  <div className="border-t border-slate-800/60 p-3 space-y-1.5">
                    {group.agents.map((agent) => (
                      <div
                        key={`${agent.app_name}-${agent.name}`}
                        className="flex items-center gap-2 p-2 bg-slate-900/40 rounded-lg"
                      >
                        <span className="text-sm">🤖</span>
                        <p className="text-xs font-medium text-slate-200 flex-1 truncate">
                          {agent.name}
                        </p>
                        <p className="text-[10px] text-slate-500">
                          {agent.app_display_name}
                        </p>
                      </div>
                    ))}
                  </div>
                )}
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}

function TabButton({
  active,
  onClick,
  label,
}: {
  readonly active: boolean;
  readonly onClick: () => void;
  readonly label: string;
}) {
  return (
    <button
      onClick={onClick}
      className={`text-xs px-4 py-2 rounded-lg border transition-colors ${
        active
          ? "bg-indigo-600/20 border-indigo-500/40 text-indigo-400 font-medium"
          : "border-slate-700 text-slate-400 hover:border-slate-600"
      }`}
    >
      {label}
    </button>
  );
}

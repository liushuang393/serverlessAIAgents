import { useEffect, useMemo, useState, type ReactNode } from "react";
import { useNavigate } from "react-router-dom";
import {
  createBuilderAgent,
  createBuilderDraft,
  createBuilderSystem,
  deleteBuilderDraft,
  fetchBuilderTemplates,
  generateBuilderCode,
  listBuilderDrafts,
  materializeBuilderApp,
  updateBuilderDraft,
  validateBuilderSpec,
} from "@/api/client";
import type {
  BuilderAgentSpec,
  BuilderDraft,
  BuilderGenerateResponse,
  BuilderOutputType,
  BuilderSpecKind,
  BuilderSystemFlow,
  BuilderSystemSpec,
  BuilderTemplate,
  BuilderValidationResult,
} from "@/types";
import { useI18n } from "@/i18n";

const DEFAULT_AGENT_SPEC: BuilderAgentSpec = {
  name: "GeneratedAgent",
  description: "",
  capabilities: [],
  required_skills: [],
  required_tools: [],
  system_prompt: "",
  engine_type: "simple",
};

const DEFAULT_SYSTEM_SPEC: BuilderSystemSpec = {
  name: "GeneratedSystem",
  description: "",
  goal: "",
  template_id: "multi_agent_system",
  agents: [
    {
      ...DEFAULT_AGENT_SPEC,
      name: "CoordinatorAgent",
      description: "Coordinate work across specialists.",
      engine_type: "pipeline",
    },
    {
      ...DEFAULT_AGENT_SPEC,
      name: "SpecialistAgent",
      description: "Execute the main domain task.",
    },
  ],
  flows: [{ source: "CoordinatorAgent", target: "SpecialistAgent", condition: "handoff" }],
  shared_skills: [],
  shared_tools: [],
  execution_mode: "coordinator",
};

export function Builder() {
  const { t } = useI18n();
  const navigate = useNavigate();

  const [templates, setTemplates] = useState<BuilderTemplate[]>([]);
  const [drafts, setDrafts] = useState<BuilderDraft[]>([]);
  const [loading, setLoading] = useState(true);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [statusMessage, setStatusMessage] = useState<string | null>(null);

  const [step, setStep] = useState<1 | 2 | 3>(1);
  const [draftId, setDraftId] = useState<string | null>(null);
  const [specKind, setSpecKind] = useState<BuilderSpecKind>("agent");
  const [templateId, setTemplateId] = useState("simple");
  const [goal, setGoal] = useState("");
  const [displayName, setDisplayName] = useState("");
  const [appName, setAppName] = useState("");
  const [outputType, setOutputType] = useState<BuilderOutputType>("fullstack");
  const [spec, setSpec] = useState<BuilderAgentSpec | BuilderSystemSpec | null>(
    null,
  );
  const [validation, setValidation] = useState<BuilderValidationResult | null>(
    null,
  );
  const [generated, setGenerated] = useState<BuilderGenerateResponse | null>(
    null,
  );
  const [selectedFile, setSelectedFile] = useState<string>("");

  useEffect(() => {
    void loadInitialData();
  }, []);

  useEffect(() => {
    const sameKindTemplates = templates.filter((item) => item.kind === specKind);
    if (!sameKindTemplates.some((item) => item.id === templateId)) {
      setTemplateId(sameKindTemplates[0]?.id ?? "");
    }
    if (specKind === "agent" && outputType === "frontend") {
      setOutputType("backend");
    }
  }, [specKind, templateId, templates, outputType]);

  useEffect(() => {
    const firstFile = Object.keys(generated?.files ?? {})[0];
    if (firstFile) {
      setSelectedFile((current) =>
        current && generated?.files[current] ? current : firstFile,
      );
    }
  }, [generated]);

  const visibleTemplates = useMemo(
    () => templates.filter((item) => item.kind === specKind),
    [specKind, templates],
  );

  const selectedFileContent = selectedFile
    ? generated?.files[selectedFile] ?? ""
    : "";

  async function loadInitialData() {
    setLoading(true);
    setError(null);
    try {
      const [templateData, draftData] = await Promise.all([
        fetchBuilderTemplates(),
        listBuilderDrafts(),
      ]);
      setTemplates(templateData.templates);
      setDrafts(draftData.drafts);
    } catch (err) {
      setError(toMessage(err));
    } finally {
      setLoading(false);
    }
  }

  async function refreshDrafts() {
    const draftData = await listBuilderDrafts();
    setDrafts(draftData.drafts);
  }

  async function handleCreateSpec() {
    if (!goal.trim()) {
      setError(t("builder.errors.goal_required"));
      return;
    }
    setBusy(true);
    setError(null);
    setStatusMessage(null);
    try {
      if (specKind === "system") {
        const response = await createBuilderSystem({
          description: goal,
          name: displayName || undefined,
          template_id: templateId || undefined,
        });
        const nextSpec = response.system_spec;
        if (!nextSpec) {
          throw new Error(response.error ?? "System spec was not returned");
        }
        setSpec(nextSpec);
        setValidation(response.validation ?? null);
        setDisplayName(nextSpec.name);
        setAppName((current) => current || slugify(nextSpec.name));
      } else {
        const response = await createBuilderAgent({
          description: goal,
          name: displayName || undefined,
        });
        const nextSpec = response.agent_spec;
        if (!nextSpec) {
          throw new Error(response.error ?? "Agent spec was not returned");
        }
        setSpec(nextSpec);
        setValidation(response.validation ?? null);
        setDisplayName(nextSpec.name);
        setAppName((current) => current || slugify(nextSpec.name));
      }
      setGenerated(null);
      setStep(2);
      setStatusMessage(t("builder.status.spec_ready"));
    } catch (err) {
      setError(toMessage(err));
    } finally {
      setBusy(false);
    }
  }

  async function handleValidate() {
    if (!spec) {
      return;
    }
    setBusy(true);
    setError(null);
    try {
      const result = await validateBuilderSpec({
        spec_kind: specKind,
        spec: toRecord(spec),
      });
      setValidation(result);
      setStep(2);
      setStatusMessage(
        result.valid
          ? t("builder.status.validated")
          : t("builder.status.validation_failed"),
      );
    } catch (err) {
      setError(toMessage(err));
    } finally {
      setBusy(false);
    }
  }

  async function handleGenerate() {
    if (!spec) {
      setError(t("builder.errors.spec_required"));
      return;
    }
    if (!appName.trim()) {
      setError(t("builder.errors.app_name_required"));
      return;
    }
    setBusy(true);
    setError(null);
    try {
      const result = await generateBuilderCode({
        spec_kind: specKind,
        spec: toRecord(spec),
        output_type: outputType,
        app_name: slugify(appName),
        framework: "fastapi",
      });
      setGenerated(result);
      setStep(3);
      setStatusMessage(t("builder.status.generated"));
    } catch (err) {
      setError(toMessage(err));
    } finally {
      setBusy(false);
    }
  }

  async function handleSaveDraft(status?: BuilderDraft["status"]) {
    const payload = {
      name: displayName,
      template_id: templateId,
      goal,
      spec_kind: specKind,
      spec: spec ? toRecord(spec) : {},
      generated_files: generated?.files ?? {},
      status:
        status ??
        (generated
          ? "generated"
          : validation?.valid
            ? "validated"
            : "draft"),
    } as const;

    setBusy(true);
    setError(null);
    try {
      const saved = draftId
        ? await updateBuilderDraft(draftId, payload)
        : await createBuilderDraft(payload);
      setDraftId(saved.id);
      await refreshDrafts();
      setStatusMessage(t("builder.status.draft_saved"));
    } catch (err) {
      setError(toMessage(err));
    } finally {
      setBusy(false);
    }
  }

  async function handleDeleteDraft(draft: BuilderDraft) {
    setBusy(true);
    setError(null);
    try {
      await deleteBuilderDraft(draft.id);
      if (draftId === draft.id) {
        resetBuilder();
      }
      await refreshDrafts();
      setStatusMessage(t("builder.status.draft_deleted"));
    } catch (err) {
      setError(toMessage(err));
    } finally {
      setBusy(false);
    }
  }

  function handleLoadDraft(draft: BuilderDraft) {
    setDraftId(draft.id);
    setDisplayName(draft.name);
    setTemplateId(draft.template_id);
    setGoal(draft.goal);
    setSpecKind(draft.spec_kind);
    setSpec(
      draft.spec_kind === "system"
        ? normalizeSystemSpec(draft.spec)
        : normalizeAgentSpec(draft.spec),
    );
    setGenerated(
      Object.keys(draft.generated_files).length > 0
        ? {
            success: true,
            files: draft.generated_files,
            entry_point: Object.keys(draft.generated_files)[0] ?? "",
            output_type: inferOutputType(draft.generated_files),
            metadata: {},
          }
        : null,
    );
    setAppName((current) => current || slugify(draft.name));
    setValidation(null);
    setStep(
      Object.keys(draft.generated_files).length > 0
        ? 3
        : Object.keys(draft.spec).length > 0
          ? 2
          : 1,
    );
    setStatusMessage(t("builder.status.draft_loaded"));
  }

  async function handleMaterialize() {
    if (!spec || !generated) {
      setError(t("builder.errors.generated_required"));
      return;
    }
    setBusy(true);
    setError(null);
    try {
      const response = await materializeBuilderApp({
        name: slugify(appName || displayName),
        display_name: displayName || undefined,
        description: spec.description,
        icon: specKind === "system" ? "🧱" : "🤖",
        template_id: templateId,
        goal,
        spec_kind: specKind,
        spec: toRecord(spec),
        generated_files: generated.files,
        output_type: outputType,
        framework: "fastapi",
      });
      await handleSaveDraft("materialized");
      navigate(`/apps/${response.app_name}`);
    } catch (err) {
      setError(toMessage(err));
    } finally {
      setBusy(false);
    }
  }

  function resetBuilder() {
    setDraftId(null);
    setSpecKind("agent");
    setTemplateId("simple");
    setGoal("");
    setDisplayName("");
    setAppName("");
    setSpec(null);
    setValidation(null);
    setGenerated(null);
    setSelectedFile("");
    setStatusMessage(null);
    setStep(1);
  }

  if (loading) {
    return (
      <div className="flex items-center justify-center h-full py-24">
        <div className="w-10 h-10 border-4 border-cyan-500/20 border-t-cyan-400 rounded-full animate-spin" />
      </div>
    );
  }

  return (
    <div className="min-h-full bg-[radial-gradient(circle_at_top_left,_rgba(34,211,238,0.18),_transparent_28%),radial-gradient(circle_at_bottom_right,_rgba(250,204,21,0.12),_transparent_28%),linear-gradient(180deg,_#050816,_#090d1f_40%,_#0b1022)]">
      <div className="max-w-7xl mx-auto px-6 py-6 space-y-6">
        <section className="rounded-3xl border border-cyan-500/15 bg-slate-950/60 overflow-hidden">
          <div className="grid gap-6 lg:grid-cols-[1.4fr_0.9fr]">
            <div className="p-8 space-y-5">
              <div className="inline-flex items-center gap-2 rounded-full border border-cyan-400/20 bg-cyan-400/10 px-3 py-1 text-xs uppercase tracking-[0.3em] text-cyan-200">
                {t("builder.badge")}
              </div>
              <div>
                <h1 className="text-3xl font-semibold text-white">
                  {t("builder.title")}
                </h1>
                <p className="mt-2 max-w-2xl text-sm leading-6 text-slate-300">
                  {t("builder.subtitle")}
                </p>
              </div>
              <div className="grid gap-3 sm:grid-cols-3">
                <StepCard
                  index={1}
                  title={t("builder.steps.step1")}
                  active={step === 1}
                />
                <StepCard
                  index={2}
                  title={t("builder.steps.step2")}
                  active={step === 2}
                />
                <StepCard
                  index={3}
                  title={t("builder.steps.step3")}
                  active={step === 3}
                />
              </div>
            </div>
            <div className="border-t border-slate-800 lg:border-t-0 lg:border-l bg-slate-950/80 border-slate-800 p-6">
              <p className="text-xs uppercase tracking-[0.24em] text-slate-500">
                {t("builder.side_title")}
              </p>
              <div className="mt-4 space-y-3">
                {drafts.length === 0 && (
                  <div className="rounded-2xl border border-dashed border-slate-700 p-4 text-sm text-slate-400">
                    {t("builder.empty_drafts")}
                  </div>
                )}
                {drafts.map((draft) => (
                  <div
                    key={draft.id}
                    className="rounded-2xl border border-slate-800 bg-slate-900/70 p-4"
                  >
                    <div className="flex items-start justify-between gap-3">
                      <div>
                        <p className="text-sm font-medium text-white">
                          {draft.name || draft.goal || draft.id}
                        </p>
                        <p className="mt-1 text-xs text-slate-500">
                          {draft.spec_kind} · {draft.status}
                        </p>
                      </div>
                      <button
                        type="button"
                        onClick={() => void handleDeleteDraft(draft)}
                        className="text-xs text-rose-300 hover:text-rose-200"
                      >
                        {t("builder.delete")}
                      </button>
                    </div>
                    <p className="mt-3 text-xs text-slate-400">
                      {draft.goal || t("builder.no_goal")}
                    </p>
                    <button
                      type="button"
                      onClick={() => handleLoadDraft(draft)}
                      className="mt-3 rounded-xl border border-slate-700 px-3 py-2 text-xs text-slate-200 hover:border-cyan-400/40 hover:text-cyan-100"
                    >
                      {t("builder.load_draft")}
                    </button>
                  </div>
                ))}
              </div>
            </div>
          </div>
        </section>

        {error && (
          <div className="rounded-2xl border border-rose-500/30 bg-rose-500/10 px-4 py-3 text-sm text-rose-200">
            {error}
          </div>
        )}
        {statusMessage && (
          <div className="rounded-2xl border border-emerald-500/20 bg-emerald-500/10 px-4 py-3 text-sm text-emerald-200">
            {statusMessage}
          </div>
        )}

        <section className="grid gap-6 xl:grid-cols-[1.1fr_1fr]">
          <div className="space-y-6">
            <BuilderPanel title={t("builder.steps.step1")}>
              <div className="grid gap-3 sm:grid-cols-2">
                <KindToggle
                  kind="agent"
                  active={specKind === "agent"}
                  label={t("builder.kind.agent")}
                  onSelect={setSpecKind}
                />
                <KindToggle
                  kind="system"
                  active={specKind === "system"}
                  label={t("builder.kind.system")}
                  onSelect={setSpecKind}
                />
              </div>

              <div className="grid gap-3 md:grid-cols-2">
                {visibleTemplates.map((template) => (
                  <button
                    key={template.id}
                    type="button"
                    onClick={() => setTemplateId(template.id)}
                    className={`rounded-2xl border px-4 py-4 text-left transition-colors ${
                      templateId === template.id
                        ? "border-cyan-400/60 bg-cyan-400/10"
                        : "border-slate-800 bg-slate-900/70 hover:border-slate-700"
                    }`}
                  >
                    <p className="text-sm font-medium text-white">
                      {template.name}
                    </p>
                    <p className="mt-2 text-xs leading-5 text-slate-400">
                      {template.description}
                    </p>
                  </button>
                ))}
              </div>

              <div className="grid gap-4 md:grid-cols-2">
                <label className="space-y-2">
                  <span className="text-xs uppercase tracking-[0.2em] text-slate-500">
                    {t("builder.labels.name")}
                  </span>
                  <input
                    value={displayName}
                    onChange={(event) => setDisplayName(event.target.value)}
                    className="input"
                    placeholder={t("builder.placeholders.name")}
                  />
                </label>
                <label className="space-y-2">
                  <span className="text-xs uppercase tracking-[0.2em] text-slate-500">
                    {t("builder.labels.app_name")}
                  </span>
                  <input
                    value={appName}
                    onChange={(event) => setAppName(slugify(event.target.value))}
                    className="input font-mono"
                    placeholder="ai_agent_builder"
                  />
                </label>
              </div>

              <label className="space-y-2">
                <span className="text-xs uppercase tracking-[0.2em] text-slate-500">
                  {t("builder.labels.goal")}
                </span>
                <textarea
                  value={goal}
                  onChange={(event) => setGoal(event.target.value)}
                  className="input min-h-[132px]"
                  placeholder={t("builder.placeholders.goal")}
                />
              </label>

              <div className="flex flex-wrap gap-3">
                <ActionButton
                  label={t("builder.actions.generate_spec")}
                  onClick={() => void handleCreateSpec()}
                  busy={busy}
                  tone="primary"
                />
                <ActionButton
                  label={t("builder.actions.save_draft")}
                  onClick={() => void handleSaveDraft("draft")}
                  busy={busy}
                  tone="secondary"
                />
              </div>
            </BuilderPanel>

            <BuilderPanel title={t("builder.steps.step2")}>
              {!spec && (
                <EmptyState message={t("builder.empty_spec")} />
              )}
              {spec && isSystemSpec(spec) ? (
                <SystemSpecEditor
                  spec={spec}
                  onChange={setSpec}
                  onValidate={() => void handleValidate()}
                />
              ) : spec ? (
                <AgentSpecEditor
                  spec={spec}
                  onChange={setSpec}
                  onValidate={() => void handleValidate()}
                />
              ) : null}

              {validation && (
                <ValidationPanel validation={validation} />
              )}

              <div className="flex flex-wrap gap-3">
                <ActionButton
                  label={t("builder.actions.validate")}
                  onClick={() => void handleValidate()}
                  busy={busy}
                  tone="secondary"
                />
                <ActionButton
                  label={t("builder.actions.save_draft")}
                  onClick={() => void handleSaveDraft()}
                  busy={busy}
                  tone="secondary"
                />
                <ActionButton
                  label={t("builder.actions.generate_code")}
                  onClick={() => void handleGenerate()}
                  busy={busy}
                  tone="primary"
                />
              </div>
            </BuilderPanel>
          </div>

          <div className="space-y-6">
            <BuilderPanel title={t("builder.steps.step3")}>
              <div className="grid gap-4 md:grid-cols-2">
                <label className="space-y-2">
                  <span className="text-xs uppercase tracking-[0.2em] text-slate-500">
                    {t("builder.labels.output")}
                  </span>
                  <select
                    value={outputType}
                    onChange={(event) =>
                      setOutputType(event.target.value as BuilderOutputType)
                    }
                    className="input"
                  >
                    <option value="backend">Backend</option>
                    <option value="fullstack">Fullstack</option>
                    <option value="frontend">Frontend</option>
                  </select>
                </label>
                <label className="space-y-2">
                  <span className="text-xs uppercase tracking-[0.2em] text-slate-500">
                    {t("builder.labels.entry")}
                  </span>
                  <input
                    readOnly
                    value={generated?.entry_point ?? ""}
                    className="input font-mono"
                    placeholder={t("builder.placeholders.entry")}
                  />
                </label>
              </div>

              {!generated && <EmptyState message={t("builder.empty_generated")} />}

              {generated && (
                <div className="space-y-4">
                  <div className="grid gap-4 lg:grid-cols-[0.9fr_1.1fr]">
                    <div className="rounded-2xl border border-slate-800 bg-slate-950/70 overflow-hidden">
                      <div className="border-b border-slate-800 px-4 py-3 text-xs uppercase tracking-[0.2em] text-slate-500">
                        {t("builder.files")}
                      </div>
                      <div className="max-h-[420px] overflow-auto p-2">
                        {Object.keys(generated.files).map((path) => (
                          <button
                            key={path}
                            type="button"
                            onClick={() => setSelectedFile(path)}
                            className={`block w-full rounded-xl px-3 py-2 text-left text-xs font-mono ${
                              selectedFile === path
                                ? "bg-cyan-400/10 text-cyan-100"
                                : "text-slate-400 hover:bg-slate-900 hover:text-slate-200"
                            }`}
                          >
                            {path}
                          </button>
                        ))}
                      </div>
                    </div>
                    <div className="rounded-2xl border border-slate-800 bg-slate-950/90 overflow-hidden">
                      <div className="border-b border-slate-800 px-4 py-3 text-xs uppercase tracking-[0.2em] text-slate-500">
                        {selectedFile || t("builder.preview")}
                      </div>
                      <pre className="max-h-[420px] overflow-auto p-4 text-[12px] leading-5 text-slate-200">
                        {selectedFileContent}
                      </pre>
                    </div>
                  </div>

                  <div className="rounded-2xl border border-amber-400/20 bg-amber-400/10 px-4 py-3 text-sm text-amber-100">
                    {t("builder.phase2")}
                  </div>
                </div>
              )}

              <div className="flex flex-wrap gap-3">
                <ActionButton
                  label={t("builder.actions.preview")}
                  onClick={() => void handleGenerate()}
                  busy={busy}
                  tone="secondary"
                />
                <ActionButton
                  label={t("builder.actions.download")}
                  onClick={() =>
                    downloadGeneratedBundle(
                      slugify(appName || displayName || "builder-artifact"),
                      generated?.files ?? {},
                    )
                  }
                  busy={false}
                  tone="secondary"
                />
                <ActionButton
                  label={t("builder.actions.materialize")}
                  onClick={() => void handleMaterialize()}
                  busy={busy}
                  tone="primary"
                />
              </div>
            </BuilderPanel>
          </div>
        </section>
      </div>
    </div>
  );
}

function AgentSpecEditor({
  spec,
  onChange,
  onValidate,
}: {
  spec: BuilderAgentSpec;
  onChange: (next: BuilderAgentSpec) => void;
  onValidate: () => void;
}) {
  return (
    <div className="space-y-4">
      <div className="grid gap-4 md:grid-cols-2">
        <InputBlock
          label="Name"
          value={spec.name}
          onChange={(value) => onChange({ ...spec, name: value })}
        />
        <SelectBlock
          label="Engine"
          value={spec.engine_type}
          onChange={(value) => onChange({ ...spec, engine_type: value })}
          options={["simple", "pipeline", "rag", "gate"]}
        />
      </div>
      <TextareaBlock
        label="Description"
        value={spec.description}
        onChange={(value) => onChange({ ...spec, description: value })}
      />
      <TextareaBlock
        label="Capabilities"
        value={spec.capabilities.join(", ")}
        onChange={(value) =>
          onChange({ ...spec, capabilities: parseList(value) })
        }
      />
      <TextareaBlock
        label="System Prompt"
        value={spec.system_prompt ?? ""}
        onChange={(value) => onChange({ ...spec, system_prompt: value })}
      />
      <button
        type="button"
        onClick={onValidate}
        className="text-xs text-cyan-300 hover:text-cyan-200"
      >
        Run validation
      </button>
    </div>
  );
}

function SystemSpecEditor({
  spec,
  onChange,
  onValidate,
}: {
  spec: BuilderSystemSpec;
  onChange: (next: BuilderSystemSpec) => void;
  onValidate: () => void;
}) {
  return (
    <div className="space-y-5">
      <div className="grid gap-4 md:grid-cols-2">
        <InputBlock
          label="Name"
          value={spec.name}
          onChange={(value) => onChange({ ...spec, name: value })}
        />
        <InputBlock
          label="Execution"
          value={spec.execution_mode ?? "coordinator"}
          onChange={(value) => onChange({ ...spec, execution_mode: value })}
        />
      </div>
      <TextareaBlock
        label="Description"
        value={spec.description}
        onChange={(value) => onChange({ ...spec, description: value })}
      />
      <TextareaBlock
        label="Goal"
        value={spec.goal}
        onChange={(value) => onChange({ ...spec, goal: value })}
      />
      <TextareaBlock
        label="Shared Skills"
        value={(spec.shared_skills ?? []).join(", ")}
        onChange={(value) => onChange({ ...spec, shared_skills: parseList(value) })}
      />

      <div className="space-y-3">
        <div className="flex items-center justify-between">
          <h3 className="text-sm font-medium text-white">Agents</h3>
          <button
            type="button"
            onClick={() =>
              onChange({
                ...spec,
                agents: [
                  ...spec.agents,
                  {
                    ...DEFAULT_AGENT_SPEC,
                    name: `Agent${spec.agents.length + 1}`,
                  },
                ],
              })
            }
            className="text-xs text-cyan-300 hover:text-cyan-200"
          >
            Add Agent
          </button>
        </div>
        {spec.agents.map((agent, index) => (
          <div
            key={`${agent.name}-${index}`}
            className="rounded-2xl border border-slate-800 bg-slate-950/60 p-4 space-y-3"
          >
            <div className="flex items-center justify-between gap-3">
              <p className="text-sm font-medium text-white">
                Agent {index + 1}
              </p>
              <button
                type="button"
                onClick={() =>
                  onChange({
                    ...spec,
                    agents: spec.agents.filter((_, current) => current !== index),
                  })
                }
                className="text-xs text-rose-300 hover:text-rose-200"
              >
                Remove
              </button>
            </div>
            <div className="grid gap-3 md:grid-cols-2">
              <InputBlock
                label="Name"
                value={agent.name}
                onChange={(value) =>
                  onChange({
                    ...spec,
                    agents: spec.agents.map((item, current) =>
                      current === index ? { ...item, name: value } : item,
                    ),
                  })
                }
              />
              <SelectBlock
                label="Engine"
                value={agent.engine_type}
                onChange={(value) =>
                  onChange({
                    ...spec,
                    agents: spec.agents.map((item, current) =>
                      current === index ? { ...item, engine_type: value } : item,
                    ),
                  })
                }
                options={["simple", "pipeline", "rag", "gate"]}
              />
            </div>
            <TextareaBlock
              label="Description"
              value={agent.description}
              onChange={(value) =>
                onChange({
                  ...spec,
                  agents: spec.agents.map((item, current) =>
                    current === index ? { ...item, description: value } : item,
                  ),
                })
              }
            />
            <TextareaBlock
              label="Capabilities"
              value={agent.capabilities.join(", ")}
              onChange={(value) =>
                onChange({
                  ...spec,
                  agents: spec.agents.map((item, current) =>
                    current === index
                      ? { ...item, capabilities: parseList(value) }
                      : item,
                  ),
                })
              }
            />
          </div>
        ))}
      </div>

      <FlowEditor
        flows={spec.flows}
        agentNames={spec.agents.map((agent) => agent.name)}
        onChange={(flows) => onChange({ ...spec, flows })}
      />

      <div className="flex gap-3">
        <button
          type="button"
          onClick={() =>
            onChange({
              ...spec,
              flows: spec.agents.slice(0, -1).map((agent, index) => ({
                source: agent.name,
                target: spec.agents[index + 1]?.name ?? agent.name,
                condition: "handoff",
              })),
            })
          }
          className="text-xs text-cyan-300 hover:text-cyan-200"
        >
          Regenerate handoff flow
        </button>
        <button
          type="button"
          onClick={onValidate}
          className="text-xs text-cyan-300 hover:text-cyan-200"
        >
          Run validation
        </button>
      </div>
    </div>
  );
}

function FlowEditor({
  flows,
  agentNames,
  onChange,
}: {
  flows: BuilderSystemFlow[];
  agentNames: string[];
  onChange: (next: BuilderSystemFlow[]) => void;
}) {
  return (
    <div className="space-y-3">
      <div className="flex items-center justify-between">
        <h3 className="text-sm font-medium text-white">Flows</h3>
        <button
          type="button"
          onClick={() =>
            onChange([
              ...flows,
              {
                source: agentNames[0] ?? "",
                target: agentNames[1] ?? "",
                condition: "",
              },
            ])
          }
          className="text-xs text-cyan-300 hover:text-cyan-200"
        >
          Add Flow
        </button>
      </div>
      {flows.length === 0 && (
        <div className="rounded-2xl border border-dashed border-slate-700 p-4 text-sm text-slate-400">
          No flow defined.
        </div>
      )}
      {flows.map((flow, index) => (
        <div
          key={`${flow.source}-${flow.target}-${index}`}
          className="grid gap-3 rounded-2xl border border-slate-800 bg-slate-950/60 p-4 md:grid-cols-[1fr_1fr_1fr_auto]"
        >
          <SelectBlock
            label="Source"
            value={flow.source}
            onChange={(value) =>
              onChange(
                flows.map((item, current) =>
                  current === index ? { ...item, source: value } : item,
                ),
              )
            }
            options={agentNames}
          />
          <SelectBlock
            label="Target"
            value={flow.target}
            onChange={(value) =>
              onChange(
                flows.map((item, current) =>
                  current === index ? { ...item, target: value } : item,
                ),
              )
            }
            options={agentNames}
          />
          <InputBlock
            label="Condition"
            value={flow.condition ?? ""}
            onChange={(value) =>
              onChange(
                flows.map((item, current) =>
                  current === index ? { ...item, condition: value } : item,
                ),
              )
            }
          />
          <button
            type="button"
            onClick={() =>
              onChange(flows.filter((_, current) => current !== index))
            }
            className="mt-7 rounded-xl border border-rose-400/20 px-3 py-2 text-xs text-rose-200 hover:border-rose-300/30"
          >
            Remove
          </button>
        </div>
      ))}
    </div>
  );
}

function ValidationPanel({
  validation,
}: {
  validation: BuilderValidationResult;
}) {
  return (
    <div className="rounded-2xl border border-slate-800 bg-slate-950/70 p-4 space-y-3">
      <div className="flex items-center justify-between">
        <p className="text-sm font-medium text-white">Validation</p>
        <span
          className={`rounded-full px-2.5 py-1 text-xs ${
            validation.valid
              ? "bg-emerald-500/15 text-emerald-200"
              : "bg-rose-500/15 text-rose-200"
          }`}
        >
          score {validation.score.toFixed(2)}
        </span>
      </div>
      {validation.errors.length > 0 && (
        <div className="space-y-1">
          {validation.errors.map((item) => (
            <p key={item} className="text-sm text-rose-200">
              {item}
            </p>
          ))}
        </div>
      )}
      {validation.warnings.length > 0 && (
        <div className="space-y-1">
          {validation.warnings.map((item) => (
            <p key={item} className="text-sm text-amber-200">
              {item}
            </p>
          ))}
        </div>
      )}
      {(validation.suggestions ?? []).length > 0 && (
        <div className="space-y-1">
          {validation.suggestions?.map((item) => (
            <p key={item} className="text-sm text-slate-300">
              {item}
            </p>
          ))}
        </div>
      )}
    </div>
  );
}

function BuilderPanel({
  title,
  children,
}: {
  title: string;
  children: ReactNode;
}) {
  return (
    <section className="rounded-3xl border border-slate-800 bg-slate-950/70 p-6 space-y-5">
      <div className="flex items-center justify-between">
        <h2 className="text-lg font-medium text-white">{title}</h2>
      </div>
      {children}
    </section>
  );
}

function StepCard({
  index,
  title,
  active,
}: {
  index: number;
  title: string;
  active: boolean;
}) {
  return (
    <div
      className={`rounded-2xl border px-4 py-4 ${
        active
          ? "border-cyan-400/40 bg-cyan-400/10 text-cyan-100"
          : "border-slate-800 bg-slate-900/50 text-slate-400"
      }`}
    >
      <p className="text-xs uppercase tracking-[0.24em]">0{index}</p>
      <p className="mt-2 text-sm font-medium">{title}</p>
    </div>
  );
}

function KindToggle({
  kind,
  label,
  active,
  onSelect,
}: {
  kind: BuilderSpecKind;
  label: string;
  active: boolean;
  onSelect: (kind: BuilderSpecKind) => void;
}) {
  return (
    <button
      type="button"
      onClick={() => onSelect(kind)}
      className={`rounded-2xl border px-4 py-4 text-left ${
        active
          ? "border-cyan-400/60 bg-cyan-400/10 text-white"
          : "border-slate-800 bg-slate-900/70 text-slate-400"
      }`}
    >
      {label}
    </button>
  );
}

function EmptyState({ message }: { message: string }) {
  return (
    <div className="rounded-2xl border border-dashed border-slate-700 p-5 text-sm text-slate-400">
      {message}
    </div>
  );
}

function InputBlock({
  label,
  value,
  onChange,
}: {
  label: string;
  value: string;
  onChange: (value: string) => void;
}) {
  return (
    <label className="space-y-2">
      <span className="text-xs uppercase tracking-[0.2em] text-slate-500">
        {label}
      </span>
      <input
        value={value}
        onChange={(event) => onChange(event.target.value)}
        className="input"
      />
    </label>
  );
}

function TextareaBlock({
  label,
  value,
  onChange,
}: {
  label: string;
  value: string;
  onChange: (value: string) => void;
}) {
  return (
    <label className="space-y-2">
      <span className="text-xs uppercase tracking-[0.2em] text-slate-500">
        {label}
      </span>
      <textarea
        value={value}
        onChange={(event) => onChange(event.target.value)}
        className="input min-h-[110px]"
      />
    </label>
  );
}

function SelectBlock({
  label,
  value,
  onChange,
  options,
}: {
  label: string;
  value: string;
  onChange: (value: string) => void;
  options: string[];
}) {
  return (
    <label className="space-y-2">
      <span className="text-xs uppercase tracking-[0.2em] text-slate-500">
        {label}
      </span>
      <select
        value={value}
        onChange={(event) => onChange(event.target.value)}
        className="input"
      >
        {options.map((option) => (
          <option key={option} value={option}>
            {option}
          </option>
        ))}
      </select>
    </label>
  );
}

function ActionButton({
  label,
  onClick,
  busy,
  tone,
}: {
  label: string;
  onClick: () => void;
  busy: boolean;
  tone: "primary" | "secondary";
}) {
  return (
    <button
      type="button"
      onClick={onClick}
      disabled={busy}
      className={`rounded-2xl px-4 py-2.5 text-sm transition-colors disabled:opacity-60 ${
        tone === "primary"
          ? "bg-cyan-400 text-slate-950 hover:bg-cyan-300"
          : "border border-slate-700 text-slate-100 hover:border-slate-500"
      }`}
    >
      {busy ? "..." : label}
    </button>
  );
}

function isSystemSpec(
  spec: BuilderAgentSpec | BuilderSystemSpec,
): spec is BuilderSystemSpec {
  return "agents" in spec;
}

function normalizeAgentSpec(raw: Record<string, unknown>): BuilderAgentSpec {
  return {
    ...DEFAULT_AGENT_SPEC,
    ...raw,
    capabilities: toStringArray(raw.capabilities),
    required_skills: toStringArray(raw.required_skills),
    required_tools: toStringArray(raw.required_tools),
    name: String(raw.name ?? DEFAULT_AGENT_SPEC.name),
    description: String(raw.description ?? ""),
    system_prompt: String(raw.system_prompt ?? ""),
    engine_type: String(raw.engine_type ?? "simple"),
  };
}

function normalizeSystemSpec(raw: Record<string, unknown>): BuilderSystemSpec {
  const agents = Array.isArray(raw.agents)
    ? raw.agents
        .filter((item): item is Record<string, unknown> => !!item && typeof item === "object")
        .map((item) => normalizeAgentSpec(item))
    : DEFAULT_SYSTEM_SPEC.agents;

  const flows = Array.isArray(raw.flows)
    ? raw.flows
        .filter((item): item is Record<string, unknown> => !!item && typeof item === "object")
        .map((item) => ({
          source: String(item.source ?? ""),
          target: String(item.target ?? ""),
          condition: String(item.condition ?? ""),
        }))
    : DEFAULT_SYSTEM_SPEC.flows;

  return {
    ...DEFAULT_SYSTEM_SPEC,
    ...raw,
    name: String(raw.name ?? DEFAULT_SYSTEM_SPEC.name),
    description: String(raw.description ?? ""),
    goal: String(raw.goal ?? ""),
    template_id: String(raw.template_id ?? "multi_agent_system"),
    agents,
    flows,
    shared_skills: toStringArray(raw.shared_skills),
    shared_tools: toStringArray(raw.shared_tools),
    execution_mode: String(raw.execution_mode ?? "coordinator"),
  };
}

function toStringArray(value: unknown): string[] {
  if (!Array.isArray(value)) {
    return [];
  }
  return value.map((item) => String(item)).filter(Boolean);
}

function parseList(value: string): string[] {
  return value
    .split(/[\n,]/)
    .map((item) => item.trim())
    .filter(Boolean);
}

function slugify(value: string): string {
  const normalized = value
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9_]+/g, "_")
    .replace(/_+/g, "_")
    .replace(/^_+|_+$/g, "");
  if (!normalized) {
    return "builder_app";
  }
  return /^[a-z]/.test(normalized) ? normalized : `app_${normalized}`;
}

function inferOutputType(files: Record<string, string>): BuilderOutputType {
  const hasBackend = Boolean(files["app.py"] || files["backend/app.py"]);
  const hasFrontend = Boolean(files["package.json"] || files["frontend/package.json"]);
  if (hasBackend && hasFrontend) {
    return "fullstack";
  }
  return hasFrontend ? "frontend" : "backend";
}

function toRecord(
  spec: BuilderAgentSpec | BuilderSystemSpec,
): Record<string, unknown> {
  return JSON.parse(JSON.stringify(spec)) as Record<string, unknown>;
}

function downloadGeneratedBundle(
  name: string,
  files: Record<string, string>,
) {
  const blob = new Blob([JSON.stringify({ files }, null, 2)], {
    type: "application/json",
  });
  const url = URL.createObjectURL(blob);
  const anchor = document.createElement("a");
  anchor.href = url;
  anchor.download = `${name}-generated-files.json`;
  anchor.click();
  URL.revokeObjectURL(url);
}

function toMessage(error: unknown): string {
  if (typeof error === "object" && error && "response" in error) {
    const response = error.response as {
      data?: { detail?: { message?: string } | string };
    };
    const detail = response.data?.detail;
    if (typeof detail === "string") {
      return detail;
    }
    if (detail && typeof detail === "object" && "message" in detail) {
      return String(detail.message);
    }
  }
  if (error instanceof Error) {
    return error.message;
  }
  return "Unknown error";
}

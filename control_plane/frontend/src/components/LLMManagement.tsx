/**
 * LLM Management - Gateway/provider/engine/registry policy console.
 */

import axios from "axios";
import { useEffect, useMemo, useState } from "react";
import {
  deleteLLMProviderSecret,
  deployLLMEngine,
  fetchLLMCatalog,
  fetchLLMDiagnostics,
  fetchLLMEngineStatus,
  fetchLLMManagementOverview,
  fetchOpenAPIPaths,
  prefetchLLMEngineModel,
  reloadLLMManagementConfig,
  setupAndSwitchLLM,
  stopLLMEngine,
  switchLLM,
  updateLLMInferenceEngines,
  updateLLMModels,
  updateLLMProviderSecret,
  updateLLMProviders,
  updateLLMRegistry,
  updateLLMRoutingPolicy,
} from "@/api/client";
import type {
  LLMBackendKind,
  LLMCatalogResponse,
  LLMDiagnosticsResponse,
  LLMEngineDeployResponse,
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
} from "@/types";
import { useI18n } from "@/i18n";

type ErrorCategory =
  | "route_missing"
  | "validation"
  | "install"
  | "health"
  | "network"
  | null;
type DraftTarget = "providers" | "engines" | "models" | "registry" | "routing";

interface DraftExample {
  id: string;
  label: string;
  description: string;
  value: string;
}

interface ContractExample {
  id: string;
  title: string;
  description: string;
  projectLabel: string;
  notes: string[];
  value: string;
}

const prettify = (value: unknown): string => JSON.stringify(value, null, 2);
const DEFAULT_ROLES = "reasoning,coding,cheap,local";
const DEFAULT_ENGINE_PORTS: Record<Exclude<LLMBackendKind, "none">, number> = {
  vllm: 18001,
  sglang: 18002,
  tgi: 18003,
  ollama: 11434,
};
const OFFICIAL_PROVIDER_MODELS: Record<string, string[]> = {
  openai: ["gpt-5.2", "gpt-5-mini", "gpt-5-nano"],
  anthropic: ["claude-opus-4-6", "claude-sonnet-4-6", "claude-haiku-4-5"],
  google: [
    "gemini-3.1-pro-preview",
    "gemini-3-flash-preview",
    "gemini-3.1-flash-lite-preview",
  ],
};
const OFFICIAL_LOCAL_BACKEND_MODELS: Record<
  Exclude<LLMBackendKind, "none">,
  string[]
> = {
  vllm: [
    "Qwen/Qwen2.5-0.5B-Instruct",
    "Qwen/Qwen2.5-Coder-7B-Instruct",
    "meta-llama/Llama-3.1-8B-Instruct",
  ],
  sglang: [
    "Qwen/Qwen2.5-0.5B-Instruct",
    "Qwen/Qwen2.5-Coder-7B-Instruct",
    "meta-llama/Llama-3.1-8B-Instruct",
  ],
  tgi: [
    "Qwen/Qwen2.5-0.5B-Instruct",
    "Qwen/Qwen2.5-Coder-7B-Instruct",
    "meta-llama/Llama-3.1-8B-Instruct",
  ],
  ollama: ["llama3.3:70b", "qwen2.5:72b", "qwen2.5-coder:32b"],
};
const OFFICIAL_VOICE_STACK_NOTES = [
  "2026-03-10 時点の公式ドキュメントを基準に、音声連携の実装しやすさ順で上位 3 系統を整理しています。",
  "1. provider=openai: speech_to_text -> gpt-4o-transcribe、text -> gpt-5.2、text_to_speech -> gpt-4o-mini-tts、realtime -> gpt-realtime。",
  "2. provider=google: native audio / speech_to_text -> gemini-2.5-flash-native-audio-preview-12-2025、text -> gemini-3.1-pro-preview または gemini-3-flash-preview、text_to_speech -> gemini-2.5-flash-preview-tts。",
  "3. provider=anthropic: text -> claude-opus-4-6。Claude は現時点でも text/image input -> text output が中心で、speech_to_text / text_to_speech は OpenAI か Google の併用前提です。",
] as const;
const EMPTY_SECRET_STATUS = {
  configured: false,
  masked: null,
  source: "unavailable",
  available: false,
  last_error: null,
} as const;

const EMPTY_CATALOG: LLMCatalogResponse = {
  providers: [],
  backends: [],
  models: [],
  generated_at: "",
};

const uniqueStrings = (values: Array<string | null | undefined>): string[] => {
  const seen = new Set<string>();
  const result: string[] = [];
  values.forEach((value) => {
    if (typeof value !== "string") {
      return;
    }
    const normalized = value.trim();
    if (!normalized || seen.has(normalized)) {
      return;
    }
    seen.add(normalized);
    result.push(normalized);
  });
  return result;
};

const parseRoles = (raw: string): string[] => {
  const roles = raw
    .split(",")
    .map((item) => item.trim().toLowerCase())
    .filter(Boolean);
  return roles.length > 0 ? roles : ["reasoning"];
};

const formatSecretSource = (source: string | null | undefined): string => {
  switch (source) {
    case "platform_encrypted":
      return "Platform 暗号化保存";
    case "ENV":
      return "環境変数";
    case ".env":
      return ".env";
    case "unavailable":
      return "未設定";
    default:
      return source ?? "未設定";
  }
};

const formatRuntimeStatus = (status: string | null | undefined): string => {
  switch (status) {
    case "available":
      return "利用可能";
    case "unavailable":
      return "利用不可";
    case "running":
      return "稼働中";
    case "failed":
      return "失敗";
    case "stopped":
      return "停止";
    case "stop_failed":
      return "停止失敗";
    case "partial":
      return "一部成功";
    case "unknown":
      return "不明";
    default:
      return status ?? "不明";
  }
};

const engineBadgeStatus = (
  engine: LLMInferenceEngineConfigItem,
  runtime: LLMEngineRuntimeStatus | undefined,
): string => {
  if (
    engine.deployment_status &&
    ["failed", "stop_failed", "stopped"].includes(engine.deployment_status)
  ) {
    return engine.deployment_status;
  }
  if (runtime?.status === "available") {
    return "available";
  }
  if (engine.deployment_status === "running") {
    return "running";
  }
  return runtime?.status ?? engine.deployment_status ?? "unknown";
};

const engineBadgeToneClass = (status: string): string => {
  switch (status) {
    case "available":
    case "running":
      return "bg-emerald-500/10 text-emerald-300";
    case "stopped":
      return "bg-slate-700 text-slate-200";
    case "failed":
    case "stop_failed":
      return "bg-rose-500/10 text-rose-300";
    default:
      return "bg-amber-500/10 text-amber-300";
  }
};

const pickCatalogModel = (
  catalog: LLMCatalogResponse,
  options: {
    provider: string;
    modelType: LLMModelConfigItem["model_type"];
    fallbackModel: string;
    fallbackModelId: string;
    preferredModelIds?: string[];
    preferredModels?: string[];
  },
): { model: string; modelId: string } => {
  const candidates = catalog.models.filter(
    (item) =>
      item.provider === options.provider &&
      item.model_type === options.modelType,
  );
  const matchedById = options.preferredModelIds?.length
    ? candidates.find((item) =>
        options.preferredModelIds?.includes(item.model_id ?? ""),
      )
    : undefined;
  const matchedByModel = options.preferredModels?.length
    ? candidates.find((item) => options.preferredModels?.includes(item.model))
    : undefined;
  const matched =
    matchedById ??
    matchedByModel ??
    candidates.find((item) => item.model_id === options.fallbackModelId) ??
    candidates[0];
  return {
    model: matched?.model ?? options.fallbackModel,
    modelId: matched?.model_id ?? options.fallbackModelId,
  };
};

const recommendedProviderModels = (
  catalog: LLMCatalogResponse,
  providerName: string,
): string[] => {
  const provider = catalog.providers.find((item) => item.name === providerName);
  return uniqueStrings([
    ...(OFFICIAL_PROVIDER_MODELS[providerName] ?? []),
    ...(provider?.recommended_models ?? []),
    ...catalog.models
      .filter((item) => item.provider === providerName)
      .map((item) => item.model),
  ]).slice(0, 8);
};

const modelMatchesBackend = (
  modelName: string,
  backend: LLMBackendKind,
): boolean => {
  if (backend === "none") {
    return true;
  }
  if (backend === "ollama") {
    return !modelName.includes("/");
  }
  return modelName.includes("/");
};

const recommendedSwitchModels = (
  catalog: LLMCatalogResponse,
  overview: LLMManagementOverviewResponse | null,
  engineRuntime: LLMEngineRuntimeStatus[],
  providerName: LLMManagementProviderKind,
  backend: LLMBackendKind,
): string[] => {
  if (providerName !== "local") {
    return recommendedProviderModels(catalog, providerName);
  }

  const selectedBackend = backend === "none" ? null : backend;
  const engineNames = overview?.inference_engines
    .filter(
      (item) =>
        selectedBackend === null ||
        item.name === selectedBackend ||
        item.engine_type === selectedBackend,
    )
    .map((item) => item.name);
  const engineNameSet = new Set(engineNames ?? []);
  const engineSpecificModels = uniqueStrings([
    ...(overview?.inference_engines
      .filter((item) => engineNameSet.has(item.name))
      .map((item) => item.served_model_name) ?? []),
    ...(overview?.models
      .filter(
        (item) =>
          item.provider === "local" &&
          item.engine !== null &&
          engineNameSet.has(item.engine),
      )
      .map((item) => item.model) ?? []),
    ...engineRuntime
      .filter((item) => engineNameSet.has(item.name))
      .flatMap((item) => item.loaded_models),
  ]);

  const compatibleEngineModels = engineSpecificModels.filter((item) =>
    modelMatchesBackend(item, backend),
  );
  const fallbackEngineModels = engineSpecificModels.filter(
    (item) => !modelMatchesBackend(item, backend),
  );
  const catalogLocalModels = catalog.models
    .filter(
      (item) =>
        item.provider === "local" && modelMatchesBackend(item.model, backend),
    )
    .map((item) => item.model);
  const providerRecommended =
    catalog.providers
      .find((item) => item.name === "local")
      ?.recommended_models.filter((item) => modelMatchesBackend(item, backend)) ??
    [];
  const officialModels =
    selectedBackend === null ? [] : OFFICIAL_LOCAL_BACKEND_MODELS[selectedBackend];

  return uniqueStrings([
    ...officialModels,
    ...compatibleEngineModels,
    ...catalogLocalModels,
    ...providerRecommended,
    ...fallbackEngineModels,
  ]).slice(0, 8);
};

const inferLocalCategory = (
  provider: LLMProviderConfigItem,
  models: LLMModelConfigItem[],
): { label: string; detail: string } => {
  if (provider.name !== "local") {
    return {
      label: provider.name,
      detail: "SaaS provider",
    };
  }

  const relatedModels = models.filter(
    (item) => item.provider === provider.name && item.enabled,
  );
  const linkedEngines = uniqueStrings(relatedModels.map((item) => item.engine));
  if (linkedEngines.length > 0) {
    return {
      label: "local",
      detail: `ローカル provider です。現在は ${linkedEngines.join(", ")} を backend として OpenAI 互換 API を公開しています。`,
    };
  }

  const apiBase = (
    provider.api_base ??
    relatedModels.find((item) => item.api_base)?.api_base ??
    ""
  ).toLowerCase();
  if (apiBase.includes("11434")) {
    return {
      label: "local",
      detail: "ローカル provider です。ポート特性から Ollama 互換 backend と推定されます。",
    };
  }
  if (apiBase.includes("8080")) {
    return {
      label: "local",
      detail: "ローカル provider です。ポート特性から LocalAI 互換 backend と推定されます。",
    };
  }
  return {
    label: "local",
    detail:
      "ローカル provider です。実装は特定できませんが、汎用の OpenAI 互換ローカル入口として扱います。",
  };
};

const buildProviderEffectiveness = (
  provider: LLMProviderConfigItem,
  runtime: LLMProviderRuntimeStatus | undefined,
  models: LLMModelConfigItem[],
  engines: LLMEngineRuntimeStatus[],
): { label: string; detail: string; toneClass: string } => {
  if (!provider.enabled) {
    return {
      label: "利用不可",
      detail: "provider は無効化されています。",
      toneClass: "bg-rose-500/10 text-rose-300",
    };
  }

  const relatedModels = models.filter(
    (item) => item.provider === provider.name && item.enabled,
  );
  const linkedEngineNames = uniqueStrings(
    relatedModels.map((item) => item.engine),
  );
  if (runtime?.status === "available") {
    return {
      label: "確認済み",
      detail: provider.api_key_env
        ? `${formatSecretSource(provider.secret_status.source)} から API Key を解決できました。`
        : runtime.source
          ? `${runtime.source} の確認に成功しました。`
          : "runtime probe に成功しました。",
      toneClass: "bg-emerald-500/10 text-emerald-300",
    };
  }

  if (runtime?.last_error) {
    return {
      label: "利用不可",
      detail: runtime.last_error,
      toneClass: "bg-rose-500/10 text-rose-300",
    };
  }

  const linkedEngineStatuses = linkedEngineNames.map((engineName) =>
    engines.find((item) => item.name === engineName),
  );
  const failedReasons = linkedEngineStatuses
    .map(
      (item, index) =>
        `${linkedEngineNames[index]}: ${item?.last_error?.trim() || "disabled_or_unreachable"}`,
    )
    .filter(Boolean);
  return {
    label: "利用不可",
    detail:
      failedReasons.join(" / ") ||
      (provider.api_key_env
        ? "API Key が未設定です。"
        : "runtime probe に失敗しました。"),
    toneClass: "bg-rose-500/10 text-rose-300",
  };
};

const buildProviderExamples = (catalog: LLMCatalogResponse): DraftExample[] => {
  const openaiText = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "text",
    fallbackModel: "gpt-5-mini",
    fallbackModelId: "platform_text_default",
    preferredModelIds: ["platform_text_default", "coding_openai"],
    preferredModels: ["gpt-5-mini", "gpt-5.2"],
  });
  const openaiEmbedding = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "embedding",
    fallbackModel: "text-embedding-3-small",
    fallbackModelId: "platform_embedding_default",
  });
  const localText = pickCatalogModel(catalog, {
    provider: "local",
    modelType: "text",
    fallbackModel: "Qwen/Qwen2.5-0.5B-Instruct",
    fallbackModelId: "local_vllm_default",
    preferredModelIds: ["local_vllm_default"],
    preferredModels: ["Qwen/Qwen2.5-0.5B-Instruct"],
  });

  return [
    {
      id: "openai-default",
      label: "OpenAI 既定例",
      description: "Platform 正本として OpenAI を使う最小構成です。",
      value: prettify([
        {
          name: "openai",
          api_base: "https://api.openai.com/v1",
          api_key_env: "OPENAI_API_KEY",
          models: [openaiText.model, openaiEmbedding.model],
          enabled: true,
          secret_status: EMPTY_SECRET_STATUS,
        },
      ]),
    },
    {
      id: "openai-local",
      label: "OpenAI + local 例",
      description: "SaaS と Docker 配備 engine を併用する構成です。",
      value: prettify([
        {
          name: "openai",
          api_base: "https://api.openai.com/v1",
          api_key_env: "OPENAI_API_KEY",
          models: [openaiText.model, openaiEmbedding.model],
          enabled: true,
          secret_status: EMPTY_SECRET_STATUS,
        },
        {
          name: "local",
          api_base: `http://127.0.0.1:${DEFAULT_ENGINE_PORTS.vllm}`,
          api_key_env: null,
          models: [localText.model],
          enabled: true,
          secret_status: EMPTY_SECRET_STATUS,
        },
      ]),
    },
  ];
};

const buildEngineExamples = (catalog: LLMCatalogResponse): DraftExample[] => {
  const localText = pickCatalogModel(catalog, {
    provider: "local",
    modelType: "text",
    fallbackModel: "Qwen/Qwen2.5-0.5B-Instruct",
    fallbackModelId: "local_vllm_default",
    preferredModelIds: ["local_vllm_default"],
    preferredModels: ["Qwen/Qwen2.5-0.5B-Instruct"],
  });

  return [
    {
      id: "vllm-docker",
      label: "vLLM Docker 例",
      description: "Platform から compose を生成して配備する基本例です。",
      value: prettify([
        {
          name: "vllm",
          engine_type: "vllm",
          base_url: `http://127.0.0.1:${DEFAULT_ENGINE_PORTS.vllm}`,
          health_path: "/health",
          metrics_path: "/metrics",
          model_list_path: "/v1/models",
          enabled: true,
          deployment_mode: "docker",
          docker_image: "vllm/vllm-openai:v0.8.5",
          served_model_name: localText.model,
          container_name: "vllm-local",
          host_port: DEFAULT_ENGINE_PORTS.vllm,
          public_base_url: "https://llm.example.internal/v1",
          gpu_enabled: true,
          gpu_devices: ["0"],
          gpu_count: 1,
          extra_env: {
            HF_HOME: "/data/hf-cache",
          },
          deployment_status: null,
          deployment_error: null,
          compose_path: null,
        },
      ]),
    },
    {
      id: "manual-external",
      label: "外部 engine 例",
      description: "既存 vLLM / TGI を手動管理して URL のみ参照する例です。",
      value: prettify([
        {
          name: "external_vllm",
          engine_type: "vllm",
          base_url: "https://llm-gateway.example.com/v1",
          health_path: "/health",
          metrics_path: "/metrics",
          model_list_path: "/v1/models",
          enabled: true,
          deployment_mode: "manual",
          docker_image: null,
          served_model_name: localText.model,
          container_name: null,
          host_port: null,
          public_base_url: "https://llm-gateway.example.com/v1",
          gpu_enabled: false,
          gpu_devices: [],
          gpu_count: null,
          extra_env: {},
          deployment_status: null,
          deployment_error: null,
          compose_path: null,
        },
      ]),
    },
  ];
};

const buildModelExamples = (catalog: LLMCatalogResponse): DraftExample[] => {
  const openaiText = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "text",
    fallbackModel: "gpt-5-mini",
    fallbackModelId: "platform_text_default",
    preferredModelIds: ["platform_text_default", "coding_openai"],
    preferredModels: ["gpt-5-mini", "gpt-5.2"],
  });
  const openaiEmbedding = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "embedding",
    fallbackModel: "text-embedding-3-small",
    fallbackModelId: "platform_embedding_default",
  });
  const openaiImage = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "image",
    fallbackModel: "gpt-image-1",
    fallbackModelId: "platform_image_default",
  });
  const openaiStt = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "speech_to_text",
    fallbackModel: "gpt-4o-transcribe",
    fallbackModelId: "platform_speech_to_text_default",
    preferredModelIds: ["platform_speech_to_text_default"],
  });
  const openaiTts = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "text_to_speech",
    fallbackModel: "gpt-4o-mini-tts",
    fallbackModelId: "platform_text_to_speech_default",
    preferredModelIds: ["platform_text_to_speech_default"],
  });
  const localText = pickCatalogModel(catalog, {
    provider: "local",
    modelType: "text",
    fallbackModel: "Qwen/Qwen2.5-0.5B-Instruct",
    fallbackModelId: "local_vllm_default",
    preferredModelIds: ["local_vllm_default"],
    preferredModels: ["Qwen/Qwen2.5-0.5B-Instruct"],
  });

  return [
    {
      id: "platform-default-set",
      label: "Platform 既定モデル例",
      description: "text / embedding / image / speech をひと通り揃える例です。",
      value: prettify([
        {
          alias: "platform_text_default",
          model_id: openaiText.modelId,
          provider: "openai",
          model: openaiText.model,
          model_type: "text",
          api_base: null,
          api_key_env: "OPENAI_API_KEY",
          engine: null,
          enabled: true,
          modalities: ["text"],
          quality_score: 0.9,
          avg_latency_ms: 850,
          cost: { input_per_1k: 0.0004, output_per_1k: 0.0016 },
        },
        {
          alias: "embedding_openai",
          model_id: openaiEmbedding.modelId,
          provider: "openai",
          model: openaiEmbedding.model,
          model_type: "embedding",
          api_base: null,
          api_key_env: "OPENAI_API_KEY",
          engine: null,
          enabled: true,
          modalities: ["embedding"],
          quality_score: 0.88,
          avg_latency_ms: 450,
          cost: { input_per_1k: 0.00002, output_per_1k: 0 },
        },
        {
          alias: "image_openai",
          model_id: openaiImage.modelId,
          provider: "openai",
          model: openaiImage.model,
          model_type: "image",
          api_base: null,
          api_key_env: "OPENAI_API_KEY",
          engine: null,
          enabled: true,
          modalities: ["image"],
          quality_score: 0.9,
          avg_latency_ms: 2400,
          cost: { input_per_1k: 0, output_per_1k: 0.04 },
        },
        {
          alias: "stt_openai",
          model_id: openaiStt.modelId,
          provider: "openai",
          model: openaiStt.model,
          model_type: "speech_to_text",
          api_base: null,
          api_key_env: "OPENAI_API_KEY",
          engine: null,
          enabled: true,
          modalities: ["audio", "text"],
          quality_score: 0.84,
          avg_latency_ms: 1200,
          cost: { input_per_1k: 0, output_per_1k: 0.006 },
        },
        {
          alias: "tts_openai",
          model_id: openaiTts.modelId,
          provider: "openai",
          model: openaiTts.model,
          model_type: "text_to_speech",
          api_base: null,
          api_key_env: "OPENAI_API_KEY",
          engine: null,
          enabled: true,
          modalities: ["text", "audio"],
          quality_score: 0.81,
          avg_latency_ms: 1300,
          cost: { input_per_1k: 0, output_per_1k: 0.015 },
        },
      ]),
    },
    {
      id: "hybrid-text-set",
      label: "SaaS + local 推論例",
      description: "高品質は SaaS、安価ロールは local engine を使う例です。",
      value: prettify([
        {
          alias: "platform_text_default",
          model_id: openaiText.modelId,
          provider: "openai",
          model: openaiText.model,
          model_type: "text",
          api_base: null,
          api_key_env: "OPENAI_API_KEY",
          engine: null,
          enabled: true,
          modalities: ["text"],
          quality_score: 0.9,
          avg_latency_ms: 850,
          cost: { input_per_1k: 0.0004, output_per_1k: 0.0016 },
        },
        {
          alias: "local_vllm_default",
          model_id: localText.modelId,
          provider: "local",
          model: localText.model,
          model_type: "text",
          api_base: `http://127.0.0.1:${DEFAULT_ENGINE_PORTS.vllm}/v1`,
          api_key_env: null,
          engine: "vllm",
          enabled: true,
          modalities: ["text"],
          quality_score: 0.68,
          avg_latency_ms: 550,
          cost: { input_per_1k: 0, output_per_1k: 0 },
        },
      ]),
    },
  ];
};

const buildRegistryExamples = (): DraftExample[] => {
  return [
    {
      id: "default-roles",
      label: "標準 role 例",
      description:
        "text / embedding / image を role 別に割り当てる基本例です。",
      value: prettify({
        reasoning: "platform_text_default",
        coding: "platform_text_default",
        cheap: "local_vllm_default",
        local: "local_vllm_default",
        embedding: "embedding_openai",
        image: "image_openai",
      }),
    },
  ];
};

const buildRoutingExamples = (): DraftExample[] => {
  return [
    {
      id: "balanced-routing",
      label: "フォールバック例",
      description: "高品質優先で local を fallback に置く例です。",
      value: prettify({
        priority: "quality",
        fallback_chain: {
          reasoning: ["platform_text_default", "local_vllm_default"],
          coding: ["platform_text_default", "local_vllm_default"],
          cheap: ["local_vllm_default", "platform_text_default"],
          image: ["image_openai"],
        },
        load_balance_strategy: "least_latency",
        cost_budget: 25,
      }),
    },
  ];
};

const buildContractExamples = (
  catalog: LLMCatalogResponse,
): ContractExample[] => {
  const openaiText = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "text",
    fallbackModel: "gpt-5-mini",
    fallbackModelId: "platform_text_default",
    preferredModelIds: ["platform_text_default"],
  });
  const openaiEmbedding = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "embedding",
    fallbackModel: "text-embedding-3-small",
    fallbackModelId: "platform_embedding_default",
    preferredModelIds: ["platform_embedding_default"],
  });
  const openaiImage = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "image",
    fallbackModel: "gpt-image-1",
    fallbackModelId: "platform_image_default",
    preferredModelIds: ["platform_image_default"],
  });
  const openaiStt = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "speech_to_text",
    fallbackModel: "gpt-4o-transcribe",
    fallbackModelId: "platform_speech_to_text_default",
    preferredModelIds: ["platform_speech_to_text_default"],
  });
  const openaiTts = pickCatalogModel(catalog, {
    provider: "openai",
    modelType: "text_to_speech",
    fallbackModel: "gpt-4o-mini-tts",
    fallbackModelId: "platform_text_to_speech_default",
    preferredModelIds: ["platform_text_to_speech_default"],
  });

  return [
    {
      id: "faq-app",
      title: "FAQ / 埋め込みアプリ例",
      description:
        "FAQ や RAG アプリが Platform 既定 text + embedding を使う最小構成です。",
      projectLabel: "FAQ / RAG",
      notes: [
        "ナレッジベース QA、企業 FAQ、検索拡張アプリ向けです。",
        "安定した `platform_*` model_id を参照し、アプリ側でモデル名を固定化しない構成にします。",
      ],
      value: prettify({
        contracts: {
          llm: {
            enabled: true,
            allowed_modalities: ["text", "embedding"],
            defaults: {
              text: {
                provider: "openai",
                model_id: openaiText.modelId,
                model_type: "text",
              },
              embedding: {
                provider: "openai",
                model_id: openaiEmbedding.modelId,
                model_type: "embedding",
              },
            },
            agent_overrides: {},
            extra_model_refs: [],
          },
        },
      }),
    },
    {
      id: "multimodal-app",
      title: "画像 / 音声アプリ例",
      description:
        "text, image, speech_to_text, text_to_speech をアプリ単位で宣言する例です。",
      projectLabel: "マルチモーダル / Voice",
      notes: [
        "音声アシスタント、画像生成、録音文字起こし系のアプリ向けです。",
        "VoiceAgent は text だけを上書きし、それ以外は Platform 既定を継続利用します。",
        "電話窓口、リアルタイム音声応対、音声読み上げの細かな結線は下の専用音声契約例を優先してください。",
      ],
      value: prettify({
        contracts: {
          llm: {
            enabled: true,
            allowed_modalities: [
              "text",
              "image",
              "speech_to_text",
              "text_to_speech",
            ],
            defaults: {
              text: {
                provider: "openai",
                model_id: openaiText.modelId,
                model_type: "text",
              },
              image: {
                provider: "openai",
                model_id: openaiImage.modelId,
                model_type: "image",
              },
              speech_to_text: {
                provider: "openai",
                model_id: openaiStt.modelId,
                model_type: "speech_to_text",
              },
              text_to_speech: {
                provider: "openai",
                model_id: openaiTts.modelId,
                model_type: "text_to_speech",
              },
            },
            agent_overrides: {
              VoiceAgent: {
                text: {
                  provider: "openai",
                  model_id: openaiText.modelId,
                  model_type: "text",
                },
              },
            },
            extra_model_refs: [],
          },
        },
      }),
    },
    {
      id: "voice-call-app",
      title: "音声窓口 / コールセンターアプリ例",
      description:
        "speech_to_text / text / text_to_speech を分離宣言し、文字起こし・推論・読み上げを明確な音声経路へ結びつける例です。",
      projectLabel: "音声 / コール",
      notes: [
        "電話窓口、会議文字起こし、音声オペレーター、音声折り返し通知向けのアプリに適しています。",
        ...OFFICIAL_VOICE_STACK_NOTES,
        "`contracts.llm` では Platform catalog に登録済みの model_id のみ参照できます。Google / Anthropic 系の音声スタックを使う場合は、先に Gateway へ対象 model_id を登録してからここで参照してください。",
      ],
      value: prettify({
        contracts: {
          llm: {
            enabled: true,
            allowed_modalities: ["text", "speech_to_text", "text_to_speech"],
            defaults: {
              speech_to_text: {
                provider: "openai",
                model_id: openaiStt.modelId,
                model_type: "speech_to_text",
              },
              text: {
                provider: "openai",
                model_id: openaiText.modelId,
                model_type: "text",
              },
              text_to_speech: {
                provider: "openai",
                model_id: openaiTts.modelId,
                model_type: "text_to_speech",
              },
            },
            agent_overrides: {
              VoiceAgent: {
                speech_to_text: {
                  provider: "openai",
                  model_id: openaiStt.modelId,
                  model_type: "speech_to_text",
                },
                text: {
                  provider: "openai",
                  model_id: openaiText.modelId,
                  model_type: "text",
                },
                text_to_speech: {
                  provider: "openai",
                  model_id: openaiTts.modelId,
                  model_type: "text_to_speech",
                },
              },
              TranscriptSummaryAgent: {
                text: {
                  provider: "openai",
                  model_id: openaiText.modelId,
                  model_type: "text",
                },
              },
            },
            extra_model_refs: [
              {
                provider: "openai",
                model_id: openaiStt.modelId,
                model_type: "speech_to_text",
              },
              {
                provider: "openai",
                model_id: openaiText.modelId,
                model_type: "text",
              },
              {
                provider: "openai",
                model_id: openaiTts.modelId,
                model_type: "text_to_speech",
              },
            ],
          },
        },
      }),
    },
  ];
};

const classifyWorkflowFailure = (
  preflight: LLMPreflightReport | null,
  switchResult: LLMSwitchResponse | null,
): ErrorCategory => {
  if (preflight) {
    if (
      preflight.steps.some(
        (step) => step.phase === "install" && step.status === "failed",
      )
    ) {
      return "install";
    }
    if (
      preflight.steps.some(
        (step) => step.phase === "health" && step.status === "failed",
      )
    ) {
      return "health";
    }
  }
  if (switchResult?.message.toLowerCase().includes("validation")) {
    return "validation";
  }
  return "network";
};

export function LLMManagement() {
  const { t } = useI18n();
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [errorCategory, setErrorCategory] = useState<ErrorCategory>(null);
  const [message, setMessage] = useState<string | null>(null);

  const [overview, setOverview] =
    useState<LLMManagementOverviewResponse | null>(null);
  const [providerRuntime, setProviderRuntime] = useState<
    LLMProviderRuntimeStatus[]
  >([]);
  const [engineRuntime, setEngineRuntime] = useState<LLMEngineRuntimeStatus[]>(
    [],
  );
  const [catalog, setCatalog] = useState<LLMCatalogResponse>(EMPTY_CATALOG);
  const [diagnostics, setDiagnostics] = useState<LLMDiagnosticsResponse | null>(
    null,
  );
  const [advancedMode, setAdvancedMode] = useState(false);
  const [openapiHasLLMRoutes, setOpenapiHasLLMRoutes] = useState<
    boolean | null
  >(null);

  const [providersDraft, setProvidersDraft] = useState("[]");
  const [enginesDraft, setEnginesDraft] = useState("[]");
  const [modelsDraft, setModelsDraft] = useState("[]");
  const [registryDraft, setRegistryDraft] = useState("{}");
  const [routingDraft, setRoutingDraft] = useState("{}");
  const [secretInputs, setSecretInputs] = useState<Record<string, string>>({});

  const [switchProvider, setSwitchProvider] =
    useState<LLMManagementProviderKind>("openai");
  const [switchModel, setSwitchModel] = useState("");
  const [switchBackend, setSwitchBackend] = useState<LLMBackendKind>("none");
  const [switchRoles, setSwitchRoles] = useState(DEFAULT_ROLES);
  const [autoSetup, setAutoSetup] = useState(true);
  const [autoInstall, setAutoInstall] = useState(true);
  const [autoStart, setAutoStart] = useState(true);
  const [healthCheck, setHealthCheck] = useState(true);
  const [validateRuntime, setValidateRuntime] = useState(true);

  const [lastPreflight, setLastPreflight] = useState<LLMPreflightReport | null>(
    null,
  );
  const [lastSwitch, setLastSwitch] = useState<LLMSwitchResponse | null>(null);
  const [lastEngineAction, setLastEngineAction] =
    useState<LLMEngineDeployResponse | null>(null);
  const [lastEngineActionLabel, setLastEngineActionLabel] =
    useState("配備結果");

  const diagnoseMissingRoute = async () => {
    const hints: string[] = [];
    try {
      const paths = await fetchOpenAPIPaths();
      const llmPaths = paths.filter((path) =>
        path.startsWith("/api/studios/framework/llm"),
      );
      const hasRoutes = llmPaths.length > 0;
      setOpenapiHasLLMRoutes(hasRoutes);
      if (!hasRoutes) {
        hints.push(
          "Backend の OpenAPI に /api/studios/framework/llm ルートがありません。",
        );
      }
    } catch (diagnoseErr) {
      const reason =
        diagnoseErr instanceof Error ? diagnoseErr.message : "unknown";
      hints.push(`/openapi.json の確認に失敗しました: ${reason}`);
      setOpenapiHasLLMRoutes(null);
    }

    try {
      const payload = await fetchLLMDiagnostics();
      setDiagnostics(payload);
      hints.push(...payload.hints);
    } catch {
      // diagnostics endpoint is unavailable when llm router is missing in stale backend processes
    }

    const combined =
      hints.length > 0
        ? hints.join(" ")
        : "LLM 管理ルートを利用できません。backend を再起動し、/openapi.json に /api/studios/framework/llm/* が含まれることを確認してください。";
    setErrorCategory("route_missing");
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
      setSecretInputs((current) => {
        const next: Record<string, string> = {};
        overviewPayload.providers.forEach((provider) => {
          next[provider.name] = current[provider.name] ?? "";
        });
        return next;
      });
      setSwitchProvider((current) => {
        const allowed = new Set<LLMManagementProviderKind>([
          ...catalogPayload.providers.map((item) => item.name),
          ...overviewPayload.providers.map(
            (item) => item.name as LLMManagementProviderKind,
          ),
        ]);
        return allowed.has(current)
          ? current
          : ((catalogPayload.providers[0]?.name ??
              overviewPayload.providers[0]?.name ??
              "openai") as LLMManagementProviderKind);
      });

      const preferredModel =
        recommendedProviderModels(catalogPayload, switchProvider)[0] ?? "";
      if (!switchModel && preferredModel) {
        setSwitchModel(preferredModel);
      }

      const engineStatus = await fetchLLMEngineStatus();
      setEngineRuntime(engineStatus.engine_status);
    } catch (err) {
      if (axios.isAxiosError(err) && err.response?.status === 404) {
        await diagnoseMissingRoute();
      } else {
        const text =
          err instanceof Error ? err.message : t("llm_mgmt.error_load");
        setErrorCategory("network");
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
    const availableProviders = providerRuntime.filter(
      (item) => item.status === "available",
    ).length;
    const availableEngines = engineRuntime.filter(
      (item) => item.status === "available",
    ).length;
    return {
      providers: `${availableProviders}/${providerRuntime.length}`,
      engines: `${availableEngines}/${engineRuntime.length}`,
      totalCost: overview?.cost_summary.total_cost_usd ?? 0,
      budgetExceeded: overview?.cost_summary.budget_exceeded ?? false,
    };
  }, [engineRuntime, overview, providerRuntime]);

  const providerRuntimeMap = useMemo(() => {
    return new Map(providerRuntime.map((item) => [item.name, item]));
  }, [providerRuntime]);

  const switchProviderOptions = useMemo(() => {
    const names = uniqueStrings([
      ...catalog.providers.map((item) => item.name),
      ...(overview?.providers.map((item) => item.name) ?? []),
    ]);
    return names as LLMManagementProviderKind[];
  }, [catalog.providers, overview?.providers]);

  const providerModelHints = useMemo(() => {
    return recommendedSwitchModels(
      catalog,
      overview ?? null,
      engineRuntime,
      switchProvider,
      switchBackend,
    );
  }, [catalog, engineRuntime, overview, switchBackend, switchProvider]);

  const switchBackendOptions = useMemo(() => {
    if (switchProvider !== "local") {
      return ["none"] as LLMBackendKind[];
    }

    const catalogLocalEngines =
      catalog.providers.find((item) => item.name === "local")?.local_engines ??
      [];
    const configuredLocalEngines =
      overview?.inference_engines.map((item) => item.name as LLMBackendKind) ??
      [];
    const merged = uniqueStrings([
      ...catalogLocalEngines,
      ...configuredLocalEngines,
    ]) as LLMBackendKind[];
    return merged.length > 0 ? merged : ["vllm", "sglang", "tgi", "ollama"];
  }, [catalog.providers, overview?.inference_engines, switchProvider]);

  useEffect(() => {
    if (switchProvider !== "local") {
      if (switchBackend !== "none") {
        setSwitchBackend("none");
      }
      return;
    }

    const availableEngine = overview?.inference_engines.find((engine) =>
      engineRuntime.some(
        (runtime) =>
          runtime.name === engine.name && runtime.status === "available",
      ),
    );
    const enabledEngine = overview?.inference_engines.find(
      (engine) => engine.enabled,
    );
    const preferredBackend =
      (availableEngine?.name as LLMBackendKind | undefined) ??
      (enabledEngine?.name as LLMBackendKind | undefined) ??
      switchBackendOptions[0];

    if (
      preferredBackend &&
      (switchBackend === "none" || !switchBackendOptions.includes(switchBackend))
    ) {
      setSwitchBackend(preferredBackend as LLMBackendKind);
    }
  }, [
    engineRuntime,
    overview?.inference_engines,
    switchBackend,
    switchBackendOptions,
    switchProvider,
  ]);

  useEffect(() => {
    if (providerModelHints.length === 0) {
      return;
    }
    setSwitchModel((current) => {
      const normalized = current.trim();
      if (!normalized || !providerModelHints.includes(normalized)) {
        return providerModelHints[0];
      }
      return current;
    });
  }, [providerModelHints]);

  const providerExamples = useMemo(
    () => buildProviderExamples(catalog),
    [catalog],
  );
  const engineExamples = useMemo(() => buildEngineExamples(catalog), [catalog]);
  const modelExamples = useMemo(() => buildModelExamples(catalog), [catalog]);
  const registryExamples = useMemo(() => buildRegistryExamples(), []);
  const routingExamples = useMemo(() => buildRoutingExamples(), []);
  const contractExamples = useMemo(
    () => buildContractExamples(catalog),
    [catalog],
  );

  const parseJson = <T,>(label: string, raw: string): T => {
    try {
      return JSON.parse(raw) as T;
    } catch (err) {
      const details = err instanceof Error ? err.message : "invalid json";
      const wrapped = new Error(`${label}: ${details}`) as Error & {
        cause?: unknown;
      };
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
      setMessage(t("llm_mgmt.saved"));
      await load();
    } catch (err) {
      if (
        axios.isAxiosError(err) &&
        (err.response?.status === 400 || err.response?.status === 422)
      ) {
        setErrorCategory("validation");
      } else {
        setErrorCategory("network");
      }
      setError(err instanceof Error ? err.message : t("llm_mgmt.error_save"));
    } finally {
      setSaving(false);
    }
  };

  const applyDraftExample = (target: DraftTarget, example: DraftExample) => {
    switch (target) {
      case "providers":
        setProvidersDraft(example.value);
        break;
      case "engines":
        setEnginesDraft(example.value);
        break;
      case "models":
        setModelsDraft(example.value);
        break;
      case "registry":
        setRegistryDraft(example.value);
        break;
      case "routing":
        setRoutingDraft(example.value);
        break;
    }
    setError(null);
    setErrorCategory(null);
    setMessage(
      `設定例「${example.label}」を読み込みました。保存すると反映されます。`,
    );
  };

  const resetDraft = (target: DraftTarget) => {
    if (!overview) {
      return;
    }
    switch (target) {
      case "providers":
        setProvidersDraft(prettify(overview.providers));
        break;
      case "engines":
        setEnginesDraft(prettify(overview.inference_engines));
        break;
      case "models":
        setModelsDraft(prettify(overview.models));
        break;
      case "registry":
        setRegistryDraft(prettify(overview.registry));
        break;
      case "routing":
        setRoutingDraft(prettify(overview.routing_policy));
        break;
    }
    setError(null);
    setErrorCategory(null);
    setMessage("保存済みの設定に戻しました。");
  };

  const formatDraft = (target: DraftTarget, label: string, raw: string) => {
    try {
      const formatted = prettify(parseJson<unknown>(label, raw));
      switch (target) {
        case "providers":
          setProvidersDraft(formatted);
          break;
        case "engines":
          setEnginesDraft(formatted);
          break;
        case "models":
          setModelsDraft(formatted);
          break;
        case "registry":
          setRegistryDraft(formatted);
          break;
        case "routing":
          setRoutingDraft(formatted);
          break;
      }
      setError(null);
      setErrorCategory(null);
      setMessage(`${label} の JSON を整形しました。`);
    } catch (err) {
      setErrorCategory("validation");
      setError(
        err instanceof Error
          ? err.message
          : `${label} の JSON 整形に失敗しました。`,
      );
    }
  };

  const saveProviders = async () =>
    withSave(async () => {
      const payload = parseJson<LLMProviderConfigItem[]>(
        t("llm_mgmt.section_providers"),
        providersDraft,
      );
      await updateLLMProviders(payload);
    });

  const saveEngines = async () =>
    withSave(async () => {
      const payload = parseJson<LLMInferenceEngineConfigItem[]>(
        t("llm_mgmt.section_engines"),
        enginesDraft,
      );
      await updateLLMInferenceEngines(payload);
    });

  const saveModels = async () =>
    withSave(async () => {
      const payload = parseJson<LLMModelConfigItem[]>(
        t("llm_mgmt.section_models"),
        modelsDraft,
      );
      await updateLLMModels(payload);
    });

  const saveRegistry = async () =>
    withSave(async () => {
      const payload = parseJson<Record<string, string>>(
        t("llm_mgmt.section_registry"),
        registryDraft,
      );
      await updateLLMRegistry(payload);
    });

  const saveRouting = async () =>
    withSave(async () => {
      const payload = parseJson<LLMRoutingPolicyConfig>(
        t("llm_mgmt.section_routing"),
        routingDraft,
      );
      await updateLLMRoutingPolicy(payload);
    });

  const saveProviderSecret = async (provider: LLMProviderConfigItem) =>
    withSave(async () => {
      const secretValue = secretInputs[provider.name]?.trim() ?? "";
      if (!secretValue) {
        throw new Error("保存する API キーを入力してください。");
      }
      await updateLLMProviderSecret(provider.name, {
        api_key_env: provider.api_key_env,
        secret_value: secretValue,
      });
      setSecretInputs((current) => ({ ...current, [provider.name]: "" }));
    });

  const removeProviderSecret = async (provider: LLMProviderConfigItem) =>
    withSave(async () => {
      await deleteLLMProviderSecret(provider.name);
    });

  const handleDeployEngine = async (engine: LLMInferenceEngineConfigItem) =>
    withSave(async () => {
      const response = await deployLLMEngine(engine.name, {
        public_base_url: engine.public_base_url,
      });
      setLastEngineActionLabel("配備結果");
      setLastEngineAction(response);
      if (!response.success) {
        throw new Error(response.message);
      }
    });

  const handleStopEngine = async (engine: LLMInferenceEngineConfigItem) =>
    withSave(async () => {
      const response = await stopLLMEngine(engine.name);
      setLastEngineActionLabel("停止結果");
      setLastEngineAction(response);
      if (!response.success) {
        throw new Error(response.message);
      }
    });

  const handlePrefetchEngineModel = async (engine: LLMInferenceEngineConfigItem) =>
    withSave(async () => {
      const response = await prefetchLLMEngineModel(engine.name);
      setLastEngineActionLabel("モデル取得結果");
      setLastEngineAction(response);
      if (!response.success) {
        throw new Error(response.message);
      }
    });

  const handleReload = async () => {
    setSaving(true);
    setError(null);
    setErrorCategory(null);
    setMessage(null);
    try {
      await reloadLLMManagementConfig();
      setMessage(t("llm_mgmt.reloaded"));
      await load();
    } catch (err) {
      setErrorCategory("network");
      setError(err instanceof Error ? err.message : t("llm_mgmt.error_reload"));
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
            backends: backend === "none" ? [] : [backend],
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
          const category = classifyWorkflowFailure(
            response.preflight,
            response.switch,
          );
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

      setMessage("切替が完了しました。");
      await load();
    } catch (err) {
      if (axios.isAxiosError(err) && err.response?.status === 404) {
        await diagnoseMissingRoute();
      } else if (
        axios.isAxiosError(err) &&
        (err.response?.status === 400 || err.response?.status === 422)
      ) {
        setErrorCategory("validation");
        setError(err.message);
      } else {
        setErrorCategory("network");
        setError(err instanceof Error ? err.message : "切替に失敗しました。");
      }
    } finally {
      setSaving(false);
    }
  };

  const Section = (props: {
    title: string;
    description: string;
    value: string;
    onChange: (next: string) => void;
    onSave: () => Promise<void>;
    helper?: string;
    examples?: DraftExample[];
    onFormat?: () => void;
    onReset?: () => void;
    testId: DraftTarget;
  }) => {
    return (
      <section className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-3">
        <div>
          <h2 className="text-sm font-semibold text-slate-100">
            {props.title}
          </h2>
          <p className="text-xs text-slate-500 mt-1">{props.description}</p>
          {props.helper && (
            <p className="text-[11px] text-slate-400 mt-1 break-all">
              {props.helper}
            </p>
          )}
        </div>
        <div className="flex flex-wrap gap-2">
          {props.onFormat && (
            <button
              type="button"
              disabled={saving}
              onClick={props.onFormat}
              className="px-3 py-1.5 text-[11px] rounded-lg bg-slate-800 hover:bg-slate-700 text-slate-100 disabled:opacity-50"
            >
              JSON を整形
            </button>
          )}
          {props.onReset && (
            <button
              type="button"
              disabled={saving}
              onClick={props.onReset}
              className="px-3 py-1.5 text-[11px] rounded-lg bg-slate-800 hover:bg-slate-700 text-slate-100 disabled:opacity-50"
            >
              保存済み値に戻す
            </button>
          )}
          {props.examples?.map((example) => (
            <button
              key={example.id}
              type="button"
              disabled={saving}
              onClick={() => applyDraftExample(props.testId, example)}
              data-testid={`llm-example-${props.testId}-${example.id}`}
              className="px-3 py-1.5 text-[11px] rounded-lg bg-indigo-600/20 hover:bg-indigo-600/30 text-indigo-100 border border-indigo-500/30 disabled:opacity-50"
              title={example.description}
            >
              例: {example.label}
            </button>
          ))}
        </div>
        <textarea
          data-testid={`${props.testId}-editor`}
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
            {saving ? t("llm_mgmt.saving") : t("common.save")}
          </button>
        </div>
      </section>
    );
  };

  if (loading) {
    return (
      <div className="p-6 max-w-6xl mx-auto">
        <p className="text-sm text-slate-400">{t("common.loading")}</p>
      </div>
    );
  }

  const errorStyle =
    errorCategory === "route_missing"
      ? "bg-amber-500/10 border-amber-500/30 text-amber-200"
      : "bg-red-500/10 border-red-500/30 text-red-300";

  return (
    <div className="p-6 max-w-6xl mx-auto space-y-6">
      <div className="flex items-start justify-between gap-4">
        <div>
          <h1 className="text-2xl font-bold text-slate-100">
            {t("llm_mgmt.title")}
          </h1>
          <p className="text-sm text-slate-500 mt-1">
            {t("llm_mgmt.subtitle")}
          </p>
        </div>
        <div className="flex items-center gap-2">
          <button
            disabled={saving}
            onClick={() => setAdvancedMode((prev) => !prev)}
            data-testid="llm-advanced-toggle"
            className="px-4 py-2 bg-slate-800 hover:bg-slate-700 text-slate-100 text-sm rounded-lg"
          >
            {advancedMode ? "詳細編集を閉じる" : "詳細編集を開く"}
          </button>
          <button
            disabled={saving}
            onClick={() => void handleReload()}
            className="px-4 py-2 bg-slate-800 hover:bg-slate-700 disabled:opacity-50 disabled:cursor-not-allowed text-slate-100 text-sm rounded-lg"
          >
            {saving ? t("llm_mgmt.saving") : t("llm_mgmt.reload")}
          </button>
        </div>
      </div>

      {error && (
        <div
          data-testid={
            errorCategory === "route_missing"
              ? "llm-route-missing-diagnostic"
              : "llm-error-banner"
          }
          className={`border rounded-lg p-3 text-sm ${errorStyle}`}
        >
          <p>{error}</p>
          {errorCategory === "route_missing" && (
            <ul className="mt-2 text-xs list-disc list-inside space-y-1">
              <li>1) Platform backend プロセスを再起動してください。</li>
              <li>
                2) backend の OpenAPI に `/api/studios/framework/llm/*`
                が含まれることを確認してください。
              </li>
              <li>3) backend 再起動後にこの画面を再読み込みしてください。</li>
              {openapiHasLLMRoutes === true && (
                <li>
                  OpenAPI には LLM ルートがあります。frontend proxy と backend
                  port の不一致を確認してください。
                </li>
              )}
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

      {overview && (
        <section className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
          <div>
            <h2 className="text-sm font-semibold text-slate-100">
              プロバイダー管理
            </h2>
            <p className="text-xs text-slate-500 mt-1">
              API Key は Platform 側で暗号化保存します。保存済み secret があれば
              app は env 直指定なしで参照できます。
            </p>
          </div>
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
            {overview.providers.map((provider) => {
              const runtime = providerRuntimeMap.get(provider.name);
              const providerCategory = inferLocalCategory(
                provider,
                overview.models,
              );
              const providerEffectiveness = buildProviderEffectiveness(
                provider,
                runtime,
                overview.models,
                engineRuntime,
              );
              return (
                <div
                  key={provider.name}
                  className="bg-slate-950/70 border border-slate-800 rounded-lg p-4 space-y-3"
                >
                  <div className="flex items-start justify-between gap-3">
                    <div>
                      <h3 className="text-sm font-semibold text-slate-100">
                        {providerCategory.label}
                      </h3>
                      <p className="text-xs text-slate-500 mt-1 break-all">
                        {provider.api_base ?? "API Base 未設定"}
                      </p>
                    </div>
                    <span
                      className={`text-[11px] px-2 py-1 rounded-full ${provider.enabled ? "bg-emerald-500/10 text-emerald-300" : "bg-slate-700 text-slate-300"}`}
                    >
                      {provider.enabled ? "有効" : "無効"}
                    </span>
                  </div>
                  <div className="text-xs text-slate-300 space-y-1">
                    <p>
                      分類:{" "}
                      <span className="text-slate-100">
                        {providerCategory.detail}
                      </span>
                    </p>
                    <p>
                      API Key 変数:{" "}
                      <span className="text-slate-100">
                        {provider.api_key_env ?? "不要"}
                      </span>
                    </p>
                    <p>
                      保存状態:{" "}
                      <span className="text-slate-100">
                        {provider.secret_status.masked ?? "未保存"}
                      </span>
                    </p>
                    <p>
                      解決元:{" "}
                      <span className="text-slate-100">
                        {formatSecretSource(provider.secret_status.source)}
                      </span>
                    </p>
                    <p>
                      実行時状態:{" "}
                      <span className="text-slate-100">
                        {formatRuntimeStatus(runtime?.status)}
                      </span>
                    </p>
                    <div className="pt-1">
                      <span
                        className={`inline-flex px-2 py-1 rounded-full text-[11px] ${providerEffectiveness.toneClass}`}
                      >
                        {providerEffectiveness.label}
                      </span>
                      <p className="text-[11px] text-slate-400 mt-1">
                        {providerEffectiveness.detail}
                      </p>
                    </div>
                  </div>
                  {provider.api_key_env && (
                    <div className="space-y-2">
                      <input
                        type="password"
                        value={secretInputs[provider.name] ?? ""}
                        onChange={(event) =>
                          setSecretInputs((current) => ({
                            ...current,
                            [provider.name]: event.target.value,
                          }))
                        }
                        placeholder={`${provider.name} の API Key`}
                        className="w-full bg-slate-900 border border-slate-700 rounded px-3 py-2 text-sm text-slate-100"
                      />
                      <div className="flex items-center justify-end gap-2">
                        <button
                          type="button"
                          disabled={saving}
                          onClick={() => void removeProviderSecret(provider)}
                          className="px-3 py-2 text-xs rounded-lg bg-slate-800 hover:bg-slate-700 text-slate-100 disabled:opacity-50"
                        >
                          保存済み secret を削除
                        </button>
                        <button
                          type="button"
                          disabled={saving}
                          onClick={() => void saveProviderSecret(provider)}
                          className="px-3 py-2 text-xs rounded-lg bg-indigo-600 hover:bg-indigo-700 text-white disabled:opacity-50"
                        >
                          API Key を暗号化保存
                        </button>
                      </div>
                    </div>
                  )}
                </div>
              );
            })}
          </div>
        </section>
      )}

      {overview && (
        <section className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
          <div>
            <h2 className="text-sm font-semibold text-slate-100">
              ローカル推論エンジン
            </h2>
            <p className="text-xs text-slate-500 mt-1">
              engine 設定を保存したあとに docker compose
              を生成して起動します。公開 URL がある場合は engine に保存して app
              参照に使えます。
            </p>
          </div>
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
            {overview.inference_engines.map((engine) => {
              const runtime = engineRuntime.find(
                (item) => item.name === engine.name,
              );
              const status = engineBadgeStatus(engine, runtime);
              return (
                <div
                  key={engine.name}
                  className="bg-slate-950/70 border border-slate-800 rounded-lg p-4 space-y-3"
                >
                  <div className="flex items-start justify-between gap-3">
                    <div>
                      <h3 className="text-sm font-semibold text-slate-100">
                        {engine.name}
                      </h3>
                      <p className="text-xs text-slate-500 mt-1">
                        {engine.engine_type} / {engine.deployment_mode}
                      </p>
                    </div>
                    <span
                      className={`text-[11px] px-2 py-1 rounded-full ${engineBadgeToneClass(status)}`}
                    >
                      {formatRuntimeStatus(status)}
                    </span>
                  </div>
                  <div className="text-xs text-slate-300 space-y-1">
                    <p>
                      接続先 URL:{" "}
                      <span className="text-slate-100 break-all">
                        {engine.base_url}
                      </span>
                    </p>
                    <p>
                      ホスト側ポート:{" "}
                      <span className="text-slate-100">
                        {engine.host_port ?? "未設定"}
                      </span>
                    </p>
                    <p>
                      公開 URL:{" "}
                      <span className="text-slate-100 break-all">
                        {engine.public_base_url ?? "未設定"}
                      </span>
                    </p>
                    <p>
                      モデル:{" "}
                      <span className="text-slate-100">
                        {engine.served_model_name ?? "未設定"}
                      </span>
                    </p>
                    <p>
                      Docker イメージ:{" "}
                      <span className="text-slate-100 break-all">
                        {engine.docker_image ?? "未設定"}
                      </span>
                    </p>
                    <p>
                      Compose ファイル:{" "}
                      <span className="text-slate-100 break-all">
                        {engine.compose_path ?? "未生成"}
                      </span>
                    </p>
                    {status === "running" && (
                      <p className="text-amber-300">
                        起動コマンドは成功しましたが、runtime probe
                        はまだ通過していません。
                        {runtime?.last_error ? ` ${runtime.last_error}` : ""}
                      </p>
                    )}
                    {runtime?.last_error &&
                      status !== "running" &&
                      status !== "available" && (
                        <p className="text-amber-300">
                          未通過理由: {runtime.last_error}
                        </p>
                      )}
                    {engine.deployment_error &&
                      ["failed", "stop_failed"].includes(status) && (
                        <p className="text-red-300">
                          エラー: {engine.deployment_error}
                        </p>
                      )}
                  </div>
                  <div className="flex items-center justify-end gap-2">
                    <button
                      type="button"
                      disabled={saving || !engine.served_model_name}
                      onClick={() => void handlePrefetchEngineModel(engine)}
                      className="px-3 py-2 text-xs rounded-lg bg-slate-800 hover:bg-slate-700 text-slate-100 disabled:opacity-50"
                    >
                      モデル取得
                    </button>
                    <button
                      type="button"
                      disabled={saving}
                      onClick={() => void handleStopEngine(engine)}
                      className="px-3 py-2 text-xs rounded-lg bg-slate-800 hover:bg-slate-700 text-slate-100 disabled:opacity-50"
                    >
                      停止
                    </button>
                    <button
                      type="button"
                      disabled={saving}
                      onClick={() => void handleDeployEngine(engine)}
                      className="px-3 py-2 text-xs rounded-lg bg-indigo-600 hover:bg-indigo-700 text-white disabled:opacity-50"
                    >
                      配備 / 再配備
                    </button>
                  </div>
                </div>
              );
            })}
          </div>
        </section>
      )}

      <section
        className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-3"
        data-testid="llm-switch-panel"
      >
        <h2 className="text-sm font-semibold text-slate-100">クイック切替</h2>
        <p className="text-xs text-slate-500">
          provider / backend / model を選んで、setup と atomic switch
          をまとめて実行します。
        </p>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
          <label className="text-xs text-slate-300 space-y-1">
            <span>プロバイダー</span>
            <select
              data-testid="llm-switch-provider"
              value={switchProvider}
              onChange={(event) =>
                setSwitchProvider(
                  event.target.value as LLMManagementProviderKind,
                )
              }
              className="w-full bg-slate-950 border border-slate-700 rounded px-3 py-2 text-sm"
            >
              {switchProviderOptions.map((providerName) => (
                <option key={providerName} value={providerName}>
                  {providerName}
                </option>
              ))}
            </select>
            <p className="text-[11px] text-slate-500">
              {switchProvider === "local"
                ? "local は provider 名です。実際の実装は中央の backend で vllm / sglang / tgi / ollama を選びます。"
                : "SaaS provider は backend なしで直接切り替えます。"}
            </p>
          </label>
          <label className="text-xs text-slate-300 space-y-1">
            <span>バックエンド</span>
            <select
              data-testid="llm-switch-backend"
              value={switchBackend}
              onChange={(event) =>
                setSwitchBackend(event.target.value as LLMBackendKind)
              }
              disabled={switchProvider !== "local"}
              className="w-full bg-slate-950 border border-slate-700 rounded px-3 py-2 text-sm"
            >
              <option value="none">なし</option>
              {catalog.backends
                .filter((backend) => switchBackendOptions.includes(backend.name))
                .map((backend) => (
                <option key={backend.name} value={backend.name}>
                  {backend.name}
                </option>
                ))}
            </select>
            <p className="text-[11px] text-slate-500">
              {switchProvider === "local"
                ? "local provider を選んだときだけ backend を指定します。"
                : "cloud provider では backend は使いません。"}
            </p>
          </label>
          <label className="text-xs text-slate-300 space-y-1">
            <span>モデル</span>
            <input
              data-testid="llm-switch-model"
              value={switchModel}
              onChange={(event) => setSwitchModel(event.target.value)}
              list="llm-model-hints"
              className="w-full bg-slate-950 border border-slate-700 rounded px-3 py-2 text-sm"
              placeholder="gpt-5.2 / claude-opus-4-6 / gemini-3.1-pro-preview"
            />
            <datalist id="llm-model-hints">
              {providerModelHints.map((model) => (
                <option key={model} value={model} />
              ))}
            </datalist>
            <p className="text-[11px] text-slate-500">
              {switchProvider === "local"
                ? "候補は provider と backend の組み合わせから自動補完します。"
                : "候補は provider に対応する catalog から自動補完します。"}
            </p>
            <div className="flex flex-wrap gap-2 pt-1">
              {providerModelHints.slice(0, 3).map((model) => (
                <button
                  key={model}
                  type="button"
                  onClick={() => setSwitchModel(model)}
                  className="px-2.5 py-1 rounded-full border border-slate-700 text-[11px] text-slate-200 hover:border-indigo-400 hover:text-indigo-200"
                >
                  公式候補: {model}
                </button>
              ))}
            </div>
          </label>
        </div>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
          <label className="text-xs text-slate-300 space-y-1">
            <span>role 一覧（カンマ区切り）</span>
            <input
              data-testid="llm-switch-roles"
              value={switchRoles}
              onChange={(event) => setSwitchRoles(event.target.value)}
              className="w-full bg-slate-950 border border-slate-700 rounded px-3 py-2 text-sm"
            />
          </label>
          <label className="text-xs text-slate-300 space-y-1">
            <span>実行モード</span>
            <select
              value={autoSetup ? "setup_switch" : "switch_only"}
              onChange={(event) =>
                setAutoSetup(event.target.value === "setup_switch")
              }
              className="w-full bg-slate-950 border border-slate-700 rounded px-3 py-2 text-sm"
            >
              <option value="setup_switch">セットアップして切替</option>
              <option value="switch_only">切替のみ</option>
            </select>
            <p className="text-[11px] text-slate-500">
              `セットアップして切替` は依存確認、ローカル backend
              起動、ヘルス確認まで実施してから切り替えます。`切替のみ` は
              Gateway の向き先だけを更新し、ローカル engine には触れません。
            </p>
          </label>
        </div>
        <div className="grid grid-cols-2 md:grid-cols-4 gap-3 text-xs text-slate-300">
          <label className="inline-flex items-center gap-2">
            <input
              type="checkbox"
              checked={autoInstall}
              onChange={(event) => setAutoInstall(event.target.checked)}
              disabled={!autoSetup}
            />
            自動インストール
          </label>
          <label className="inline-flex items-center gap-2">
            <input
              type="checkbox"
              checked={autoStart}
              onChange={(event) => setAutoStart(event.target.checked)}
              disabled={!autoSetup}
            />
            自動起動
          </label>
          <label className="inline-flex items-center gap-2">
            <input
              type="checkbox"
              checked={healthCheck}
              onChange={(event) => setHealthCheck(event.target.checked)}
              disabled={!autoSetup}
            />
            ヘルスチェック
          </label>
          <label className="inline-flex items-center gap-2">
            <input
              type="checkbox"
              checked={validateRuntime}
              onChange={(event) => setValidateRuntime(event.target.checked)}
            />
            実行時検証
          </label>
        </div>
        <div className="flex justify-end">
          <button
            disabled={saving || !switchModel.trim()}
            onClick={() => void handleSetupAndSwitch()}
            data-testid="llm-setup-switch-button"
            className="px-4 py-2 bg-indigo-600 hover:bg-indigo-700 disabled:opacity-50 disabled:cursor-not-allowed text-white text-sm font-medium rounded-lg transition-colors"
          >
            {saving ? "実行中..." : autoSetup ? "セットアップして切替" : "切替"}
          </button>
        </div>
      </section>

      <section className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-4">
        <div>
          <h2 className="text-sm font-semibold text-slate-100">
            app_config.json の契約例
          </h2>
          <p className="text-xs text-slate-500 mt-1">
            app は Platform catalog に登録済み provider / model_id
            だけを参照します。以下の `contracts.llm` を各 app の
            `app_config.json` に貼り付けて調整してください。音声 provider /
            model の注記は 2026-03-10 時点の公式ドキュメントで照合済みです。
          </p>
        </div>
        <div className="grid grid-cols-1 xl:grid-cols-2 gap-4">
          {contractExamples.map((example) => (
            <details
              key={example.id}
              className="bg-slate-950/70 border border-slate-800 rounded-lg p-4 space-y-3"
            >
              <summary className="cursor-pointer list-none">
                <div className="flex items-start justify-between gap-3">
                  <div>
                    <h3 className="text-sm font-semibold text-slate-100">
                      {example.title}
                    </h3>
                    <p className="text-xs text-slate-500 mt-1">
                      {example.description}
                    </p>
                  </div>
                  <span className="shrink-0 px-2 py-1 rounded-full bg-slate-800 text-[11px] text-slate-200">
                    {example.projectLabel}
                  </span>
                </div>
              </summary>
              <div className="pt-3 space-y-3">
                <div className="space-y-1">
                  {example.notes.map((note) => (
                    <p key={note} className="text-[11px] text-slate-400">
                      {note}
                    </p>
                  ))}
                </div>
                <pre className="bg-slate-950 border border-slate-800 rounded-lg p-3 text-[11px] text-slate-200 overflow-x-auto whitespace-pre-wrap">
                  {example.value}
                </pre>
              </div>
            </details>
          ))}
        </div>
      </section>

      {(lastPreflight || lastSwitch || lastEngineAction || diagnostics) && (
        <section className="bg-slate-900/50 border border-slate-800 rounded-xl p-5 space-y-3">
          <h2 className="text-sm font-semibold text-slate-100">直近の結果</h2>
          {diagnostics && (
            <div className="text-xs text-slate-300">
              <p>ルート数: {diagnostics.route_count}</p>
              <p>設定バージョン: {diagnostics.config_version ?? "N/A"}</p>
              <p>LLM ルート: {diagnostics.has_llm_routes ? "あり" : "なし"}</p>
            </div>
          )}
          {lastPreflight && (
            <div>
              <p className="text-xs text-slate-300 mb-2">
                事前確認: {lastPreflight.summary}
              </p>
              <div className="space-y-1">
                {lastPreflight.steps.map((step, index) => (
                  <div
                    key={`${step.target}-${step.phase}-${index}`}
                    className="text-xs text-slate-400"
                  >
                    [{step.status}] {step.category}.{step.phase} ({step.target})
                    - {step.message}
                  </div>
                ))}
              </div>
            </div>
          )}
          {lastSwitch && (
            <div>
              <p className="text-xs text-slate-300">
                切替結果: {lastSwitch.message}
              </p>
              <p className="text-xs text-slate-400">
                適用 alias: {lastSwitch.applied_alias ?? "N/A"}
              </p>
              <p className="text-xs text-slate-400">
                ロールバック: {lastSwitch.rolled_back ? "あり" : "なし"}
              </p>
              {lastSwitch.runtime_check.provider_status && (
                <p className="text-xs text-slate-400">
                  Provider 検証:{" "}
                  {formatRuntimeStatus(lastSwitch.runtime_check.provider_status)}
                </p>
              )}
              {lastSwitch.runtime_check.backend_status && (
                <p className="text-xs text-slate-400">
                  Backend 検証:{" "}
                  {formatRuntimeStatus(lastSwitch.runtime_check.backend_status)}
                </p>
              )}
              {lastSwitch.runtime_check.model_status && (
                <p className="text-xs text-slate-400">
                  Model 検証:{" "}
                  {formatRuntimeStatus(lastSwitch.runtime_check.model_status)}
                </p>
              )}
              {lastSwitch.runtime_check.errors.length > 0 && (
                <ul className="mt-2 space-y-1 text-xs text-rose-300">
                  {lastSwitch.runtime_check.errors.map((item) => (
                    <li key={item}>{item}</li>
                  ))}
                </ul>
              )}
            </div>
          )}
          {lastEngineAction && (
            <div>
              <p className="text-xs text-slate-300">
                {lastEngineActionLabel}: {lastEngineAction.message}
              </p>
              <p className="text-xs text-slate-400">
                対象: {lastEngineAction.engine.name}
              </p>
              <p className="text-xs text-slate-400">
                状態:{" "}
                {formatRuntimeStatus(lastEngineAction.engine.deployment_status)}
              </p>
              {lastEngineAction.command &&
                (lastEngineAction.command.stdout ||
                  lastEngineAction.command.stderr ||
                  lastEngineAction.command.error) && (
                  <pre
                    className={`mt-2 whitespace-pre-wrap rounded-lg border p-3 text-[11px] ${
                      lastEngineAction.success
                        ? "border-slate-700 bg-slate-950 text-slate-300"
                        : "border-rose-900/60 bg-rose-950/40 text-rose-200"
                    }`}
                  >
                    {lastEngineAction.command.stdout ||
                      lastEngineAction.command.stderr ||
                      lastEngineAction.command.error}
                  </pre>
                )}
            </div>
          )}
        </section>
      )}

      <section className="bg-slate-900/50 border border-slate-800 rounded-xl p-5">
        <h2 className="text-sm font-semibold text-slate-100">
          {t("llm_mgmt.runtime")}
        </h2>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-3 mt-3">
          <div className="bg-slate-950/70 border border-slate-800 rounded-lg p-3">
            <p className="text-[10px] uppercase tracking-wider text-slate-500">
              {t("llm_mgmt.providers_available")}
            </p>
            <p className="text-lg text-slate-100 font-semibold">
              {runtimeSummary.providers}
            </p>
          </div>
          <div className="bg-slate-950/70 border border-slate-800 rounded-lg p-3">
            <p className="text-[10px] uppercase tracking-wider text-slate-500">
              {t("llm_mgmt.engines_available")}
            </p>
            <p className="text-lg text-slate-100 font-semibold">
              {runtimeSummary.engines}
            </p>
          </div>
          <div className="bg-slate-950/70 border border-slate-800 rounded-lg p-3">
            <p className="text-[10px] uppercase tracking-wider text-slate-500">
              {t("llm_mgmt.total_cost")}
            </p>
            <p className="text-lg text-slate-100 font-semibold">
              ${runtimeSummary.totalCost.toFixed(4)}
            </p>
            {runtimeSummary.budgetExceeded && (
              <p className="text-xs text-amber-300 mt-1">
                {t("llm_mgmt.budget_exceeded")}
              </p>
            )}
          </div>
        </div>
      </section>

      {advancedMode && (
        <>
          {Section({
            title: t("llm_mgmt.section_providers"),
            description: t("llm_mgmt.section_providers_desc"),
            value: providersDraft,
            onChange: setProvidersDraft,
            onSave: saveProviders,
            helper:
              t("llm_mgmt.provider_runtime_hint") +
              ` ${prettify(providerRuntime)}`,
            examples: providerExamples,
            onFormat: () =>
              formatDraft(
                "providers",
                t("llm_mgmt.section_providers"),
                providersDraft,
              ),
            onReset: () => resetDraft("providers"),
            testId: "providers",
          })}

          {Section({
            title: t("llm_mgmt.section_engines"),
            description: t("llm_mgmt.section_engines_desc"),
            value: enginesDraft,
            onChange: setEnginesDraft,
            onSave: saveEngines,
            helper:
              t("llm_mgmt.engine_runtime_hint") + ` ${prettify(engineRuntime)}`,
            examples: engineExamples,
            onFormat: () =>
              formatDraft(
                "engines",
                t("llm_mgmt.section_engines"),
                enginesDraft,
              ),
            onReset: () => resetDraft("engines"),
            testId: "engines",
          })}

          {Section({
            title: t("llm_mgmt.section_models"),
            description: t("llm_mgmt.section_models_desc"),
            value: modelsDraft,
            onChange: setModelsDraft,
            onSave: saveModels,
            examples: modelExamples,
            onFormat: () =>
              formatDraft("models", t("llm_mgmt.section_models"), modelsDraft),
            onReset: () => resetDraft("models"),
            testId: "models",
          })}

          {Section({
            title: t("llm_mgmt.section_registry"),
            description: t("llm_mgmt.section_registry_desc"),
            value: registryDraft,
            onChange: setRegistryDraft,
            onSave: saveRegistry,
            examples: registryExamples,
            onFormat: () =>
              formatDraft(
                "registry",
                t("llm_mgmt.section_registry"),
                registryDraft,
              ),
            onReset: () => resetDraft("registry"),
            testId: "registry",
          })}

          {Section({
            title: t("llm_mgmt.section_routing"),
            description: t("llm_mgmt.section_routing_desc"),
            value: routingDraft,
            onChange: setRoutingDraft,
            onSave: saveRouting,
            examples: routingExamples,
            onFormat: () =>
              formatDraft(
                "routing",
                t("llm_mgmt.section_routing"),
                routingDraft,
              ),
            onReset: () => resetDraft("routing"),
            testId: "routing",
          })}
        </>
      )}
    </div>
  );
}

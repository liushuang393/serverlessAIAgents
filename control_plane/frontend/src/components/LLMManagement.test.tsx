import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { act } from "react";
import { createRoot, type Root } from "react-dom/client";
import { I18nProvider } from "@/i18n";
import { LLMManagement } from "@/components/LLMManagement";

const apiMocks = vi.hoisted(() => ({
  deleteLLMProviderSecret: vi.fn(),
  deployLLMEngine: vi.fn(),
  fetchLLMCatalog: vi.fn(),
  fetchLLMDiagnostics: vi.fn(),
  fetchLLMEngineStatus: vi.fn(),
  fetchLLMManagementOverview: vi.fn(),
  fetchOpenAPIPaths: vi.fn(),
  reloadLLMManagementConfig: vi.fn(),
  setupAndSwitchLLM: vi.fn(),
  switchLLM: vi.fn(),
  updateLLMInferenceEngines: vi.fn(),
  updateLLMModels: vi.fn(),
  updateLLMProviderSecret: vi.fn(),
  updateLLMProviders: vi.fn(),
  updateLLMRegistry: vi.fn(),
  updateLLMRoutingPolicy: vi.fn(),
  stopLLMEngine: vi.fn(),
}));

vi.mock("@/api/client", () => apiMocks);

const flush = async (): Promise<void> => {
  await Promise.resolve();
  await Promise.resolve();
};

describe("LLMManagement component", () => {
  let container: HTMLDivElement;
  let root: Root;

  beforeEach(() => {
    // React 18 testing signal
    (
      globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }
    ).IS_REACT_ACT_ENVIRONMENT = true;
    container = document.createElement("div");
    document.body.appendChild(container);
    root = createRoot(container);

    apiMocks.fetchLLMManagementOverview.mockResolvedValue({
      gateway: {
        default_role: "reasoning",
        request_timeout_seconds: 120,
        max_retries: 2,
      },
      providers: [
        {
          name: "openai",
          api_base: "https://api.openai.com/v1",
          api_key_env: "OPENAI_API_KEY",
          models: [],
          enabled: true,
          secret_status: {
            configured: false,
            masked: null,
            source: "unavailable",
            available: false,
            last_error: null,
          },
        },
      ],
      providers_runtime: [],
      inference_engines: [],
      models: [],
      registry: { reasoning: "reasoning_openai" },
      routing_policy: {
        priority: "latency",
        fallback_chain: {},
        load_balance_strategy: "round_robin",
        cost_budget: null,
      },
      cost_summary: {
        total_cost_usd: 0,
        details: [],
        cost_budget: null,
        budget_exceeded: false,
      },
      config_version: "version-a",
    });
    apiMocks.fetchLLMEngineStatus.mockResolvedValue({ engine_status: [] });
    apiMocks.fetchLLMCatalog.mockResolvedValue({
      providers: [
        {
          name: "openai",
          canonical_name: "openai",
          aliases: [],
          requires_api_key: true,
          default_api_key_env: "OPENAI_API_KEY",
          default_api_base: "https://api.openai.com/v1",
          recommended_models: ["gpt-5-mini"],
          install_recipes: [],
        },
      ],
      backends: [],
      models: [
        {
          alias: "platform_text_default",
          model_id: "platform_text_default",
          provider: "openai",
          model: "gpt-5-mini",
          model_type: "text",
          capabilities: ["reasoning"],
          context_window: 128000,
          recommended_for: ["reasoning"],
        },
      ],
      generated_at: "2026-03-05T00:00:00",
    });
    apiMocks.fetchOpenAPIPaths.mockResolvedValue([
      "/api/studios/framework/llm/overview",
    ]);
    apiMocks.fetchLLMDiagnostics.mockResolvedValue({
      has_llm_routes: true,
      route_count: 10,
      config_path: "/tmp/.bizcore/llm_gateway.yaml",
      config_exists: true,
      config_version: "version-a",
      last_preflight: null,
      hints: ["healthy"],
      server_time: "2026-03-05T00:00:00",
    });
  });

  afterEach(async () => {
    await act(async () => {
      root.unmount();
    });
    container.remove();
    vi.clearAllMocks();
  });

  it("renders quick switch panel and toggles advanced mode", async () => {
    await act(async () => {
      root.render(
        <I18nProvider>
          <LLMManagement />
        </I18nProvider>,
      );
      await flush();
    });

    expect(
      container.querySelector('[data-testid="llm-switch-panel"]'),
    ).not.toBeNull();
    expect(container.querySelectorAll("textarea").length).toBe(0);

    const toggle = container.querySelector(
      '[data-testid="llm-advanced-toggle"]',
    );
    expect(toggle).not.toBeNull();
    await act(async () => {
      toggle?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
      await flush();
    });

    expect(container.querySelectorAll("textarea").length).toBeGreaterThan(0);
  });

  it("loads an engine example into the advanced editor", async () => {
    await act(async () => {
      root.render(
        <I18nProvider>
          <LLMManagement />
        </I18nProvider>,
      );
      await flush();
    });

    const toggle = container.querySelector(
      '[data-testid="llm-advanced-toggle"]',
    );
    expect(toggle).not.toBeNull();
    await act(async () => {
      toggle?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
      await flush();
    });

    const exampleButton = container.querySelector(
      '[data-testid="llm-example-engines-vllm-docker"]',
    );
    expect(exampleButton).not.toBeNull();
    await act(async () => {
      exampleButton?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
      await flush();
    });

    const editor = container.querySelector<HTMLTextAreaElement>(
      '[data-testid="engines-editor"]',
    );
    expect(editor).not.toBeNull();
    expect(editor?.value).toContain('"name": "vllm"');
    expect(editor?.value).toContain("vllm/vllm-openai:v0.8.5");
  });

  it("shows route-missing diagnostics on overview 404", async () => {
    apiMocks.fetchLLMManagementOverview.mockRejectedValueOnce({
      isAxiosError: true,
      response: { status: 404 },
      message: "Request failed with status code 404",
    });
    apiMocks.fetchLLMCatalog.mockRejectedValueOnce(new Error("404"));
    apiMocks.fetchOpenAPIPaths.mockResolvedValueOnce(["/health"]);
    apiMocks.fetchLLMDiagnostics.mockResolvedValueOnce({
      has_llm_routes: false,
      route_count: 0,
      config_path: "/tmp/.bizcore/llm_gateway.yaml",
      config_exists: true,
      config_version: "version-a",
      last_preflight: null,
      hints: [
        "LLM 管理ルートが見つかりません。Platform backend を最新コードで再起動してください。",
      ],
      server_time: "2026-03-05T00:00:00",
    });

    await act(async () => {
      root.render(
        <I18nProvider>
          <LLMManagement />
        </I18nProvider>,
      );
      await flush();
    });

    const diagnostic = container.querySelector(
      '[data-testid="llm-route-missing-diagnostic"]',
    );
    expect(diagnostic).not.toBeNull();
    expect(diagnostic?.textContent).toContain("OpenAPI");
  });

  it("renders the voice contract example with official provider notes", async () => {
    await act(async () => {
      root.render(
        <I18nProvider>
          <LLMManagement />
        </I18nProvider>,
      );
      await flush();
    });

    expect(container.textContent).toContain(
      "音声窓口 / コールセンターアプリ例",
    );
    expect(container.textContent).toContain("gpt-4o-transcribe");
    expect(container.textContent).toContain("gemini-2.5-flash-preview-tts");
    expect(container.textContent).toContain("claude-opus-4-6");
  });

  it("does not render the provider review-required state", async () => {
    apiMocks.fetchLLMManagementOverview.mockResolvedValueOnce({
      gateway: {
        default_role: "reasoning",
        request_timeout_seconds: 120,
        max_retries: 2,
      },
      providers: [
        {
          name: "custom",
          api_base: "http://127.0.0.1:19000/v1",
          api_key_env: null,
          models: [],
          enabled: true,
          secret_status: {
            configured: false,
            masked: null,
            source: "unavailable",
            available: false,
            last_error: null,
          },
        },
      ],
      providers_runtime: [
        {
          name: "custom",
          status: "unavailable",
          api_key_env: null,
          source: "probe:v1_models",
          masked: null,
          last_error: "v1_models:status=503",
        },
      ],
      inference_engines: [],
      models: [],
      registry: { reasoning: "reasoning_openai" },
      routing_policy: {
        priority: "latency",
        fallback_chain: {},
        load_balance_strategy: "round_robin",
        cost_budget: null,
      },
      cost_summary: {
        total_cost_usd: 0,
        details: [],
        cost_budget: null,
        budget_exceeded: false,
      },
      config_version: "version-a",
    });

    await act(async () => {
      root.render(
        <I18nProvider>
          <LLMManagement />
        </I18nProvider>,
      );
      await flush();
    });

    expect(container.textContent).not.toContain("要確認");
    expect(container.textContent).toContain("利用不可");
  });

  it("keeps successful engine deploy output in the result area without card errors", async () => {
    const overview = {
      gateway: {
        default_role: "reasoning",
        request_timeout_seconds: 120,
        max_retries: 2,
      },
      providers: [
        {
          name: "local",
          api_base: "http://127.0.0.1:18001/v1",
          api_key_env: null,
          models: ["Qwen/Qwen2.5-0.5B-Instruct"],
          enabled: true,
          secret_status: {
            configured: false,
            masked: null,
            source: "unavailable",
            available: false,
            last_error: null,
          },
        },
      ],
      providers_runtime: [
        {
          name: "local",
          status: "unavailable",
          api_key_env: null,
          source: "engine:vllm",
          masked: null,
          last_error: "linked_engine_unhealthy:vllm:health status=503",
        },
      ],
      inference_engines: [
        {
          name: "vllm",
          engine_type: "vllm",
          base_url: "http://127.0.0.1:18001",
          health_path: "/health",
          metrics_path: "/metrics",
          model_list_path: "/v1/models",
          enabled: true,
          deployment_mode: "docker",
          docker_image: "vllm/vllm-openai:v0.8.5",
          served_model_name: "Qwen/Qwen2.5-0.5B-Instruct",
          container_name: "llm-vllm",
          host_port: 18001,
          public_base_url: "http://127.0.0.1:18001/v1",
          gpu_enabled: false,
          gpu_devices: [],
          gpu_count: null,
          extra_env: {},
          deployment_status: "running",
          deployment_error: null,
          compose_path: "/tmp/docker-compose.yml",
        },
      ],
      models: [
        {
          alias: "local_vllm_default",
          model_id: "local_vllm_default",
          provider: "local",
          model: "Qwen/Qwen2.5-0.5B-Instruct",
          model_type: "text",
          api_base: "http://127.0.0.1:18001/v1",
          api_key_env: null,
          engine: "vllm",
          enabled: true,
          modalities: ["text"],
          quality_score: 0.9,
          avg_latency_ms: 800,
          cost: { input_per_1k: 0, output_per_1k: 0 },
        },
      ],
      registry: { reasoning: "local_vllm_default" },
      routing_policy: {
        priority: "latency",
        fallback_chain: {},
        load_balance_strategy: "round_robin",
        cost_budget: null,
      },
      cost_summary: {
        total_cost_usd: 0,
        details: [],
        cost_budget: null,
        budget_exceeded: false,
      },
      config_version: "version-a",
    };
    apiMocks.fetchLLMManagementOverview.mockResolvedValue(overview);
    apiMocks.fetchLLMEngineStatus.mockResolvedValue({
      engine_status: [
        {
          name: "vllm",
          engine_type: "vllm",
          status: "unavailable",
          latency_ms: null,
          gpu_usage: null,
          loaded_models: [],
          last_error: "health status=503",
        },
      ],
    });
    apiMocks.deployLLMEngine.mockResolvedValue({
      success: true,
      message: "engine の配備が完了しました。",
      engine: overview.inference_engines[0],
      command: {
        command: ["docker", "compose", "up", "-d"],
        cwd: "/tmp",
        return_code: 0,
        stdout: "Created container llm-vllm",
        stderr: "",
        error: null,
        allowed: true,
        timed_out: false,
      },
    });

    await act(async () => {
      root.render(
        <I18nProvider>
          <LLMManagement />
        </I18nProvider>,
      );
      await flush();
    });

    const deployButton = Array.from(container.querySelectorAll("button")).find(
      (button) => button.textContent === "配備 / 再配備",
    );
    expect(deployButton).not.toBeUndefined();

    await act(async () => {
      deployButton?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
      await flush();
    });

    expect(container.textContent).toContain(
      "配備結果: engine の配備が完了しました。",
    );
    expect(container.textContent).toContain("Created container llm-vllm");
    expect(container.textContent).toContain("稼働中");
    expect(container.textContent).not.toContain("エラー:");
  });
});

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { I18nProvider } from '@/i18n';
import { LLMManagement } from '@/components/LLMManagement';

const apiMocks = vi.hoisted(() => ({
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
  updateLLMProviders: vi.fn(),
  updateLLMRegistry: vi.fn(),
  updateLLMRoutingPolicy: vi.fn(),
}));

vi.mock('@/api/client', () => apiMocks);

const flush = async (): Promise<void> => {
  await Promise.resolve();
  await Promise.resolve();
};

describe('LLMManagement component', () => {
  let container: HTMLDivElement;
  let root: Root;

  beforeEach(() => {
    // React 18 testing signal
    (globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);

    apiMocks.fetchLLMManagementOverview.mockResolvedValue({
      gateway: { default_role: 'reasoning', request_timeout_seconds: 120, max_retries: 2 },
      providers: [{ name: 'openai', api_base: 'https://api.openai.com/v1', api_key_env: 'OPENAI_API_KEY', models: [], enabled: true }],
      providers_runtime: [],
      inference_engines: [],
      models: [],
      registry: { reasoning: 'reasoning_openai' },
      routing_policy: {
        priority: 'latency',
        fallback_chain: {},
        load_balance_strategy: 'round_robin',
        cost_budget: null,
      },
      cost_summary: {
        total_cost_usd: 0,
        details: [],
        cost_budget: null,
        budget_exceeded: false,
      },
      config_version: 'version-a',
    });
    apiMocks.fetchLLMEngineStatus.mockResolvedValue({ engine_status: [] });
    apiMocks.fetchLLMCatalog.mockResolvedValue({
      providers: [{ name: 'openai', canonical_name: 'openai', aliases: [], requires_api_key: true, default_api_key_env: 'OPENAI_API_KEY', default_api_base: 'https://api.openai.com/v1', recommended_models: ['gpt-4o-mini'], install_recipes: [] }],
      backends: [],
      models: [{ alias: 'gpt_4o_mini', provider: 'openai', model: 'gpt-4o-mini', capabilities: ['reasoning'], context_window: 128000, recommended_for: ['reasoning'] }],
      generated_at: '2026-03-05T00:00:00',
    });
    apiMocks.fetchOpenAPIPaths.mockResolvedValue(['/api/studios/framework/llm/overview']);
    apiMocks.fetchLLMDiagnostics.mockResolvedValue({
      has_llm_routes: true,
      route_count: 10,
      config_path: '/tmp/.agentflow/llm_gateway.yaml',
      config_exists: true,
      config_version: 'version-a',
      last_preflight: null,
      hints: ['healthy'],
      server_time: '2026-03-05T00:00:00',
    });
  });

  afterEach(async () => {
    await act(async () => {
      root.unmount();
    });
    container.remove();
    vi.clearAllMocks();
  });

  it('renders quick switch panel and toggles advanced mode', async () => {
    await act(async () => {
      root.render(
        <I18nProvider>
          <LLMManagement />
        </I18nProvider>,
      );
      await flush();
    });

    expect(container.querySelector('[data-testid="llm-switch-panel"]')).not.toBeNull();
    expect(container.querySelectorAll('textarea').length).toBe(0);

    const toggle = container.querySelector('[data-testid="llm-advanced-toggle"]');
    expect(toggle).not.toBeNull();
    await act(async () => {
      toggle?.dispatchEvent(new MouseEvent('click', { bubbles: true }));
      await flush();
    });

    expect(container.querySelectorAll('textarea').length).toBeGreaterThan(0);
  });

  it('shows route-missing diagnostics on overview 404', async () => {
    apiMocks.fetchLLMManagementOverview.mockRejectedValueOnce({
      isAxiosError: true,
      response: { status: 404 },
      message: 'Request failed with status code 404',
    });
    apiMocks.fetchLLMCatalog.mockRejectedValueOnce(new Error('404'));
    apiMocks.fetchOpenAPIPaths.mockResolvedValueOnce(['/health']);
    apiMocks.fetchLLMDiagnostics.mockResolvedValueOnce({
      has_llm_routes: false,
      route_count: 0,
      config_path: '/tmp/.agentflow/llm_gateway.yaml',
      config_exists: true,
      config_version: 'version-a',
      last_preflight: null,
      hints: ['LLM routes are missing. Restart platform backend with latest code.'],
      server_time: '2026-03-05T00:00:00',
    });

    await act(async () => {
      root.render(
        <I18nProvider>
          <LLMManagement />
        </I18nProvider>,
      );
      await flush();
    });

    const diagnostic = container.querySelector('[data-testid="llm-route-missing-diagnostic"]');
    expect(diagnostic).not.toBeNull();
    expect(diagnostic?.textContent).toContain('OpenAPI');
  });
});

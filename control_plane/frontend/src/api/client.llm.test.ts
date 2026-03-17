import { beforeEach, describe, expect, it, vi } from 'vitest';

const axiosMocks = vi.hoisted(() => ({
  getMock: vi.fn(),
  postMock: vi.fn(),
  putMock: vi.fn(),
  patchMock: vi.fn(),
  deleteMock: vi.fn(),
}));

vi.mock('axios', () => {
  const create = vi.fn(() => ({
    get: axiosMocks.getMock,
    post: axiosMocks.postMock,
    put: axiosMocks.putMock,
    patch: axiosMocks.patchMock,
    delete: axiosMocks.deleteMock,
  }));
  return {
    default: {
      create,
      isAxiosError: (value: unknown) => Boolean((value as { isAxiosError?: boolean })?.isAxiosError),
    },
    create,
    isAxiosError: (value: unknown) => Boolean((value as { isAxiosError?: boolean })?.isAxiosError),
  };
});

import { fetchLLMCatalog, fetchOpenAPIPaths, setupAndSwitchLLM } from '@/api/client';

describe('LLM management api client', () => {
  beforeEach(() => {
    axiosMocks.getMock.mockReset();
    axiosMocks.postMock.mockReset();
    axiosMocks.putMock.mockReset();
    axiosMocks.patchMock.mockReset();
    axiosMocks.deleteMock.mockReset();
  });

  it('calls catalog endpoint', async () => {
    axiosMocks.getMock.mockResolvedValueOnce({
      data: {
        providers: [],
        backends: [],
        models: [],
        generated_at: '2026-03-05T00:00:00',
      },
    });

    const data = await fetchLLMCatalog();

    expect(axiosMocks.getMock).toHaveBeenCalledWith('/studios/framework/llm/catalog');
    expect(data.providers).toEqual([]);
  });

  it('calls setup-and-switch endpoint', async () => {
    axiosMocks.postMock.mockResolvedValueOnce({
      data: {
        preflight: {
          status: 'success',
          started_at: '2026-03-05T00:00:00',
          completed_at: '2026-03-05T00:00:01',
          request: {
            providers: ['openai'],
            backends: [],
            auto_install: true,
            auto_start: true,
            health_check: true,
            dry_run: false,
          },
          steps: [],
          summary: 'ok',
        },
        switch: {
          success: true,
          rolled_back: false,
          config_version: 'abc',
          applied_alias: 'reasoning_openai_gpt_4o_mini',
          registry: { reasoning: 'reasoning_openai_gpt_4o_mini' },
          diffs: [],
          runtime_check: { provider_status: 'available', backend_status: null, errors: [] },
          message: 'ok',
        },
        success: true,
        message: 'ok',
      },
    });

    const payload = await setupAndSwitchLLM({
      preflight: {
        providers: ['openai'],
        backends: [],
        auto_install: true,
        auto_start: true,
        health_check: true,
        dry_run: false,
      },
      switch: {
        provider: 'openai',
        model: 'gpt-4o-mini',
        backend: 'none',
        roles: ['reasoning'],
        model_alias: null,
        auto_enable_provider: true,
        update_fallback_chain: true,
        validate_runtime: true,
      },
    });

    expect(axiosMocks.postMock).toHaveBeenCalledWith('/studios/framework/llm/setup-and-switch', expect.any(Object));
    expect(payload.success).toBe(true);
  });

  it('reads openapi paths', async () => {
    axiosMocks.getMock.mockResolvedValueOnce({
      data: {
        paths: {
          '/health': {},
          '/api/studios/framework/llm/overview': {},
        },
      },
    });

    const paths = await fetchOpenAPIPaths();

    expect(axiosMocks.getMock).toHaveBeenCalledWith('/openapi.json');
    expect(paths).toContain('/api/studios/framework/llm/overview');
  });
});

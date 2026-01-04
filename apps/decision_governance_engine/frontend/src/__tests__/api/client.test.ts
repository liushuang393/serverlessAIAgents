/**
 * DecisionApiClient テスト.
 * 
 * 目的: API クライアントのロジック検証
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { DecisionApiClient, DecisionApiError } from '../../api/client';

describe('DecisionApiClient', () => {
  let client: DecisionApiClient;

  beforeEach(() => {
    client = new DecisionApiClient('http://localhost:8000');
    vi.clearAllMocks();
  });

  afterEach(() => {
    client.cancelAllRequests();
  });

  describe('processDecision', () => {
    it('成功時にレスポンスを返す', async () => {
      const mockResponse = {
        status: 'success',
        report_id: 'test-123',
        data: { dao: {}, fa: {}, shu: {}, qi: {}, review: {} },
      };

      globalThis.fetch = vi.fn().mockResolvedValue({
        ok: true,
        json: () => Promise.resolve(mockResponse),
      });

      const result = await client.processDecision({
        question: 'テスト質問です。これは十分な長さです。',
        technical_constraints: [],
        regulatory_constraints: [],
        human_resources: [],
      });

      expect(result).toEqual(mockResponse);
    });

    it('400エラー時に適切なエラーメッセージを返す', async () => {
      globalThis.fetch = vi.fn().mockResolvedValue({
        ok: false,
        status: 400,
        statusText: 'Bad Request',
      });

      await expect(
        client.processDecision({
          question: 'テスト',
          technical_constraints: [],
          regulatory_constraints: [],
          human_resources: [],
        })
      ).rejects.toThrow('リクエストが不正です');
    });

    it('500エラー時にリトライする', async () => {
      const mockResponse = { status: 'success' };
      let callCount = 0;

      globalThis.fetch = vi.fn().mockImplementation(() => {
        callCount++;
        if (callCount < 3) {
          return Promise.resolve({
            ok: false,
            status: 500,
            statusText: 'Internal Server Error',
          });
        }
        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve(mockResponse),
        });
      });

      const result = await client.processDecision({
        question: 'テスト質問です。',
        technical_constraints: [],
        regulatory_constraints: [],
        human_resources: [],
      });

      expect(callCount).toBe(3);
      expect(result).toEqual(mockResponse);
    });
  });

  describe('cancelRequest', () => {
    it('リクエストをキャンセルできる', async () => {
      globalThis.fetch = vi.fn().mockImplementation(
        () => new Promise((_, reject) => {
          setTimeout(() => reject(new Error('AbortError')), 100);
        })
      );

      const promise = client.processDecision(
        {
          question: 'テスト',
          technical_constraints: [],
          regulatory_constraints: [],
          human_resources: [],
        },
        'test-request-id'
      );

      client.cancelRequest('test-request-id');

      await expect(promise).rejects.toThrow();
    });
  });

  describe('DecisionApiError', () => {
    it('fromResponse で適切なエラーを生成する', () => {
      const mockResponse = {
        status: 429,
        statusText: 'Too Many Requests',
      } as Response;

      const error = DecisionApiError.fromResponse(mockResponse);

      expect(error.message).toBe('リクエストが多すぎます。しばらく待ってから再試行してください');
      expect(error.statusCode).toBe(429);
      expect(error.isRetryable).toBe(true);
    });

    it('5xx エラーはリトライ可能', () => {
      const error = DecisionApiError.fromResponse({
        status: 502,
        statusText: 'Bad Gateway',
      } as Response);

      expect(error.isRetryable).toBe(true);
    });

    it('4xx エラーはリトライ不可（429除く）', () => {
      const error = DecisionApiError.fromResponse({
        status: 404,
        statusText: 'Not Found',
      } as Response);

      expect(error.isRetryable).toBe(false);
    });
  });
});


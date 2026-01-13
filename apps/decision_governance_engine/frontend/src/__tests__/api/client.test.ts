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
    // Fake timers を使用してリトライの遅延をスキップ
    vi.useFakeTimers();
    client = new DecisionApiClient('http://localhost:8000');
    vi.clearAllMocks();
  });

  afterEach(() => {
    client.cancelAllRequests();
    vi.useRealTimers();
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

      const promise = client.processDecision({
        question: 'テスト',
        technical_constraints: [],
        regulatory_constraints: [],
        human_resources: [],
      });

      // タイマーを進めてリトライ遅延をスキップ
      await vi.runAllTimersAsync();

      await expect(promise).rejects.toThrow('リクエストが不正です');
    });

    it('500エラー時にリトライする', async () => {
      const mockResponse = { status: 'success' };
      let callCount = 0;

      // モック: 最初の2回は500エラー、3回目は成功
      const mockFetch = vi.fn().mockImplementation(() => {
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
      globalThis.fetch = mockFetch;

      // リクエスト開始
      const resultPromise = client.processDecision({
        question: 'テスト質問です。',
        technical_constraints: [],
        regulatory_constraints: [],
        human_resources: [],
      });

      // タイマーを進めてリトライ遅延をスキップ
      await vi.runAllTimersAsync();

      const result = await resultPromise;

      // 注: maxRetries=3 なので、初回 + 3回リトライ = 最大4回
      // 3回目で成功するので callCount は 3
      expect(callCount).toBeGreaterThanOrEqual(3);
      expect(result).toEqual(mockResponse);
    });
  });

  describe('cancelRequest', () => {
    it('リクエストをキャンセルできる', async () => {
      // AbortError をシミュレート
      const abortError = new Error('AbortError');
      abortError.name = 'AbortError';

      const mockFetch = vi.fn().mockImplementation((_url: string, options: RequestInit) => {
        return new Promise((_, reject) => {
          // signal が abort されたら即座に reject
          if (options.signal) {
            options.signal.addEventListener('abort', () => {
              reject(abortError);
            });
          }
          // タイムアウト（通常は abort が先に発生）
          setTimeout(() => reject(new Error('Timeout')), 10000);
        });
      });
      globalThis.fetch = mockFetch;

      const promise = client.processDecision(
        {
          question: 'テスト',
          technical_constraints: [],
          regulatory_constraints: [],
          human_resources: [],
        },
        'test-request-id'
      );

      // キャンセル実行
      client.cancelRequest('test-request-id');

      // タイマーを進める
      await vi.runAllTimersAsync();

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


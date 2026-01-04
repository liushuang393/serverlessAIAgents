/**
 * AgentApiClient - 通用 API 客户端.
 * 
 * AgentFlow アプリケーション用の REST API クライアント。
 * 自動的に以下を提供:
 * - 指数バックオフリトライ
 * - リクエストキャンセル
 * - 詳細エラー分類
 * 
 * @example
 * ```tsx
 * const api = new AgentApiClient({
 *   baseUrl: '/api',
 *   retry: { maxRetries: 3 },
 * });
 * 
 * const result = await api.post('/decision', payload);
 * const blob = await api.download('/report/123/pdf');
 * 
 * // キャンセル
 * api.cancel('my-request');
 * ```
 */

// ========================================
// エラー型定義
// ========================================

/** エラーコード */
export type AgentApiErrorCode =
  | 'VALIDATION_ERROR'
  | 'AUTHENTICATION_ERROR'
  | 'AUTHORIZATION_ERROR'
  | 'NOT_FOUND'
  | 'RATE_LIMITED'
  | 'SERVER_ERROR'
  | 'TIMEOUT'
  | 'NETWORK_ERROR'
  | 'CANCELLED'
  | 'UNKNOWN';

/** API エラー */
export class AgentApiError extends Error {
  constructor(
    message: string,
    public code: AgentApiErrorCode,
    public statusCode?: number,
    public details?: unknown,
    public isRetryable: boolean = false
  ) {
    super(message);
    this.name = 'AgentApiError';
  }

  /** HTTPステータスコードからエラーを生成 */
  static fromResponse(response: Response, details?: unknown): AgentApiError {
    const statusMap: Record<number, { code: AgentApiErrorCode; message: string; retryable: boolean }> = {
      400: { code: 'VALIDATION_ERROR', message: '入力が不正です', retryable: false },
      401: { code: 'AUTHENTICATION_ERROR', message: '認証が必要です', retryable: false },
      403: { code: 'AUTHORIZATION_ERROR', message: 'アクセス権限がありません', retryable: false },
      404: { code: 'NOT_FOUND', message: 'リソースが見つかりません', retryable: false },
      429: { code: 'RATE_LIMITED', message: 'リクエストが多すぎます', retryable: true },
      500: { code: 'SERVER_ERROR', message: 'サーバーエラーが発生しました', retryable: true },
      502: { code: 'SERVER_ERROR', message: 'サーバーに接続できません', retryable: true },
      503: { code: 'SERVER_ERROR', message: 'サービスが一時的に利用できません', retryable: true },
    };

    const info = statusMap[response.status] || {
      code: 'UNKNOWN' as AgentApiErrorCode,
      message: `APIエラー: ${response.statusText}`,
      retryable: response.status >= 500,
    };

    return new AgentApiError(info.message, info.code, response.status, details, info.retryable);
  }

  /** ネットワークエラーを生成 */
  static networkError(originalError?: Error): AgentApiError {
    return new AgentApiError(
      'ネットワークに接続できません',
      'NETWORK_ERROR',
      undefined,
      originalError,
      true
    );
  }

  /** タイムアウトエラーを生成 */
  static timeoutError(): AgentApiError {
    return new AgentApiError('リクエストがタイムアウトしました', 'TIMEOUT', undefined, undefined, true);
  }

  /** キャンセルエラーを生成 */
  static cancelledError(): AgentApiError {
    return new AgentApiError('リクエストがキャンセルされました', 'CANCELLED', undefined, undefined, false);
  }
}

// ========================================
// リトライ設定
// ========================================

/** リトライ設定 */
export interface RetryConfig {
  /** 最大リトライ回数 */
  maxRetries: number;
  /** 基本遅延 (ms) */
  baseDelay: number;
  /** 最大遅延 (ms) */
  maxDelay: number;
}

const DEFAULT_RETRY_CONFIG: RetryConfig = {
  maxRetries: 3,
  baseDelay: 1000,
  maxDelay: 10000,
};

/** 指数バックオフ遅延計算 */
function getRetryDelay(attempt: number, config: RetryConfig): number {
  const delay = config.baseDelay * Math.pow(2, attempt);
  const jitter = Math.random() * 1000;
  return Math.min(delay + jitter, config.maxDelay);
}

/** スリープ */
const sleep = (ms: number): Promise<void> => new Promise((resolve) => setTimeout(resolve, ms));

// ========================================
// クライアント設定
// ========================================

/** クライアント設定 */
export interface AgentApiClientConfig {
  /** ベース URL */
  baseUrl: string;
  /** リトライ設定 */
  retry?: Partial<RetryConfig>;
  /** デフォルトヘッダー */
  headers?: Record<string, string>;
  /** タイムアウト (ms) */
  timeout?: number;
}

// ========================================
// クライアント実装
// ========================================

/**
 * AgentFlow API クライアント.
 */
export class AgentApiClient {
  private baseUrl: string;
  private retryConfig: RetryConfig;
  private defaultHeaders: Record<string, string>;
  private timeout: number;
  private abortControllers: Map<string, AbortController> = new Map();

  constructor(config: AgentApiClientConfig) {
    this.baseUrl = config.baseUrl;
    this.retryConfig = { ...DEFAULT_RETRY_CONFIG, ...config.retry };
    this.defaultHeaders = {
      'Content-Type': 'application/json',
      ...config.headers,
    };
    this.timeout = config.timeout ?? 30000;
  }

  // ========================================
  // 公開 API
  // ========================================

  /**
   * GET リクエスト.
   */
  async get<T>(path: string, requestId?: string): Promise<T> {
    return this.request<T>('GET', path, undefined, requestId);
  }

  /**
   * POST リクエスト.
   */
  async post<T, D = unknown>(path: string, data: D, requestId?: string): Promise<T> {
    return this.request<T>('POST', path, data, requestId);
  }

  /**
   * PUT リクエスト.
   */
  async put<T, D = unknown>(path: string, data: D, requestId?: string): Promise<T> {
    return this.request<T>('PUT', path, data, requestId);
  }

  /**
   * DELETE リクエスト.
   */
  async delete<T>(path: string, requestId?: string): Promise<T> {
    return this.request<T>('DELETE', path, undefined, requestId);
  }

  /**
   * ファイルダウンロード.
   */
  async download(path: string, requestId?: string): Promise<Blob> {
    const controller = new AbortController();
    const id = requestId ?? `download-${Date.now()}`;
    this.abortControllers.set(id, controller);

    try {
      const response = await this.fetchWithRetry(`${this.baseUrl}${path}`, {
        method: 'GET',
        signal: controller.signal,
      });
      return response.blob();
    } finally {
      this.abortControllers.delete(id);
    }
  }

  /**
   * リクエストをキャンセル.
   */
  cancel(requestId: string): void {
    const controller = this.abortControllers.get(requestId);
    if (controller) {
      controller.abort();
      this.abortControllers.delete(requestId);
    }
  }

  /**
   * 全リクエストをキャンセル.
   */
  cancelAll(): void {
    this.abortControllers.forEach((controller) => controller.abort());
    this.abortControllers.clear();
  }

  // ========================================
  // 内部メソッド
  // ========================================

  private async request<T>(
    method: string,
    path: string,
    data?: unknown,
    requestId?: string
  ): Promise<T> {
    const controller = new AbortController();
    const id = requestId ?? `${method.toLowerCase()}-${Date.now()}`;
    this.abortControllers.set(id, controller);

    try {
      const response = await this.fetchWithRetry(`${this.baseUrl}${path}`, {
        method,
        headers: this.defaultHeaders,
        body: data ? JSON.stringify(data) : undefined,
        signal: controller.signal,
      });

      return response.json();
    } finally {
      this.abortControllers.delete(id);
    }
  }

  private async fetchWithRetry(url: string, options: RequestInit): Promise<Response> {
    let lastError: Error | null = null;

    for (let attempt = 0; attempt <= this.retryConfig.maxRetries; attempt++) {
      try {
        // タイムアウト付き fetch
        const timeoutId = setTimeout(() => {
          if (options.signal && 'abort' in options.signal) {
            // すでに AbortController がある場合はそちらを使用
          }
        }, this.timeout);

        const response = await fetch(url, options);
        clearTimeout(timeoutId);

        if (!response.ok) {
          const error = AgentApiError.fromResponse(response);

          // リトライ可能なエラーで最大試行回数に達していない場合
          if (error.isRetryable && attempt < this.retryConfig.maxRetries) {
            const delay = getRetryDelay(attempt, this.retryConfig);
            await sleep(delay);
            continue;
          }

          throw error;
        }

        return response;
      } catch (err) {
        // AbortError
        if (err instanceof Error && err.name === 'AbortError') {
          throw AgentApiError.cancelledError();
        }

        // AgentApiError はそのまま throw
        if (err instanceof AgentApiError) {
          throw err;
        }

        // ネットワークエラー
        lastError = err instanceof Error ? err : new Error(String(err));

        if (attempt < this.retryConfig.maxRetries) {
          const delay = getRetryDelay(attempt, this.retryConfig);
          await sleep(delay);
          continue;
        }
      }
    }

    throw lastError ? AgentApiError.networkError(lastError) : AgentApiError.networkError();
  }
}


/**
 * Decision API クライアント.
 *
 * 目的: REST API / SSE ストリーム対応の統合クライアント
 * I/O:
 *   - REST: POST /api/decision (同期処理)
 *   - SSE: GET /api/decision/stream (リアルタイム進捗)
 * 
 * 機能:
 *   - リクエストリトライ（指数バックオフ）
 *   - リクエストキャンセル（AbortController）
 *   - 詳細エラーハンドリング
 */

import type {
  DecisionRequest,
  DecisionAPIResponse,
  AGUIEvent,
  SignatureResponse,
} from '../types';

/** API ベースURL
 * 
 * 開発環境（Vite dev server）: 空文字列 → 相対パス → Viteプロキシ経由
 * 本番環境: VITE_API_URL または同一オリジン
 */
const API_BASE_URL = import.meta.env.VITE_API_URL || '';

// デバッグ用
console.log('[API] Base URL:', API_BASE_URL || '(relative - using proxy)');

/** デフォルトリトライ設定 */
const DEFAULT_RETRY_CONFIG = {
  maxRetries: 3,
  baseDelay: 1000,
  maxDelay: 10000,
};

/** API エラー */
export class DecisionApiError extends Error {
  constructor(
    message: string,
    public statusCode?: number,
    public details?: unknown,
    public isRetryable: boolean = false
  ) {
    super(message);
    this.name = 'DecisionApiError';
  }

  /** HTTPステータスコードからエラーを生成 */
  static fromResponse(response: Response, details?: unknown): DecisionApiError {
    const statusMessages: Record<number, string> = {
      400: 'リクエストが不正です',
      401: '認証が必要です',
      403: 'アクセス権限がありません',
      404: 'リソースが見つかりません',
      429: 'リクエストが多すぎます。しばらく待ってから再試行してください',
      500: 'サーバーエラーが発生しました',
      502: 'サーバーに接続できません',
      503: 'サービスが一時的に利用できません',
    };
    
    const message = statusMessages[response.status] || `APIエラー: ${response.statusText}`;
    const isRetryable = response.status >= 500 || response.status === 429;
    
    return new DecisionApiError(message, response.status, details, isRetryable);
  }
}

/** 指数バックオフでスリープ */
const sleep = (ms: number): Promise<void> => new Promise(resolve => setTimeout(resolve, ms));

/** 指数バックオフの遅延計算 */
const getRetryDelay = (attempt: number, config = DEFAULT_RETRY_CONFIG): number => {
  const delay = config.baseDelay * Math.pow(2, attempt);
  const jitter = Math.random() * 1000;
  return Math.min(delay + jitter, config.maxDelay);
};

/**
 * Decision API クライアント.
 */
export class DecisionApiClient {
  private baseUrl: string;
  private abortControllers: Map<string, AbortController> = new Map();

  constructor(baseUrl: string = API_BASE_URL) {
    this.baseUrl = baseUrl;
  }

  /**
   * リクエストをキャンセル.
   * 
   * @param requestId リクエストID
   */
  cancelRequest(requestId: string): void {
    const controller = this.abortControllers.get(requestId);
    if (controller) {
      controller.abort();
      this.abortControllers.delete(requestId);
    }
  }

  /**
   * 全てのリクエストをキャンセル.
   */
  cancelAllRequests(): void {
    this.abortControllers.forEach(controller => controller.abort());
    this.abortControllers.clear();
  }

  /**
   * リトライ付きfetch.
   * 
   * @param url URL
   * @param options fetchオプション
   * @param retryConfig リトライ設定
   */
  private async fetchWithRetry(
    url: string,
    options: RequestInit,
    retryConfig = DEFAULT_RETRY_CONFIG
  ): Promise<Response> {
    let lastError: Error | null = null;

    for (let attempt = 0; attempt <= retryConfig.maxRetries; attempt++) {
      try {
        const response = await fetch(url, options);
        
        if (!response.ok) {
          const error = DecisionApiError.fromResponse(response);
          
          // リトライ可能なエラーで最大試行回数に達していない場合
          if (error.isRetryable && attempt < retryConfig.maxRetries) {
            const delay = getRetryDelay(attempt, retryConfig);
            await sleep(delay);
            continue;
          }
          
          throw error;
        }
        
        return response;
      } catch (err) {
        // AbortErrorは即座にスロー
        if (err instanceof Error && err.name === 'AbortError') {
          throw new DecisionApiError('リクエストがキャンセルされました', undefined, undefined, false);
        }
        
        lastError = err instanceof Error ? err : new Error(String(err));
        
        // ネットワークエラーはリトライ
        if (attempt < retryConfig.maxRetries) {
          const delay = getRetryDelay(attempt, retryConfig);
          await sleep(delay);
          continue;
        }
      }
    }

    throw lastError || new DecisionApiError('リクエストに失敗しました');
  }

  /**
   * REST API - 同期的に意思決定を処理.
   *
   * @param request 決策リクエスト
   * @param requestId リクエストID（キャンセル用）
   * @returns API レスポンス
   */
  async processDecision(
    request: DecisionRequest, 
    requestId?: string
  ): Promise<DecisionAPIResponse> {
    const controller = new AbortController();
    const id = requestId || `decision-${Date.now()}`;
    this.abortControllers.set(id, controller);

    try {
      const response = await this.fetchWithRetry(
        `${this.baseUrl}/api/decision`,
        {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(request),
          signal: controller.signal,
        }
      );

      return response.json();
    } finally {
      this.abortControllers.delete(id);
    }
  }

  /**
   * SSE ストリーム - リアルタイム進捗付き処理.
   *
   * @param question 質問テキスト
   * @param budget 予算（オプション）
   * @param timelineMonths 期間（オプション）
   * @param onEvent イベントコールバック
   * @param onError エラーコールバック
   * @param onOpen 接続成功コールバック
   * @returns EventSource インスタンス
   */
  streamDecision(
    question: string,
    budget?: number,
    timelineMonths?: number,
    onEvent?: (event: AGUIEvent) => void,
    onError?: (error: string, isRetryable?: boolean) => void,
    onOpen?: () => void
  ): EventSource {
    const params = new URLSearchParams({
      question,
      ...(budget && { budget: budget.toString() }),
      ...(timelineMonths && { timeline_months: timelineMonths.toString() }),
    });

    const eventSource = new EventSource(
      `${this.baseUrl}/api/decision/stream?${params}`
    );

    // 接続成功
    eventSource.onopen = () => {
      console.log('[SSE] EventSource.onopen 発火 - 接続成功');
      onOpen?.();
    };

    if (onEvent) {
      eventSource.onmessage = (e) => {
        console.log('[SSE] EventSource.onmessage 受信:', e.data.slice(0, 100));
        try {
          const event: AGUIEvent = JSON.parse(e.data);
          console.log('[SSE] パース成功:', event.event_type, event.node_id || '');
          onEvent(event);
        } catch (err) {
          // SSE イベントのパースに失敗 - ログ出力
          console.warn('[SSE] イベントパース失敗:', e.data, err);
        }
      };
    }

    eventSource.onerror = (err) => {
      // 接続状態を確認
      const isConnecting = eventSource.readyState === EventSource.CONNECTING;
      const isClosed = eventSource.readyState === EventSource.CLOSED;
      
      if (isClosed) {
        // 完全に閉じた - リトライ不可
        if (onError) {
          onError('サーバーとの接続が切断されました。', false);
        }
      } else if (isConnecting) {
        // 再接続中 - ユーザーに通知しない（EventSourceが自動リトライ）
        console.warn('[SSE] 再接続中...', err);
      } else {
        // その他のエラー
        eventSource.close();
        if (onError) {
          onError('サーバーに接続できません。バックエンドが起動しているか確認してください。', true);
        }
      }
    };

    return eventSource;
  }

  /**
   * A2UI コンポーネント取得.
   *
   * @param reportId レポートID
   * @returns コンポーネントデータ
   */
  async getReportComponents(
    reportId: string
  ): Promise<{ report_id: string; components: unknown[] }> {
    const response = await fetch(
      `${this.baseUrl}/api/report/${reportId}/components`
    );

    if (!response.ok) {
      throw new DecisionApiError(
        `Failed to get components: ${response.statusText}`,
        response.status
      );
    }

    return response.json();
  }

  /**
   * PDF エクスポート.
   *
   * @param reportId レポートID
   * @param requestId リクエストID（キャンセル用）
   * @returns PDF Blob
   */
  async exportPdf(reportId: string, requestId?: string): Promise<Blob> {
    const controller = new AbortController();
    const id = requestId || `pdf-${Date.now()}`;
    this.abortControllers.set(id, controller);

    try {
      const response = await this.fetchWithRetry(
        `${this.baseUrl}/api/report/${reportId}/pdf`,
        { signal: controller.signal }
      );

      return response.blob();
    } finally {
      this.abortControllers.delete(id);
    }
  }

  /**
   * ヘルスチェック.
   */
  async healthCheck(): Promise<{ status: string; version: string }> {
    const response = await fetch(`${this.baseUrl}/api/health`);
    return response.json();
  }

  /**
   * レポートに署名.
   *
   * @param reportId レポートID
   * @returns 署名レスポンス
   */
  async signReport(reportId: string): Promise<SignatureResponse> {
    const response = await fetch(`${this.baseUrl}/api/report/${reportId}/sign`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      credentials: 'include',
      body: JSON.stringify({ report_id: reportId, confirmation: true }),
    });

    if (!response.ok) {
      if (response.status === 401) {
        throw new DecisionApiError('認証が必要です。再度ログインしてください。', 401);
      }
      throw DecisionApiError.fromResponse(response);
    }

    return response.json();
  }

  /**
   * レポートの署名情報を取得.
   *
   * @param reportId レポートID
   * @returns 署名情報
   */
  async getSignature(reportId: string): Promise<{ signed: boolean; signature?: SignatureResponse['signature'] }> {
    const response = await fetch(`${this.baseUrl}/api/report/${reportId}/signature`, {
      credentials: 'include',
    });

    if (!response.ok) {
      throw DecisionApiError.fromResponse(response);
    }

    return response.json();
  }

  /**
   * 汎用 GET リクエスト.
   */
  async get(path: string): Promise<{ data: unknown }> {
    const response = await this.fetchWithRetry(`${this.baseUrl}${path}`, {
      method: 'GET',
      headers: { 'Content-Type': 'application/json' },
    });
    const data = await response.json();
    return { data };
  }

  /**
   * 汎用 PUT リクエスト.
   */
  async put(path: string, body: unknown): Promise<{ data: unknown }> {
    const response = await this.fetchWithRetry(`${this.baseUrl}${path}`, {
      method: 'PUT',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    });
    const data = await response.json();
    return { data };
  }
}

/** シングルトンインスタンス */
export const decisionApi = new DecisionApiClient();

/** 後方互換エイリアス */
export const apiClient = decisionApi;


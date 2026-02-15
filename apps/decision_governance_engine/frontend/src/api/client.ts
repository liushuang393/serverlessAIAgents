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
  FindingRecheckRequest,
  FindingRecheckResponse,
  FindingNoteRequest,
  FindingNoteResponse,
  SignatureResponse,
  HistoryListResponse,
  HistoryDetailResponse,
} from '../types';

/** API ベースURL
 * 
 * 開発環境（Vite dev server）: 空文字列 → 相対パス → Viteプロキシ経由
 * 本番環境: VITE_API_URL または同一オリジン
 */
const API_BASE_URL = import.meta.env.VITE_API_URL || '';

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

/** エクスポートファイル情報 */
export interface ExportFileResponse {
  blob: Blob;
  contentType: string;
  filename: string;
}

/**
 * Content-Disposition からファイル名を抽出.
 */
const parseFilenameFromDisposition = (disposition: string | null): string | null => {
  if (!disposition) {
    return null;
  }
  const utf8Match = disposition.match(/filename\*=UTF-8''([^;]+)/i);
  if (utf8Match && utf8Match[1]) {
    try {
      return decodeURIComponent(utf8Match[1]);
    } catch {
      return utf8Match[1];
    }
  }
  const normalMatch = disposition.match(/filename="?([^";]+)"?/i);
  if (normalMatch && normalMatch[1]) {
    return normalMatch[1];
  }
  return null;
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
   * @param stakeholders ステークホルダー情報（オプション）
   * @returns EventSource インスタンス
   */
  streamDecision(
    question: string,
    budget?: number,
    timelineMonths?: number,
    onEvent?: (event: AGUIEvent) => void,
    onError?: (error: string, isRetryable?: boolean) => void,
    onOpen?: () => void,
    stakeholders?: {
      product_owner?: string;
      tech_lead?: string;
      business_owner?: string;
      legal_reviewer?: string;
    },
    technicalConstraints?: string[],
    regulatoryConstraints?: string[],
    team?: string,
    streamOptions?: {
      requestId?: string;
      resume?: boolean;
    }
  ): EventSource {
    const params = new URLSearchParams({
      question,
      ...(budget && { budget: budget.toString() }),
      ...(timelineMonths && { timeline_months: timelineMonths.toString() }),
    });

    // ステークホルダー情報をクエリパラメータに追加
    if (stakeholders) {
      if (stakeholders.product_owner) params.set('stakeholder_product_owner', stakeholders.product_owner);
      if (stakeholders.tech_lead) params.set('stakeholder_tech_lead', stakeholders.tech_lead);
      if (stakeholders.business_owner) params.set('stakeholder_business_owner', stakeholders.business_owner);
      if (stakeholders.legal_reviewer) params.set('stakeholder_legal_reviewer', stakeholders.legal_reviewer);
    }

    // 技術制約をクエリパラメータに追加（リスト → 複数パラメータ）
    if (technicalConstraints) {
      for (const tc of technicalConstraints) {
        if (tc) params.append('technical_constraints', tc);
      }
    }

    // 規制制約をクエリパラメータに追加
    if (regulatoryConstraints) {
      for (const rc of regulatoryConstraints) {
        if (rc) params.append('regulatory_constraints', rc);
      }
    }

    // 人的リソースをクエリパラメータに追加
    if (team) {
      params.set('human_resources', team);
    }

    // 途中再開（同一 request_id）
    if (streamOptions?.requestId) {
      params.set('request_id', streamOptions.requestId);
    }
    if (streamOptions?.resume) {
      params.set('resume', 'true');
    }

    const url = `${this.baseUrl}/api/decision/stream?${params}`;

    const eventSource = new EventSource(url);

    // onopen が発火したかを追跡（一部環境で発火しないケースがある）
    let hasCalledOnOpen = false;

    // 接続成功
    eventSource.onopen = () => {
      if (!hasCalledOnOpen) {
        hasCalledOnOpen = true;
        onOpen?.();
      }
    };

    if (onEvent) {
      eventSource.onmessage = (e) => {
        // 最初のメッセージ受信時に onopen が発火していなければ手動で呼ぶ
        // これにより isConnected が確実に true になる
        if (!hasCalledOnOpen) {
          hasCalledOnOpen = true;
          onOpen?.();
        }

        try {
          const event: AGUIEvent = JSON.parse(e.data);
          onEvent(event);
        } catch (error_) {
          // SSE イベントのパースに失敗
          // 注意: パース失敗はサーバー側の問題の可能性があるため、onErrorで通知
          if (onError) {
            const errorMessage = error_ instanceof Error
              ? `SSEデータのパースに失敗: ${error_.message}`
              : 'SSEデータのパースに失敗しました';
            onError(errorMessage, false);
          }
        }
      };
    }

    eventSource.onerror = (_err) => {
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
        // No action needed
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
   * @returns エクスポートファイル情報
   */
  async exportPdf(reportId: string, requestId?: string): Promise<ExportFileResponse> {
    const controller = new AbortController();
    const id = requestId || `pdf-${Date.now()}`;
    this.abortControllers.set(id, controller);

    try {
      const response = await this.fetchWithRetry(
        `${this.baseUrl}/api/report/${reportId}/pdf`,
        { signal: controller.signal }
      );
      const blob = await response.blob();
      const contentType = response.headers.get('content-type') || blob.type || 'application/octet-stream';
      const filename =
        parseFilenameFromDisposition(response.headers.get('content-disposition')) ||
        `decision_report_${reportId}.pdf`;

      return { blob, contentType, filename };
    } finally {
      this.abortControllers.delete(id);
    }
  }

  /**
   * HTML エクスポート.
   *
   * @param reportId レポートID
   * @param requestId リクエストID（キャンセル用）
   * @returns エクスポートファイル情報
   */
  async exportHtml(reportId: string, requestId?: string): Promise<ExportFileResponse> {
    const controller = new AbortController();
    const id = requestId || `html-${Date.now()}`;
    this.abortControllers.set(id, controller);

    try {
      const response = await this.fetchWithRetry(
        `${this.baseUrl}/api/report/${reportId}/html`,
        { signal: controller.signal }
      );
      const blob = await response.blob();
      const contentType = response.headers.get('content-type') || blob.type || 'text/html';
      const filename =
        parseFilenameFromDisposition(response.headers.get('content-disposition')) ||
        `decision_report_${reportId}.html`;

      return { blob, contentType, filename };
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
   * 重要指摘の人間確認を AI で再評価.
   */
  async recheckFinding(request: FindingRecheckRequest): Promise<FindingRecheckResponse> {
    const response = await this.fetchWithRetry(
      `${this.baseUrl}/human-review/recheck-finding`,
      {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(request),
      }
    );

    return response.json();
  }

  /**
   * 重要指摘に対する任意メモを保存.
   */
  async logFindingNote(request: FindingNoteRequest): Promise<FindingNoteResponse> {
    const response = await this.fetchWithRetry(
      `${this.baseUrl}/human-review/log-finding-note`,
      {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(request),
      }
    );

    return response.json();
  }

  /**
   * 履歴一覧取得.
   *
   * @param options フィルタオプション
   * @returns 履歴一覧
   */
  async getHistory(options?: {
    limit?: number;
    decision_role?: string;
    mode?: string;
  }): Promise<HistoryListResponse> {
    const params = new URLSearchParams();
    if (options?.limit) params.append('limit', options.limit.toString());
    if (options?.decision_role) params.append('decision_role', options.decision_role);
    if (options?.mode) params.append('mode', options.mode);

    const query = params.toString() ? `?${params.toString()}` : '';
    const response = await this.fetchWithRetry(
      `${this.baseUrl}/api/decision/history${query}`,
      { method: 'GET' }
    );

    return response.json();
  }

  /**
   * 履歴詳細取得.
   *
   * @param requestId リクエストID
   * @returns 履歴詳細
   */
  async getHistoryDetail(requestId: string): Promise<HistoryDetailResponse> {
    const response = await this.fetchWithRetry(
      `${this.baseUrl}/api/decision/history/${requestId}`,
      { method: 'GET' }
    );

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


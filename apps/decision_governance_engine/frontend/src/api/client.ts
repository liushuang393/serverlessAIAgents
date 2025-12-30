/**
 * Decision API クライアント.
 *
 * 目的: REST API / SSE ストリーム対応の統合クライアント
 * I/O:
 *   - REST: POST /api/decision (同期処理)
 *   - SSE: GET /api/decision/stream (リアルタイム進捗)
 */

import type {
  DecisionRequest,
  DecisionAPIResponse,
  DecisionReport,
  AGUIEvent,
} from '../types';

/** API ベースURL */
const API_BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:8000';

/** API エラー */
export class DecisionApiError extends Error {
  constructor(
    message: string,
    public statusCode?: number,
    public details?: unknown
  ) {
    super(message);
    this.name = 'DecisionApiError';
  }
}

/**
 * Decision API クライアント.
 */
export class DecisionApiClient {
  private baseUrl: string;

  constructor(baseUrl: string = API_BASE_URL) {
    this.baseUrl = baseUrl;
  }

  /**
   * REST API - 同期的に意思決定を処理.
   *
   * @param request 決策リクエスト
   * @returns API レスポンス
   */
  async processDecision(request: DecisionRequest): Promise<DecisionAPIResponse> {
    const response = await fetch(`${this.baseUrl}/api/decision`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(request),
    });

    if (!response.ok) {
      throw new DecisionApiError(
        `API error: ${response.statusText}`,
        response.status
      );
    }

    return response.json();
  }

  /**
   * SSE ストリーム - リアルタイム進捗付き処理.
   *
   * @param question 質問テキスト
   * @param budget 予算（オプション）
   * @param timelineMonths 期間（オプション）
   * @param onEvent イベントコールバック
   * @returns EventSource インスタンス
   */
  streamDecision(
    question: string,
    budget?: number,
    timelineMonths?: number,
    onEvent?: (event: AGUIEvent) => void
  ): EventSource {
    const params = new URLSearchParams({
      question,
      ...(budget && { budget: budget.toString() }),
      ...(timelineMonths && { timeline_months: timelineMonths.toString() }),
    });

    const eventSource = new EventSource(
      `${this.baseUrl}/api/decision/stream?${params}`
    );

    if (onEvent) {
      eventSource.onmessage = (e) => {
        try {
          const event: AGUIEvent = JSON.parse(e.data);
          onEvent(event);
        } catch (err) {
          console.error('Failed to parse SSE event:', err);
        }
      };
    }

    eventSource.onerror = (err) => {
      console.error('SSE connection error:', err);
      eventSource.close();
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
   * @returns PDF Blob
   */
  async exportPdf(reportId: string): Promise<Blob> {
    const response = await fetch(
      `${this.baseUrl}/api/report/${reportId}/pdf`
    );

    if (!response.ok) {
      throw new DecisionApiError(
        `Failed to export PDF: ${response.statusText}`,
        response.status
      );
    }

    return response.blob();
  }

  /**
   * ヘルスチェック.
   */
  async healthCheck(): Promise<{ status: string; version: string }> {
    const response = await fetch(`${this.baseUrl}/api/health`);
    return response.json();
  }
}

/** シングルトンインスタンス */
export const decisionApi = new DecisionApiClient();


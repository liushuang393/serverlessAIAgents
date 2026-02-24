/**
 * API クライアント.
 * 
 * 目的: バックエンドAPIとの通信を一元管理
 * I/O:
 *   - Input: API リクエストパラメータ
 *   - Output: 型付きレスポンス
 * 注意: エラーハンドリングを適切に行うこと
 */

import axios, { AxiosInstance, AxiosError } from 'axios';
import type {
  CollectJob,
  CollectRequest,
  CollectResponse,
  TrendsResponse,
  ReportsResponse,
} from '@/types';

function normalizeBaseURL(baseURL: string): string {
  return baseURL.endsWith('/') ? baseURL.slice(0, -1) : baseURL;
}

function extractErrorMessage(data: unknown): string | null {
  if (typeof data === 'string') {
    return data;
  }

  if (data && typeof data === 'object') {
    const detail = (data as { detail?: unknown }).detail;
    if (typeof detail === 'string') {
      return detail;
    }

    try {
      return JSON.stringify(data);
    } catch {
      return null;
    }
  }

  return null;
}

function resolveApiBaseURL(): string {
  const envBaseURL = import.meta.env.VITE_API_BASE_URL as string | undefined;
  if (typeof envBaseURL === 'string' && envBaseURL.trim().length > 0) {
    return normalizeBaseURL(envBaseURL.trim());
  }

  if (
    typeof __MARKET_TREND_MONITOR_API_BASE_URL__ === 'string' &&
    __MARKET_TREND_MONITOR_API_BASE_URL__.trim().length > 0
  ) {
    return normalizeBaseURL(__MARKET_TREND_MONITOR_API_BASE_URL__);
  }

  return '/api';
}

/**
 * API エラークラス.
 */
export class ApiError extends Error {
  constructor(
    message: string,
    public statusCode?: number,
    public details?: unknown
  ) {
    super(message);
    this.name = 'ApiError';
  }
}

/**
 * API クライアントクラス.
 */
class ApiClient {
  private client: AxiosInstance;

  constructor(baseURL: string = resolveApiBaseURL()) {
    this.client = axios.create({
      baseURL: normalizeBaseURL(baseURL),
      timeout: 30000,
      headers: {
        'Content-Type': 'application/json',
      },
    });

    // レスポンスインターセプター
    this.client.interceptors.response.use(
      (response) => response,
      (error: AxiosError) => {
        const details = error.response?.data;
        const message = extractErrorMessage(details) ?? error.message;
        throw new ApiError(
          message,
          error.response?.status,
          details
        );
      }
    );
  }

  /**
   * データ収集を実行.
   */
  async collect(request: CollectRequest): Promise<CollectResponse> {
    const response = await this.client.post<CollectResponse>(
      '/collect',
      request,
      {
        // 収集処理は外部API呼び出しを含むため、通常APIより長いタイムアウトを設定
        timeout: 120000,
      }
    );
    return response.data;
  }

  /**
   * トレンド一覧を取得.
   */
  async getTrends(limit?: number): Promise<TrendsResponse> {
    const response = await this.client.get<TrendsResponse>('/trends', {
      params: { limit },
    });
    return response.data;
  }

  /**
   * レポート一覧を取得.
   */
  async getReports(limit?: number): Promise<ReportsResponse> {
    const response = await this.client.get<ReportsResponse>('/reports', {
      params: { limit },
    });
    return response.data;
  }

  /**
   * レポートをPDF形式でエクスポート.
   */
  async exportReport(
    reportId: string,
    format: 'pdf'
  ): Promise<{ blob: Blob; filename: string }> {
    const response = await this.client.get<Blob>(
      `/reports/${reportId}/export/${format}`,
      {
        responseType: 'blob',
      }
    );

    const disposition = response.headers['content-disposition'] as string | undefined;
    const matched = disposition?.match(/filename=\"?([^\";]+)\"?/i);
    const filename = matched?.[1] ?? `market_trend_report_${reportId}.pdf`;

    return {
      blob: response.data,
      filename,
    };
  }

  /**
   * 指定ジョブIDの状態を取得.
   */
  async getJob(jobId: string): Promise<CollectJob> {
    const response = await this.client.get<CollectJob>(`/jobs/${jobId}`);
    return response.data;
  }

  /**
   * 最新ジョブの状態を取得（存在しない場合は null）.
   */
  async getLatestJob(): Promise<CollectJob | null> {
    try {
      const response = await this.client.get<CollectJob>('/jobs/latest');
      return response.data;
    } catch (error) {
      if (error instanceof ApiError && error.statusCode === 404) {
        return null;
      }
      throw error;
    }
  }

  /**
   * ヘルスチェック.
   */
  async healthCheck(): Promise<{ status: string }> {
    const response = await this.client.get<{ status: string }>('/health');
    return response.data;
  }

  /**
   * 汎用GETリクエスト.
   *
   * 注意: pathにはbaseURL相対パスを指定（例: '/signals'）
   */
  async get<T = Record<string, unknown>>(
    path: string,
    config?: { params?: Record<string, unknown> }
  ): Promise<{ data: T }> {
    const response = await this.client.get<T>(path, config);
    return { data: response.data };
  }

  /**
   * 汎用POSTリクエスト.
   *
   * 注意: pathにはbaseURL相対パスを指定（例: '/predictions'）
   */
  async post<T = Record<string, unknown>>(
    path: string,
    data?: unknown,
    config?: { params?: Record<string, unknown>; timeout?: number }
  ): Promise<{ data: T }> {
    const response = await this.client.post<T>(path, data, config);
    return { data: response.data };
  }

  /**
   * 汎用PUTリクエスト.
   *
   * 注意: pathにはbaseURL相対パスを指定（例: '/competitors/config'）
   */
  async put<T = Record<string, unknown>>(
    path: string,
    data?: unknown,
    config?: { params?: Record<string, unknown> }
  ): Promise<{ data: T }> {
    const response = await this.client.put<T>(path, data, config);
    return { data: response.data };
  }
}

// シングルトンインスタンス
export const apiClient = new ApiClient();

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
  CollectRequest,
  CollectResponse,
  TrendsResponse,
  ReportsResponse,
} from '@/types';

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

  constructor(baseURL: string = '/api') {
    this.client = axios.create({
      baseURL,
      timeout: 30000,
      headers: {
        'Content-Type': 'application/json',
      },
    });

    // レスポンスインターセプター
    this.client.interceptors.response.use(
      (response) => response,
      (error: AxiosError) => {
        const message = error.response?.data
          ? String(error.response.data)
          : error.message;
        throw new ApiError(
          message,
          error.response?.status,
          error.response?.data
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
      request
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
   * ヘルスチェック.
   */
  async healthCheck(): Promise<{ status: string }> {
    const response = await this.client.get<{ status: string }>('/health');
    return response.data;
  }
}

// シングルトンインスタンス
export const apiClient = new ApiClient();


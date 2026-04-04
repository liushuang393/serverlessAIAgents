/**
 * API クライアント.
 *
 * 目的: バックエンドAPIとの通信を一元管理
 * I/O:
 *   - Input: API リクエストパラメータ
 *   - Output: 型付きレスポンス
 * 注意: エラーハンドリングを適切に行うこと
 *
 * 呼び出しパターン: fetch ベース（axios 不使用、規約準拠）
 */

import type {
  CollectJob,
  CollectRequest,
  CollectResponse,
  TrendsResponse,
  ReportsResponse,
} from "@/types";

declare const __MARKET_TREND_MONITOR_API_BASE_URL__: string | undefined;

function normalizeBaseURL(baseURL: string): string {
  return baseURL.endsWith("/") ? baseURL.slice(0, -1) : baseURL;
}

function extractErrorMessage(data: unknown): string | null {
  if (typeof data === "string") {
    return data;
  }

  if (data && typeof data === "object") {
    const detail = (data as { detail?: unknown }).detail;
    if (typeof detail === "string") {
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

/**
 * API ベースURLを解決する.
 *
 * 優先順位:
 *   1. 環境変数 VITE_API_BASE_URL（ビルド時に注入）
 *   2. define マクロ __MARKET_TREND_MONITOR_API_BASE_URL__（vite.config.ts）
 *   3. 相対パス "/api"（nginx リバースプロキシ経由を想定）
 *
 * 注意: 値が "localhost" を含む絶対URLの場合、ブラウザの実際の
 *       ホスト名と一致しなければ相対パスにフォールバックする。
 *       これにより Docker 環境でも LAN 環境でも安定して動作する。
 */
function isLocalhostURL(url: string): boolean {
  try {
    const parsed = new URL(url);
    return parsed.hostname === "localhost" || parsed.hostname === "127.0.0.1";
  } catch {
    return false;
  }
}

function shouldUseRelativePath(candidate: string): boolean {
  if (!isLocalhostURL(candidate)) return false;
  if (typeof window === "undefined") return false;
  const browserHost = window.location.hostname;
  return browserHost !== "localhost" && browserHost !== "127.0.0.1";
}

function resolveApiBaseURL(): string {
  const envBaseURL = import.meta.env.VITE_API_BASE_URL as string | undefined;
  if (typeof envBaseURL === "string" && envBaseURL.trim().length > 0) {
    const trimmed = normalizeBaseURL(envBaseURL.trim());
    if (shouldUseRelativePath(trimmed)) return "/api";
    return trimmed;
  }

  if (
    typeof __MARKET_TREND_MONITOR_API_BASE_URL__ === "string" &&
    __MARKET_TREND_MONITOR_API_BASE_URL__.trim().length > 0
  ) {
    const defined = normalizeBaseURL(__MARKET_TREND_MONITOR_API_BASE_URL__);
    if (shouldUseRelativePath(defined)) return "/api";
    return defined;
  }

  return "/api";
}

/**
 * API エラークラス.
 */
export class ApiError extends Error {
  constructor(
    message: string,
    public statusCode?: number,
    public details?: unknown,
  ) {
    super(message);
    this.name = "ApiError";
  }
}

/**
 * レスポンスを処理し、エラー時は ApiError をスローする.
 */
async function handleResponse<T>(response: Response): Promise<T> {
  if (!response.ok) {
    let details: unknown;
    try {
      details = await response.json();
    } catch {
      details = undefined;
    }
    const message = extractErrorMessage(details) ?? response.statusText;
    throw new ApiError(message, response.status, details);
  }
  return (await response.json()) as T;
}

/**
 * クエリパラメータをURLに付与する.
 */
function buildURL(
  base: string,
  path: string,
  params?: Record<string, unknown>,
): string {
  const url = `${base}${path}`;
  if (!params) return url;
  const searchParams = new URLSearchParams();
  for (const [key, value] of Object.entries(params)) {
    if (value !== undefined && value !== null) {
      searchParams.set(key, String(value));
    }
  }
  const qs = searchParams.toString();
  return qs ? `${url}?${qs}` : url;
}

/**
 * API クライアントクラス（fetch ベース）.
 */
class ApiClient {
  private baseURL: string;

  constructor(baseURL: string = resolveApiBaseURL()) {
    this.baseURL = normalizeBaseURL(baseURL);
  }

  /**
   * データ収集を実行.
   */
  async collect(request: CollectRequest): Promise<CollectResponse> {
    const response = await fetch(`${this.baseURL}/collect`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(request),
      signal: AbortSignal.timeout(120000),
    });
    return handleResponse<CollectResponse>(response);
  }

  /**
   * トレンド一覧を取得.
   */
  async getTrends(limit?: number): Promise<TrendsResponse> {
    const url = buildURL(this.baseURL, "/trends", limit !== undefined ? { limit } : undefined);
    const response = await fetch(url, { signal: AbortSignal.timeout(30000) });
    return handleResponse<TrendsResponse>(response);
  }

  /**
   * レポート一覧を取得.
   */
  async getReports(limit?: number): Promise<ReportsResponse> {
    const url = buildURL(this.baseURL, "/reports", limit !== undefined ? { limit } : undefined);
    const response = await fetch(url, { signal: AbortSignal.timeout(30000) });
    return handleResponse<ReportsResponse>(response);
  }

  /**
   * レポートをPDF形式でエクスポート.
   */
  async exportReport(
    reportId: string,
    format: "pdf",
  ): Promise<{ blob: Blob; filename: string }> {
    const response = await fetch(
      `${this.baseURL}/reports/${reportId}/export/${format}`,
      { signal: AbortSignal.timeout(30000) },
    );
    if (!response.ok) {
      throw new ApiError(response.statusText, response.status);
    }

    const disposition = response.headers.get("content-disposition");
    const matched = disposition?.match(/filename="?([^";]+)"?/i);
    const filename = matched?.[1] ?? `market_trend_report_${reportId}.pdf`;

    return {
      blob: await response.blob(),
      filename,
    };
  }

  /**
   * 指定ジョブIDの状態を取得.
   */
  async getJob(jobId: string): Promise<CollectJob> {
    const response = await fetch(`${this.baseURL}/jobs/${jobId}`, {
      signal: AbortSignal.timeout(30000),
    });
    return handleResponse<CollectJob>(response);
  }

  /**
   * 最新ジョブの状態を取得（存在しない場合は null）.
   */
  async getLatestJob(): Promise<CollectJob | null> {
    try {
      const response = await fetch(`${this.baseURL}/jobs/latest`, {
        signal: AbortSignal.timeout(30000),
      });
      if (response.status === 404) return null;
      return handleResponse<CollectJob>(response);
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
    const response = await fetch(`${this.baseURL}/health`, {
      signal: AbortSignal.timeout(30000),
    });
    return handleResponse<{ status: string }>(response);
  }

  /**
   * 汎用GETリクエスト.
   *
   * 注意: pathにはbaseURL相対パスを指定（例: '/signals'）
   */
  async get<T = Record<string, unknown>>(
    path: string,
    config?: { params?: Record<string, unknown> },
  ): Promise<{ data: T }> {
    const url = buildURL(this.baseURL, path, config?.params);
    const response = await fetch(url, { signal: AbortSignal.timeout(30000) });
    const data = await handleResponse<T>(response);
    return { data };
  }

  /**
   * 汎用POSTリクエスト.
   *
   * 注意: pathにはbaseURL相対パスを指定（例: '/predictions'）
   */
  async post<T = Record<string, unknown>>(
    path: string,
    data?: unknown,
    config?: { params?: Record<string, unknown>; timeout?: number },
  ): Promise<{ data: T }> {
    const url = buildURL(this.baseURL, path, config?.params);
    const response = await fetch(url, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: data !== undefined ? JSON.stringify(data) : undefined,
      signal: AbortSignal.timeout(config?.timeout ?? 30000),
    });
    const result = await handleResponse<T>(response);
    return { data: result };
  }

  /**
   * 汎用PUTリクエスト.
   *
   * 注意: pathにはbaseURL相対パスを指定（例: '/competitors/config'）
   */
  async put<T = Record<string, unknown>>(
    path: string,
    data?: unknown,
    config?: { params?: Record<string, unknown> },
  ): Promise<{ data: T }> {
    const url = buildURL(this.baseURL, path, config?.params);
    const response = await fetch(url, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: data !== undefined ? JSON.stringify(data) : undefined,
      signal: AbortSignal.timeout(30000),
    });
    const result = await handleResponse<T>(response);
    return { data: result };
  }
}

// シングルトンインスタンス
export const apiClient = new ApiClient();

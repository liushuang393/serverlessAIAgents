/**
 * API クライアント - バックエンドとの通信
 */

const API_BASE = "/api";

/**
 * API エラー
 */
export class ApiError extends Error {
  constructor(
    public status: number,
    message: string,
  ) {
    super(message);
    this.name = "ApiError";
  }
}

interface ApiErrorPayload {
  detail?: string;
  error?: string;
  message?: string;
  messages?: unknown;
}

function normalizeApiErrorMessage(status: number, rawBody: string): string {
  const fallback = rawBody.trim() || `${status} request failed`;
  try {
    const parsed = JSON.parse(rawBody) as ApiErrorPayload;
    if (typeof parsed.message === "string" && parsed.message.trim()) {
      return parsed.message.trim();
    }
    if (typeof parsed.detail === "string" && parsed.detail.trim()) {
      return parsed.detail.trim();
    }
    if (Array.isArray(parsed.messages)) {
      const items = parsed.messages.filter(
        (item): item is string =>
          typeof item === "string" && item.trim().length > 0,
      );
      if (items.length > 0) {
        return items.join(", ");
      }
    }
    if (typeof parsed.error === "string" && parsed.error.trim()) {
      return parsed.error.trim();
    }
  } catch {
    // no-op
  }
  return fallback;
}

/**
 * 汎用 fetch ラッパー
 */
async function fetchApi<T>(
  endpoint: string,
  options?: RequestInit,
): Promise<T> {
  const response = await fetch(`${API_BASE}${endpoint}`, {
    ...options,
    headers: {
      "Content-Type": "application/json",
      ...options?.headers,
    },
  });

  if (!response.ok) {
    const bodyText = await response.text();
    throw new ApiError(
      response.status,
      normalizeApiErrorMessage(response.status, bodyText),
    );
  }

  return response.json();
}

/**
 * プラットフォーム状態
 */
export interface PlatformStatus {
  name: string;
  displayName?: string;
  icon?: string;
  description?: string;
  authMode?: string;
  authUrl?: string | null;
  docsUrl?: string | null;
  managed?: boolean;
  connected: boolean;
  lastActivity: string | null;
  messageCount: number;
  credentialFields?: PlatformCredentialField[];
}

export interface PlatformCredentialField {
  key: string;
  label: string;
  required: boolean;
  placeholder?: string;
  configured: boolean;
  maskedValue?: string | null;
}

export interface PlatformCredentialMutationResponse {
  ok: boolean;
  platform: string;
  connected: boolean;
  message?: string;
  updatedFields?: string[];
  platformInfo?: PlatformStatus;
}

/**
 * セッション情報
 */
export interface Session {
  id: string;
  platform: string;
  userId: string;
  userName: string;
  startedAt: string;
  lastMessageAt: string;
  messageCount: number;
}

/**
 * 統計情報
 */
export interface Statistics {
  totalMessages: number;
  totalSessions: number;
  activeSessions: number;
  platformStats: Record<string, number>;
}

/**
 * ヘルス状態を取得
 */
export function getHealth() {
  return fetchApi<{ status: string; uptime: number }>("/health");
}

/**
 * 統計情報を取得
 */
export function getStatistics() {
  return fetchApi<Statistics>("/stats");
}

/**
 * プラットフォーム一覧を取得
 */
export function getPlatforms() {
  return fetchApi<PlatformStatus[]>("/platforms");
}

/**
 * プラットフォーム認証情報を保存
 */
export function savePlatformCredentials(
  platformName: string,
  values: Record<string, string>,
  autoConnect = true,
) {
  return fetchApi<PlatformCredentialMutationResponse>(
    `/platforms/${encodeURIComponent(platformName)}/credentials`,
    {
      method: "POST",
      body: JSON.stringify({
        values,
        auto_connect: autoConnect,
      }),
    },
  );
}

/**
 * プラットフォーム接続を再試行
 */
export function connectPlatform(platformName: string) {
  return fetchApi<PlatformCredentialMutationResponse>(
    `/platforms/${encodeURIComponent(platformName)}/connect`,
    {
      method: "POST",
    },
  );
}

/**
 * セッション一覧を取得
 */
export function getSessions() {
  return fetchApi<Session[]>("/sessions");
}

/**
 * 会話履歴をエクスポート
 */
export function exportConversations(format: "json" | "csv" | "markdown") {
  return fetchApi<{ data: string }>(`/export?format=${format}`);
}

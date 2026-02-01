/**
 * API クライアント - バックエンドとの通信
 */

const API_BASE = '/api';

/**
 * API エラー
 */
export class ApiError extends Error {
  constructor(
    public status: number,
    message: string
  ) {
    super(message);
    this.name = 'ApiError';
  }
}

/**
 * 汎用 fetch ラッパー
 */
async function fetchApi<T>(
  endpoint: string,
  options?: RequestInit
): Promise<T> {
  const response = await fetch(`${API_BASE}${endpoint}`, {
    ...options,
    headers: {
      'Content-Type': 'application/json',
      ...options?.headers,
    },
  });

  if (!response.ok) {
    throw new ApiError(response.status, await response.text());
  }

  return response.json();
}

/**
 * プラットフォーム状態
 */
export interface PlatformStatus {
  name: string;
  connected: boolean;
  lastActivity: string | null;
  messageCount: number;
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
  return fetchApi<{ status: string; uptime: number }>('/health');
}

/**
 * 統計情報を取得
 */
export function getStatistics() {
  return fetchApi<Statistics>('/stats');
}

/**
 * プラットフォーム一覧を取得
 */
export function getPlatforms() {
  return fetchApi<PlatformStatus[]>('/platforms');
}

/**
 * セッション一覧を取得
 */
export function getSessions() {
  return fetchApi<Session[]>('/sessions');
}

/**
 * 会話履歴をエクスポート
 */
export function exportConversations(format: 'json' | 'csv' | 'markdown') {
  return fetchApi<{ data: string }>(`/export?format=${format}`);
}


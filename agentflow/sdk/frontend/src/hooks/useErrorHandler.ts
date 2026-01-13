/**
 * useErrorHandler - 統一エラーハンドリング Hook.
 *
 * 目的: コンポーネント間で一貫したエラー処理を提供
 * 機能:
 *   - try-catch のラップ
 *   - エラー状態管理
 *   - 自動ログ記録
 *   - リトライ機能
 *   - 通知連携
 *
 * @example
 * ```tsx
 * const { error, isError, handleAsync, clearError } = useErrorHandler();
 *
 * // 自動エラーハンドリング
 * const handleSubmit = handleAsync(async () => {
 *   await api.post('/data', payload);
 * });
 *
 * // エラー表示
 * {isError && <ErrorAlert message={error} onDismiss={clearError} />}
 * ```
 */

import { useState, useCallback, useRef } from 'react';
import { createLogger, type Logger } from '../utils/logger';

// ========================================
// 型定義
// ========================================

/** エラー情報 */
export interface ErrorInfo {
  message: string;
  code?: string;
  details?: Record<string, unknown>;
  retryable?: boolean;
  timestamp: Date;
  originalError?: Error;
}

/** Hook 設定 */
export interface UseErrorHandlerConfig {
  /** ロガー名 */
  loggerName?: string;
  /** デフォルトエラーメッセージ */
  defaultMessage?: string;
  /** エラー発生時のコールバック */
  onError?: (error: ErrorInfo) => void;
  /** エラークリア時のコールバック */
  onClear?: () => void;
  /** 自動クリア時間（ms, 0=無効） */
  autoClearMs?: number;
}

/** Hook 戻り値 */
export interface UseErrorHandlerReturn {
  /** 現在のエラーメッセージ */
  error: string | null;
  /** エラー詳細情報 */
  errorInfo: ErrorInfo | null;
  /** エラー状態か */
  isError: boolean;
  /** 処理中か */
  isLoading: boolean;
  /** エラーをクリア */
  clearError: () => void;
  /** エラーを設定 */
  setError: (message: string, details?: Partial<ErrorInfo>) => void;
  /** 非同期処理をラップ（エラー自動キャッチ） */
  handleAsync: <T>(fn: () => Promise<T>) => Promise<T | undefined>;
  /** リトライ可能な非同期処理 */
  handleAsyncWithRetry: <T>(
    fn: () => Promise<T>,
    maxRetries?: number
  ) => Promise<T | undefined>;
  /** 同期処理をラップ */
  handleSync: <T>(fn: () => T) => T | undefined;
}

// ========================================
// ユーティリティ
// ========================================

/**
 * エラーからメッセージを抽出.
 */
function extractErrorMessage(error: unknown, defaultMessage: string): string {
  if (error instanceof Error) {
    return error.message;
  }
  if (typeof error === 'string') {
    return error;
  }
  if (typeof error === 'object' && error !== null) {
    const obj = error as Record<string, unknown>;
    if (typeof obj.message === 'string') return obj.message;
    if (typeof obj.error === 'string') return obj.error;
  }
  return defaultMessage;
}

/**
 * エラーからコードを抽出.
 */
function extractErrorCode(error: unknown): string | undefined {
  if (typeof error === 'object' && error !== null) {
    const obj = error as Record<string, unknown>;
    if (typeof obj.code === 'string') return obj.code;
  }
  return undefined;
}

// ========================================
// Hook 実装
// ========================================

/**
 * 統一エラーハンドリング Hook.
 */
export function useErrorHandler(
  config: UseErrorHandlerConfig = {}
): UseErrorHandlerReturn {
  const {
    loggerName = 'error-handler',
    defaultMessage = '予期せぬエラーが発生しました',
    onError,
    onClear,
    autoClearMs = 0,
  } = config;

  const [errorInfo, setErrorInfo] = useState<ErrorInfo | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const loggerRef = useRef<Logger | null>(null);
  const autoClearTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  // ロガー取得（遅延初期化）
  const getLogger = useCallback(() => {
    if (!loggerRef.current) {
      loggerRef.current = createLogger(loggerName);
    }
    return loggerRef.current;
  }, [loggerName]);

  // 自動クリアタイマーをセット
  const scheduleAutoClear = useCallback(() => {
    if (autoClearMs > 0) {
      if (autoClearTimerRef.current) {
        clearTimeout(autoClearTimerRef.current);
      }
      autoClearTimerRef.current = setTimeout(() => {
        setErrorInfo(null);
        onClear?.();
      }, autoClearMs);
    }
  }, [autoClearMs, onClear]);

  // エラークリア
  const clearError = useCallback(() => {
    if (autoClearTimerRef.current) {
      clearTimeout(autoClearTimerRef.current);
    }
    setErrorInfo(null);
    onClear?.();
  }, [onClear]);

  // エラー設定
  const setError = useCallback(
    (message: string, details?: Partial<ErrorInfo>) => {
      const info: ErrorInfo = {
        message,
        timestamp: new Date(),
        ...details,
      };
      setErrorInfo(info);
      getLogger().error(message, info.originalError, {
        code: info.code,
        details: info.details,
      });
      onError?.(info);
      scheduleAutoClear();
    },
    [getLogger, onError, scheduleAutoClear]
  );

  // 非同期処理ラップ
  const handleAsync = useCallback(
    async <T>(fn: () => Promise<T>): Promise<T | undefined> => {
      clearError();
      setIsLoading(true);
      try {
        const result = await fn();
        return result;
      } catch (err) {
        const message = extractErrorMessage(err, defaultMessage);
        const code = extractErrorCode(err);
        setError(message, {
          code,
          originalError: err instanceof Error ? err : undefined,
          retryable: code === 'NETWORK_ERROR' || code === 'TIMEOUT',
        });
        return undefined;
      } finally {
        setIsLoading(false);
      }
    },
    [clearError, defaultMessage, setError]
  );

  // リトライ付き非同期処理
  const handleAsyncWithRetry = useCallback(
    async <T>(fn: () => Promise<T>, maxRetries = 3): Promise<T | undefined> => {
      clearError();
      setIsLoading(true);

      let lastError: unknown;
      for (let attempt = 0; attempt <= maxRetries; attempt++) {
        try {
          const result = await fn();
          return result;
        } catch (err) {
          lastError = err;
          getLogger().warn(`リトライ ${attempt + 1}/${maxRetries + 1}`, {
            error: extractErrorMessage(err, ''),
          });
          if (attempt < maxRetries) {
            // 指数バックオフ
            await new Promise((r) => setTimeout(r, Math.pow(2, attempt) * 500));
          }
        }
      }

      // 全リトライ失敗
      const message = extractErrorMessage(lastError, defaultMessage);
      setError(`${message}（リトライ上限到達）`, {
        code: extractErrorCode(lastError),
        originalError: lastError instanceof Error ? lastError : undefined,
        retryable: false,
      });
      setIsLoading(false);
      return undefined;
    },
    [clearError, defaultMessage, getLogger, setError]
  );

  // 同期処理ラップ
  const handleSync = useCallback(
    <T>(fn: () => T): T | undefined => {
      clearError();
      try {
        return fn();
      } catch (err) {
        const message = extractErrorMessage(err, defaultMessage);
        setError(message, {
          code: extractErrorCode(err),
          originalError: err instanceof Error ? err : undefined,
        });
        return undefined;
      }
    },
    [clearError, defaultMessage, setError]
  );

  return {
    error: errorInfo?.message ?? null,
    errorInfo,
    isError: errorInfo !== null,
    isLoading,
    clearError,
    setError,
    handleAsync,
    handleAsyncWithRetry,
    handleSync,
  };
}


/**
 * AgentFlow Frontend Logger - 統一ロギングユーティリティ.
 *
 * 目的: console.log を置き換え、構造化・制御可能なログを提供
 * 機能:
 *   - 環境に応じたログレベル制御
 *   - 構造化ログ（メタデータ付き）
 *   - 開発/本番モード切替
 *   - エラー報告サービス連携（拡張可能）
 *
 * @example
 * ```tsx
 * import { logger, createLogger } from '@agentflow/ui';
 *
 * // デフォルトロガー使用
 * logger.info('User logged in', { userId: '123' });
 * logger.error('API failed', error, { endpoint: '/api/data' });
 *
 * // カスタムロガー作成
 * const apiLogger = createLogger('api-client');
 * apiLogger.debug('Request sent', { url, method });
 * ```
 */

// ========================================
// 型定義
// ========================================

/** ログレベル */
export type LogLevel = 'debug' | 'info' | 'warn' | 'error' | 'silent';

/** ログレベル優先度 */
const LOG_LEVEL_PRIORITY: Record<LogLevel, number> = {
  debug: 0,
  info: 1,
  warn: 2,
  error: 3,
  silent: 4,
};

/** ログエントリ */
export interface LogEntry {
  timestamp: string;
  level: LogLevel;
  logger: string;
  message: string;
  meta?: Record<string, unknown>;
  error?: Error;
}

/** ロガー設定 */
export interface LoggerConfig {
  /** ログレベル（default: 開発=debug, 本番=warn） */
  level?: LogLevel;
  /** ロガー名 */
  name?: string;
  /** タイムスタンプを含めるか */
  includeTimestamp?: boolean;
  /** 本番環境でログを無効化するか */
  disableInProduction?: boolean;
  /** カスタムハンドラー（エラー報告サービス連携など） */
  onLog?: (entry: LogEntry) => void;
  /** エラーハンドラー */
  onError?: (entry: LogEntry) => void;
}

/** グローバル設定 */
interface GlobalLoggerConfig {
  level: LogLevel;
  disableInProduction: boolean;
  onLog?: (entry: LogEntry) => void;
  onError?: (entry: LogEntry) => void;
}

// ========================================
// グローバル設定
// ========================================

/** 本番環境かどうかを判定（ブラウザ/Node.js 両対応） */
function detectProduction(): boolean {
  // Vite 環境
  try {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const meta = (globalThis as any).import?.meta;
    if (meta?.env?.PROD !== undefined) {
      return Boolean(meta.env.PROD);
    }
  } catch {
    // Vite 環境ではない
  }

  // Node.js 環境
  try {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const proc = (globalThis as any).process;
    if (proc?.env?.NODE_ENV) {
      return proc.env.NODE_ENV === 'production';
    }
  } catch {
    // Node.js 環境ではない
  }

  // デフォルト: 開発環境
  return false;
}

const isProduction = detectProduction();

let globalConfig: GlobalLoggerConfig = {
  level: isProduction ? 'warn' : 'debug',
  disableInProduction: false,
};

/**
 * グローバルログ設定を更新.
 *
 * @param config 設定オプション
 */
export function configureLogger(config: Partial<GlobalLoggerConfig>): void {
  globalConfig = { ...globalConfig, ...config };
}

// ========================================
// Logger クラス
// ========================================

/**
 * AgentFlow ロガー.
 *
 * 構造化ログとレベル制御を提供。
 */
export class Logger {
  private readonly name: string;
  private readonly config: Required<LoggerConfig>;

  constructor(config: LoggerConfig = {}) {
    this.name = config.name ?? 'agentflow';
    this.config = {
      level: config.level ?? globalConfig.level,
      name: this.name,
      includeTimestamp: config.includeTimestamp ?? true,
      disableInProduction: config.disableInProduction ?? globalConfig.disableInProduction,
      onLog: config.onLog ?? globalConfig.onLog ?? (() => {}),
      onError: config.onError ?? globalConfig.onError ?? (() => {}),
    };
  }

  /**
   * ログを出力すべきか判定.
   */
  private shouldLog(level: LogLevel): boolean {
    if (isProduction && this.config.disableInProduction) {
      return false;
    }
    return LOG_LEVEL_PRIORITY[level] >= LOG_LEVEL_PRIORITY[this.config.level];
  }

  /**
   * ログエントリを作成.
   */
  private createEntry(
    level: LogLevel,
    message: string,
    meta?: Record<string, unknown>,
    error?: Error
  ): LogEntry {
    return {
      timestamp: this.config.includeTimestamp ? new Date().toISOString() : '',
      level,
      logger: this.name,
      message,
      meta,
      error,
    };
  }

  /**
   * ログを出力.
   */
  private log(
    level: LogLevel,
    message: string,
    meta?: Record<string, unknown>,
    error?: Error
  ): void {
    if (!this.shouldLog(level)) return;

    const entry = this.createEntry(level, message, meta, error);

    // カスタムハンドラー呼び出し
    this.config.onLog(entry);
    if (level === 'error') {
      this.config.onError(entry);
    }

    // 開発環境でのみコンソール出力
    if (!isProduction) {
      const prefix = `[${this.name}]`;
      const timestamp = this.config.includeTimestamp
        ? `[${entry.timestamp.split('T')[1]?.slice(0, 8) ?? ''}]`
        : '';

      switch (level) {
        case 'debug':
          // eslint-disable-next-line no-console
          console.debug(`${timestamp}${prefix}`, message, meta ?? '');
          break;
        case 'info':
          // eslint-disable-next-line no-console
          console.info(`${timestamp}${prefix}`, message, meta ?? '');
          break;
        case 'warn':
          // eslint-disable-next-line no-console
          console.warn(`${timestamp}${prefix}`, message, meta ?? '');
          break;
        case 'error':
          // eslint-disable-next-line no-console
          console.error(`${timestamp}${prefix}`, message, error ?? meta ?? '');
          break;
      }
    }
  }

  /** デバッグログ */
  debug(message: string, meta?: Record<string, unknown>): void {
    this.log('debug', message, meta);
  }

  /** 情報ログ */
  info(message: string, meta?: Record<string, unknown>): void {
    this.log('info', message, meta);
  }

  /** 警告ログ */
  warn(message: string, meta?: Record<string, unknown>): void {
    this.log('warn', message, meta);
  }

  /** エラーログ */
  error(message: string, error?: Error | unknown, meta?: Record<string, unknown>): void {
    const err = error instanceof Error ? error : undefined;
    const mergedMeta = err ? meta : { ...(meta ?? {}), errorDetail: error };
    this.log('error', message, mergedMeta, err);
  }

  /**
   * 子ロガーを作成（名前空間追加）.
   *
   * @param namespace 追加の名前空間
   */
  child(namespace: string): Logger {
    return new Logger({
      ...this.config,
      name: `${this.name}:${namespace}`,
    });
  }
}

// ========================================
// ファクトリー関数
// ========================================

/**
 * 新しいロガーを作成.
 *
 * @param name ロガー名
 * @param config 追加設定
 */
export function createLogger(name: string, config?: Omit<LoggerConfig, 'name'>): Logger {
  return new Logger({ ...config, name });
}

// ========================================
// デフォルトロガー
// ========================================

/** デフォルトロガーインスタンス */
export const logger = new Logger({ name: 'app' });


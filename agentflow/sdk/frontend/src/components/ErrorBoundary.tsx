/**
 * ErrorBoundary - エラー境界コンポーネント.
 *
 * React コンポーネントツリー内のエラーをキャッチし、
 * フォールバック UI を表示。
 *
 * @example
 * ```tsx
 * <ErrorBoundary
 *   fallback={<ErrorFallback />}
 *   onError={(error, errorInfo) => logError(error)}
 * >
 *   <MyApp />
 * </ErrorBoundary>
 * ```
 */

import React, { Component, ErrorInfo, ReactNode } from 'react';

// ========================================
// 環境検出
// ========================================

/** 開発環境かどうかを判定 */
function isDevelopment(): boolean {
  try {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const proc = (globalThis as any).process;
    if (proc?.env?.NODE_ENV) {
      return proc.env.NODE_ENV === 'development';
    }
  } catch {
    // Node.js 環境ではない
  }
  // デフォルト: 開発環境として扱う（エラー詳細を表示）
  return true;
}

/** 本番環境かどうかを判定 */
function isProduction(): boolean {
  return !isDevelopment();
}

// ========================================
// 型定義
// ========================================

/** Props */
export interface ErrorBoundaryProps {
  children: ReactNode;
  /** カスタムフォールバック */
  fallback?: ReactNode | ((error: Error, reset: () => void) => ReactNode);
  /** エラーコールバック */
  onError?: (error: Error, errorInfo: ErrorInfo) => void;
  /** リセット時コールバック */
  onReset?: () => void;
}

/** State */
interface ErrorBoundaryState {
  hasError: boolean;
  error: Error | null;
}

// ========================================
// デフォルトフォールバック
// ========================================

interface DefaultFallbackProps {
  error: Error;
  onReset: () => void;
}

/**
 * デフォルトエラー表示.
 */
function DefaultErrorFallback({ error, onReset }: DefaultFallbackProps) {
  return (
    <div className="flex flex-col items-center justify-center min-h-[300px] p-8 bg-red-50 rounded-xl border-2 border-red-200">
      <div className="text-6xl mb-4">⚠️</div>
      <h2 className="text-xl font-bold text-red-800 mb-2">エラーが発生しました</h2>
      <p className="text-red-600 mb-4 text-center max-w-md">
        予期しないエラーが発生しました。問題が解決しない場合は、ページを再読み込みしてください。
      </p>
      {isDevelopment() && (
        <details className="mb-4 p-4 bg-red-100 rounded-lg max-w-full overflow-auto">
          <summary className="cursor-pointer text-red-700 font-medium">
            エラー詳細（開発環境のみ）
          </summary>
          <pre className="mt-2 text-sm text-red-800 whitespace-pre-wrap">
            {error.message}
            {'\n'}
            {error.stack}
          </pre>
        </details>
      )}
      <div className="flex gap-3">
        <button
          onClick={onReset}
          className="px-4 py-2 bg-red-600 text-white rounded-lg hover:bg-red-700 transition-colors"
        >
          再試行
        </button>
        <button
          onClick={() => globalThis.location.reload()}
          className="px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors"
        >
          ページ再読み込み
        </button>
      </div>
    </div>
  );
}

// ========================================
// ErrorBoundary 実装
// ========================================

/**
 * エラー境界コンポーネント.
 */
export class ErrorBoundary extends Component<ErrorBoundaryProps, ErrorBoundaryState> {
  constructor(props: ErrorBoundaryProps) {
    super(props);
    this.state = { hasError: false, error: null };
  }

  static getDerivedStateFromError(error: Error): ErrorBoundaryState {
    return { hasError: true, error };
  }

  componentDidCatch(error: Error, errorInfo: ErrorInfo): void {
    this.props.onError?.(error, errorInfo);

    // 本番環境ではエラー報告サービスに送信
    if (isProduction()) {
      // 注: 本番環境では適切なエラー報告サービス（Sentry等）を使用
      // 例: Sentry.captureException(error);
    }
  }

  handleReset = (): void => {
    this.setState({ hasError: false, error: null });
    this.props.onReset?.();
  };

  render(): ReactNode {
    if (this.state.hasError && this.state.error) {
      const { fallback } = this.props;

      // カスタムフォールバック（関数）
      if (typeof fallback === 'function') {
        return fallback(this.state.error, this.handleReset);
      }

      // カスタムフォールバック（コンポーネント）
      if (fallback) {
        return fallback;
      }

      // デフォルトフォールバック
      return <DefaultErrorFallback error={this.state.error} onReset={this.handleReset} />;
    }

    return this.props.children;
  }
}

// ========================================
// withErrorBoundary HOC
// ========================================

/**
 * ErrorBoundary HOC.
 * 
 * @example
 * ```tsx
 * const SafeComponent = withErrorBoundary(MyComponent, {
 *   onError: (error) => logError(error),
 * });
 * ```
 */
export function withErrorBoundary<P extends object>(
  WrappedComponent: React.ComponentType<P>,
  errorBoundaryProps?: Omit<ErrorBoundaryProps, 'children'>
): React.FC<P> {
  const WithErrorBoundary: React.FC<P> = (props) => (
    <ErrorBoundary {...errorBoundaryProps}>
      <WrappedComponent {...props} />
    </ErrorBoundary>
  );

  WithErrorBoundary.displayName = `withErrorBoundary(${WrappedComponent.displayName || WrappedComponent.name || 'Component'})`;

  return WithErrorBoundary;
}


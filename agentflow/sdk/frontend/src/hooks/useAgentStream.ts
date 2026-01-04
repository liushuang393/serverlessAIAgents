/**
 * useAgentStream - SSE ストリーム処理 Hook.
 * 
 * AgentFlow フレームワークの SSE ストリーム処理を抽象化。
 * アプリケーションは接続・再接続・エラー処理を意識せずに利用可能。
 * 
 * @example
 * ```tsx
 * const { agents, isConnected, start, stop } = useAgentStream({
 *   endpoint: '/api/decision/stream',
 *   agents: [
 *     { id: 'dao', name: '道', label: '本質分析' },
 *     { id: 'fa', name: '法', label: '戦略選定' },
 *   ],
 *   onComplete: (result) => console.log('完了:', result),
 * });
 * ```
 */

import { useState, useCallback, useRef, useEffect } from 'react';
import type { AGUIEvent } from '../types/events';

// ========================================
// 型定義
// ========================================

/** Agent 定義 */
export interface AgentDefinition {
  id: string;
  name: string;
  label: string;
  icon?: string;
}

/** Agent 進捗状態 */
export interface AgentProgress {
  id: string;
  name: string;
  label: string;
  icon?: string;
  status: 'waiting' | 'running' | 'completed' | 'failed';
  progress: number;
  message: string;
  result?: Record<string, unknown>;
}

/** ストリーム設定 */
export interface UseAgentStreamConfig<TResult = unknown> {
  /** SSE エンドポイント URL */
  endpoint: string;
  
  /** Agent 定義リスト */
  agents: AgentDefinition[];
  
  /** ベース URL (デフォルト: '') */
  baseUrl?: string;
  
  /** 自動再接続を有効化 (デフォルト: true) */
  autoReconnect?: boolean;
  
  /** 最大再接続回数 (デフォルト: 3) */
  maxReconnectAttempts?: number;
  
  /** 接続タイムアウト ms (デフォルト: 30000) */
  connectionTimeout?: number;
  
  /** 完了時コールバック */
  onComplete?: (result: TResult) => void;
  
  /** エラー時コールバック */
  onError?: (error: string) => void;
  
  /** 接続時コールバック */
  onConnect?: () => void;
}

/** ストリーム状態 */
export interface StreamState<TResult = unknown> {
  isConnected: boolean;
  isComplete: boolean;
  error: string | null;
  isRetryable: boolean;
  retryCount: number;
  agents: AgentProgress[];
  result: TResult | null;
}

/** ストリーム戻り値 */
export interface UseAgentStreamReturn<TResult = unknown, TParams = Record<string, unknown>> extends StreamState<TResult> {
  /** ストリーム開始 */
  start: (params: TParams) => void;
  /** ストリーム停止 */
  stop: () => void;
  /** 再接続試行 */
  retry: () => void;
}

// ========================================
// 定数
// ========================================

const DEFAULT_CONFIG = {
  baseUrl: '',
  autoReconnect: true,
  maxReconnectAttempts: 3,
  connectionTimeout: 30000,
};

// ========================================
// Hook 実装
// ========================================

/**
 * SSE ストリーム処理 Hook.
 * 
 * @param config ストリーム設定
 * @returns ストリーム状態とコントロール関数
 */
export function useAgentStream<TResult = unknown, TParams = Record<string, unknown>>(
  config: UseAgentStreamConfig<TResult>
): UseAgentStreamReturn<TResult, TParams> {
  const {
    endpoint,
    agents: agentDefs,
    baseUrl = DEFAULT_CONFIG.baseUrl,
    autoReconnect = DEFAULT_CONFIG.autoReconnect,
    maxReconnectAttempts = DEFAULT_CONFIG.maxReconnectAttempts,
    connectionTimeout = DEFAULT_CONFIG.connectionTimeout,
    onComplete,
    onError,
    onConnect,
  } = config;

  // 初期 Agent 状態を生成
  const initialAgents: AgentProgress[] = agentDefs.map((def) => ({
    ...def,
    status: 'waiting',
    progress: 0,
    message: '',
  }));

  const [state, setState] = useState<StreamState<TResult>>({
    isConnected: false,
    isComplete: false,
    error: null,
    isRetryable: false,
    retryCount: 0,
    agents: initialAgents,
    result: null,
  });

  const eventSourceRef = useRef<EventSource | null>(null);
  const timeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const lastParamsRef = useRef<TParams | null>(null);

  // ========================================
  // ユーティリティ関数
  // ========================================

  const clearConnectionTimeout = useCallback(() => {
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
      timeoutRef.current = null;
    }
  }, []);

  const updateAgent = useCallback(
    (agentId: string, updates: Partial<AgentProgress>) => {
      setState((prev) => ({
        ...prev,
        agents: prev.agents.map((a) =>
          a.id === agentId ? { ...a, ...updates } : a
        ),
      }));
    },
    []
  );

  // ========================================
  // イベントハンドラー
  // ========================================

  const handleEvent = useCallback(
    (event: AGUIEvent) => {
      switch (event.event_type) {
        case 'flow.start':
          setState((prev) => ({ ...prev, isConnected: true, error: null }));
          onConnect?.();
          break;

        case 'node.start':
          if ('node_id' in event) {
            updateAgent(event.node_id as string, {
              status: 'running',
              progress: 10,
              message: `${(event as { node_name?: string }).node_name || event.node_id} 処理開始...`,
            });
          }
          break;

        case 'progress':
          if ('node_id' in event && 'percentage' in event) {
            updateAgent(event.node_id as string, {
              progress: event.percentage as number,
              message: (event as { message?: string }).message || '',
            });
          }
          break;

        case 'node.complete':
          if ('node_id' in event) {
            updateAgent(event.node_id as string, {
              status: 'completed',
              progress: 100,
              message: '完了',
              result: event.data,
            });
          }
          break;

        case 'node.error':
          if ('node_id' in event) {
            updateAgent(event.node_id as string, {
              status: 'failed',
              message: (event as { error_message?: string }).error_message || 'エラー発生',
            });
          }
          break;

        case 'flow.complete': {
          const result = event.data as TResult;
          setState((prev) => ({
            ...prev,
            isComplete: true,
            result,
          }));
          eventSourceRef.current?.close();
          onComplete?.(result);
          break;
        }

        case 'flow.error':
          setState((prev) => ({
            ...prev,
            error: (event as { error_message?: string }).error_message || 'フロー実行エラー',
            isRetryable: true,
          }));
          eventSourceRef.current?.close();
          onError?.((event as { error_message?: string }).error_message || 'エラー');
          break;
      }
    },
    [updateAgent, onConnect, onComplete, onError]
  );

  const handleConnectionError = useCallback(
    (errorMessage: string, isRetryable: boolean) => {
      clearConnectionTimeout();
      setState((prev) => ({
        ...prev,
        isConnected: false,
        error: errorMessage,
        isRetryable,
      }));
      onError?.(errorMessage);
    },
    [clearConnectionTimeout, onError]
  );

  // ========================================
  // コントロール関数
  // ========================================

  const start = useCallback(
    (params: TParams) => {
      // 既存接続をクローズ
      eventSourceRef.current?.close();
      clearConnectionTimeout();

      // パラメータ保存（再接続用）
      lastParamsRef.current = params;

      // 状態リセット
      setState({
        isConnected: false,
        isComplete: false,
        error: null,
        isRetryable: false,
        retryCount: 0,
        agents: initialAgents,
        result: null,
      });

      // URL 構築
      const searchParams = new URLSearchParams();
      Object.entries(params as Record<string, unknown>).forEach(([key, value]) => {
        if (value !== undefined && value !== null) {
          searchParams.set(key, String(value));
        }
      });

      const url = `${baseUrl}${endpoint}?${searchParams.toString()}`;

      // SSE 接続
      const eventSource = new EventSource(url);
      eventSourceRef.current = eventSource;

      eventSource.onopen = () => {
        clearConnectionTimeout();
        setState((prev) => ({
          ...prev,
          isConnected: true,
          error: null,
          retryCount: 0,
        }));
      };

      eventSource.onmessage = (e) => {
        try {
          const event: AGUIEvent = JSON.parse(e.data);
          handleEvent(event);
        } catch (err) {
          console.warn('[AgentStream] イベントパース失敗:', e.data, err);
        }
      };

      eventSource.onerror = () => {
        const isClosed = eventSource.readyState === EventSource.CLOSED;
        
        if (isClosed) {
          handleConnectionError('サーバーとの接続が切断されました。', autoReconnect);
        } else {
          handleConnectionError(
            'サーバーに接続できません。バックエンドが起動しているか確認してください。',
            true
          );
        }
      };

      // タイムアウト設定
      timeoutRef.current = setTimeout(() => {
        if (!state.isConnected && !state.isComplete) {
          eventSource.close();
          handleConnectionError('接続がタイムアウトしました。再試行してください。', true);
        }
      }, connectionTimeout);
    },
    [
      baseUrl,
      endpoint,
      initialAgents,
      connectionTimeout,
      autoReconnect,
      clearConnectionTimeout,
      handleEvent,
      handleConnectionError,
      state.isConnected,
      state.isComplete,
    ]
  );

  const stop = useCallback(() => {
    clearConnectionTimeout();
    eventSourceRef.current?.close();
    eventSourceRef.current = null;
    lastParamsRef.current = null;
    setState((prev) => ({ ...prev, isConnected: false }));
  }, [clearConnectionTimeout]);

  const retry = useCallback(() => {
    const params = lastParamsRef.current;
    if (!params || state.retryCount >= maxReconnectAttempts) {
      return;
    }

    setState((prev) => ({
      ...prev,
      retryCount: prev.retryCount + 1,
      error: null,
    }));

    start(params);
  }, [state.retryCount, maxReconnectAttempts, start]);

  // クリーンアップ
  useEffect(() => {
    return () => {
      clearConnectionTimeout();
      eventSourceRef.current?.close();
    };
  }, [clearConnectionTimeout]);

  return {
    ...state,
    start,
    stop,
    retry,
  };
}


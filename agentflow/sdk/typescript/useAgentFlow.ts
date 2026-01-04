/**
 * AgentFlow React Hook.
 *
 * 目的: SSE ストリーム接続・Agent 状態管理を簡単に
 * 依存: React 18+
 *
 * 使用例:
 *   import { useAgentFlow } from '@agentflow/sdk';
 *
 *   function MyComponent() {
 *     const {
 *       agents,
 *       isConnected,
 *       isComplete,
 *       error,
 *       result,
 *       startFlow,
 *       stopFlow,
 *     } = useAgentFlow({
 *       baseUrl: 'http://localhost:8000',
 *       endpoint: '/api/decision/stream',
 *       flowId: 'decision-engine',
 *     });
 *
 *     return (
 *       <button onClick={() => startFlow({ question: '...' })}>
 *         分析開始
 *       </button>
 *     );
 *   }
 */

import { useState, useCallback, useRef, useEffect } from 'react';
import type {
  AGUIEvent,
  AgentProgress,
  FlowDefinition,
  FlowResult,
} from './agentflow-client';
import { AgentFlowClient } from './agentflow-client';

/** Hook 設定 */
export interface UseAgentFlowConfig {
  baseUrl?: string;
  endpoint: string;
  flowId?: string;
  timeout?: number;
  maxRetries?: number;
  autoLoadDefinition?: boolean;
}

/** Hook 状態 */
export interface AgentFlowState {
  isConnected: boolean;
  isComplete: boolean;
  isLoading: boolean;
  error: string | null;
  agents: AgentProgress[];
  resultId: string | null;
  result: FlowResult | null;
  definition: FlowDefinition | null;
}

/** Hook 戻り値 */
export interface UseAgentFlowReturn extends AgentFlowState {
  startFlow: (params: Record<string, string | number | undefined>) => void;
  stopFlow: () => void;
  retry: () => void;
  loadDefinition: () => Promise<void>;
}

/**
 * AgentFlow React Hook.
 */
export function useAgentFlow(config: UseAgentFlowConfig): UseAgentFlowReturn {
  const client = useRef(new AgentFlowClient(config.baseUrl));
  const eventSourceRef = useRef<EventSource | null>(null);
  const lastParamsRef = useRef<Record<string, string | number | undefined> | null>(null);

  const [state, setState] = useState<AgentFlowState>({
    isConnected: false,
    isComplete: false,
    isLoading: false,
    error: null,
    agents: [],
    resultId: null,
    result: null,
    definition: null,
  });

  /** Flow 定義を読み込み */
  const loadDefinition = useCallback(async () => {
    if (!config.flowId) return;
    try {
      const def = await client.current.getFlowDefinition(config.flowId);
      const agents: AgentProgress[] = def.agents.map((a) => ({
        ...a,
        status: 'waiting',
        progress: 0,
        message: '',
      }));
      setState((prev) => ({ ...prev, definition: def, agents }));
    } catch (err) {
      setState((prev) => ({ ...prev, error: `定義読み込み失敗: ${err}` }));
    }
  }, [config.flowId]);

  /** Agent 状態を更新 */
  const updateAgent = useCallback((agentId: string, updates: Partial<AgentProgress>) => {
    setState((prev) => ({
      ...prev,
      agents: prev.agents.map((a) =>
        a.id === agentId ? { ...a, ...updates } : a
      ),
    }));
  }, []);

  /** イベントハンドラー */
  const handleEvent = useCallback((event: AGUIEvent) => {
    switch (event.event_type) {
      case 'flow.start':
        setState((prev) => ({ ...prev, isConnected: true }));
        break;
      case 'node.start':
        if (event.node_id) {
          updateAgent(event.node_id, { status: 'running', progress: 10, message: '処理中...' });
        }
        break;
      case 'progress':
        if (event.node_id && event.percentage !== undefined) {
          updateAgent(event.node_id, { progress: event.percentage, message: event.message || '' });
        }
        break;
      case 'node.complete':
        if (event.node_id) {
          updateAgent(event.node_id, {
            status: 'completed',
            progress: 100,
            message: '完了',
            result: event.data as Record<string, unknown>,
          });
        }
        break;
      case 'node.error':
        if (event.node_id) {
          updateAgent(event.node_id, { status: 'failed', message: event.message || 'エラー' });
        }
        break;
      case 'flow.complete':
        setState((prev) => ({
          ...prev,
          isComplete: true,
          resultId: event.result_id || null,
          result: event.result ? (event.result as unknown as FlowResult) : null,
        }));
        eventSourceRef.current?.close();
        break;
      case 'flow.error':
        setState((prev) => ({
          ...prev,
          error: event.error_message || event.message || 'フローエラー',
        }));
        eventSourceRef.current?.close();
        break;
    }
  }, [updateAgent]);

  /** ストリーム開始 */
  const startFlow = useCallback((params: Record<string, string | number | undefined>) => {
    eventSourceRef.current?.close();
    lastParamsRef.current = params;

    // 状態リセット
    setState((prev) => ({
      ...prev,
      isConnected: false,
      isComplete: false,
      error: null,
      resultId: null,
      result: null,
      agents: prev.agents.map((a) => ({ ...a, status: 'waiting', progress: 0, message: '' })),
    }));

    eventSourceRef.current = client.current.streamFlow(config.endpoint, params, {
      onEvent: handleEvent,
      onError: (err) => setState((prev) => ({ ...prev, error: err })),
    });
  }, [config.endpoint, handleEvent]);

  /** ストリーム停止 */
  const stopFlow = useCallback(() => {
    eventSourceRef.current?.close();
    eventSourceRef.current = null;
    setState((prev) => ({ ...prev, isConnected: false }));
  }, []);

  /** リトライ */
  const retry = useCallback(() => {
    if (lastParamsRef.current) {
      startFlow(lastParamsRef.current);
    }
  }, [startFlow]);

  // 初回読み込み
  useEffect(() => {
    if (config.autoLoadDefinition && config.flowId) {
      loadDefinition();
    }
  }, [config.autoLoadDefinition, config.flowId, loadDefinition]);

  // クリーンアップ
  useEffect(() => {
    return () => eventSourceRef.current?.close();
  }, []);

  return {
    ...state,
    startFlow,
    stopFlow,
    retry,
    loadDefinition,
  };
}


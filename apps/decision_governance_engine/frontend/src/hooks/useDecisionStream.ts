/**
 * SSE ストリーム用カスタムフック.
 *
 * 目的: AG-UI イベントのリアルタイム受信・状態管理
 * 使用場所: 進捗画面（ProcessingPage）
 * 
 * 機能:
 *   - 自動再接続（最大3回）
 *   - 接続タイムアウト検出
 *   - 詳細エラーハンドリング
 */

import { useState, useCallback, useRef, useEffect } from 'react';
import { decisionApi } from '../api/client';
import type { AGUIEvent, DecisionReport } from '../types';

/** 再接続設定 */
const RECONNECT_CONFIG = {
  maxRetries: 3,
  baseDelay: 2000,
  maxDelay: 10000,
};

/** 接続タイムアウト（30秒） */
const CONNECTION_TIMEOUT = 30000;

/** Ref で最新状態を追跡するためのヘルパー */
const useLatestRef = <T,>(value: T) => {
  const ref = useRef(value);
  ref.current = value;
  return ref;
};

/** Agent 進捗状態 */
export interface AgentProgress {
  id: string;
  name: string;
  label: string;
  status: 'waiting' | 'running' | 'completed' | 'failed';
  progress: number;
  message: string;
  result?: Record<string, unknown>;
}

/** LLM思考ログ */
export interface ThinkingLog {
  timestamp: number;
  agentId: string;
  agentName: string;
  content: string;
}

/** ストリーム状態 */
export interface StreamState {
  isConnected: boolean;
  isComplete: boolean;
  error: string | null;
  isRetryable: boolean;
  retryCount: number;
  agents: AgentProgress[];
  report: DecisionReport | null;
  thinkingLogs: ThinkingLog[];
}

/** 初期 Agent 状態（認知前処理・門番・診断・道・法・術・器・検証の8 Agent） */
const initialAgents: AgentProgress[] = [
  { id: 'cognitive_gate', name: '認知', label: '認知前処理', status: 'waiting', progress: 0, message: '' },
  { id: 'gatekeeper', name: '門番', label: '入口検証', status: 'waiting', progress: 0, message: '' },
  { id: 'clarification', name: '診断', label: '問題診断', status: 'waiting', progress: 0, message: '' },
  { id: 'dao', name: '道', label: '本質分析', status: 'waiting', progress: 0, message: '' },
  { id: 'fa', name: '法', label: '戦略選定', status: 'waiting', progress: 0, message: '' },
  { id: 'shu', name: '術', label: '実行計画', status: 'waiting', progress: 0, message: '' },
  { id: 'qi', name: '器', label: '技術実装', status: 'waiting', progress: 0, message: '' },
  { id: 'review', name: '検証', label: '最終検証', status: 'waiting', progress: 0, message: '' },
];

/**
 * Decision SSE ストリームフック.
 */
export function useDecisionStream() {
  const [state, setState] = useState<StreamState>({
    isConnected: false,
    isComplete: false,
    error: null,
    isRetryable: false,
    retryCount: 0,
    agents: [...initialAgents],
    report: null,
    thinkingLogs: [],
  });

  // 最新の state を ref で追跡（stale closure 回避）
  const stateRef = useLatestRef(state);

  const eventSourceRef = useRef<EventSource | null>(null);
  const timeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const lastParamsRef = useRef<{question: string; budget?: number; timeline?: number} | null>(null);

  /** Agent 状態を更新 */
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

  /** 思考ログを追加 */
  const addThinkingLog = useCallback(
    (agentId: string, agentName: string, content: string) => {
      setState((prev) => ({
        ...prev,
        thinkingLogs: [
          ...prev.thinkingLogs,
          { timestamp: Date.now(), agentId, agentName, content },
        ],
      }));
    },
    []
  );

  /** AG-UI イベントハンドラー */
  const handleEvent = useCallback(
    (event: AGUIEvent) => {
      // 統一フォーマット: type → event_type に正規化
      const eventType = event.event_type || (event as unknown as { type?: string }).type;
      console.log('[useDecisionStream] handleEvent 受信:', eventType, event.node_id || '', JSON.stringify(event).slice(0, 200));
      
      // type フィールドのイベントを処理（PipelineEngine形式）
      if (!event.event_type && (event as unknown as { type?: string }).type) {
        const legacyEvent = event as unknown as { type: string; data?: Record<string, unknown> };
        switch (legacyEvent.type) {
          case 'progress':
            // {type: "progress", data: {stage: "xxx", progress: 12.5}}
            if (legacyEvent.data) {
              const stage = legacyEvent.data.stage as string;
              const progress = legacyEvent.data.progress as number;
              if (stage) {
                updateAgent(stage, { progress, message: `${progress}% 完了` });
                addThinkingLog(stage, stage, `進捗: ${progress}%`);
              }
            }
            return;
          case 'result':
            // {type: "result", data: {status: "xxx", results: {...}}}
            if (legacyEvent.data) {
              const status = legacyEvent.data.status as string;
              const results = legacyEvent.data.results as Record<string, unknown>;
              if (status === 'rejected') {
                setState((prev) => ({
                  ...prev,
                  isComplete: true,
                  error: '入力が拒否されました。質問を修正してください。',
                }));
                addThinkingLog('system', 'System', '❌ 入力が拒否されました');
              } else if (results) {
                // 成功結果を構築
                setState((prev) => ({
                  ...prev,
                  isComplete: true,
                  report: results as unknown as DecisionReport,
                }));
                addThinkingLog('system', 'System', '✅ 分析完了');
              }
            }
            return;
          case 'gate_rejected':
            // ゲートで拒否された
            addThinkingLog('system', 'System', '⚠️ ゲートチェックで処理が停止しました');
            return;
        }
      }
      
      switch (event.event_type) {
        case 'connection.established':
          // 接続確認イベント（サーバーから即座に送信される）
          console.log('[useDecisionStream] 接続確認イベント受信');
          setState((prev) => ({
            ...prev,
            isConnected: true,
          }));
          addThinkingLog('system', 'System', '🔗 サーバーに接続しました');
          break;

        case 'flow.start':
          // 接続開始時、最初のagentをrunning状態に
          setState((prev) => ({
            ...prev,
            isConnected: true,
            agents: prev.agents.map((a, i) =>
              i === 0 ? { ...a, status: 'running' as const, progress: 10, message: '処理開始...' } : a
            ),
          }));
          break;

        case 'node.start':
          if (event.node_id) {
            updateAgent(event.node_id, {
              status: 'running',
              progress: 10,
              message: `${event.node_name || event.node_id} 処理開始...`,
            });
            // ログ追加
            addThinkingLog(event.node_id, event.node_name || event.node_id, '処理を開始しました');
          }
          break;

        case 'progress':
          {
            const nodeId = event.node_id || (event.data as Record<string, unknown>)?.node_id;
            const message = event.message || (event.data as Record<string, unknown>)?.message || '';
            if (nodeId && event.percentage !== undefined) {
              updateAgent(nodeId as string, {
                progress: event.percentage,
                message: message as string,
              });
              // ログ追加
              if (message) {
                const agent = initialAgents.find(a => a.id === nodeId);
                addThinkingLog(nodeId as string, agent?.name || nodeId as string, message as string);
              }
            }
          }
          break;

        case 'log':
          // LLM思考ログイベント
          {
            const nodeId = event.node_id || 'system';
            const nodeName = event.node_name || 'System';
            const content = event.message || (event.data as Record<string, unknown>)?.content || '';
            if (content) {
              addThinkingLog(nodeId, nodeName, content as string);
            }
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
            // ログ追加
            const agent = initialAgents.find(a => a.id === event.node_id);
            addThinkingLog(event.node_id, agent?.name || event.node_id, '✓ 分析完了');
          }
          break;

        case 'node.error':
          if (event.node_id) {
            // NodeErrorEvent は error_message フィールドを使用
            const errorMsg = event.error_message || event.message || 'エラー発生';
            updateAgent(event.node_id, {
              status: 'failed',
              message: errorMsg,
            });
            // ログ追加
            const agent = initialAgents.find(a => a.id === event.node_id);
            addThinkingLog(event.node_id, agent?.name || event.node_id, `❌ ${errorMsg}`);
          }
          break;

        case 'flow.complete':
          setState((prev) => ({
            ...prev,
            isComplete: true,
            report: (event.result as unknown as DecisionReport) || null,
          }));
          addThinkingLog('system', 'System', '✅ 全分析が完了しました');
          eventSourceRef.current?.close();
          break;

        case 'flow.error':
          // FlowErrorEvent は error_message フィールドを使用
          setState((prev) => ({
            ...prev,
            error: event.error_message || event.message || 'フロー実行エラー',
          }));
          addThinkingLog('system', 'System', `🚨 ${event.error_message || event.message || 'フロー実行エラー'}`);
          eventSourceRef.current?.close();
          break;

      }
    },
    [updateAgent, addThinkingLog]
  );

  /** タイムアウトをクリア */
  const clearConnectionTimeout = useCallback(() => {
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
      timeoutRef.current = null;
    }
  }, []);

  /** 接続タイムアウト設定 */
  const setConnectionTimeout = useCallback(() => {
    clearConnectionTimeout();
    timeoutRef.current = setTimeout(() => {
      // stateRef を使用して最新の状態を参照（stale closure 回避）
      const currentState = stateRef.current;
      console.log('[useDecisionStream] タイムアウトチェック:', {
        isConnected: currentState.isConnected,
        isComplete: currentState.isComplete
      });
      if (!currentState.isConnected && !currentState.isComplete) {
        console.log('[useDecisionStream] タイムアウト発火 - 接続をクローズ');
        eventSourceRef.current?.close();
        setState((prev) => ({
          ...prev,
          error: '接続がタイムアウトしました。再試行してください。',
          isRetryable: true,
        }));
      }
    }, CONNECTION_TIMEOUT);
  }, [clearConnectionTimeout]);

  /** SSE 接続成功ハンドラー */
  const handleOpen = useCallback(() => {
    console.log('[useDecisionStream] handleOpen - 接続成功！');
    clearConnectionTimeout();
    setState((prev) => {
      console.log('[useDecisionStream] setState: isConnected = true');
      return {
        ...prev,
        isConnected: true,
        error: null,
        retryCount: 0,
      };
    });
  }, [clearConnectionTimeout]);

  /** SSE 接続エラーハンドラー */
  const handleError = useCallback((errorMessage: string, isRetryable?: boolean) => {
    console.log('[useDecisionStream] handleError:', errorMessage);
    clearConnectionTimeout();
    setState((prev) => {
      console.log('[useDecisionStream] setState: isConnected = false, error =', errorMessage);
      return {
        ...prev,
        isConnected: false,
        error: errorMessage,
        isRetryable: isRetryable ?? false,
      };
    });
  }, [clearConnectionTimeout]);

  /** 自動再接続 */
  const attemptReconnect = useCallback(() => {
    const params = lastParamsRef.current;
    const currentRetryCount = stateRef.current.retryCount;
    if (!params || currentRetryCount >= RECONNECT_CONFIG.maxRetries) {
      return;
    }

    const delay = Math.min(
      RECONNECT_CONFIG.baseDelay * Math.pow(2, currentRetryCount),
      RECONNECT_CONFIG.maxDelay
    );

    setTimeout(() => {
      setState((prev) => ({ ...prev, retryCount: prev.retryCount + 1, error: null }));

      eventSourceRef.current?.close();
      eventSourceRef.current = decisionApi.streamDecision(
        params.question,
        params.budget,
        params.timeline,
        handleEvent,
        handleError,
        handleOpen
      );
      setConnectionTimeout();
    }, delay);
  }, [handleEvent, handleError, handleOpen, setConnectionTimeout]);

  /** ストリーム開始 */
  const startStream = useCallback(
    (question: string, budget?: number, timelineMonths?: number) => {
      console.log('🔘 [STEP4] startStream() 開始', { 
        question: question?.slice(0, 50), 
        budget, 
        timelineMonths,
        existingConnection: eventSourceRef.current?.readyState 
      });
      
      // 既に接続中の場合はスキップ（React Strict Mode 対策）
      if (eventSourceRef.current && eventSourceRef.current.readyState !== EventSource.CLOSED) {
        console.log('🔘 [STEP4] ⚠️ 既存接続あり、スキップ readyState=', eventSourceRef.current.readyState);
        return;
      }

      // 既存接続をクローズ
      eventSourceRef.current?.close();
      clearConnectionTimeout();

      // パラメータ保存（再接続用）
      lastParamsRef.current = { question, budget, timeline: timelineMonths };

      // 状態リセット（最初のagentをrunning状態に）
      const startingAgents = initialAgents.map((a, i) =>
        i === 0 ? { ...a, status: 'running' as const, progress: 5, message: '接続中...' } : { ...a }
      );
      setState({
        isConnected: false,
        isComplete: false,
        error: null,
        isRetryable: false,
        retryCount: 0,
        agents: startingAgents,
        report: null,
        thinkingLogs: [{ timestamp: Date.now(), agentId: 'system', agentName: 'System', content: '🚀 分析を開始します...' }],
      });

      console.log('🔘 [STEP4] → decisionApi.streamDecision() を呼び出し');

      // SSE 接続開始
      eventSourceRef.current = decisionApi.streamDecision(
        question,
        budget,
        timelineMonths,
        handleEvent,
        handleError,
        handleOpen
      );
      
      console.log('🔘 [STEP4] EventSource 作成完了, readyState=', eventSourceRef.current?.readyState);

      // タイムアウト設定
      setConnectionTimeout();
    },
    [handleEvent, handleError, handleOpen, clearConnectionTimeout, setConnectionTimeout]
  );

  /** ストリーム停止 */
  const stopStream = useCallback(() => {
    console.log('[SSE] ストリーム停止');
    clearConnectionTimeout();
    if (eventSourceRef.current) {
      eventSourceRef.current.close();
      eventSourceRef.current = null;
    }
    lastParamsRef.current = null;
    setState((prev) => ({ ...prev, isConnected: false }));
  }, [clearConnectionTimeout]);

  /** クリーンアップ */
  useEffect(() => {
    return () => {
      clearConnectionTimeout();
      eventSourceRef.current?.close();
    };
  }, [clearConnectionTimeout]);

  return {
    ...state,
    startStream,
    stopStream,
    attemptReconnect,
  };
}


/**
 * SSE ストリーム用カスタムフック.
 *
 * 目的: AG-UI イベントのリアルタイム受信・状態管理
 * 使用場所: 進捗画面（ProcessingPage）
 */

import { useState, useCallback, useRef, useEffect } from 'react';
import { decisionApi } from '../api/client';
import type { AGUIEvent, DecisionReport } from '../types';

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

/** ストリーム状態 */
export interface StreamState {
  isConnected: boolean;
  isComplete: boolean;
  error: string | null;
  agents: AgentProgress[];
  report: DecisionReport | null;
}

/** 初期 Agent 状態 */
const initialAgents: AgentProgress[] = [
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
    agents: [...initialAgents],
    report: null,
  });

  const eventSourceRef = useRef<EventSource | null>(null);

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

  /** AG-UI イベントハンドラー */
  const handleEvent = useCallback(
    (event: AGUIEvent) => {
      switch (event.event_type) {
        case 'flow.start':
          setState((prev) => ({ ...prev, isConnected: true }));
          break;

        case 'node.start':
          if (event.node_id) {
            updateAgent(event.node_id, {
              status: 'running',
              progress: 10,
              message: `${event.node_name || event.node_id} 処理開始...`,
            });
          }
          break;

        case 'progress':
          if (event.node_id && event.percentage !== undefined) {
            updateAgent(event.node_id, {
              progress: event.percentage,
              message: event.message || '',
            });
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
            updateAgent(event.node_id, {
              status: 'failed',
              message: event.message || 'エラー発生',
            });
          }
          break;

        case 'flow.complete':
          setState((prev) => ({
            ...prev,
            isComplete: true,
            report: event.data as unknown as DecisionReport,
          }));
          eventSourceRef.current?.close();
          break;

        case 'flow.error':
          setState((prev) => ({
            ...prev,
            error: event.message || 'フロー実行エラー',
          }));
          eventSourceRef.current?.close();
          break;
      }
    },
    [updateAgent]
  );

  /** ストリーム開始 */
  const startStream = useCallback(
    (question: string, budget?: number, timelineMonths?: number) => {
      // 既存接続をクローズ
      eventSourceRef.current?.close();

      // 状態リセット
      setState({
        isConnected: false,
        isComplete: false,
        error: null,
        agents: [...initialAgents],
        report: null,
      });

      // SSE 接続開始
      eventSourceRef.current = decisionApi.streamDecision(
        question,
        budget,
        timelineMonths,
        handleEvent
      );
    },
    [handleEvent]
  );

  /** ストリーム停止 */
  const stopStream = useCallback(() => {
    eventSourceRef.current?.close();
    eventSourceRef.current = null;
    setState((prev) => ({ ...prev, isConnected: false }));
  }, []);

  /** クリーンアップ */
  useEffect(() => {
    return () => {
      eventSourceRef.current?.close();
    };
  }, []);

  return {
    ...state,
    startStream,
    stopStream,
  };
}


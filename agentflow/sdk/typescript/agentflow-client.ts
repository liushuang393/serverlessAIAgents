/**
 * AgentFlow TypeScript SDK.
 *
 * 目的: 前後端統一のSSE接続・状態管理を提供
 * 使用場所: React/Vue/Svelte等のフロントエンド
 *
 * 使用例:
 *   import { AgentFlowClient, useAgentFlow } from '@agentflow/sdk';
 *
 *   // 方式1: 直接使用
 *   const client = new AgentFlowClient('http://localhost:8000');
 *   const stream = client.streamFlow('decision', { question: '...' });
 *   for await (const event of stream) {
 *     console.log(event);
 *   }
 *
 *   // 方式2: React Hook
 *   const { startFlow, agents, isComplete, result } = useAgentFlow('decision');
 */

// ============================================
// 型定義（後端と自動同期）
// ============================================

/** Agent 状態 */
export type AgentStatus = 'waiting' | 'running' | 'completed' | 'failed';

/** AG-UI イベントタイプ */
export type AGUIEventType =
  | 'flow.start'
  | 'flow.complete'
  | 'flow.error'
  | 'flow.cancel'
  | 'node.start'
  | 'node.complete'
  | 'node.error'
  | 'progress'
  | 'log';

/** AG-UI イベント基底型 */
export interface AGUIEvent {
  event_type: AGUIEventType;
  timestamp: number;
  flow_id: string;
  data?: Record<string, unknown>;
  node_id?: string;
  node_name?: string;
  percentage?: number;
  message?: string;
  error_message?: string;
  result_id?: string;
  result?: Record<string, unknown>;
}

/** Agent 定義 */
export interface AgentDefinition {
  id: string;
  name: string;
  label: string;
  icon: string;
  description?: string;
}

/** Agent 進捗状態 */
export interface AgentProgress extends AgentDefinition {
  status: AgentStatus;
  progress: number;
  message: string;
  result?: Record<string, unknown>;
}

/** Flow 定義 */
export interface FlowDefinition {
  flow_id: string;
  name: string;
  version: string;
  description: string;
  agents: AgentDefinition[];
}

/** Flow 結果 */
export interface FlowResult {
  result_id: string;
  flow_id: string;
  status: string;
  data: Record<string, unknown>;
  created_at: string;
  metadata: Record<string, unknown>;
}

// ============================================
// AgentFlow クライアント
// ============================================

/** SSE 接続設定 */
export interface StreamConfig {
  timeout?: number;
  maxRetries?: number;
  retryDelay?: number;
  onEvent?: (event: AGUIEvent) => void;
  onError?: (error: string) => void;
  onComplete?: (result: FlowResult | null) => void;
}

/**
 * AgentFlow API クライアント.
 *
 * SSE ストリーム接続と REST API 呼び出しを統合管理。
 */
export class AgentFlowClient {
  private baseUrl: string;

  constructor(baseUrl: string = 'http://localhost:8000') {
    this.baseUrl = baseUrl;
  }

  /**
   * Flow 定義を取得（前端同期用）.
   */
  async getFlowDefinition(flowId: string): Promise<FlowDefinition> {
    const res = await fetch(`${this.baseUrl}/api/flows/${flowId}/definition`);
    if (!res.ok) throw new Error(`Failed to get flow definition: ${res.statusText}`);
    return res.json();
  }

  /**
   * 結果を取得.
   */
  async getResult(resultId: string): Promise<FlowResult> {
    const res = await fetch(`${this.baseUrl}/api/results/${resultId}`);
    if (!res.ok) throw new Error(`Failed to get result: ${res.statusText}`);
    return res.json();
  }

  /**
   * SSE ストリームを開始.
   */
  streamFlow(
    endpoint: string,
    params: Record<string, string | number | undefined>,
    config: StreamConfig = {}
  ): EventSource {
    const searchParams = new URLSearchParams();
    Object.entries(params).forEach(([k, v]) => {
      if (v !== undefined) searchParams.set(k, String(v));
    });

    const url = `${this.baseUrl}${endpoint}?${searchParams}`;
    const eventSource = new EventSource(url);

    eventSource.onmessage = (e) => {
      try {
        const event: AGUIEvent = JSON.parse(e.data);
        config.onEvent?.(event);
      } catch {
        // パースエラー無視
      }
    };

    eventSource.onerror = () => {
      eventSource.close();
      config.onError?.('接続エラー');
    };

    return eventSource;
  }
}

/** デフォルトクライアント */
export const agentFlowClient = new AgentFlowClient();


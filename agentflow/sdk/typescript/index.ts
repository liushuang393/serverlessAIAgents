/**
 * AgentFlow TypeScript SDK - 統一エクスポート.
 *
 * 使用例:
 *   import {
 *     AgentFlowClient,
 *     useAgentFlow,
 *     type AGUIEvent,
 *     type AgentProgress,
 *   } from '@agentflow/sdk';
 */

// クライアント
export { AgentFlowClient, agentFlowClient } from './agentflow-client';

// React Hook
export { useAgentFlow } from './useAgentFlow';
export type { UseAgentFlowConfig, UseAgentFlowReturn, AgentFlowState } from './useAgentFlow';

// 型定義
export type {
  AgentStatus,
  AGUIEventType,
  AGUIEvent,
  AgentDefinition,
  AgentProgress,
  FlowDefinition,
  FlowResult,
  StreamConfig,
} from './agentflow-client';


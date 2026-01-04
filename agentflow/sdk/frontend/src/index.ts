/**
 * @agentflow/ui - AgentFlow Frontend SDK.
 * 
 * React Hooks & Components for AI Agent Applications.
 * 
 * @packageDocumentation
 * 
 * @example
 * ```tsx
 * import {
 *   useAgentStream,
 *   createAgentStore,
 *   AgentApiClient,
 *   AgentProgress,
 *   Notification,
 *   ErrorBoundary,
 * } from '@agentflow/ui';
 * 
 * // SSE ストリーム
 * const { agents, start, isComplete } = useAgentStream({
 *   endpoint: '/api/decision/stream',
 *   agents: [{ id: 'dao', name: '道', label: '本質分析' }],
 * });
 * 
 * // Store 作成
 * const useMyStore = createAgentStore({
 *   name: 'my-app',
 *   initialState: { question: '' },
 * });
 * 
 * // API クライアント
 * const api = new AgentApiClient({ baseUrl: '/api' });
 * ```
 */

// ========================================
// Hooks
// ========================================

export {
  useAgentStream,
  type UseAgentStreamConfig,
  type UseAgentStreamReturn,
  type AgentDefinition,
  type AgentProgress as AgentProgressState,
  type StreamState,
} from './hooks/useAgentStream';

// ========================================
// Store
// ========================================

export {
  createAgentStore,
  type CreateAgentStoreConfig,
  type BaseAgentState,
  type BaseAgentActions,
  type PageState,
  type HistoryItem,
} from './store/createAgentStore';

// ========================================
// API Client
// ========================================

export {
  AgentApiClient,
  AgentApiError,
  type AgentApiClientConfig,
  type AgentApiErrorCode,
  type RetryConfig,
} from './api/AgentApiClient';

// ========================================
// Components
// ========================================

export {
  AgentProgress,
  type AgentProgressProps,
  type AgentProgressItem,
  type AgentStatus,
  type ProgressVariant,
  type ProgressTheme,
} from './components/AgentProgress';

export {
  Notification,
  NotificationProvider,
  useNotification,
  type NotificationProps,
  type NotificationType,
  type NotificationItem,
} from './components/Notification';

export {
  ErrorBoundary,
  withErrorBoundary,
  type ErrorBoundaryProps,
} from './components/ErrorBoundary';

// ========================================
// Types (AG-UI Events)
// ========================================

export type {
  AGUIEventType,
  AGUIEvent,
  FlowStartEvent,
  FlowCompleteEvent,
  FlowErrorEvent,
  NodeStartEvent,
  NodeCompleteEvent,
  NodeErrorEvent,
  ProgressEvent,
  LogEvent,
} from './types/events';

export {
  isFlowStartEvent,
  isFlowCompleteEvent,
  isFlowErrorEvent,
  isNodeStartEvent,
  isNodeCompleteEvent,
  isNodeErrorEvent,
  isProgressEvent,
} from './types/events';


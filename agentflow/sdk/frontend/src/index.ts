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
 *   // Hooks
 *   useAgentStream,
 *   useErrorHandler,
 *
 *   // Store
 *   createAgentStore,
 *
 *   // API
 *   AgentApiClient,
 *
 *   // Components
 *   AgentProgress,
 *   Notification,
 *   ErrorBoundary,
 *
 *   // Utils
 *   logger,
 *   createLogger,
 * } from '@agentflow/ui';
 *
 * // SSE ストリーム
 * const { agents, start, isComplete } = useAgentStream({
 *   endpoint: '/api/decision/stream',
 *   agents: [{ id: 'dao', name: '道', label: '本質分析' }],
 * });
 *
 * // エラーハンドリング
 * const { error, handleAsync, isLoading } = useErrorHandler();
 * const handleSubmit = handleAsync(async () => {
 *   await api.post('/data', payload);
 * });
 *
 * // ログ
 * logger.info('User action', { userId: '123' });
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

export {
  useErrorHandler,
  type UseErrorHandlerConfig,
  type UseErrorHandlerReturn,
  type ErrorInfo,
} from './hooks/useErrorHandler';

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

// ========================================
// Utils
// ========================================

export {
  Logger,
  logger,
  createLogger,
  configureLogger,
  type LogLevel,
  type LogEntry,
  type LoggerConfig,
} from './utils/logger';

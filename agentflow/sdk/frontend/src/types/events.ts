/**
 * AG-UI イベント型定義.
 * 
 * AgentFlow フレームワークの AG-UI プロトコルイベント型。
 * 全てのアプリケーションで共通利用。
 */

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
  | 'log'
  | 'clarification.required'
  | 'clarification.received'
  | 'approval_required'
  | 'approval_submitted'
  | 'approval_timeout'
  | 'a2ui.component'
  | 'a2ui.update'
  | 'a2ui.clear';

/** AG-UI 基底イベント */
export interface AGUIEvent {
  event_type: AGUIEventType;
  timestamp: number;
  flow_id: string;
  data?: Record<string, unknown>;
  [key: string]: unknown;
}

/** フロー開始イベント */
export interface FlowStartEvent extends AGUIEvent {
  event_type: 'flow.start';
}

/** フロー完了イベント */
export interface FlowCompleteEvent extends AGUIEvent {
  event_type: 'flow.complete';
  result_id?: string;
  result?: Record<string, unknown>;
}

/** フローエラーイベント */
export interface FlowErrorEvent extends AGUIEvent {
  event_type: 'flow.error';
  error_message: string;
  error_type: string;
}

/** ノード開始イベント */
export interface NodeStartEvent extends AGUIEvent {
  event_type: 'node.start';
  node_id: string;
  node_name: string;
}

/** ノード完了イベント */
export interface NodeCompleteEvent extends AGUIEvent {
  event_type: 'node.complete';
  node_id: string;
  node_name: string;
}

/** ノードエラーイベント */
export interface NodeErrorEvent extends AGUIEvent {
  event_type: 'node.error';
  node_id: string;
  node_name: string;
  error_message: string;
  error_type: string;
}

/** プログレスイベント */
export interface ProgressEvent extends AGUIEvent {
  event_type: 'progress';
  node_id?: string;
  current: number;
  total: number;
  percentage: number;
  message?: string;
}

/** ログイベント */
export interface LogEvent extends AGUIEvent {
  event_type: 'log';
  level: 'DEBUG' | 'INFO' | 'WARNING' | 'ERROR';
  message: string;
  source?: string;
}

export interface ClarificationRequiredEvent extends AGUIEvent {
  event_type: 'clarification.required';
  original_question: string;
  questions: Array<Record<string, unknown>>;
  message?: string;
}

export interface ApprovalRequiredEvent extends AGUIEvent {
  event_type: 'approval_required';
  request_id: string;
  action: string;
  reason: string;
  risk_level?: string;
  context?: Record<string, unknown>;
  options?: Array<Record<string, unknown>>;
}

export interface ApprovalSubmittedEvent extends AGUIEvent {
  event_type: 'approval_submitted';
  request_id: string;
  approved: boolean;
  approver?: string;
  comment?: string;
  modifications?: Record<string, unknown>;
}

export interface A2UIComponentNode {
  type: string;
  id?: string;
  props?: Record<string, unknown>;
  children?: A2UIComponentNode[];
  style?: Record<string, unknown>;
}

export interface A2UIComponentEvent extends AGUIEvent {
  event_type: 'a2ui.component';
  surface_id: string;
  component: A2UIComponentNode;
}

export interface A2UIUpdateEvent extends AGUIEvent {
  event_type: 'a2ui.update';
  surface_id: string;
  component_id: string;
  updates: Record<string, unknown>;
}

export interface A2UIClearEvent extends AGUIEvent {
  event_type: 'a2ui.clear';
  surface_id: string;
}

/** イベント型ガード */
export function isFlowStartEvent(event: AGUIEvent): event is FlowStartEvent {
  return event.event_type === 'flow.start';
}

export function isFlowCompleteEvent(event: AGUIEvent): event is FlowCompleteEvent {
  return event.event_type === 'flow.complete';
}

export function isFlowErrorEvent(event: AGUIEvent): event is FlowErrorEvent {
  return event.event_type === 'flow.error';
}

export function isNodeStartEvent(event: AGUIEvent): event is NodeStartEvent {
  return event.event_type === 'node.start';
}

export function isNodeCompleteEvent(event: AGUIEvent): event is NodeCompleteEvent {
  return event.event_type === 'node.complete';
}

export function isNodeErrorEvent(event: AGUIEvent): event is NodeErrorEvent {
  return event.event_type === 'node.error';
}

export function isProgressEvent(event: AGUIEvent): event is ProgressEvent {
  return event.event_type === 'progress';
}

function readEventField(event: AGUIEvent, key: string): unknown {
  const directValue = event[key];
  if (directValue !== undefined) {
    return directValue;
  }
  return event.data?.[key];
}

export function getEventString(event: AGUIEvent, ...keys: string[]): string | undefined {
  for (const key of keys) {
    const value = readEventField(event, key);
    if (typeof value === 'string' && value.trim()) {
      return value;
    }
  }
  return undefined;
}

export function getEventNumber(event: AGUIEvent, ...keys: string[]): number | undefined {
  for (const key of keys) {
    const value = readEventField(event, key);
    if (typeof value === 'number' && Number.isFinite(value)) {
      return value;
    }
  }
  return undefined;
}

export function getEventRecord(
  event: AGUIEvent,
  ...keys: string[]
): Record<string, unknown> | undefined {
  for (const key of keys) {
    const value = readEventField(event, key);
    if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
      return value as Record<string, unknown>;
    }
  }
  return undefined;
}

export function getEventResult<TResult = unknown>(event: AGUIEvent): TResult | null {
  const directResult = readEventField(event, 'result');
  if (directResult !== undefined) {
    return directResult as TResult;
  }
  if (event.data !== undefined) {
    return event.data as TResult;
  }
  return null;
}

export function resolveAgentTarget(event: AGUIEvent): string | undefined {
  return getEventString(event, 'node_id', 'node_name', 'agent', 'task_id');
}

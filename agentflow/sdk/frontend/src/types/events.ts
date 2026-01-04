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
  | 'log';

/** AG-UI 基底イベント */
export interface AGUIEvent {
  event_type: AGUIEventType;
  timestamp: number;
  flow_id: string;
  data?: Record<string, unknown>;
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


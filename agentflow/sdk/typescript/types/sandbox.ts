/**
 * AgentFlow Sandbox TypeScript 型定義.
 *
 * Daytonaスタイルのサンドボックス管理のための型定義。
 *
 * 使用例:
 *   import type { SandboxState, ManagedSandbox, Workspace } from '@agentflow/sdk/sandbox';
 */

/**
 * サンドボックス状態（Daytonaスタイル）.
 */
export type SandboxState =
  | 'created'
  | 'started'
  | 'stopped'
  | 'archived'
  | 'deleted';

/**
 * リソース使用状況.
 */
export interface ResourceUsage {
  /** CPU使用率 (0.0-1.0) */
  cpu_percent: number;
  /** メモリ使用量 (MB) */
  memory_mb: number;
  /** ディスク使用量 (MB) */
  disk_mb: number;
  /** ネットワーク送信量 (bytes) */
  network_tx_bytes: number;
  /** ネットワーク受信量 (bytes) */
  network_rx_bytes: number;
}

/**
 * リソース制限.
 */
export interface ResourceLimits {
  /** 最大CPU数 */
  max_cpus: number;
  /** 最大メモリ (MB) */
  max_memory_mb: number;
  /** 最大ディスク (MB) */
  max_disk_mb: number;
  /** ネットワーク許可 */
  network_enabled: boolean;
}

/**
 * サンドボックス設定.
 */
export interface SandboxConfig {
  /** タイムアウト秒 */
  timeout?: number;
  /** メモリ制限 (MB) */
  memory_mb?: number;
  /** CPU数 */
  cpus?: number;
  /** Dockerイメージ */
  image?: string;
  /** 自動停止秒 */
  auto_stop_seconds?: number;
  /** 自動アーカイブ秒 */
  auto_archive_seconds?: number;
  /** リソース制限 */
  resource_limits?: ResourceLimits;
}

/**
 * 実行結果.
 */
export interface ExecutionResult {
  /** 標準出力 */
  stdout: string;
  /** 標準エラー出力 */
  stderr: string;
  /** 終了コード */
  exit_code: number;
  /** 実行時間 (ms) */
  duration_ms: number;
  /** エラーメッセージ */
  error?: string;
}

/**
 * サンドボックスイベントタイプ.
 */
export type SandboxEventType =
  | 'state_changed'
  | 'execution_started'
  | 'execution_completed'
  | 'resource_updated'
  | 'error';

/**
 * サンドボックスイベント.
 */
export interface SandboxEvent {
  /** イベントタイプ */
  type: SandboxEventType;
  /** サンドボックスID */
  sandbox_id: string;
  /** タイムスタンプ */
  timestamp: string;
  /** イベントデータ */
  data: Record<string, unknown>;
}

/**
 * マネージドサンドボックス.
 */
export interface ManagedSandbox {
  /** サンドボックスID */
  sandbox_id: string;
  /** 現在の状態 */
  state: SandboxState;
  /** 作成時刻 */
  created_at: string;
  /** 最終アクティビティ時刻 */
  last_activity_at: string;
  /** 実行回数 */
  execution_count: number;
  /** 実行中かどうか */
  is_running: boolean;
  /** 統計情報 */
  stats: {
    total_execution_ms: number;
    avg_execution_ms: number;
  };
}

/**
 * ファイル情報.
 */
export interface FileInfo {
  /** ファイルパス */
  path: string;
  /** サイズ (bytes) */
  size: number;
  /** 更新時刻 */
  modified_at: string;
  /** ディレクトリかどうか */
  is_directory: boolean;
}

/**
 * ワークスペース.
 */
export interface Workspace {
  /** ワークスペースID */
  workspace_id: string;
  /** ワークスペース名 */
  name: string;
  /** サンドボックス状態 */
  state: SandboxState;
  /** ファイル数 */
  file_count: number;
  /** 合計サイズ (bytes) */
  total_size: number;
  /** 作成時刻 */
  created_at: string;
}

/**
 * ワークスペース状態（保存用）.
 */
export interface WorkspaceState {
  /** ワークスペースID */
  workspace_id: string;
  /** ワークスペース名 */
  name: string;
  /** ファイル内容 (Base64) */
  files: Record<string, string>;
  /** メタデータ */
  metadata: Record<string, unknown>;
  /** 保存時刻 */
  saved_at: string;
}


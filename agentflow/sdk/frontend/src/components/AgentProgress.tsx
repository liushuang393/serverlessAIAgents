/**
 * AgentProgress - Agent 進捗表示コンポーネント.
 * 
 * 複数 Agent の処理進捗を視覚的に表示。
 * 
 * @example
 * ```tsx
 * <AgentProgress
 *   agents={[
 *     { id: 'dao', name: '道', label: '本質分析', status: 'completed', progress: 100 },
 *     { id: 'fa', name: '法', label: '戦略選定', status: 'running', progress: 45 },
 *   ]}
 *   variant="vertical"
 * />
 * ```
 */

import React from 'react';

// ========================================
// 型定義
// ========================================

/** Agent 状態 */
export type AgentStatus = 'waiting' | 'running' | 'completed' | 'failed';

/** Agent 進捗情報 */
export interface AgentProgressItem {
  id: string;
  name: string;
  label: string;
  icon?: string;
  status: AgentStatus;
  progress: number;
  message?: string;
}

/** バリアント */
export type ProgressVariant = 'vertical' | 'horizontal' | 'compact';

/** テーマ */
export type ProgressTheme = 'default' | 'dark' | 'light';

/** Props */
export interface AgentProgressProps {
  /** Agent リスト */
  agents: AgentProgressItem[];
  /** 表示バリアント */
  variant?: ProgressVariant;
  /** テーマ */
  theme?: ProgressTheme;
  /** カスタムクラス */
  className?: string;
  /** Agent クリック時 */
  onAgentClick?: (agent: AgentProgressItem) => void;
}

// ========================================
// スタイル定義
// ========================================

const statusColors: Record<AgentStatus, { bg: string; text: string; border: string }> = {
  waiting: { bg: 'bg-gray-100', text: 'text-gray-500', border: 'border-gray-200' },
  running: { bg: 'bg-blue-50', text: 'text-blue-600', border: 'border-blue-300' },
  completed: { bg: 'bg-green-50', text: 'text-green-600', border: 'border-green-300' },
  failed: { bg: 'bg-red-50', text: 'text-red-600', border: 'border-red-300' },
};

const statusIcons: Record<AgentStatus, string> = {
  waiting: '○',
  running: '◉',
  completed: '✓',
  failed: '✕',
};

// ========================================
// コンポーネント実装
// ========================================

/**
 * 単一 Agent 進捗アイテム.
 */
function AgentProgressItemComponent({
  agent,
  variant,
  onClick,
}: {
  agent: AgentProgressItem;
  variant: ProgressVariant;
  onClick?: () => void;
}) {
  const colors = statusColors[agent.status];
  const icon = agent.icon || statusIcons[agent.status];

  if (variant === 'compact') {
    return (
      <div
        className={`
          flex items-center gap-2 px-3 py-2 rounded-lg border cursor-pointer
          transition-all duration-200 hover:shadow-md
          ${colors.bg} ${colors.border}
        `}
        onClick={onClick}
        role="button"
        tabIndex={0}
      >
        <span className={`text-lg ${colors.text}`}>{icon}</span>
        <span className={`font-medium ${colors.text}`}>{agent.name}</span>
        {agent.status === 'running' && (
          <span className="text-xs text-gray-500">{agent.progress}%</span>
        )}
      </div>
    );
  }

  return (
    <div
      className={`
        ${variant === 'vertical' ? 'flex-col' : 'flex-row items-center'}
        flex gap-3 p-4 rounded-xl border-2 cursor-pointer
        transition-all duration-300 hover:shadow-lg
        ${colors.bg} ${colors.border}
      `}
      onClick={onClick}
      role="button"
      tabIndex={0}
    >
      {/* アイコン/名前 */}
      <div className="flex items-center gap-3">
        <div
          className={`
            w-12 h-12 rounded-full flex items-center justify-center
            text-2xl font-bold ${colors.text}
            ${agent.status === 'running' ? 'animate-pulse' : ''}
          `}
        >
          {icon}
        </div>
        <div>
          <div className={`font-bold text-lg ${colors.text}`}>{agent.name}</div>
          <div className="text-sm text-gray-500">{agent.label}</div>
        </div>
      </div>

      {/* プログレスバー */}
      <div className="flex-1">
        <div className="h-2 bg-gray-200 rounded-full overflow-hidden">
          <div
            className={`
              h-full rounded-full transition-all duration-500
              ${agent.status === 'completed' ? 'bg-green-500' : ''}
              ${agent.status === 'running' ? 'bg-blue-500' : ''}
              ${agent.status === 'failed' ? 'bg-red-500' : ''}
              ${agent.status === 'waiting' ? 'bg-gray-300' : ''}
            `}
            style={{ width: `${agent.progress}%` }}
          />
        </div>
        {agent.message && (
          <div className={`text-sm mt-1 ${colors.text}`}>{agent.message}</div>
        )}
      </div>
    </div>
  );
}

/**
 * Agent 進捗表示コンポーネント.
 */
export function AgentProgress({
  agents,
  variant = 'vertical',
  theme = 'default',
  className = '',
  onAgentClick,
}: AgentProgressProps) {
  const containerClass = {
    vertical: 'flex flex-col gap-4',
    horizontal: 'flex flex-row gap-4 overflow-x-auto',
    compact: 'flex flex-wrap gap-2',
  }[variant];

  return (
    <div
      className={`${containerClass} ${className}`}
      data-theme={theme}
      role="progressbar"
      aria-label="Agent 処理進捗"
    >
      {agents.map((agent) => (
        <AgentProgressItemComponent
          key={agent.id}
          agent={agent}
          variant={variant}
          onClick={() => onAgentClick?.(agent)}
        />
      ))}
    </div>
  );
}


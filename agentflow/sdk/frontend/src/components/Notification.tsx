/**
 * Notification - 通知コンポーネント.
 * 
 * Toast スタイルの通知表示。自動非表示対応。
 * 
 * @example
 * ```tsx
 * // 単一通知
 * <Notification
 *   type="success"
 *   message="処理が完了しました"
 *   autoClose={5000}
 *   onClose={() => {}}
 * />
 * 
 * // NotificationProvider + useNotification hook
 * const { notify } = useNotification();
 * notify.success('保存しました');
 * ```
 */

import React, { useState, useEffect, useCallback, createContext, useContext } from 'react';

// ========================================
// 型定義
// ========================================

/** 通知タイプ */
export type NotificationType = 'success' | 'error' | 'warning' | 'info';

/** 通知アイテム */
export interface NotificationItem {
  id: string;
  type: NotificationType;
  message: string;
  autoClose?: number;
}

/** Props */
export interface NotificationProps {
  type: NotificationType;
  message: string;
  autoClose?: number;
  onClose?: () => void;
  className?: string;
}

// ========================================
// スタイル定義
// ========================================

const typeStyles: Record<NotificationType, { bg: string; text: string; icon: string; border: string }> = {
  success: {
    bg: 'bg-green-50',
    text: 'text-green-800',
    icon: '✓',
    border: 'border-green-200',
  },
  error: {
    bg: 'bg-red-50',
    text: 'text-red-800',
    icon: '✕',
    border: 'border-red-200',
  },
  warning: {
    bg: 'bg-yellow-50',
    text: 'text-yellow-800',
    icon: '⚠',
    border: 'border-yellow-200',
  },
  info: {
    bg: 'bg-blue-50',
    text: 'text-blue-800',
    icon: 'ℹ',
    border: 'border-blue-200',
  },
};

// ========================================
// 単一通知コンポーネント
// ========================================

/**
 * 単一通知表示.
 */
export function Notification({
  type,
  message,
  autoClose,
  onClose,
  className = '',
}: NotificationProps) {
  const [isVisible, setIsVisible] = useState(true);
  const [isExiting, setIsExiting] = useState(false);
  const styles = typeStyles[type];

  const handleClose = useCallback(() => {
    setIsExiting(true);
    setTimeout(() => {
      setIsVisible(false);
      onClose?.();
    }, 300);
  }, [onClose]);

  useEffect(() => {
    if (autoClose && autoClose > 0) {
      const timer = setTimeout(handleClose, autoClose);
      return () => clearTimeout(timer);
    }
  }, [autoClose, handleClose]);

  if (!isVisible) return null;

  return (
    <div
      className={`
        flex items-center gap-3 px-4 py-3 rounded-lg border shadow-lg
        transition-all duration-300
        ${styles.bg} ${styles.border}
        ${isExiting ? 'opacity-0 translate-x-4' : 'opacity-100 translate-x-0'}
        ${className}
      `}
      role="alert"
    >
      <span className={`text-xl ${styles.text}`}>{styles.icon}</span>
      <span className={`flex-1 ${styles.text}`}>{message}</span>
      <button
        onClick={handleClose}
        className={`p-1 rounded hover:bg-black/10 transition-colors ${styles.text}`}
        aria-label="閉じる"
      >
        ✕
      </button>
    </div>
  );
}

// ========================================
// NotificationProvider
// ========================================

interface NotificationContextValue {
  notifications: NotificationItem[];
  notify: {
    success: (message: string, autoClose?: number) => void;
    error: (message: string, autoClose?: number) => void;
    warning: (message: string, autoClose?: number) => void;
    info: (message: string, autoClose?: number) => void;
  };
  remove: (id: string) => void;
  clear: () => void;
}

const NotificationContext = createContext<NotificationContextValue | null>(null);

/**
 * 通知プロバイダー.
 */
export function NotificationProvider({ children }: { children: React.ReactNode }) {
  const [notifications, setNotifications] = useState<NotificationItem[]>([]);

  const addNotification = useCallback(
    (type: NotificationType, message: string, autoClose = 5000) => {
      const id = `notification-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
      setNotifications((prev) => [...prev, { id, type, message, autoClose }]);
      return id;
    },
    []
  );

  const remove = useCallback((id: string) => {
    setNotifications((prev) => prev.filter((n) => n.id !== id));
  }, []);

  const clear = useCallback(() => {
    setNotifications([]);
  }, []);

  const notify = {
    success: (message: string, autoClose?: number) => addNotification('success', message, autoClose),
    error: (message: string, autoClose?: number) => addNotification('error', message, autoClose ?? 0),
    warning: (message: string, autoClose?: number) => addNotification('warning', message, autoClose),
    info: (message: string, autoClose?: number) => addNotification('info', message, autoClose),
  };

  return (
    <NotificationContext.Provider value={{ notifications, notify, remove, clear }}>
      {children}
      {/* 通知コンテナ */}
      <div className="fixed top-4 right-4 z-50 flex flex-col gap-2 max-w-md">
        {notifications.map((item) => (
          <Notification
            key={item.id}
            type={item.type}
            message={item.message}
            autoClose={item.autoClose}
            onClose={() => remove(item.id)}
          />
        ))}
      </div>
    </NotificationContext.Provider>
  );
}

/**
 * 通知 Hook.
 */
export function useNotification() {
  const context = useContext(NotificationContext);
  if (!context) {
    throw new Error('useNotification must be used within NotificationProvider');
  }
  return context;
}


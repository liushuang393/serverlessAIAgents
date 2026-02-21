/**
 * AlertRenderer コンポーネント
 *
 * @description 情報・成功・警告・エラーアラートをレンダリング
 */

import React from "react";
import type { RichComponent, AlertType } from "../types";

/**
 * アラートタイプ別のスタイル設定
 */
const ALERT_STYLES: Record<
  AlertType,
  { bg: string; border: string; icon: string; iconColor: string }
> = {
  info: {
    bg: "bg-blue-50 dark:bg-blue-950",
    border: "border-blue-200 dark:border-blue-800",
    icon: "ℹ️",
    iconColor: "text-blue-600 dark:text-blue-400",
  },
  success: {
    bg: "bg-green-50 dark:bg-green-950",
    border: "border-green-200 dark:border-green-800",
    icon: "✓",
    iconColor: "text-green-600 dark:text-green-400",
  },
  warning: {
    bg: "bg-yellow-50 dark:bg-yellow-950",
    border: "border-yellow-200 dark:border-yellow-800",
    icon: "⚠",
    iconColor: "text-yellow-600 dark:text-yellow-400",
  },
  error: {
    bg: "bg-red-50 dark:bg-red-950",
    border: "border-red-200 dark:border-red-800",
    icon: "✕",
    iconColor: "text-red-600 dark:text-red-400",
  },
};

interface AlertRendererProps {
  /** RichComponent */
  component: RichComponent;
}

/**
 * AlertRenderer
 *
 * @param props - AlertRendererProps
 * @returns JSX.Element
 */
export function AlertRenderer({
  component,
}: AlertRendererProps): React.JSX.Element {
  const { alertType = "info", title, message = "" } = component.props;
  const style = ALERT_STYLES[alertType as AlertType];

  return (
    <div
      role="alert"
      className={`
        flex items-start gap-3 p-4 rounded-lg border
        ${style.bg} ${style.border}
      `}
      data-testid={`alert-${alertType}`}
    >
      {/* アイコン */}
      <span className={`flex-shrink-0 text-lg font-bold ${style.iconColor}`} aria-hidden="true">
        {style.icon}
      </span>

      {/* コンテンツ */}
      <div className="flex-1 min-w-0">
        {title && (
          <h4 className="font-semibold text-gray-900 dark:text-gray-100 mb-1">{String(title)}</h4>
        )}
        <p className="text-sm text-gray-700 dark:text-gray-300">
          {String(message)}
        </p>
      </div>
    </div>
  );
}

export default AlertRenderer;

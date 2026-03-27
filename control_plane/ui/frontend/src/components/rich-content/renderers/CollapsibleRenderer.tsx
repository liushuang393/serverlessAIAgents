/**
 * CollapsibleRenderer コンポーネント
 *
 * @description 折りたたみ可能なセクションをレンダリング
 */

import React, { useState } from "react";
import type { RichComponent } from "../types";

interface CollapsibleRendererProps {
  /** RichComponent */
  component: RichComponent;
  /** 子コンポーネントレンダラー */
  renderComponent: (comp: RichComponent, idx: number) => React.ReactNode;
}

/**
 * CollapsibleRenderer
 *
 * @param props - CollapsibleRendererProps
 * @returns JSX.Element
 */
export function CollapsibleRenderer({
  component,
  renderComponent,
}: CollapsibleRendererProps): React.JSX.Element {
  const { title = "", content, defaultOpen } = component.props;
  const [isOpen, setIsOpen] = useState(Boolean(defaultOpen));
  const contentArr: RichComponent[] = Array.isArray(content) ? content : [];

  return (
    <div className="border border-gray-200 dark:border-gray-700 rounded-lg overflow-hidden">
      {/* ヘッダー（クリッカブル） */}
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="w-full flex items-center justify-between px-4 py-3 bg-gray-50 dark:bg-gray-800 hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors"
        aria-expanded={isOpen}
      >
        <span className="font-medium text-gray-900 dark:text-gray-100">
          {String(title)}
        </span>
        <span
          className={`transform transition-transform ${isOpen ? "rotate-180" : ""}`}
          aria-hidden="true"
        >
          ▼
        </span>
      </button>

      {/* コンテンツ（折りたたみ） */}
      {isOpen && (
        <div className="p-4 bg-white dark:bg-gray-900">
          {contentArr.map((child, idx) => renderComponent(child, idx))}
        </div>
      )}
    </div>
  );
}

export default CollapsibleRenderer;

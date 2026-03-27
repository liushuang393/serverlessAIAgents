/**
 * TabsRenderer コンポーネント
 *
 * @description タブ付きコンテンツをレンダリング
 */

import React, { useState } from "react";
import type { RichComponent } from "../types";

interface TabsRendererProps {
  /** RichComponent */
  component: RichComponent;
  /** 子コンポーネントレンダラー */
  renderComponent: (comp: RichComponent, idx: number) => React.ReactNode;
}

/**
 * TabsRenderer
 *
 * @param props - TabsRendererProps
 * @returns JSX.Element
 */
/** タブ項目定義 */
interface TabItem {
  label: string;
  value: string;
  content: RichComponent[];
}

export function TabsRenderer({
  component,
  renderComponent,
}: TabsRendererProps): React.JSX.Element {
  const rawTabs = component.props.tabs;
  const tabs: TabItem[] = Array.isArray(rawTabs) ? rawTabs : [];
  const defaultValue = component.props.defaultValue;

  const [activeTab, setActiveTab] = useState(
    (typeof defaultValue === "string" ? defaultValue : tabs[0]?.value) || "",
  );

  if (tabs.length === 0) {
    return <div className="text-gray-500 text-sm">タブがありません</div>;
  }

  const activeContent = tabs.find((t) => t.value === activeTab)?.content ?? [];

  return (
    <div className="rounded-lg border border-gray-200 dark:border-gray-700 overflow-hidden">
      {/* タブヘッダー */}
      <div
        className="flex border-b border-gray-200 dark:border-gray-700 bg-gray-50 dark:bg-gray-800"
        role="tablist"
      >
        {tabs.map((tab) => (
          <button
            key={tab.value}
            role="tab"
            aria-selected={activeTab === tab.value}
            onClick={() => setActiveTab(tab.value)}
            className={`
              px-4 py-2 text-sm font-medium transition-colors
              ${
                activeTab === tab.value
                  ? "text-blue-600 dark:text-blue-400 border-b-2 border-blue-600 dark:border-blue-400 -mb-px bg-white dark:bg-gray-900"
                  : "text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-gray-200"
              }
            `}
          >
            {tab.label}
          </button>
        ))}
      </div>

      {/* タブコンテンツ */}
      <div
        className="p-4 bg-white dark:bg-gray-900"
        role="tabpanel"
        aria-labelledby={activeTab}
      >
        {activeContent.map((child, idx) => renderComponent(child, idx))}
      </div>
    </div>
  );
}

export default TabsRenderer;

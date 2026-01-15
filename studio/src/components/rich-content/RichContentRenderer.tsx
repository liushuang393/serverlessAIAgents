/**
 * RichContentRenderer コンポーネント
 *
 * @description バックエンド RichResponse をフロントエンドでレンダリング
 */

import React, { useCallback } from 'react';
import type {
  RichContentRendererProps,
  RichComponent,
  RichComponentType,
} from './types';
import {
  AlertRenderer,
  CodeBlockRenderer,
  MarkdownRenderer,
  DataTableRenderer,
  CollapsibleRenderer,
  CitationRenderer,
  TabsRenderer,
} from './renderers';

/**
 * 未対応コンポーネント用フォールバック
 */
function UnknownRenderer({ component }: { component: RichComponent }): React.JSX.Element {
  return (
    <div className="p-4 bg-yellow-50 dark:bg-yellow-950 border border-yellow-200 dark:border-yellow-800 rounded-lg">
      <p className="text-sm text-yellow-700 dark:text-yellow-300">
        ⚠️ 未対応のコンポーネントタイプ: <code>{component.type}</code>
      </p>
      <pre className="mt-2 text-xs text-yellow-600 dark:text-yellow-400 overflow-auto">
        {JSON.stringify(component.props, null, 2)}
      </pre>
    </div>
  );
}

/**
 * RichContentRenderer
 *
 * @description メインレンダラーコンポーネント
 * @param props - RichContentRendererProps
 * @returns JSX.Element
 */
export function RichContentRenderer({
  response,
  customRenderers = {},
  fallback = null,
  theme = 'light',
  className = '',
}: RichContentRendererProps): React.JSX.Element {
  /**
   * 単一コンポーネントをレンダリング
   */
  const renderComponent = useCallback(
    (component: RichComponent, idx: number): React.ReactNode => {
      const key = component.id ?? `rich-${idx}`;
      const type = component.type as RichComponentType;

      // カスタムレンダラーを優先
      const CustomRenderer = customRenderers[type];
      if (CustomRenderer) {
        return <CustomRenderer key={key} {...component} />;
      }

      // 標準レンダラー
      switch (type) {
        case 'markdown':
          return <MarkdownRenderer key={key} component={component} />;

        case 'code_block':
          return <CodeBlockRenderer key={key} component={component} />;

        case 'alert':
          return <AlertRenderer key={key} component={component} />;

        case 'data_table':
          return <DataTableRenderer key={key} component={component} />;

        case 'citation':
          return <CitationRenderer key={key} component={component} />;

        case 'collapsible':
          return (
            <CollapsibleRenderer
              key={key}
              component={component}
              renderComponent={renderComponent}
            />
          );

        case 'tabs':
          return (
            <TabsRenderer
              key={key}
              component={component}
              renderComponent={renderComponent}
            />
          );

        default:
          return <UnknownRenderer key={key} component={component} />;
      }
    },
    [customRenderers]
  );

  // レスポンスが空の場合
  if (!response?.components?.length) {
    if (fallback) {
      return <>{fallback}</>;
    }
    return (
      <div className="text-gray-500 dark:text-gray-400 text-sm p-4">
        コンテンツがありません
      </div>
    );
  }

  return (
    <div
      className={`rich-content-renderer space-y-4 ${theme === 'dark' ? 'dark' : ''} ${className}`}
      data-theme={theme}
    >
      {response.components.map((component, idx) =>
        renderComponent(component, idx)
      )}
    </div>
  );
}

export default RichContentRenderer;


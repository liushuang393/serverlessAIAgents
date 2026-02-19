/**
 * CitationRenderer コンポーネント
 *
 * @description 引用元情報をレンダリング
 */

import React from "react";
import type { RichComponent } from "../types";

interface CitationRendererProps {
  /** RichComponent */
  component: RichComponent;
}

/**
 * CitationRenderer
 *
 * @param props - CitationRendererProps
 * @returns JSX.Element
 */
/** 引用元定義 */
interface Source {
  title: string;
  url?: string;
  author?: string;
  date?: string;
}

export function CitationRenderer({
  component,
}: CitationRendererProps): React.JSX.Element {
  const rawSources = component.props.sources;
  const sources: Source[] = Array.isArray(rawSources) ? rawSources : [];

  if (sources.length === 0) {
    return <div className="text-gray-500 text-sm">引用元がありません</div>;
  }

  return (
    <div className="border-l-4 border-gray-300 dark:border-gray-600 pl-4 py-2">
      <h4 className="text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2">
        📚 引用元
      </h4>
      <ol className="list-decimal list-inside space-y-2">
        {sources.map((source, idx) => (
          <li
            key={`cite-${source.title}-${idx}`}
            className="text-sm text-gray-600 dark:text-gray-400"
          >
            {source.url ? (
              <a
                href={source.url}
                target="_blank"
                rel="noopener noreferrer"
                className="text-blue-500 hover:underline"
              >
                {source.title}
              </a>
            ) : (
              <span>{source.title}</span>
            )}
            {source.author && (
              <span className="ml-2 text-gray-500">- {source.author}</span>
            )}
            {source.date && (
              <span className="ml-2 text-gray-400">({source.date})</span>
            )}
          </li>
        ))}
      </ol>
    </div>
  );
}

export default CitationRenderer;

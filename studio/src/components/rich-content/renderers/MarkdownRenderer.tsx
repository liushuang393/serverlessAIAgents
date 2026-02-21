/**
 * MarkdownRenderer コンポーネント
 *
 * @description Markdown テキストを HTML としてレンダリング
 * @note 本番環境では react-markdown + remark-gfm を使用推奨
 */

import React, { useMemo } from 'react';
import type { RichComponent } from '../types';

/**
 * シンプルな Markdown パーサー（依存ライブラリなし）
 * 基本的なフォーマットのみ対応
 */
function parseMarkdown(text: string): string {
  let html = text;

  // コードブロック ```
  html = html.replace(/```(\w*)\n([\s\S]*?)```/g, (_match, lang, code) => {
    const langAttr = lang ? ` data-language="${lang}"` : '';
    return `<pre class="code-block"${langAttr}><code>${escapeHtml(code.trim())}</code></pre>`;
  });

  // インラインコード `
  html = html.replace(/`([^`]+)`/g, '<code class="inline-code">$1</code>');

  // 見出し
  html = html.replace(/^### (.+)$/gm, '<h3 class="text-lg font-semibold mt-4 mb-2">$1</h3>');
  html = html.replace(/^## (.+)$/gm, '<h2 class="text-xl font-bold mt-5 mb-3">$1</h2>');
  html = html.replace(/^# (.+)$/gm, '<h1 class="text-2xl font-bold mt-6 mb-4">$1</h1>');

  // 太字・斜体
  html = html.replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>');
  html = html.replace(/\*(.+?)\*/g, '<em>$1</em>');

  // リンク
  html = html.replace(
    /\[([^\]]+)\]\(([^)]+)\)/g,
    '<a href="$2" class="text-blue-500 hover:underline" target="_blank" rel="noopener noreferrer">$1</a>',
  );

  // リスト
  html = html.replace(/^- (.+)$/gm, '<li class="ml-4 list-disc">$1</li>');
  html = html.replace(/^(\d+)\. (.+)$/gm, '<li class="ml-4 list-decimal">$2</li>');

  // 引用
  html = html.replace(
    /^> (.+)$/gm,
    '<blockquote class="border-l-4 border-gray-300 pl-4 italic text-gray-600 dark:text-gray-400">$1</blockquote>',
  );

  // 水平線
  html = html.replace(/^---$/gm, '<hr class="my-4 border-gray-300 dark:border-gray-600" />');

  // 改行を <br> に変換（段落内）
  html = html.replace(/\n\n/g, '</p><p class="mb-3">');
  html = `<p class="mb-3">${html}</p>`;

  return html;
}

/**
 * HTML エスケープ
 */
function escapeHtml(text: string): string {
  const map: Record<string, string> = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    "'": '&#039;',
  };
  return text.replace(/[&<>"']/g, (m) => map[m] ?? m);
}

interface MarkdownRendererProps {
  /** RichComponent */
  component: RichComponent;
}

/**
 * MarkdownRenderer
 *
 * @param props - MarkdownRendererProps
 * @returns JSX.Element
 */
export function MarkdownRenderer({ component }: MarkdownRendererProps): React.JSX.Element {
  const { content = '' } = component.props;

  const htmlContent = useMemo(() => {
    return parseMarkdown(String(content));
  }, [content]);

  return (
    <div
      className="markdown-content prose prose-sm dark:prose-invert max-w-none"
      dangerouslySetInnerHTML={{ __html: htmlContent }}
    />
  );
}

export default MarkdownRenderer;

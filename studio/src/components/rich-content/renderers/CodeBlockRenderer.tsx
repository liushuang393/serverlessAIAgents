/**
 * CodeBlockRenderer コンポーネント
 *
 * @description シンタックスハイライト付きコードブロックをレンダリング
 */

import React, { useCallback, useState } from "react";
import type { RichComponent } from "../types";

/**
 * シンプルなシンタックスハイライト（依存ライブラリなし）
 * 本番では prism-react-renderer を使用推奨
 */
function highlightCode(code: string, language: string): React.ReactNode {
  // 基本的なキーワードハイライト
  const keywords: Record<string, string[]> = {
    python: [
      'def',
      'class',
      'import',
      'from',
      'return',
      'if',
      'else',
      'for',
      'in',
      'while',
      'try',
      'except',
      'async',
      'await',
      'None',
      'True',
      'False',
    ],
    typescript: [
      'const',
      'let',
      'var',
      'function',
      'return',
      'if',
      'else',
      'for',
      'while',
      'class',
      'interface',
      'type',
      'export',
      'import',
      'async',
      'await',
      'null',
      'undefined',
    ],
    javascript: [
      'const',
      'let',
      'var',
      'function',
      'return',
      'if',
      'else',
      'for',
      'while',
      'class',
      'export',
      'import',
      'async',
      'await',
      'null',
      'undefined',
    ],
  };

  const langKeywords = keywords[language.toLowerCase()] || [];
  if (langKeywords.length === 0) {
    return <code>{code}</code>;
  }

  const pattern = new RegExp(`\\b(${langKeywords.join("|")})\\b`, "g");
  const parts = code.split(pattern);

  return (
    <code>
      {parts.map((part, idx) => {
        const isKeyword = langKeywords.includes(part);
        return (
          <span key={idx} className={isKeyword ? 'text-purple-400 font-semibold' : ''}>
            {part}
          </span>
        );
      })}
    </code>
  );
}

interface CodeBlockRendererProps {
  /** RichComponent */
  component: RichComponent;
}

/**
 * CodeBlockRenderer
 *
 * @param props - CodeBlockRendererProps
 * @returns JSX.Element
 */
export function CodeBlockRenderer({
  component,
}: CodeBlockRendererProps): React.JSX.Element {
  const {
    code = "",
    language = "text",
    filename,
    showLineNumbers,
  } = component.props;
  const [copied, setCopied] = useState(false);

  const codeStr = String(code);
  const langStr = String(language);

  const handleCopy = useCallback(async () => {
    try {
      await navigator.clipboard.writeText(codeStr);
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } catch {
      // クリップボードアクセス失敗時は無視
    }
  }, [codeStr]);

  return (
    <div className="relative group rounded-lg overflow-hidden border border-gray-700">
      {/* ヘッダー */}
      <div className="flex items-center justify-between px-4 py-2 bg-gray-800 text-gray-300 text-sm">
        <div className="flex items-center gap-2">
          {filename && <span className="font-mono text-gray-400">{String(filename)}</span>}
          <span className="px-2 py-0.5 bg-gray-700 rounded text-xs">{langStr}</span>
        </div>
        <button
          onClick={handleCopy}
          className="px-2 py-1 text-xs rounded hover:bg-gray-700 transition-colors"
          aria-label="コードをコピー"
        >
          {copied ? "✓ コピー済み" : "コピー"}
        </button>
      </div>

      {/* コード本体 */}
      <pre
        className={`
          p-4 bg-gray-900 text-gray-100 overflow-x-auto
          font-mono text-sm leading-relaxed
          ${showLineNumbers ? "pl-12" : ""}
        `}
      >
        {showLineNumbers ? (
          <div className="relative">
            <div className="absolute left-0 top-0 text-gray-500 select-none text-right pr-4">
              {codeStr.split("\n").map((_, idx) => (
                <div key={`line-${idx}`}>{idx + 1}</div>
              ))}
            </div>
            <div className="ml-8">{highlightCode(codeStr, langStr)}</div>
          </div>
        ) : (
          highlightCode(codeStr, langStr)
        )}
      </pre>
    </div>
  );
}

export default CodeBlockRenderer;

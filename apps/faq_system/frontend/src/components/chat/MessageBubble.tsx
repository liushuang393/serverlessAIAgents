import React, { lazy, Suspense } from "react";
import { User, Sparkles, Database } from "lucide-react";
import { MarkdownRenderer } from "../rich/MarkdownRenderer";
import { CodeBlock } from "../rich/CodeBlock";
import { DataTable } from "../rich/DataTable";
import { SuggestionChips } from "../rich/SuggestionChips";
import type { ChatMessage } from "../../api/types";
import { useI18n } from "../../i18n";

// ECharts は大きいので遅延ロード
const ChartRenderer = lazy(() =>
  import("../rich/ChartRenderer").then((m) => ({ default: m.ChartRenderer })),
);

interface MessageBubbleProps {
  message: ChatMessage;
}

/** アシスタントメッセージのリッチコンテンツ部分をレンダリング */
const AssistantRichContent: React.FC<{ message: ChatMessage }> = ({
  message,
}) => {
  const hasData = message.data && message.data.length > 0;
  const hasSql = message.sql && message.sql.length > 0;
  const hasChart = message.chart != null;
  const hasSuggestions = message.suggestions && message.suggestions.length > 0;
  const hasRich = hasData || hasSql || hasChart || hasSuggestions;

  if (!hasRich) return null;

  return (
    <div className="mt-4 space-y-6">
      {/* SQL クエリ表示 */}
      {hasSql && <CodeBlock language="sql" value={message.sql!} />}

      {/* データテーブル */}
      {hasData && (
        <DataTable
          data={message.data!}
          columns={message.columns}
          title={`結果: ${message.data!.length} 件`}
        />
      )}

      {/* チャート */}
      {hasChart && (
        <Suspense
          fallback={
            <div className="h-80 flex items-center justify-center text-[var(--text-muted)]">
              チャート読込中…
            </div>
          }
        >
          <ChartRenderer chart={message.chart!} />
        </Suspense>
      )}

      {/* フォローアップ提案 */}
      {hasSuggestions && <SuggestionChips suggestions={message.suggestions!} />}
    </div>
  );
};

/** クエリタイプバッジ */
const QueryTypeBadge: React.FC<{ queryType?: string }> = ({ queryType }) => {
  if (!queryType || queryType === "faq") return null;
  const labels: Record<string, string> = {
    sql: "SQL",
    hybrid: "Hybrid",
    chat: "Chat",
  };
  return (
    <span className="inline-flex items-center gap-1 px-1.5 py-0.5 rounded text-[10px] font-medium bg-white/5 border border-white/10 text-[var(--text-muted)]">
      <Database size={10} />
      {labels[queryType] ?? queryType}
    </span>
  );
};

export const MessageBubble: React.FC<MessageBubbleProps> = ({ message }) => {
  const { t } = useI18n();
  const isUser = message.role === "user";
  const isAssistant = message.role === "assistant" || message.role === "system";

  return (
    <div
      className={`w-full flex ${isUser ? "justify-end" : "justify-start"} animate-in fade-in duration-500`}
    >
      <div
        className={`flex gap-4 max-w-[90%] md:max-w-[80%] ${isUser ? "flex-row-reverse" : "flex-row"}`}
      >
        {/* Avatar */}
        <div className="flex-shrink-0 mt-1">
          <div
            className={`w-8 h-8 flex items-center justify-center rounded-lg border ${isUser
                ? "bg-white/5 border-white/10 text-white"
                : "bg-[var(--primary)]/10 border-[var(--primary)]/20 text-[var(--primary)]"
              }`}
          >
            {isUser ? <User size={16} /> : <Sparkles size={16} />}
          </div>
        </div>

        {/* Content Container */}
        <div
          className={`flex flex-col gap-1.5 ${isUser ? "items-end" : "items-start"}`}
        >
          <div className="flex items-center gap-2">
            <span className="text-[10px] font-semibold text-[var(--text-muted)] uppercase tracking-[0.12em] px-1">
              {isUser ? t("chat.you") : t("chat.ai_name")}
            </span>
            {isAssistant && <QueryTypeBadge queryType={message.query_type} />}
          </div>

          <div
            className={`p-5 md:p-6 rounded-2xl ${isUser
                ? "bg-[var(--bg-card)] text-white border border-white/5 rounded-tr-sm shadow-xl"
                : "bg-transparent text-[var(--text-main)] leading-relaxed rounded-tl-sm"
              }`}
          >
            {/* メイン Markdown コンテンツ */}
            <div className="markdown-content">
              {isAssistant ? (
                <MarkdownRenderer
                  content={
                    message.content ||
                    (message.role === "assistant" ? t("chat.thinking") : "")
                  }
                />
              ) : (
                <div className="whitespace-pre-wrap text-[15px]">
                  {message.content}
                </div>
              )}
            </div>

            {/* リッチコンテンツ（テーブル、チャート、SQL、提案） */}
            {isAssistant && <AssistantRichContent message={message} />}
          </div>
        </div>
      </div>
    </div>
  );
};

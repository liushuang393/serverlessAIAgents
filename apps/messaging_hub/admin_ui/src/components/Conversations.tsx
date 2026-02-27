import { useEffect, useMemo, useState } from "react";
import {
  Download,
  FileCode,
  FileJson,
  FileText,
  Loader2,
  Plus,
  Send,
} from "lucide-react";
import clsx from "clsx";

interface ConversationSummary {
  conversation_id: string;
  message_count: number;
  last_message_at: string | null;
}

interface SRMessage {
  message_id: string;
  conversation_id: string;
  role: string;
  content: string;
  created_at: string;
  updated_at: string;
  metadata?: Record<string, unknown>;
}

/**
 * 独自チャット画面
 *
 * sr_chat API を利用して会話作成・履歴表示・送信を行う。
 */
export default function Conversations() {
  const [selectedFormat, setSelectedFormat] = useState<
    "json" | "csv" | "markdown"
  >("json");
  const [isExporting, setIsExporting] = useState(false);
  const [conversations, setConversations] = useState<ConversationSummary[]>([]);
  const [selectedConversationId, setSelectedConversationId] =
    useState<string>("");
  const [messages, setMessages] = useState<SRMessage[]>([]);
  const [draft, setDraft] = useState("");
  const [userId, setUserId] = useState("admin_ui");
  const [loadingConversations, setLoadingConversations] = useState(false);
  const [loadingMessages, setLoadingMessages] = useState(false);
  const [sending, setSending] = useState(false);

  const formats = [
    {
      value: "json",
      label: "JSON",
      icon: FileJson,
      description: "構造化データ形式",
    },
    {
      value: "csv",
      label: "CSV",
      icon: FileText,
      description: "スプレッドシート用",
    },
    {
      value: "markdown",
      label: "Markdown",
      icon: FileCode,
      description: "ドキュメント用",
    },
  ] as const;

  const selectedConversation = useMemo(
    () =>
      conversations.find(
        (item) => item.conversation_id === selectedConversationId,
      ) ?? null,
    [conversations, selectedConversationId],
  );

  useEffect(() => {
    void refreshConversations();
    const timer = window.setInterval(() => {
      void refreshConversations();
    }, 5000);
    return () => {
      window.clearInterval(timer);
    };
  }, []);

  useEffect(() => {
    if (!selectedConversationId) {
      setMessages([]);
      return;
    }
    void refreshMessages(selectedConversationId);
  }, [selectedConversationId]);

  const refreshConversations = async () => {
    setLoadingConversations(true);
    try {
      const response = await fetch("/api/sr_chat/conversations.list");
      if (!response.ok) {
        return;
      }
      const data = await response.json();
      const items = Array.isArray(data.conversations)
        ? (data.conversations as ConversationSummary[])
        : [];
      setConversations(items);
      if (!selectedConversationId && items.length > 0) {
        setSelectedConversationId(items[0].conversation_id);
      }
    } catch (error) {
      console.error("Conversations fetch error:", error);
    } finally {
      setLoadingConversations(false);
    }
  };

  const refreshMessages = async (conversationId: string) => {
    setLoadingMessages(true);
    try {
      const response = await fetch(
        `/api/sr_chat/conversations.history?conversation_id=${encodeURIComponent(conversationId)}&limit=200`,
      );
      if (!response.ok) {
        return;
      }
      const data = await response.json();
      const rawItems = Array.isArray(data.messages)
        ? (data.messages as SRMessage[])
        : [];
      // API は新しい順で返すため、画面では古い順にそろえる。
      setMessages([...rawItems].reverse());
    } catch (error) {
      console.error("Messages fetch error:", error);
    } finally {
      setLoadingMessages(false);
    }
  };

  const createConversation = () => {
    const idSuffix =
      typeof crypto !== "undefined" && "randomUUID" in crypto
        ? crypto.randomUUID().slice(0, 8)
        : String(Date.now());
    const conversationId = `chat:${idSuffix}`;
    setSelectedConversationId(conversationId);
    setMessages([]);
  };

  const sendMessage = async () => {
    const text = draft.trim();
    if (!text || sending || !selectedConversationId) {
      return;
    }
    setSending(true);
    try {
      const response = await fetch("/api/sr_chat/chat.postMessage", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          text,
          user_id: userId || "admin_ui",
          conversation_id: selectedConversationId,
        }),
      });
      if (!response.ok) {
        return;
      }
      setDraft("");
      await Promise.all([
        refreshConversations(),
        refreshMessages(selectedConversationId),
      ]);
    } catch (error) {
      console.error("Send message error:", error);
    } finally {
      setSending(false);
    }
  };

  const handleExport = async () => {
    setIsExporting(true);
    try {
      const response = await fetch(`/api/export?format=${selectedFormat}`);
      const blob = await response.blob();

      const url = window.URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = `conversations.${selectedFormat === "markdown" ? "md" : selectedFormat}`;
      document.body.appendChild(a);
      a.click();
      window.URL.revokeObjectURL(url);
      a.remove();
    } catch (error) {
      console.error("Export failed:", error);
    } finally {
      setIsExporting(false);
    }
  };

  const formatTime = (isoString: string | null) => {
    if (!isoString) {
      return "-";
    }
    const date = new Date(isoString);
    return date.toLocaleString("ja-JP", {
      month: "2-digit",
      day: "2-digit",
      hour: "2-digit",
      minute: "2-digit",
    });
  };

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold text-slate-900">
            会話ワークスペース
          </h2>
          <p className="text-sm text-muted mt-1">
            Messaging Hub 独自チャット画面（sr_chat API 接続）
          </p>
        </div>
        <button
          onClick={createConversation}
          className="neo-button elevated flex items-center gap-2 px-4 py-2"
        >
          <Plus size={16} />
          新規会話
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        <div className="glass-panel">
          <div className="px-4 py-3 border-b flex items-center justify-between">
            <h3 className="font-semibold">会話一覧</h3>
            {loadingConversations && (
              <Loader2 size={16} className="animate-spin text-primary-500" />
            )}
          </div>
          <div className="max-h-[520px] overflow-auto">
            {conversations.length === 0 ? (
              <div className="p-4 text-sm text-gray-500">
                会話がありません。新規会話を作成してください。
              </div>
            ) : (
              conversations.map((item) => (
                <button
                  key={item.conversation_id}
                  onClick={() =>
                    setSelectedConversationId(item.conversation_id)
                  }
                  className={clsx(
                    "w-full text-left px-4 py-3 border-b transition-colors",
                    selectedConversationId === item.conversation_id
                      ? "bg-cyan-50/70"
                      : "hover:bg-white/70",
                  )}
                >
                  <div className="font-medium text-sm">
                    {item.conversation_id}
                  </div>
                  <div className="text-xs text-muted mt-1">
                    {item.message_count} messages ・{" "}
                    {formatTime(item.last_message_at)}
                  </div>
                </button>
              ))
            )}
          </div>
        </div>

        <div className="lg:col-span-2 glass-panel flex flex-col">
          <div className="px-4 py-3 border-b">
            <div className="font-semibold">
              {selectedConversation?.conversation_id ??
                (selectedConversationId || "未選択")}
            </div>
            <div className="text-xs text-muted mt-1">
              User ID:{" "}
              <input
                value={userId}
                onChange={(event) => setUserId(event.target.value)}
                className="border border-white/80 rounded px-2 py-0.5 ml-1 bg-white/80"
              />
            </div>
          </div>

          <div className="flex-1 min-h-[360px] max-h-[420px] overflow-auto p-4 space-y-3 bg-slate-100/60">
            {loadingMessages ? (
              <div className="flex items-center gap-2 text-sm text-muted">
                <Loader2 size={16} className="animate-spin" />
                履歴を読み込み中...
              </div>
            ) : messages.length === 0 ? (
              <div className="text-sm text-muted">メッセージがありません。</div>
            ) : (
              messages.map((message) => (
                <div
                  key={message.message_id}
                  className={clsx(
                    "rounded-xl px-3 py-2 max-w-[85%] text-sm",
                    message.role === "user"
                      ? "bg-cyan-100/90 ml-auto shadow"
                      : "bg-white/90 border border-white shadow",
                  )}
                >
                  <div className="text-xs text-muted mb-1">
                    {message.role} ・ {formatTime(message.created_at)}
                  </div>
                  <div className="whitespace-pre-wrap">{message.content}</div>
                </div>
              ))
            )}
          </div>

          <div className="p-4 border-t space-y-2">
            <textarea
              value={draft}
              onChange={(event) => setDraft(event.target.value)}
              rows={3}
              className="w-full border border-white/80 bg-white/90 rounded-lg px-3 py-2 text-sm"
              placeholder="メッセージを入力..."
            />
            <div className="flex justify-end">
              <button
                onClick={sendMessage}
                disabled={sending || !selectedConversationId || !draft.trim()}
                className="neo-button flex items-center gap-2 px-4 py-2 disabled:opacity-50"
              >
                {sending ? (
                  <Loader2 size={16} className="animate-spin" />
                ) : (
                  <Send size={16} />
                )}
                送信
              </button>
            </div>
          </div>
        </div>
      </div>

      <div className="glass-panel p-6">
        <h3 className="text-lg font-semibold mb-4">会話エクスポート</h3>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
          {formats.map(({ value, label, icon: Icon, description }) => (
            <button
              key={value}
              onClick={() => setSelectedFormat(value)}
              className={clsx(
                "p-4 rounded-xl border-2 text-left transition-all elevated",
                selectedFormat === value
                  ? "border-primary-500 bg-cyan-50/70"
                  : "border-white/70 hover:border-primary-300 bg-white/70",
              )}
            >
              <div className="flex items-center gap-3 mb-2">
                <Icon
                  className={
                    selectedFormat === value
                      ? "text-primary-600"
                      : "text-gray-500"
                  }
                  size={24}
                />
                <span className="font-medium">{label}</span>
              </div>
              <p className="text-sm text-gray-500">{description}</p>
            </button>
          ))}
        </div>
        <button
          onClick={handleExport}
          disabled={isExporting}
          className="neo-button flex items-center gap-2 px-6 py-3 transition-colors disabled:opacity-50"
        >
          <Download size={20} />
          {isExporting ? "エクスポート中..." : "エクスポート"}
        </button>
      </div>
    </div>
  );
}

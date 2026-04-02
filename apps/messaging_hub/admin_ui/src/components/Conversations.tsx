import { useEffect, useMemo, useRef, useState, type ChangeEvent } from "react";
import {
  CheckCircle2,
  ChevronDown,
  ChevronUp,
  Download,
  FileCode,
  FileJson,
  FileText,
  Loader2,
  Pencil,
  Plus,
  RefreshCw,
  Save,
  Send,
  Upload,
  Wifi,
  WifiOff,
  X,
} from "lucide-react";
import clsx from "clsx";
import { useWebSocket } from "../hooks/useWebSocket";
import { usePageVisibility } from "../hooks/usePageVisibility";

interface ConversationSummary {
  conversation_id: string;
  message_count: number;
  last_message_at: string | null;
  /** バックエンドが返す最初のユーザーメッセージ先頭50文字（オプション） */
  first_message_preview?: string | null;
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

interface SubscriptionResponse {
  ok: boolean;
  subscription_id: string;
  ws_url: string;
  rooms?: string[];
}

type ExportFormat = "json" | "csv" | "markdown";

function getErrorMessage(raw: string): string {
  if (!raw) {
    return "不明なエラーが発生しました";
  }
  try {
    const parsed = JSON.parse(raw) as { detail?: string; error?: string };
    if (typeof parsed.detail === "string" && parsed.detail.trim()) {
      return parsed.detail;
    }
    if (typeof parsed.error === "string" && parsed.error.trim()) {
      return parsed.error;
    }
  } catch {
    // ignore
  }
  return raw.slice(0, 280);
}

async function readErrorMessage(response: Response): Promise<string> {
  const body = await response.text();
  return getErrorMessage(body);
}

function toBase64(file: File): Promise<string> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = () => {
      const value = typeof reader.result === "string" ? reader.result : "";
      const base64 = value.includes(",") ? value.split(",")[1] : value;
      resolve(base64);
    };
    reader.onerror = () =>
      reject(new Error("ファイルの読み込みに失敗しました"));
    reader.readAsDataURL(file);
  });
}

function getConversationTitle(item: ConversationSummary): string {
  const preview = item.first_message_preview?.trim();
  if (preview) {
    return preview.length > 15 ? `${preview.slice(0, 15)}…` : preview;
  }
  return item.conversation_id;
}

/**
 * sr_chat API を利用した独自チャット画面。
 */
export default function Conversations() {
  const [selectedFormat, setSelectedFormat] = useState<ExportFormat>("json");
  const [isExporting, setIsExporting] = useState(false);
  const [exportOpen, setExportOpen] = useState(true);
  const [conversations, setConversations] = useState<ConversationSummary[]>([]);
  const [selectedConversationId, setSelectedConversationId] =
    useState<string>("");
  const [messages, setMessages] = useState<SRMessage[]>([]);
  const [draft, setDraft] = useState("");
  const [userId, setUserId] = useState("admin_ui");
  const [loadingConversations, setLoadingConversations] = useState(false);
  const [loadingMessages, setLoadingMessages] = useState(false);
  const [sending, setSending] = useState(false);
  const [uploading, setUploading] = useState(false);
  const [pageError, setPageError] = useState<string | null>(null);
  const [statusMessage, setStatusMessage] = useState<string | null>(null);
  const [subscriptionId, setSubscriptionId] = useState<string | null>(null);
  const [wsUrl, setWsUrl] = useState("");
  const [wsRooms, setWsRooms] = useState<string[]>([]);
  const [editingMessageId, setEditingMessageId] = useState<string | null>(null);
  const [editingText, setEditingText] = useState("");
  const [updatingMessage, setUpdatingMessage] = useState(false);
  const [authChecked, setAuthChecked] = useState(false);
  const isVisible = usePageVisibility();

  const clientId = useMemo(() => {
    const key = "MESSAGING_HUB_CLIENT_ID";
    const existing = window.localStorage.getItem(key);
    if (existing) {
      return existing;
    }
    const next = `admin_ui_${Date.now().toString(36)}`;
    window.localStorage.setItem(key, next);
    return next;
  }, []);

  const { isConnected, lastMessage, sendMessage } = useWebSocket(wsUrl);
  const messageListRef = useRef<HTMLDivElement | null>(null);
  const subscribedRoomsRef = useRef<string[]>([]);

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
    void verifyAuth();
    void refreshConversations();
  }, []);

  useEffect(() => {
    if (!isVisible) {
      return;
    }
    void refreshConversations(false);
    if (selectedConversationId) {
      void refreshMessages(selectedConversationId, false);
    }
    const timer = window.setInterval(() => {
      void refreshConversations(false);
    }, 7000);
    return () => {
      window.clearInterval(timer);
    };
  }, [isVisible, selectedConversationId]);

  useEffect(() => {
    if (!selectedConversationId) {
      setMessages([]);
      return;
    }
    void refreshMessages(selectedConversationId);
    void subscribeEvents(selectedConversationId);
  }, [selectedConversationId]);

  useEffect(() => {
    if (!lastMessage) {
      return;
    }
    const eventType = lastMessage.type;
    if (
      eventType === "RunFinished" ||
      eventType === "ToolExecuted" ||
      eventType === "approval_decided" ||
      eventType === "approval_request" ||
      eventType === "flow.complete" ||
      eventType === "clarification.required" ||
      eventType === "a2ui.component" ||
      eventType === "notification"
    ) {
      void refreshConversations(false);
      if (selectedConversationId) {
        void refreshMessages(selectedConversationId, false);
      }
    }
    setStatusMessage(`リアルタイムイベント受信: ${eventType}`);
  }, [lastMessage, selectedConversationId]);

  useEffect(() => {
    if (!messageListRef.current) {
      return;
    }
    messageListRef.current.scrollTop = messageListRef.current.scrollHeight;
  }, [messages, sending]);

  useEffect(() => {
    if (!isConnected) {
      subscribedRoomsRef.current = [];
      return;
    }
    const previousRooms = new Set(subscribedRoomsRef.current);
    const nextRooms = new Set(wsRooms);
    previousRooms.forEach((room) => {
      if (!nextRooms.has(room)) {
        sendMessage({ type: "unsubscribe", room, data: {} });
      }
    });
    nextRooms.forEach((room) => {
      if (!previousRooms.has(room)) {
        sendMessage({ type: "subscribe", room, data: {} });
      }
    });
    subscribedRoomsRef.current = [...wsRooms];
  }, [isConnected, sendMessage, wsRooms]);

  const verifyAuth = async () => {
    try {
      const response = await fetch("/api/sr_chat/auth.test", {
        method: "POST",
      });
      if (!response.ok) {
        setPageError(await readErrorMessage(response));
        return;
      }
    } catch (error) {
      setPageError(
        error instanceof Error ? error.message : "認証確認に失敗しました",
      );
    } finally {
      setAuthChecked(true);
    }
  };

  const refreshConversations = async (showLoading = true) => {
    if (showLoading) {
      setLoadingConversations(true);
    }
    try {
      const response = await fetch("/api/sr_chat/conversations.list");
      if (!response.ok) {
        setPageError(await readErrorMessage(response));
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
      setPageError(null);
    } catch (error) {
      setPageError(
        error instanceof Error ? error.message : "会話一覧の取得に失敗しました",
      );
    } finally {
      if (showLoading) {
        setLoadingConversations(false);
      }
    }
  };

  const refreshMessages = async (
    conversationId: string,
    showLoading = true,
  ) => {
    if (showLoading) {
      setLoadingMessages(true);
    }
    try {
      const response = await fetch(
        `/api/sr_chat/conversations.history?conversation_id=${encodeURIComponent(conversationId)}&limit=200`,
      );
      if (!response.ok) {
        setPageError(await readErrorMessage(response));
        return;
      }
      const data = await response.json();
      const rawItems = Array.isArray(data.messages)
        ? (data.messages as SRMessage[])
        : [];
      setMessages([...rawItems].reverse());
      setPageError(null);
    } catch (error) {
      setPageError(
        error instanceof Error ? error.message : "履歴取得に失敗しました",
      );
    } finally {
      if (showLoading) {
        setLoadingMessages(false);
      }
    }
  };

  const subscribeEvents = async (conversationId: string) => {
    try {
      const response = await fetch("/api/sr_chat/events.subscribe", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          client_id: clientId,
          conversation_id: conversationId,
          user_id: userId || "admin_ui",
          event_types: [
            "RunStarted",
            "StepStarted",
            "ToolApprovalRequested",
            "ToolExecuted",
            "EvidenceAdded",
            "RunFinished",
            "flow.start",
            "progress",
            "clarification.required",
            "a2ui.component",
            "flow.complete",
            "flow.error",
          ],
        }),
      });
      if (!response.ok) {
        setPageError(await readErrorMessage(response));
        return;
      }
      const data = (await response.json()) as SubscriptionResponse;
      if (data.ok) {
        setSubscriptionId(data.subscription_id);
        setWsUrl(data.ws_url);
        setWsRooms(Array.isArray(data.rooms) ? data.rooms : []);
      }
    } catch (error) {
      setPageError(
        error instanceof Error ? error.message : "イベント購読に失敗しました",
      );
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
    setStatusMessage("新規会話を作成しました");
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
        setPageError(await readErrorMessage(response));
        return;
      }
      setDraft("");
      await Promise.all([
        refreshConversations(false),
        refreshMessages(selectedConversationId, false),
      ]);
      setStatusMessage("メッセージを送信しました");
      setPageError(null);
    } catch (error) {
      setPageError(
        error instanceof Error ? error.message : "送信に失敗しました",
      );
    } finally {
      setSending(false);
    }
  };

  const handleMessageEditStart = (message: SRMessage) => {
    setEditingMessageId(message.message_id);
    setEditingText(message.content);
  };

  const handleMessageUpdate = async () => {
    if (!editingMessageId || !selectedConversationId || !editingText.trim()) {
      return;
    }
    setUpdatingMessage(true);
    try {
      const response = await fetch("/api/sr_chat/chat.update", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          message_id: editingMessageId,
          conversation_id: selectedConversationId,
          text: editingText.trim(),
        }),
      });
      if (!response.ok) {
        setPageError(await readErrorMessage(response));
        return;
      }
      await refreshMessages(selectedConversationId, false);
      setEditingMessageId(null);
      setEditingText("");
      setStatusMessage("メッセージを更新しました");
    } catch (error) {
      setPageError(
        error instanceof Error ? error.message : "更新に失敗しました",
      );
    } finally {
      setUpdatingMessage(false);
    }
  };

  const handleUpload = async (event: ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    event.target.value = "";
    if (!file) {
      return;
    }
    if (!selectedConversationId) {
      setPageError("先に会話を選択してください");
      return;
    }
    setUploading(true);
    try {
      const contentBase64 = await toBase64(file);
      const response = await fetch("/api/sr_chat/files.upload", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          conversation_id: selectedConversationId,
          file_name: file.name,
          content_base64: contentBase64,
          mime_type: file.type || "application/octet-stream",
          user_id: userId || "admin_ui",
        }),
      });
      if (!response.ok) {
        setPageError(await readErrorMessage(response));
        return;
      }
      setStatusMessage(`ファイルアップロードを記録しました: ${file.name}`);
      setPageError(null);
    } catch (error) {
      setPageError(
        error instanceof Error ? error.message : "アップロードに失敗しました",
      );
    } finally {
      setUploading(false);
    }
  };

  const handleExport = async () => {
    if (!selectedConversationId) {
      setPageError("エクスポートする会話を選択してください");
      return;
    }
    setIsExporting(true);
    try {
      const response = await fetch(
        `/api/sr_chat/export?conversation_id=${encodeURIComponent(selectedConversationId)}&format=${selectedFormat}`,
      );
      if (!response.ok) {
        setPageError(await readErrorMessage(response));
        return;
      }
      const payload = (await response.json()) as {
        ok: boolean;
        data: string;
        filename: string;
        format: ExportFormat;
      };
      const mimeByFormat: Record<ExportFormat, string> = {
        json: "application/json;charset=utf-8",
        csv: "text/csv;charset=utf-8",
        markdown: "text/markdown;charset=utf-8",
      };
      const blob = new Blob([payload.data], {
        type: mimeByFormat[payload.format],
      });
      const url = window.URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = payload.filename;
      document.body.appendChild(a);
      a.click();
      window.URL.revokeObjectURL(url);
      a.remove();
      setStatusMessage("エクスポートを完了しました");
      setPageError(null);
    } catch (error) {
      setPageError(
        error instanceof Error ? error.message : "エクスポートに失敗しました",
      );
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
    <div className="flex flex-col gap-4 h-[calc(100vh-6rem)]">
      {(pageError || statusMessage) && (
        <div className="space-y-2">
          {pageError && (
            <div className="glass-panel border border-rose-300/70 bg-rose-50/80 p-3 text-sm text-rose-800 flex items-center justify-between gap-2">
              <span>{pageError}</span>
              <button
                onClick={() => setPageError(null)}
                className="text-rose-700"
              >
                <X size={14} />
              </button>
            </div>
          )}
          {statusMessage && (
            <div className="glass-panel border border-emerald-300/70 bg-emerald-50/80 p-3 text-sm text-emerald-800 flex items-center gap-2">
              <CheckCircle2 size={14} />
              <span>{statusMessage}</span>
            </div>
          )}
        </div>
      )}

      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold text-slate-900">
            会話ワークスペース
          </h2>
          <p className="text-sm text-muted mt-1">
            Messaging Hub 独自チャット画面（sr_chat API 接続）
          </p>
          <div className="text-xs text-muted mt-1 flex items-center gap-3">
            <span className="flex items-center gap-1">
              {isConnected ? <Wifi size={14} /> : <WifiOff size={14} />}
              {isConnected ? "WebSocket 接続中" : "WebSocket 未接続"}
            </span>
            {subscriptionId && <span>subscription: {subscriptionId}</span>}
            {!authChecked && (
              <span className="flex items-center gap-1">
                <Loader2 size={12} className="animate-spin" />
                auth check...
              </span>
            )}
          </div>
        </div>
        <div className="flex items-center gap-2">
          <button
            onClick={() => {
              void refreshConversations();
              if (selectedConversationId) {
                void refreshMessages(selectedConversationId);
              }
            }}
            className="glass-panel px-3 py-2 text-sm flex items-center gap-2"
          >
            <RefreshCw size={14} />
            更新
          </button>
          <button
            onClick={createConversation}
            className="neo-button elevated flex items-center gap-2 px-4 py-2"
          >
            <Plus size={16} />
            新規会話
          </button>
        </div>
      </div>

      {/* ===== チャットエリア: 会話一覧（狭） + メインチャット ===== */}
      <div className="flex gap-4 flex-1 min-h-0 overflow-hidden">
        {/* 会話一覧（現在の約1/3幅 = 固定 w-44） */}
        <div className="w-44 flex-shrink-0 glass-panel flex flex-col">
          <div className="px-3 py-2.5 border-b flex items-center justify-between">
            <h3 className="font-semibold text-sm">会話一覧</h3>
            {loadingConversations && (
              <Loader2 size={14} className="animate-spin text-primary-500" />
            )}
          </div>
          <div className="flex-1 overflow-auto">
            {conversations.length === 0 ? (
              <div className="p-3 text-xs text-gray-500">
                会話がありません。
              </div>
            ) : (
              conversations.map((item) => (
                <button
                  key={item.conversation_id}
                  onClick={() =>
                    setSelectedConversationId(item.conversation_id)
                  }
                  className={clsx(
                    "w-full text-left px-3 py-2.5 border-b transition-colors",
                    selectedConversationId === item.conversation_id
                      ? "bg-cyan-50/70"
                      : "hover:bg-white/70",
                  )}
                >
                  <div className="font-medium text-xs leading-snug truncate">
                    {getConversationTitle(item)}
                  </div>
                  <div className="text-[10px] text-muted mt-0.5">
                    {formatTime(item.last_message_at)}
                  </div>
                </button>
              ))
            )}
          </div>
        </div>

        {/* メインチャットエリア */}
        <div className="flex-1 min-w-0 glass-panel flex flex-col">
          <div className="px-4 py-3 border-b">
            <div className="font-semibold text-sm">
              {selectedConversation
                ? getConversationTitle(selectedConversation)
                : selectedConversationId
                  ? `チャット #${selectedConversationId.split(":")[1]?.slice(0, 8) ?? selectedConversationId}`
                  : "未選択"}
            </div>
            <div className="text-[10px] text-muted mt-0.5 truncate">
              {selectedConversationId || ""}
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

          <div
            ref={messageListRef}
            className="flex-1 overflow-auto p-4 space-y-3 bg-slate-100/60"
          >
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
                  <div className="text-xs text-muted mb-1 flex items-center justify-between gap-2">
                    <span>
                      {message.role} ・ {formatTime(message.created_at)}
                    </span>
                    {message.role === "assistant" &&
                      editingMessageId !== message.message_id && (
                        <button
                          onClick={() => handleMessageEditStart(message)}
                          className="text-primary-600 hover:text-primary-700"
                          title="メッセージを編集"
                        >
                          <Pencil size={14} />
                        </button>
                      )}
                  </div>

                  {editingMessageId === message.message_id ? (
                    <div className="space-y-2">
                      <textarea
                        value={editingText}
                        onChange={(event) => setEditingText(event.target.value)}
                        rows={3}
                        className="w-full border rounded bg-white/80 px-2 py-1"
                      />
                      <div className="flex justify-end gap-2">
                        <button
                          onClick={() => {
                            setEditingMessageId(null);
                            setEditingText("");
                          }}
                          className="px-3 py-1 rounded border bg-white/70"
                        >
                          キャンセル
                        </button>
                        <button
                          onClick={handleMessageUpdate}
                          disabled={updatingMessage || !editingText.trim()}
                          className="neo-button px-3 py-1 disabled:opacity-50 flex items-center gap-1"
                        >
                          {updatingMessage ? (
                            <Loader2 size={14} className="animate-spin" />
                          ) : (
                            <Save size={14} />
                          )}
                          更新
                        </button>
                      </div>
                    </div>
                  ) : (
                    <div className="whitespace-pre-wrap">{message.content}</div>
                  )}
                </div>
              ))
            )}
          </div>

          <div className="p-4 border-t space-y-3">
            <textarea
              value={draft}
              onChange={(event) => setDraft(event.target.value)}
              rows={3}
              className="w-full border border-white/80 bg-white/90 rounded-lg px-3 py-2 text-sm"
              placeholder="メッセージを入力..."
            />

            <div className="flex flex-wrap items-center justify-between gap-2 text-xs">
              <label className="inline-flex items-center gap-2 cursor-pointer border rounded px-2 py-1 bg-white/70">
                {uploading ? (
                  <Loader2 size={14} className="animate-spin" />
                ) : (
                  <Upload size={14} />
                )}
                ファイルアップロード記録
                <input
                  type="file"
                  onChange={handleUpload}
                  className="hidden"
                  disabled={uploading}
                />
              </label>
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
      </div>

      {/* ===== 会話エクスポート（折り畳み・下部固定） ===== */}
      <div className="glass-panel overflow-hidden flex-shrink-0">
        {/* アコーディオン ヘッダー */}
        <button
          onClick={() => setExportOpen((prev) => !prev)}
          className="w-full flex items-center justify-between px-5 py-3.5 text-left hover:bg-white/30 transition-colors"
        >
          <div className="flex items-center gap-2 font-semibold text-slate-700">
            <Download size={16} className="text-primary-600" />
            会話エクスポート
          </div>
          {exportOpen ? (
            <ChevronUp size={16} className="text-slate-400" />
          ) : (
            <ChevronDown size={16} className="text-slate-400" />
          )}
        </button>

        {/* アコーディオン コンテンツ */}
        {exportOpen && (
          <div className="px-5 pb-5 border-t pt-4">
            <div className="grid grid-cols-1 md:grid-cols-3 gap-3 mb-4">
              {formats.map(({ value, label, icon: Icon, description }) => (
                <button
                  key={value}
                  onClick={() => setSelectedFormat(value)}
                  className={clsx(
                    "p-3 rounded-xl border-2 text-left transition-all elevated",
                    selectedFormat === value
                      ? "border-primary-500 bg-cyan-50/70"
                      : "border-white/70 hover:border-primary-300 bg-white/70",
                  )}
                >
                  <div className="flex items-center gap-2 mb-1">
                    <Icon
                      className={
                        selectedFormat === value
                          ? "text-primary-600"
                          : "text-gray-500"
                      }
                      size={18}
                    />
                    <span className="font-medium text-sm">{label}</span>
                  </div>
                  <p className="text-xs text-gray-500">{description}</p>
                </button>
              ))}
            </div>
            <button
              onClick={handleExport}
              disabled={isExporting || !selectedConversationId}
              className="neo-button flex items-center gap-2 px-5 py-2.5 transition-colors disabled:opacity-50 text-sm"
            >
              <Download size={16} />
              {isExporting ? "エクスポート中..." : "エクスポート"}
            </button>
          </div>
        )}
      </div>
    </div>
  );
}

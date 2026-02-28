import { create } from 'zustand';
import type { SessionSummary, ChatMessage, ChatResponse } from '../api/types';
import { chatApi } from '../api/chat';

/** RAG / SQL サービスの稼働状態 */
export interface ServiceStatus {
    /** バックエンドと通信済みか */
    checked: boolean;
    ragEnabled: boolean;
    sqlEnabled: boolean;
}

/** バックエンドのシステム状態をポーリングして取得する独立ストア.
 *
 * ChatWindow マウント時に `startPolling()` を呼び出すことで
 * 30 秒間隔で自動更新される。アンマウント時に `stopPolling()` で停止する。
 */
interface ServiceStatusState extends ServiceStatus {
    startPolling: () => void;
    stopPolling: () => void;
}

let _pollingTimer: ReturnType<typeof setInterval> | null = null;

export const useServiceStatus = create<ServiceStatusState>((set) => ({
    checked: false,
    ragEnabled: false,
    sqlEnabled: false,

    startPolling: () => {
        /** 即時取得 + インターバル設定 */
        const fetchStatus = async () => {
            try {
                const health = await chatApi.getHealth();
                set({
                    checked: true,
                    ragEnabled: health.rag?.enabled ?? false,
                    sqlEnabled: health.sql?.enabled ?? false,
                });
            } catch {
                /* ネットワークエラー等は無視（checked は変更しない） */
            }
        };

        void fetchStatus();
        _pollingTimer ??= setInterval(() => void fetchStatus(), 30_000);
    },

    stopPolling: () => {
        if (_pollingTimer !== null) {
            clearInterval(_pollingTimer);
            _pollingTimer = null;
        }
    },
}));

/* SSE イベントのパース済み型 */
interface SseTokenEvent { type: 'token'; data: string }
interface SseProgressEvent {
    type: 'progress';
    progress: number;
    message: string;
    phase?: string;
    agent?: string;
    query_type?: string;
    skill?: string;
    step?: number;
    total_steps?: number;
}
interface SseResultEvent { type: 'result'; data: ChatResponse }
interface SseErrorEvent { type: 'error'; message: string }
type SseEvent = SseTokenEvent | SseProgressEvent | SseResultEvent | SseErrorEvent;

export interface StreamRuntimeStatus {
    progress: number;
    message: string;
    phase: string;
    agent: string;
    queryType: string;
    skill: string;
    step: number;
    totalSteps: number;
}

/** 生の SSE 行を型安全なイベントに変換する。パース不可なら null を返す。 */
function parseSseLine(line: string): SseEvent | null {
    if (!line.startsWith('data: ')) return null;
    const jsonStr = line.slice(6);
    if (jsonStr.trim() === '[DONE]') return null;
    try {
        const raw: Record<string, unknown> = JSON.parse(jsonStr);
        const eventType = raw.type;
        if (eventType === 'token') {
            return { type: 'token', data: typeof raw.data === 'string' ? raw.data : '' };
        }
        if (eventType === 'progress') {
            return {
                type: 'progress',
                progress: typeof raw.progress === 'number' ? raw.progress : 0,
                message: typeof raw.message === 'string' ? raw.message : '',
                phase: typeof raw.phase === 'string' ? raw.phase : undefined,
                agent: typeof raw.agent === 'string' ? raw.agent : undefined,
                query_type: typeof raw.query_type === 'string' ? raw.query_type : undefined,
                skill: typeof raw.skill === 'string' ? raw.skill : undefined,
                step: typeof raw.step === 'number' ? raw.step : undefined,
                total_steps: typeof raw.total_steps === 'number' ? raw.total_steps : undefined,
            };
        }
        if (eventType === 'result') {
            return { type: 'result', data: raw.data as ChatResponse };
        }
        if (eventType === 'error') {
            return { type: 'error', message: typeof raw.message === 'string' ? raw.message : 'Unknown error' };
        }
        return null;
    } catch {
        return null;
    }
}

/** SSE ストリームの result イベントを処理し、回答テキストを返す */
function handleResultEvent(
    event: SseResultEvent,
    updateLastMessage: (content: string) => void,
): string {
    const data = event.data;
    if (!data) return '';

    const answer = data.answer;
    if (typeof answer === 'string' && answer.length > 0) {
        updateLastMessage(answer);
        return answer;
    }

    // error フィールドからフォールバック
    const error = data.error;
    if (typeof error === 'string' && error.length > 0) {
        const fallback = `回答を生成できませんでした: ${error}`;
        updateLastMessage(fallback);
        return fallback;
    }

    const fallback = '回答を取得できませんでした。再度お試しください。';
    updateLastMessage(fallback);
    return fallback;
}

/** 進捗イベントをチャット表示向けの文面に整形する。 */
function formatProgressText(event: SseProgressEvent): string {
    const agent = event.agent ? `[${event.agent}] ` : '';
    const stepText = (typeof event.step === 'number' && typeof event.total_steps === 'number')
        ? ` (${event.step}/${event.total_steps})`
        : '';
    return `⏳ ${agent}${event.message}${stepText}`;
}

/** ステートフルな SSE パーサ。TCP チャンク境界とイベント境界のずれを吸収する */
class SseParser {
    private buffer = '';

    /** チャンクを受け取り、完全なイベントのみ返す。不完全フラグメントは内部バッファに保持 */
    feed(chunk: string): SseEvent[] {
        this.buffer += chunk;
        const parts = this.buffer.split('\n\n');
        this.buffer = parts.pop() ?? ''; // 最後の不完全フラグメントを保持
        return parts
            .map((p) => parseSseLine(p.trim()))
            .filter((e): e is SseEvent => e !== null);
    }

    /** ストリーム終了時にバッファ残留分をフラッシュ */
    flush(): SseEvent[] {
        if (!this.buffer.trim()) return [];
        const event = parseSseLine(this.buffer.trim());
        this.buffer = '';
        return event ? [event] : [];
    }
}

/** 単一の SSE イベントを処理し、現在の累積レスポンスを返す */
function applyEvent(
    event: SseEvent,
    currentResponse: string,
    updateLastMessage: (content: string) => void,
    setRuntimeStatus: (status: StreamRuntimeStatus | null) => void,
): string {
    switch (event.type) {
        case 'token': {
            const next = currentResponse + event.data;
            updateLastMessage(next);
            return next;
        }
        case 'progress':
            setRuntimeStatus({
                progress: event.progress,
                message: event.message,
                phase: event.phase ?? '',
                agent: event.agent ?? '',
                queryType: event.query_type ?? '',
                skill: event.skill ?? '',
                step: typeof event.step === 'number' ? event.step : 0,
                totalSteps: typeof event.total_steps === 'number' ? event.total_steps : 0,
            });
            updateLastMessage(formatProgressText(event));
            return currentResponse;
        case 'result':
            setRuntimeStatus(null);
            return handleResultEvent(event, updateLastMessage) || currentResponse;
        case 'error':
            setRuntimeStatus(null);
            updateLastMessage(`⚠️ ${event.message}`);
            return currentResponse;
    }
}

interface ChatState {
    sessions: SessionSummary[];
    currentSessionId: string | null;
    messages: ChatMessage[];
    isLoading: boolean;
    isStreaming: boolean;
    lastQueryType: string | null;
    runtimeStatus: StreamRuntimeStatus | null;

    fetchSessions: () => Promise<void>;
    selectSession: (sessionId: string) => Promise<void>;
    createSession: () => void;
    deleteSession: (sessionId: string) => Promise<void>;

    sendMessage: (content: string, options?: Record<string, unknown>) => Promise<void>;
    addMessage: (message: ChatMessage) => void;
    updateLastMessage: (content: string) => void;
    updateLastAssistantMeta: (data: ChatResponse) => void;
    setRuntimeStatus: (status: StreamRuntimeStatus | null) => void;
}

export const useChatStore = create<ChatState>((set, get) => ({
    sessions: [],
    currentSessionId: null,
    messages: [],
    isLoading: false,
    isStreaming: false,
    lastQueryType: null,
    runtimeStatus: null,

    fetchSessions: async () => {
        try {
            const res = await chatApi.listSessions();
            set({ sessions: res.sessions });
        } catch {
            /* セッション一覧取得失敗は UI 上で無視する */
        }
    },

    selectSession: async (sessionId) => {
        set({ currentSessionId: sessionId, isLoading: true, lastQueryType: null, runtimeStatus: null });
        try {
            const res = await chatApi.getHistory(sessionId);
            set({ messages: res.messages || [] });
        } catch {
            set({ messages: [] });
        } finally {
            set({ isLoading: false });
        }
    },

    createSession: () => {
        set({ currentSessionId: null, messages: [], lastQueryType: null, runtimeStatus: null });
    },

    deleteSession: async (sessionId) => {
        try {
            await chatApi.deleteSession(sessionId);
            set((state) => ({
                sessions: state.sessions.filter((s) => s.session_id !== sessionId),
                currentSessionId: state.currentSessionId === sessionId ? null : state.currentSessionId,
                messages: state.currentSessionId === sessionId ? [] : state.messages,
                lastQueryType: state.currentSessionId === sessionId ? null : state.lastQueryType,
                runtimeStatus: state.currentSessionId === sessionId ? null : state.runtimeStatus,
            }));
        } catch {
            /* 削除失敗時は UI 上で無視する */
        }
    },

    addMessage: (msg) => set((state) => ({ messages: [...state.messages, msg] })),

    updateLastMessage: (content) => set((state) => {
        const msgs = [...state.messages];
        const last = msgs.at(-1);
        if (last) {
            msgs[msgs.length - 1] = { ...last, content };
        }
        return { messages: msgs };
    }),

    updateLastAssistantMeta: (data) => set((state) => {
        const msgs = [...state.messages];
        const last = msgs.at(-1);
        const queryType = typeof data.query_type === 'string' ? data.query_type : null;
        if (last && last.role === 'assistant') {
            msgs[msgs.length - 1] = {
                ...last,
                query_type: queryType ?? undefined,
                verification: data.verification ?? undefined,
            };
        }
        return { messages: msgs, lastQueryType: queryType };
    }),

    setRuntimeStatus: (status) => set({ runtimeStatus: status }),

    sendMessage: async (content, options) => {
        const {
            currentSessionId,
            addMessage,
            updateLastMessage,
            updateLastAssistantMeta,
            fetchSessions,
            setRuntimeStatus,
        } = get();

        addMessage({ role: 'user', content });
        set({ isStreaming: true, runtimeStatus: null });
        addMessage({ role: 'assistant', content: '' });

        let fullResponse = '';
        try {
            const stream = chatApi.streamMessage({
                message: content,
                session_id: currentSessionId || undefined,
                options,
            });

            const parser = new SseParser();
            for await (const chunk of stream) {
                for (const event of parser.feed(chunk)) {
                    fullResponse = applyEvent(event, fullResponse, updateLastMessage, setRuntimeStatus);
                    if (event.type === 'result' && event.data) {
                        updateLastAssistantMeta(event.data);
                    }
                    if (event.type === 'result' && !currentSessionId && event.data?.session_id) {
                        set({ currentSessionId: event.data.session_id });
                        await fetchSessions();
                    }
                }
            }
            // ストリーム終了後にバッファ残留分を処理
            for (const event of parser.flush()) {
                fullResponse = applyEvent(event, fullResponse, updateLastMessage, setRuntimeStatus);
                if (event.type === 'result' && event.data) {
                    updateLastAssistantMeta(event.data);
                }
                if (event.type === 'result' && !currentSessionId && event.data?.session_id) {
                    set({ currentSessionId: event.data.session_id });
                    await fetchSessions();
                }
            }
        } catch (error: unknown) {
            const msg = error instanceof Error ? error.message : String(error);
            updateLastMessage(`⚠️ ${msg}`);
        } finally {
            set({ isStreaming: false, runtimeStatus: null });
        }
    },
}));

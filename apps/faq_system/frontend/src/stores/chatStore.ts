import { create } from 'zustand';
import type { SessionSummary, ChatMessage, ChatResponse } from '../api/types';
import { chatApi } from '../api/chat';

/* SSE イベントのパース済み型 */
interface SseTokenEvent { type: 'token'; data: string }
interface SseProgressEvent { type: 'progress'; progress: number; message: string }
interface SseResultEvent { type: 'result'; data: ChatResponse }
interface SseErrorEvent { type: 'error'; message: string }
type SseEvent = SseTokenEvent | SseProgressEvent | SseResultEvent | SseErrorEvent;

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
    const answer = event.data?.answer;
    if (answer) {
        updateLastMessage(answer);
        return answer;
    }
    return '';
}

/** SSE チャンクから全イベントをパースして返す */
function parseChunk(chunk: string): SseEvent[] {
    return chunk
        .split('\n\n')
        .map(parseSseLine)
        .filter((e): e is SseEvent => e !== null);
}

/** 単一の SSE イベントを処理し、現在の累積レスポンスを返す */
function applyEvent(
    event: SseEvent,
    currentResponse: string,
    updateLastMessage: (content: string) => void,
): string {
    switch (event.type) {
        case 'token': {
            const next = currentResponse + event.data;
            updateLastMessage(next);
            return next;
        }
        case 'progress':
            updateLastMessage(`⏳ ${event.message} (${event.progress}%)`);
            return currentResponse;
        case 'result':
            return handleResultEvent(event, updateLastMessage) || currentResponse;
        case 'error':
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

    fetchSessions: () => Promise<void>;
    selectSession: (sessionId: string) => Promise<void>;
    createSession: () => void;
    deleteSession: (sessionId: string) => Promise<void>;

    sendMessage: (content: string, options?: Record<string, unknown>) => Promise<void>;
    addMessage: (message: ChatMessage) => void;
    updateLastMessage: (content: string) => void;
}

export const useChatStore = create<ChatState>((set, get) => ({
    sessions: [],
    currentSessionId: null,
    messages: [],
    isLoading: false,
    isStreaming: false,

    fetchSessions: async () => {
        try {
            const res = await chatApi.listSessions();
            set({ sessions: res.sessions });
        } catch {
            /* セッション一覧取得失敗は UI 上で無視する */
        }
    },

    selectSession: async (sessionId) => {
        set({ currentSessionId: sessionId, isLoading: true });
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
        set({ currentSessionId: null, messages: [] });
    },

    deleteSession: async (sessionId) => {
        try {
            await chatApi.deleteSession(sessionId);
            set((state) => ({
                sessions: state.sessions.filter((s) => s.session_id !== sessionId),
                currentSessionId: state.currentSessionId === sessionId ? null : state.currentSessionId,
                messages: state.currentSessionId === sessionId ? [] : state.messages,
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

    sendMessage: async (content, options) => {
        const { currentSessionId, addMessage, updateLastMessage, fetchSessions } = get();

        addMessage({ role: 'user', content });
        set({ isStreaming: true });
        addMessage({ role: 'assistant', content: '' });

        let fullResponse = '';
        try {
            const stream = chatApi.streamMessage({
                message: content,
                session_id: currentSessionId || undefined,
                options,
            });

            for await (const chunk of stream) {
                for (const event of parseChunk(chunk)) {
                    fullResponse = applyEvent(event, fullResponse, updateLastMessage);
                    if (event.type === 'result' && !currentSessionId && event.data?.session_id) {
                        set({ currentSessionId: event.data.session_id });
                        await fetchSessions();
                    }
                }
            }
        } catch (error: unknown) {
            const msg = error instanceof Error ? error.message : String(error);
            updateLastMessage(`⚠️ ${msg}`);
        } finally {
            set({ isStreaming: false });
        }
    },
}));

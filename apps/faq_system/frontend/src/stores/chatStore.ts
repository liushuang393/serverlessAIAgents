import { create } from 'zustand';
import type { SessionSummary, ChatMessage, ChatResponse } from '../api/types';
import { chatApi } from '../api/chat';

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
        } catch (error) {
            console.error('Failed to list sessions', error);
        }
    },

    selectSession: async (sessionId) => {
        set({ currentSessionId: sessionId, isLoading: true });
        try {
            const res = await chatApi.getHistory(sessionId);
            set({ messages: res.messages || [] });
        } catch (error) {
            console.error('Failed to load history', error);
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
        } catch (error) {
            console.error('Failed to delete session', error);
        }
    },

    addMessage: (msg) => set((state) => ({ messages: [...state.messages, msg] })),

    updateLastMessage: (content) => set((state) => {
        const msgs = [...state.messages];
        if (msgs.length > 0) {
            msgs[msgs.length - 1] = { ...msgs[msgs.length - 1], content };
        }
        return { messages: msgs };
    }),

    sendMessage: async (content, options) => {
        const { currentSessionId, addMessage, updateLastMessage, fetchSessions } = get();

        // Add user message
        const userMsg: ChatMessage = { role: 'user', content };
        addMessage(userMsg);
        set({ isStreaming: true });

        let fullResponse = '';
        // Add placeholder assistant message
        addMessage({ role: 'assistant', content: '' });

        try {
            const stream = chatApi.streamMessage({
                message: content,
                session_id: currentSessionId || undefined, // undefined for new session
                options,
            });

            for await (const chunk of stream) {
                // Check for potential SSE format "data: {json}\n\n"
                const lines = chunk.split('\n\n');
                for (const line of lines) {
                    if (line.startsWith('data: ')) {
                        const jsonStr = line.slice(6);
                        if (jsonStr.trim() === '[DONE]') continue;
                        try {
                            const event = JSON.parse(jsonStr);
                            if (event.type === 'token') {
                                fullResponse += event.data;
                                updateLastMessage(fullResponse);
                            } else if (event.type === 'result') {
                                // Final result with metadata
                                // Update session ID if it was a new session
                                const result = event.data as ChatResponse;
                                if (!currentSessionId && result.session_id) {
                                    set({ currentSessionId: result.session_id });
                                    await fetchSessions(); // Refresh list to show new session
                                }
                            } else if (event.type === 'error') {
                                updateLastMessage(`Error: ${event.message}`);
                            }
                        } catch {
                            // Fallback for plain text or malformed json
                            // ignore malformed chunk
                        }
                    }
                }
            }
        } catch (error) {
            updateLastMessage(`Error: ${error}`);
        } finally {
            set({ isStreaming: false });
        }
    },
}));

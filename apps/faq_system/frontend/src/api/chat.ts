import { apiClient } from './client';
import type { ChatHistoryResponse, ChatRequest, ChatResponse, SessionListResponse } from './types';

export const chatApi = {
    sendMessage: (data: ChatRequest) => apiClient.post<ChatResponse>('/chat', data),

    streamMessage: (data: ChatRequest) => apiClient.stream('/chat/stream', data),

    listSessions: (limit = 50, offset = 0) =>
        apiClient.get<SessionListResponse>(`/chat/sessions?limit=${limit}&offset=${offset}`),

    deleteSession: (sessionId: string) =>
        apiClient.delete<{ success: boolean }>(`/chat/sessions/${sessionId}`),

    getHistory: (sessionId: string) =>
        apiClient.get<ChatHistoryResponse>(`/chat/history?session_id=${sessionId}`),
};

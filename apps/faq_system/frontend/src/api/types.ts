export interface UserInfo {
    user_id: string;
    username: string;
    display_name: string;
    role: string;
    department?: string;
    position?: string;
}

export interface RegisterRequest {
    username: string;
    password: string;
    display_name: string;
    department?: string;
    position?: string;
    email?: string;
}

export interface AuthResponse {
    success: boolean;
    message: string;
    user?: UserInfo;
    access_token?: string;
    token_type?: string;
}

export interface SessionSummary {
    session_id: string;
    title: string;
    message_count: number;
    last_message_at: string; // ISO Date
    preview: string;
}

export interface SessionListResponse {
    count: number;
    sessions: SessionSummary[];
}

export interface ChatMessage {
    id?: string;
    role: 'user' | 'assistant' | 'system';
    content: string;
    created_at?: string;
}

export interface ChatResponse {
    answer: string;
    session_id: string;
    query_type: string;
    documents?: Array<Record<string, unknown>>;
    chart?: Record<string, unknown> | null;
    suggestions?: Array<Record<string, unknown> | string>;
    rich_response?: Record<string, unknown> | null;
}

export interface ChatRequest {
    message: string;
    session_id?: string;
    options?: Record<string, unknown>;
}

export interface ChatHistoryResponse {
    messages: ChatMessage[];
}

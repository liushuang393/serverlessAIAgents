declare const __APP_CONFIG__: {
    apiPort: number;
    frontendPort: number;
    backendUrl: string;
};

// 実行時にベース URL を決定する（ハードコードを避ける）
const getBaseUrl = () => {
    if (import.meta.env.DEV) return '/api';

    const configUrl = __APP_CONFIG__.backendUrl;
    if (configUrl) return `${configUrl}/api`;

    // 設定がない場合は現在のホストと設定されたポートから推測する
    const { protocol, hostname } = window.location;
    return `${protocol}//${hostname}:${__APP_CONFIG__.apiPort}/api`;
};

const BASE_URL = getBaseUrl();

/** API エラー body の detail（FastAPI は文字列 or バリデーション配列）を表示用文字列に変換する */
function normalizeErrorDetail(detail: unknown): string {
    if (detail == null) return '';
    if (typeof detail === 'string') return detail;
    if (Array.isArray(detail)) {
        const messages = detail
            .map((item: { msg?: string; loc?: unknown[] }) => (item && typeof item.msg === 'string' ? item.msg : null))
            .filter(Boolean);
        return messages.length > 0 ? messages.join(' ') : JSON.stringify(detail);
    }
    if (typeof detail === 'object') return JSON.stringify(detail);
    return String(detail);
}

class ApiClient {
    private getToken(): string | null {
        return localStorage.getItem('access_token');
    }

    private async request<T>(endpoint: string, options: RequestInit = {}): Promise<T> {
        const token = this.getToken();
        const headers = {
            'Content-Type': 'application/json',
            ...(token ? { Authorization: `Bearer ${token}` } : {}),
            ...options.headers,
        };

        const response = await fetch(`${BASE_URL}${endpoint}`, {
            ...options,
            headers,
        });

        if (response.status === 401) {
            // Token expired or invalid
            localStorage.removeItem('access_token');
            localStorage.removeItem('user_info');
            if (!window.location.pathname.startsWith('/login')) {
                window.location.href = '/login';
            }
            throw new Error('Unauthorized');
        }

        if (!response.ok) {
            const errorData = await response.json().catch(() => ({ detail: response.statusText }));
            const message = normalizeErrorDetail(errorData.detail) || `Error ${response.status}`;
            throw new Error(message);
        }

        return response.json();
    }

    get<T>(endpoint: string): Promise<T> {
        return this.request<T>(endpoint, { method: 'GET' });
    }

    post<T>(endpoint: string, body: unknown): Promise<T> {
        return this.request<T>(endpoint, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    }

    delete<T>(endpoint: string): Promise<T> {
        return this.request<T>(endpoint, { method: 'DELETE' });
    }

    async *stream(endpoint: string, body: unknown): AsyncGenerator<string, void, unknown> {
        const token = this.getToken();
        const response = await fetch(`${BASE_URL}${endpoint}`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                ...(token ? { Authorization: `Bearer ${token}` } : {}),
            },
            body: JSON.stringify(body),
        });

        if (!response.ok || !response.body) {
            const errorData = await response.json().catch(() => ({ detail: response.statusText }));
            const message = normalizeErrorDetail(errorData.detail) || `Error ${response.status}`;
            throw new Error(message);
        }

        const reader = response.body.getReader();
        const decoder = new TextDecoder();

        try {
            while (true) {
                const { done, value } = await reader.read();
                if (done) break;
                yield decoder.decode(value, { stream: true });
            }
        } finally {
            reader.releaseLock();
        }
    }
}

export const apiClient = new ApiClient();

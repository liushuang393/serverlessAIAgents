import { apiClient } from './client';
import type { AuthResponse, RegisterRequest } from './types';

export const authApi = {
    login: (username: string, password: string, totpCode?: string): Promise<AuthResponse> => {
        const body: Record<string, string> = { username, password };
        if (totpCode) {
            body.totp_code = totpCode;
        }
        return apiClient.post<AuthResponse>('/auth/login', body);
    },

    register: (data: RegisterRequest) => apiClient.post<AuthResponse>('/auth/register', data),

    logout: () => apiClient.post<{ success: boolean }>('/auth/logout', {}),

    getMe: () => apiClient.get<AuthResponse>('/auth/me'),
};

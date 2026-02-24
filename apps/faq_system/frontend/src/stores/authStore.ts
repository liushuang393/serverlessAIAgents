import { create } from 'zustand';
import type { UserInfo } from '../api/types';
import { authApi } from '../api/auth';

interface AuthState {
    user: UserInfo | null;
    isAuthenticated: boolean;
    isLoading: boolean;
    login: (token: string, user: UserInfo) => void;
    logout: () => Promise<void>;
    checkAuth: () => Promise<void>;
}

export const useAuthStore = create<AuthState>((set) => ({
    user: null,
    isAuthenticated: false,
    isLoading: true,

    login: (token, user) => {
        localStorage.setItem('access_token', token);
        set({ user, isAuthenticated: true });
    },

    logout: async () => {
        try {
            await authApi.logout();
        } catch {
            // Best-effort: clear local state even if backend call fails
        }
        localStorage.removeItem('access_token');
        set({ user: null, isAuthenticated: false });
    },

    checkAuth: async () => {
        const token = localStorage.getItem('access_token');
        if (!token) {
            set({ isLoading: false, isAuthenticated: false });
            return;
        }

        try {
            const response = await authApi.getMe();
            if (response.success && response.user) {
                set({ user: response.user, isAuthenticated: true });
            } else {
                throw new Error('Invalid session');
            }
        } catch {
            /* 認証チェック失敗時はトークンをクリアしてログアウト状態にする */
            localStorage.removeItem('access_token');
            set({ user: null, isAuthenticated: false });
        } finally {
            set({ isLoading: false });
        }
    },
}));

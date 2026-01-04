/**
 * 認証状態管理ストア.
 *
 * 目的: ログイン状態・ユーザー情報の管理
 * 技術: Zustand
 */

import { create } from 'zustand';
import { persist, createJSONStorage } from 'zustand/middleware';
import type { UserInfo, AuthResponse } from '../types';

/** 認証ストア状態 */
interface AuthState {
  // ユーザー情報
  user: UserInfo | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  error: string | null;

  // アクション
  setUser: (user: UserInfo | null) => void;
  setLoading: (loading: boolean) => void;
  setError: (error: string | null) => void;
  logout: () => void;
  
  // API連携
  checkAuth: () => Promise<boolean>;
  login: (username: string, password: string) => Promise<AuthResponse>;
  performLogout: () => Promise<void>;
}

const API_BASE = 'http://localhost:8000';

/**
 * 認証ストア.
 */
export const useAuthStore = create<AuthState>()(
  persist(
    (set, _get) => ({
      // 初期状態
      user: null,
      isAuthenticated: false,
      isLoading: true,
      error: null,

      // アクション
      setUser: (user) =>
        set({
          user,
          isAuthenticated: !!user,
          error: null,
        }),

      setLoading: (loading) => set({ isLoading: loading }),

      setError: (error) => set({ error }),

      logout: () =>
        set({
          user: null,
          isAuthenticated: false,
          error: null,
        }),

      // 認証状態チェック
      checkAuth: async () => {
        set({ isLoading: true });
        try {
          const response = await fetch(`${API_BASE}/api/auth/me`, {
            credentials: 'include',
          });
          const data: AuthResponse = await response.json();

          if (data.success && data.user) {
            set({
              user: data.user,
              isAuthenticated: true,
              isLoading: false,
              error: null,
            });
            return true;
          } else {
            set({
              user: null,
              isAuthenticated: false,
              isLoading: false,
            });
            return false;
          }
        } catch {
          set({
            user: null,
            isAuthenticated: false,
            isLoading: false,
            error: '認証状態の確認に失敗しました',
          });
          return false;
        }
      },

      // ログイン
      login: async (username, password) => {
        set({ isLoading: true, error: null });
        try {
          const response = await fetch(`${API_BASE}/api/auth/login`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            credentials: 'include',
            body: JSON.stringify({ username, password }),
          });
          const data: AuthResponse = await response.json();

          if (data.success && data.user) {
            set({
              user: data.user,
              isAuthenticated: true,
              isLoading: false,
              error: null,
            });
          } else {
            set({
              isLoading: false,
              error: data.message || 'ログインに失敗しました',
            });
          }
          return data;
        } catch (err) {
          const message = err instanceof Error ? err.message : 'ログインに失敗しました';
          set({ isLoading: false, error: message });
          return { success: false, message };
        }
      },

      // ログアウト
      performLogout: async () => {
        try {
          await fetch(`${API_BASE}/api/auth/logout`, {
            method: 'POST',
            credentials: 'include',
          });
        } catch {
          // ログアウトエラーは無視
        }
        set({
          user: null,
          isAuthenticated: false,
          error: null,
        });
      },
    }),
    {
      name: 'auth-storage',
      storage: createJSONStorage(() => localStorage),
      partialize: (state) => ({
        user: state.user,
        isAuthenticated: state.isAuthenticated,
      }),
    }
  )
);


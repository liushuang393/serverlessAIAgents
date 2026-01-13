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

// 开发环境使用相对路径（通过 Vite 代理），生产环境使用环境变量
const API_BASE = import.meta.env.VITE_API_URL || '';

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
        // 既にログイン済みの場合は isLoading を設定しない（画面フリッカー防止）
        const currentState = _get();
        if (!currentState.isAuthenticated) {
          set({ isLoading: true });
        }
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
        } catch (err) {
          // 認証チェック失敗時はエラー詳細を含める
          const errorMessage = err instanceof Error
            ? `認証状態の確認に失敗しました: ${err.message}`
            : '認証状態の確認に失敗しました';
          set({
            user: null,
            isAuthenticated: false,
            isLoading: false,
            error: errorMessage,
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
        let logoutError: string | null = null;
        try {
          await fetch(`${API_BASE}/api/auth/logout`, {
            method: 'POST',
            credentials: 'include',
          });
        } catch (err) {
          // ログアウトAPIが失敗しても、ローカル状態はクリアする
          // ただし、ユーザーにはサーバー側の問題を通知
          logoutError = err instanceof Error
            ? `ログアウト通信エラー（ローカルセッションはクリア済み）: ${err.message}`
            : 'ログアウト通信エラー（ローカルセッションはクリア済み）';
        }
        set({
          user: null,
          isAuthenticated: false,
          error: logoutError,
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


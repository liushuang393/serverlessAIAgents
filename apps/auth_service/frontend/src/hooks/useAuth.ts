/**
 * 認証状態管理フック
 *
 * React Context を利用してアプリ全体で認証状態を共有する。
 * 管理者権限（permissions に "*" を含む）を確認する機能を提供する。
 */

import {
  createContext,
  useContext,
  useState,
  useEffect,
  useCallback,
  createElement,
  type ReactNode,
} from "react";
import {
  type UserInfo,
  login as apiLogin,
  logout as apiLogout,
  fetchMe,
  getAccessToken,
  clearTokens,
} from "../api/client";

// --------------------------------------------------------------------------
// 型定義
// --------------------------------------------------------------------------

interface AuthState {
  /** 現在のユーザー情報（未認証時は null） */
  user: UserInfo | null;
  /** 認証状態の読み込み中フラグ */
  loading: boolean;
  /** 管理者権限フラグ（permissions に "*" を含む） */
  isAdmin: boolean;
  /** ログイン処理 */
  login: (username: string, password: string) => Promise<string | null>;
  /** ログアウト処理 */
  logout: () => Promise<void>;
}

// --------------------------------------------------------------------------
// Context
// --------------------------------------------------------------------------

const AuthContext = createContext<AuthState | null>(null);

// --------------------------------------------------------------------------
// Provider
// --------------------------------------------------------------------------

export function AuthProvider({ children }: { children: ReactNode }) {
  const [user, setUser] = useState<UserInfo | null>(null);
  const [loading, setLoading] = useState(true);

  const isAdmin = user !== null && user.permissions.includes("*");

  // 初期化時にトークンが存在すればユーザー情報を取得
  useEffect(() => {
    let cancelled = false;

    async function init() {
      const token = getAccessToken();
      if (!token) {
        setLoading(false);
        return;
      }
      try {
        const res = await fetchMe();
        if (!cancelled && res.success) {
          setUser(res.user);
        }
      } catch {
        // トークン無効の場合はクリア
        if (!cancelled) {
          clearTokens();
        }
      } finally {
        if (!cancelled) {
          setLoading(false);
        }
      }
    }

    init();

    return () => {
      cancelled = true;
    };
  }, []);

  const login = useCallback(
    async (username: string, password: string): Promise<string | null> => {
      try {
        const res = await apiLogin(username, password);
        if (res.success && res.user) {
          // 管理者チェック
          if (!res.user.permissions.includes("*")) {
            clearTokens();
            return "管理者権限がありません";
          }
          setUser(res.user);
          return null;
        }
        return res.message || "ログインに失敗しました";
      } catch (err) {
        if (err instanceof Error) {
          return err.message;
        }
        return "ログインに失敗しました";
      }
    },
    [],
  );

  const logoutFn = useCallback(async () => {
    try {
      await apiLogout();
    } finally {
      setUser(null);
    }
  }, []);

  const value: AuthState = {
    user,
    loading,
    isAdmin,
    login,
    logout: logoutFn,
  };

  return createElement(AuthContext.Provider, { value }, children);
}

// --------------------------------------------------------------------------
// フック
// --------------------------------------------------------------------------

export function useAuth(): AuthState {
  const ctx = useContext(AuthContext);
  if (!ctx) {
    throw new Error("useAuth は AuthProvider 内で使用してください");
  }
  return ctx;
}

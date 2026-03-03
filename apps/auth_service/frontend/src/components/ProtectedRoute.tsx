/**
 * 認証ガードコンポーネント
 *
 * 未認証ユーザーをログインページへリダイレクトする。
 * 管理者権限がない場合もアクセスを拒否する。
 */

import { Navigate } from "react-router-dom";
import { useAuth } from "../hooks/useAuth";
import type { ReactNode } from "react";

// --------------------------------------------------------------------------
// スタイル
// --------------------------------------------------------------------------

const loadingStyle: React.CSSProperties = {
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  height: "100vh",
  fontSize: "1.1rem",
  color: "#666",
};

const deniedContainer: React.CSSProperties = {
  display: "flex",
  flexDirection: "column",
  alignItems: "center",
  justifyContent: "center",
  height: "100vh",
  gap: "1rem",
};

const deniedTitle: React.CSSProperties = {
  fontSize: "1.4rem",
  fontWeight: 700,
  color: "#c0392b",
};

const deniedText: React.CSSProperties = {
  color: "#666",
};

const logoutBtn: React.CSSProperties = {
  padding: "0.5rem 1.5rem",
  background: "#e74c3c",
  color: "#fff",
  border: "none",
  borderRadius: "4px",
  cursor: "pointer",
  fontSize: "0.9rem",
};

// --------------------------------------------------------------------------
// コンポーネント
// --------------------------------------------------------------------------

interface Props {
  children: ReactNode;
}

export function ProtectedRoute({ children }: Props) {
  const { user, loading, isAdmin, logout } = useAuth();

  if (loading) {
    return <div style={loadingStyle}>読み込み中...</div>;
  }

  if (!user) {
    return <Navigate to="/login" replace />;
  }

  if (!isAdmin) {
    return (
      <div style={deniedContainer}>
        <div style={deniedTitle}>アクセス拒否</div>
        <div style={deniedText}>管理者権限が必要です。</div>
        <button
          type="button"
          style={logoutBtn}
          onClick={() => {
            void logout();
          }}
        >
          ログアウト
        </button>
      </div>
    );
  }

  return <>{children}</>;
}

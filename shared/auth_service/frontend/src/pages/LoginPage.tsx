/**
 * ログインページ
 *
 * ユーザー名とパスワードで認証を行う。
 * 管理者権限がないユーザーはログイン拒否する。
 */

import { useState, type FormEvent } from "react";
import { useNavigate } from "react-router-dom";
import { useAuth } from "../hooks/useAuth";

// --------------------------------------------------------------------------
// スタイル
// --------------------------------------------------------------------------

const page: React.CSSProperties = {
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  minHeight: "100vh",
  background: "#f0f2f5",
};

const card: React.CSSProperties = {
  background: "#fff",
  borderRadius: "8px",
  boxShadow: "0 2px 12px rgba(0,0,0,0.08)",
  padding: "2.5rem 2rem",
  width: "100%",
  maxWidth: "380px",
};

const title: React.CSSProperties = {
  textAlign: "center" as const,
  fontSize: "1.4rem",
  fontWeight: 700,
  marginBottom: "0.3rem",
  color: "#1a1a2e",
};

const subtitle: React.CSSProperties = {
  textAlign: "center" as const,
  fontSize: "0.85rem",
  color: "#888",
  marginBottom: "1.8rem",
};

const label: React.CSSProperties = {
  display: "block",
  fontSize: "0.85rem",
  fontWeight: 600,
  color: "#333",
  marginBottom: "0.3rem",
};

const input: React.CSSProperties = {
  width: "100%",
  padding: "0.6rem 0.8rem",
  border: "1px solid #d0d0d0",
  borderRadius: "4px",
  fontSize: "0.95rem",
  outline: "none",
  marginBottom: "1rem",
};

const button: React.CSSProperties = {
  width: "100%",
  padding: "0.7rem",
  background: "#1a1a2e",
  color: "#fff",
  border: "none",
  borderRadius: "4px",
  fontSize: "1rem",
  fontWeight: 600,
  cursor: "pointer",
  marginTop: "0.5rem",
};

const buttonDisabled: React.CSSProperties = {
  ...button,
  background: "#999",
  cursor: "not-allowed",
};

const errorBox: React.CSSProperties = {
  background: "#ffeaea",
  color: "#c0392b",
  padding: "0.6rem 0.8rem",
  borderRadius: "4px",
  fontSize: "0.85rem",
  marginBottom: "1rem",
};

// --------------------------------------------------------------------------
// コンポーネント
// --------------------------------------------------------------------------

export function LoginPage() {
  const navigate = useNavigate();
  const { user, login } = useAuth();

  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState<string | null>(null);
  const [submitting, setSubmitting] = useState(false);

  // 既にログイン済みならリダイレクト
  if (user) {
    navigate("/users", { replace: true });
    return null;
  }

  async function handleSubmit(e: FormEvent) {
    e.preventDefault();
    setError(null);
    setSubmitting(true);
    try {
      const errMsg = await login(username, password);
      if (errMsg) {
        setError(errMsg);
      } else {
        navigate("/users", { replace: true });
      }
    } finally {
      setSubmitting(false);
    }
  }

  return (
    <div style={page}>
      <form style={card} onSubmit={(e) => void handleSubmit(e)}>
        <div style={title}>Auth Service</div>
        <div style={subtitle}>管理パネルログイン</div>

        {error && <div style={errorBox}>{error}</div>}

        <label style={label} htmlFor="username">
          ユーザー名
        </label>
        <input
          id="username"
          style={input}
          type="text"
          autoComplete="username"
          value={username}
          onChange={(e) => setUsername(e.target.value)}
          required
        />

        <label style={label} htmlFor="password">
          パスワード
        </label>
        <input
          id="password"
          style={input}
          type="password"
          autoComplete="current-password"
          value={password}
          onChange={(e) => setPassword(e.target.value)}
          required
        />

        <button
          type="submit"
          style={submitting ? buttonDisabled : button}
          disabled={submitting}
        >
          {submitting ? "ログイン中..." : "ログイン"}
        </button>
      </form>
    </div>
  );
}

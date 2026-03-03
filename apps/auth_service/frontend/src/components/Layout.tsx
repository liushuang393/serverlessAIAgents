/**
 * 共通レイアウトコンポーネント
 *
 * サイドバーナビゲーション + メインコンテンツ領域。
 */

import { NavLink, Outlet } from "react-router-dom";
import { useAuth } from "../hooks/useAuth";

// --------------------------------------------------------------------------
// スタイル
// --------------------------------------------------------------------------

const wrapper: React.CSSProperties = {
  display: "flex",
  minHeight: "100vh",
};

const sidebar: React.CSSProperties = {
  width: "220px",
  background: "#1a1a2e",
  color: "#e0e0e0",
  display: "flex",
  flexDirection: "column",
  flexShrink: 0,
};

const brand: React.CSSProperties = {
  padding: "1.2rem 1rem",
  fontSize: "1.1rem",
  fontWeight: 700,
  borderBottom: "1px solid #2d2d44",
  color: "#fff",
};

const nav: React.CSSProperties = {
  flex: 1,
  display: "flex",
  flexDirection: "column",
  padding: "0.5rem 0",
};

const navItemBase: React.CSSProperties = {
  display: "block",
  padding: "0.7rem 1.2rem",
  color: "#b0b0c0",
  textDecoration: "none",
  fontSize: "0.95rem",
  borderLeft: "3px solid transparent",
  transition: "background 0.15s, color 0.15s",
};

const navItemActive: React.CSSProperties = {
  ...navItemBase,
  color: "#fff",
  background: "rgba(255,255,255,0.08)",
  borderLeftColor: "#4a9eff",
};

const userArea: React.CSSProperties = {
  padding: "1rem",
  borderTop: "1px solid #2d2d44",
  fontSize: "0.85rem",
};

const userName: React.CSSProperties = {
  color: "#fff",
  fontWeight: 600,
  marginBottom: "0.3rem",
};

const logoutButton: React.CSSProperties = {
  background: "none",
  border: "1px solid #555",
  color: "#b0b0c0",
  padding: "0.3rem 0.8rem",
  borderRadius: "4px",
  cursor: "pointer",
  fontSize: "0.8rem",
  marginTop: "0.5rem",
  width: "100%",
};

const main: React.CSSProperties = {
  flex: 1,
  padding: "2rem",
  overflow: "auto",
};

// --------------------------------------------------------------------------
// ナビゲーション定義
// --------------------------------------------------------------------------

const navItems = [
  { to: "/users", label: "ユーザー管理" },
  { to: "/roles", label: "ロール管理" },
  { to: "/resources", label: "リソース権限" },
] as const;

// --------------------------------------------------------------------------
// コンポーネント
// --------------------------------------------------------------------------

export function Layout() {
  const { user, logout } = useAuth();

  return (
    <div style={wrapper}>
      <aside style={sidebar}>
        <div style={brand}>Auth 管理パネル</div>
        <nav style={nav}>
          {navItems.map((item) => (
            <NavLink
              key={item.to}
              to={item.to}
              style={({ isActive }) =>
                isActive ? navItemActive : navItemBase
              }
            >
              {item.label}
            </NavLink>
          ))}
        </nav>
        <div style={userArea}>
          <div style={userName}>{user?.display_name ?? ""}</div>
          <div style={{ color: "#888" }}>@{user?.username ?? ""}</div>
          <button
            type="button"
            style={logoutButton}
            onClick={() => {
              void logout();
            }}
          >
            ログアウト
          </button>
        </div>
      </aside>
      <main style={main}>
        <Outlet />
      </main>
    </div>
  );
}

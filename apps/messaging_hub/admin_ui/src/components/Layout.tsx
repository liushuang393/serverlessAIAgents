import { Outlet, NavLink } from "react-router-dom";
import {
  LayoutDashboard,
  MessageSquare,
  Users,
  Settings,
  Radio,
} from "lucide-react";
import clsx from "clsx";

/**
 * ナビゲーションリンク
 */
const navItems = [
  { to: "/", icon: LayoutDashboard, label: "ダッシュボード" },
  { to: "/platforms", icon: Radio, label: "プラットフォーム" },
  { to: "/sessions", icon: Users, label: "セッション" },
  { to: "/conversations", icon: MessageSquare, label: "チャット" },
  { to: "/settings", icon: Settings, label: "設定" },
];

/**
 * レイアウトコンポーネント
 *
 * サイドバーナビゲーションとメインコンテンツエリア
 */
export default function Layout() {
  return (
    <div className="app-shell">
      <div className="ambient-orb ambient-orb--one" />
      <div className="ambient-orb ambient-orb--two" />
      {/* サイドバー */}
      <div className="relative z-10 flex h-screen">
        <aside className="w-72 glass-panel m-3 p-1 hidden md:block">
          <div className="p-6">
            <h1 className="text-xl text-primary-700 sidebar-title">
              📱 Messaging Hub
            </h1>
            <p className="text-sm text-muted mt-1">Control Deck</p>
          </div>

          <nav className="mt-2">
            {navItems.map(({ to, icon: Icon, label }) => (
              <NavLink
                key={to}
                to={to}
                end={to === "/"}
                className={({ isActive }) =>
                  clsx(
                    "mx-3 mb-2 flex items-center gap-3 px-4 py-3 rounded-xl text-slate-700 transition-all",
                    "hover:bg-white/90 hover:text-primary-700 elevated",
                    isActive && "bg-white text-primary-700 shadow",
                  )
                }
              >
                <Icon size={18} />
                <span className="font-medium">{label}</span>
              </NavLink>
            ))}
          </nav>
        </aside>

        {/* メインコンテンツ */}
        <main className="flex-1 overflow-auto p-4 md:p-8">
          <div className="md:hidden glass-panel mb-4 p-2">
            <nav className="flex flex-wrap gap-2">
              {navItems.map(({ to, icon: Icon, label }) => (
                <NavLink
                  key={`mobile-${to}`}
                  to={to}
                  end={to === "/"}
                  className={({ isActive }) =>
                    clsx(
                      "flex items-center gap-2 px-3 py-2 rounded-lg text-sm",
                      isActive
                        ? "bg-white text-primary-700 shadow"
                        : "text-slate-700",
                    )
                  }
                >
                  <Icon size={15} />
                  <span>{label}</span>
                </NavLink>
              ))}
            </nav>
          </div>
          <Outlet />
        </main>
      </div>
    </div>
  );
}

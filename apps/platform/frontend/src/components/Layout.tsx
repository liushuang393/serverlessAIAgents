/**
 * Layout - サイドバー + ヘッダー + メインコンテンツ.
 *
 * 全画面共通のレイアウトコンポーネント。
 */

import { NavLink, Outlet } from 'react-router-dom';

/** ナビゲーション項目 */
const NAV_ITEMS = [
  { to: '/', icon: '📊', label: 'Dashboard' },
  { to: '/apps', icon: '📦', label: 'Apps' },
  { to: '/agents', icon: '🤖', label: 'Agents' },
  { to: '/skills', icon: '🧩', label: 'Skills' },
  { to: '/rag', icon: '📚', label: 'RAG' },
  { to: '/mcp', icon: '🔌', label: 'MCP' },
  { to: '/cli', icon: '📖', label: 'CLI' },
  { to: '/settings', icon: '⚙️', label: 'Settings' },
] as const;

export function Layout() {
  return (
    <div className="flex min-h-screen">
      {/* サイドバー */}
      <aside className="w-56 bg-slate-900/60 border-r border-slate-800 flex flex-col shrink-0">
        {/* ロゴ */}
        <div className="p-4 border-b border-slate-800">
          <NavLink to="/" className="flex items-center gap-2">
            <span className="text-2xl">🏗️</span>
            <div>
              <h1 className="text-sm font-bold text-slate-100">AgentFlow</h1>
              <p className="text-[10px] text-slate-500">Platform v2.0</p>
            </div>
          </NavLink>
        </div>

        {/* ナビゲーション */}
        <nav className="flex-1 p-3 space-y-1">
          {NAV_ITEMS.map((item) => (
            <NavLink
              key={item.to}
              to={item.to}
              end={item.to === '/'}
              className={({ isActive }) =>
                `flex items-center gap-3 px-3 py-2.5 rounded-lg text-sm transition-colors ${
                  isActive
                    ? 'bg-indigo-600/20 text-indigo-400 font-medium'
                    : 'text-slate-400 hover:bg-slate-800/60 hover:text-slate-200'
                }`
              }
            >
              <span className="text-base">{item.icon}</span>
              {item.label}
            </NavLink>
          ))}
        </nav>

        {/* フッター */}
        <div className="p-4 border-t border-slate-800">
          <p className="text-[10px] text-slate-600 text-center">
            AgentFlow Platform © 2024
          </p>
        </div>
      </aside>

      {/* メインコンテンツ */}
      <main className="flex-1 overflow-auto">
        <Outlet />
      </main>
    </div>
  );
}

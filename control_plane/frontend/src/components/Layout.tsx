/**
 * Layout - サイドバー + ヘッダー + メインコンテンツ.
 *
 * 全画面共通のレイアウトコンポーネント。
 */

import { NavLink, Outlet } from 'react-router-dom';
import { LocaleSwitcher, useI18n } from '@/i18n';

export function Layout() {
  const { t } = useI18n();

  /** ロケール変化時に再計算されるナビゲーション項目 */
  const navItems = [
    { to: '/',                    icon: '📊', label: t('nav.dashboard') },
    { to: '/apps',                icon: '📦', label: t('nav.apps') },
    { to: '/agents',              icon: '🤖', label: t('nav.agents') },
    { to: '/agent-orchestration', icon: '🔀', label: t('nav.orchestration') },
    { to: '/agent-patterns',      icon: '🧬', label: t('nav.patterns') },
    { to: '/skills',              icon: '🧩', label: t('nav.skills') },
    { to: '/rag',                 icon: '📚', label: t('nav.rag') },
    { to: '/mcp',                 icon: '🔌', label: t('nav.mcp') },
    { to: '/cli',                 icon: '📖', label: t('nav.cli') },
    { to: '/llm-management',      icon: '🧠', label: t('nav.llm_management') },
    { to: '/settings',            icon: '⚙️', label: t('nav.settings') },
  ];

  return (
    <div className="flex min-h-screen">
      {/* サイドバー */}
      <aside className="w-56 bg-slate-900/60 border-r border-slate-800 flex flex-col shrink-0">
        {/* ロゴ */}
        <div className="p-4 border-b border-slate-800">
          <NavLink to="/" className="flex items-center gap-2">
            <span className="text-2xl">🏗️</span>
            <div>
              <h1 className="text-sm font-bold text-slate-100">BizCore</h1>
              <p className="text-[10px] text-slate-500">Platform v2.0</p>
            </div>
          </NavLink>
        </div>

        {/* ナビゲーション */}
        <nav className="flex-1 p-3 space-y-1">
          {navItems.map((item) => (
            <NavLink
              key={item.to}
              to={item.to}
              end={item.to === '/'}
              data-testid={item.to === '/rag' ? 'nav-rag' : undefined}
              className={({ isActive }) =>
                `flex items-center gap-3 px-3 py-2.5 rounded-lg text-sm transition-colors ${
                  isActive
                    ? 'bg-indigo-600/20 text-indigo-400 font-medium'
                    : 'text-slate-400 hover:bg-slate-800/60 hover:text-slate-200'
                }`
              }
            >
              <span aria-hidden="true" className="text-base">{item.icon}</span>
              {item.label}
            </NavLink>
          ))}
        </nav>

        {/* フッター */}
        <div className="p-4 border-t border-slate-800">
          {/* 言語切り替え */}
          <LocaleSwitcher className="w-full bg-transparent border border-slate-700 rounded px-2 py-1 text-[11px] text-slate-500 cursor-pointer mb-2" />
          <p className="text-[10px] text-slate-600 text-center">
            BizCore Control Plane © 2024
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

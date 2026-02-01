import { Outlet, NavLink } from 'react-router-dom';
import {
  LayoutDashboard,
  MessageSquare,
  Users,
  Settings,
  Radio,
} from 'lucide-react';
import clsx from 'clsx';

/**
 * ナビゲーションリンク
 */
const navItems = [
  { to: '/', icon: LayoutDashboard, label: 'ダッシュボード' },
  { to: '/platforms', icon: Radio, label: 'プラットフォーム' },
  { to: '/sessions', icon: Users, label: 'セッション' },
  { to: '/conversations', icon: MessageSquare, label: '会話履歴' },
  { to: '/settings', icon: Settings, label: '設定' },
];

/**
 * レイアウトコンポーネント
 *
 * サイドバーナビゲーションとメインコンテンツエリア
 */
export default function Layout() {
  return (
    <div className="flex h-screen bg-gray-100">
      {/* サイドバー */}
      <aside className="w-64 bg-white shadow-lg">
        <div className="p-6">
          <h1 className="text-xl font-bold text-primary-600">
            📱 Messaging Hub
          </h1>
          <p className="text-sm text-gray-500 mt-1">管理画面</p>
        </div>

        <nav className="mt-4">
          {navItems.map(({ to, icon: Icon, label }) => (
            <NavLink
              key={to}
              to={to}
              end={to === '/'}
              className={({ isActive }) =>
                clsx(
                  'flex items-center gap-3 px-6 py-3 text-gray-700 hover:bg-primary-50 hover:text-primary-600 transition-colors',
                  isActive && 'bg-primary-50 text-primary-600 border-r-4 border-primary-600'
                )
              }
            >
              <Icon size={20} />
              <span>{label}</span>
            </NavLink>
          ))}
        </nav>
      </aside>

      {/* メインコンテンツ */}
      <main className="flex-1 overflow-auto p-8">
        <Outlet />
      </main>
    </div>
  );
}


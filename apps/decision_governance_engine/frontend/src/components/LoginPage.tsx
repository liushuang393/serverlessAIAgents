/**
 * ログイン画面コンポーネント.
 *
 * 目的: ユーザー認証を行う美しいログイン画面
 * 設計: モダンなグラデーション背景、アニメーション、フォームバリデーション
 */

import React, { useState, useCallback } from 'react';
import { useAuthStore } from '../store/useAuthStore';

/** デモユーザー情報（ログインヒント用） */
const DEMO_USERS = [
  { username: 'admin', password: 'admin123', name: '管理者 太郎', dept: '経営企画部' },
  { username: 'tanaka', password: 'tanaka123', name: '田中 一郎', dept: '技術開発本部' },
  { username: 'suzuki', password: 'suzuki123', name: '鈴木 花子', dept: '事業戦略室' },
  { username: 'yamamoto', password: 'yamamoto123', name: '山本 健太', dept: 'DX推進部' },
];

export const LoginPage: React.FC = () => {
  const { login, isLoading, error } = useAuthStore();
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');
  const [showPassword, setShowPassword] = useState(false);
  const [showDemoHint, setShowDemoHint] = useState(false);

  const handleSubmit = useCallback(
    async (e: React.FormEvent) => {
      e.preventDefault();
      if (!username.trim() || !password.trim()) return;
      await login(username, password);
    },
    [username, password, login]
  );

  const handleDemoLogin = useCallback(
    async (user: typeof DEMO_USERS[number]) => {
      setUsername(user.username);
      setPassword(user.password);
      await login(user.username, user.password);
    },
    [login]
  );

  return (
    <div className="min-h-screen bg-[#0a0a0f] flex items-center justify-center p-4 relative overflow-hidden">
      {/* 背景装飾 */}
      <div className="absolute inset-0 overflow-hidden">
        {/* グラデーションオーブ */}
        <div className="absolute top-1/4 left-1/4 w-96 h-96 bg-indigo-500/20 rounded-full blur-[128px] animate-pulse" />
        <div className="absolute bottom-1/4 right-1/4 w-80 h-80 bg-violet-500/20 rounded-full blur-[128px] animate-pulse" style={{ animationDelay: '1s' }} />
        <div className="absolute top-1/2 right-1/3 w-64 h-64 bg-blue-500/10 rounded-full blur-[100px] animate-pulse" style={{ animationDelay: '2s' }} />
        
        {/* グリッドパターン */}
        <div 
          className="absolute inset-0 opacity-[0.02]"
          style={{
            backgroundImage: `linear-gradient(rgba(255,255,255,0.1) 1px, transparent 1px), linear-gradient(90deg, rgba(255,255,255,0.1) 1px, transparent 1px)`,
            backgroundSize: '50px 50px',
          }}
        />
      </div>

      {/* ログインカード */}
      <div className="relative w-full max-w-md">
        {/* ロゴ・タイトル */}
        <div className="text-center mb-8">
          <div className="inline-flex items-center justify-center w-20 h-20 rounded-2xl bg-gradient-to-br from-indigo-500 via-violet-500 to-purple-600 shadow-2xl shadow-indigo-500/30 mb-6 animate-float">
            <span className="text-4xl">⚡</span>
          </div>
          <h1 className="text-3xl font-bold text-white mb-2 tracking-tight">
            Decision Agent
          </h1>
          <p className="text-slate-400 text-sm">
            ⚖️意思決定を構造化するAIプラットフォーム
          </p>
        </div>

        {/* ログインフォーム */}
        <div className="bg-[#12121a]/80 backdrop-blur-xl rounded-2xl border border-white/5 p-8 shadow-2xl">
          <form onSubmit={handleSubmit} className="space-y-6">
            {/* エラー表示 */}
            {error && (
              <div className="bg-red-500/10 border border-red-500/20 rounded-lg px-4 py-3 text-red-400 text-sm flex items-center gap-2 animate-shake">
                <span>⚠️</span>
                {error}
              </div>
            )}

            {/* ユーザー名 */}
            <div>
              <label className="block text-sm text-slate-400 mb-2">
                ユーザー名
              </label>
              <div className="relative">
                <span className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-500">
                  👤
                </span>
                <input
                  type="text"
                  value={username}
                  onChange={(e) => setUsername(e.target.value)}
                  className="w-full bg-[#0a0a0f] border border-white/10 rounded-xl pl-12 pr-4 py-3 text-white placeholder-slate-600 focus:outline-none focus:border-indigo-500/50 focus:ring-2 focus:ring-indigo-500/20 transition-all"
                  placeholder="ユーザー名を入力"
                  disabled={isLoading}
                  autoComplete="username"
                />
              </div>
            </div>

            {/* パスワード */}
            <div>
              <label className="block text-sm text-slate-400 mb-2">
                パスワード
              </label>
              <div className="relative">
                <span className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-500">
                  🔐
                </span>
                <input
                  type={showPassword ? 'text' : 'password'}
                  value={password}
                  onChange={(e) => setPassword(e.target.value)}
                  className="w-full bg-[#0a0a0f] border border-white/10 rounded-xl pl-12 pr-12 py-3 text-white placeholder-slate-600 focus:outline-none focus:border-indigo-500/50 focus:ring-2 focus:ring-indigo-500/20 transition-all"
                  placeholder="パスワードを入力"
                  disabled={isLoading}
                  autoComplete="current-password"
                />
                <button
                  type="button"
                  onClick={() => setShowPassword(!showPassword)}
                  className="absolute right-4 top-1/2 -translate-y-1/2 text-slate-500 hover:text-slate-300 transition-colors"
                >
                  {showPassword ? '🙈' : '👁️'}
                </button>
              </div>
            </div>

            {/* ログインボタン */}
            <button
              type="submit"
              disabled={isLoading || !username.trim() || !password.trim()}
              className="w-full py-4 rounded-xl font-semibold text-white bg-gradient-to-r from-indigo-600 via-violet-600 to-purple-600 hover:from-indigo-500 hover:via-violet-500 hover:to-purple-500 shadow-lg shadow-indigo-500/25 disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-300 flex items-center justify-center gap-2"
            >
              {isLoading ? (
                <>
                  <div className="w-5 h-5 border-2 border-white/30 border-t-white rounded-full animate-spin" />
                  認証中...
                </>
              ) : (
                <>
                  <span>🚀</span>
                  ログイン
                </>
              )}
            </button>
          </form>

          {/* デモアカウントヒント */}
          <div className="mt-6 pt-6 border-t border-white/5">
            <button
              onClick={() => setShowDemoHint(!showDemoHint)}
              className="w-full text-sm text-slate-500 hover:text-slate-400 flex items-center justify-center gap-2 transition-colors"
            >
              <span>💡</span>
              デモアカウントを使用する
              <span className={`transition-transform ${showDemoHint ? 'rotate-180' : ''}`}>▼</span>
            </button>

            {showDemoHint && (
              <div className="mt-4 space-y-2 animate-slide-down">
                {DEMO_USERS.map((user) => (
                  <button
                    key={user.username}
                    onClick={() => handleDemoLogin(user)}
                    disabled={isLoading}
                    className="w-full flex items-center justify-between px-4 py-3 bg-[#0a0a0f] hover:bg-[#0a0a0f]/50 rounded-lg border border-white/5 hover:border-indigo-500/30 transition-all group"
                  >
                    <div className="flex items-center gap-3">
                      <div className="w-10 h-10 rounded-full bg-gradient-to-br from-indigo-500/20 to-violet-500/20 flex items-center justify-center text-lg">
                        {user.name.charAt(0)}
                      </div>
                      <div className="text-left">
                        <div className="text-sm text-white font-medium">{user.name}</div>
                        <div className="text-xs text-slate-500">{user.dept}</div>
                      </div>
                    </div>
                    <span className="text-indigo-400 opacity-0 group-hover:opacity-100 transition-opacity">
                      →
                    </span>
                  </button>
                ))}
              </div>
            )}
          </div>
        </div>

        {/* フッター */}
        <div className="text-center mt-6 text-slate-600 text-xs">
          © 2026 Decision Governance Engine
        </div>
      </div>

      {/* カスタムアニメーション用スタイル */}
      <style>{`
        @keyframes float {
          0%, 100% { transform: translateY(0); }
          50% { transform: translateY(-10px); }
        }
        @keyframes shake {
          0%, 100% { transform: translateX(0); }
          25% { transform: translateX(-5px); }
          75% { transform: translateX(5px); }
        }
        @keyframes slide-down {
          from { opacity: 0; transform: translateY(-10px); }
          to { opacity: 1; transform: translateY(0); }
        }
        .animate-float { animation: float 3s ease-in-out infinite; }
        .animate-shake { animation: shake 0.5s ease-in-out; }
        .animate-slide-down { animation: slide-down 0.3s ease-out; }
      `}</style>
    </div>
  );
};


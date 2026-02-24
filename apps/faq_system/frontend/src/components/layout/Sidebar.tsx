import { useEffect, useState } from 'react';
import { useChatStore } from '../../stores/chatStore';
import { Plus, Trash2, LogOut, Settings, Hash, MessageCircle, PanelLeftClose } from 'lucide-react';
import { useAuthStore } from '../../stores/authStore';
import { useNavigate } from 'react-router-dom';
import { useI18n } from '../../i18n';

/** サイドバーの props 型定義 */
interface SidebarProps {
    /** サイドバーが展開中か */
    isOpen: boolean;
    /** サイドバーの開閉をトグルするコールバック */
    onToggle: () => void;
}

export const Sidebar = ({ isOpen, onToggle }: SidebarProps) => {
    const { t } = useI18n();
    const { sessions, currentSessionId, selectSession, createSession, deleteSession, fetchSessions } = useChatStore();
    const { logout, user } = useAuthStore();
    const navigate = useNavigate();
    const [loggingOut, setLoggingOut] = useState(false);

    useEffect(() => {
        fetchSessions();
    }, [fetchSessions]);

    const handleLogout = async () => {
        setLoggingOut(true);
        await logout();
        navigate('/login', { replace: true });
    };

    return (
        <div
            className={`h-full glass border-r border-white/5 flex flex-col text-white z-20 overflow-hidden relative transition-all duration-300 ease-in-out ${
                isOpen ? 'w-[var(--sidebar-width)] min-w-[var(--sidebar-width)]' : 'w-0 min-w-0 border-r-0'
            }`}
        >
            {/* サイドバー内コンテンツ（折り畳み時は非表示にしつつ幅ゼロにする） */}
            <div className={`flex flex-col h-full w-[var(--sidebar-width)] transition-opacity duration-200 ${isOpen ? 'opacity-100' : 'opacity-0 pointer-events-none'}`}>
            {/* Background Blur Accent */}
            <div className="absolute -top-20 -left-20 w-40 h-40 bg-[var(--primary)]/10 blur-[80px] rounded-full pointer-events-none" />

            {/* Logo/Header */}
            <div className="p-6 pb-2 flex items-center justify-between">
                <div className="flex items-center gap-2.5">
                    <div className="w-7 h-7 rounded-lg bg-[var(--primary)] flex items-center justify-center shadow-[0_0_12px_var(--primary-glow)]">
                        <MessageCircle size={14} className="text-black" />
                    </div>
                    <span className="font-bold text-sm tracking-tight">FAQ Intelligence</span>
                </div>
                <div className="flex items-center gap-1">
                    <button
                        onClick={() => navigate('/settings')}
                        className="text-[var(--text-muted)] hover:text-white transition-colors p-1.5 rounded-lg hover:bg-white/5"
                        title={t('sidebar.settings')}
                    >
                        <Settings size={15} />
                    </button>
                    <button
                        onClick={onToggle}
                        className="text-[var(--text-muted)] hover:text-white transition-colors p-1.5 rounded-lg hover:bg-white/5"
                        title={t('sidebar.collapse') ?? 'サイドバーを閉じる'}
                    >
                        <PanelLeftClose size={15} />
                    </button>
                </div>
            </div>

            {/* Action Area */}
            <div className="p-4">
                <button
                    onClick={createSession}
                    className="w-full bg-[var(--primary)]/10 border border-[var(--primary)]/20 rounded-2xl p-4 flex items-center justify-center gap-3 hover:bg-[var(--primary)]/20 transition-all text-sm font-bold text-[var(--primary)] shadow-lg group"
                >
                    <Plus size={18} className="group-hover:rotate-90 transition-transform duration-300" />
                    {t('chat.new_chat')}
                </button>
            </div>

            {/* Navigation / History */}
            <div className="flex-1 overflow-y-auto px-4 py-2 flex flex-col gap-1 custom-scrollbar">
                <div className="flex items-center justify-between px-2 py-3">
                    <span className="text-[10px] font-bold text-[var(--text-muted)] uppercase tracking-[0.2em]">{t('sidebar.history')}</span>
                    <span className="text-[10px] bg-white/5 px-2 py-0.5 rounded text-[var(--text-muted)]">{sessions.length}</span>
                </div>

                {sessions.length === 0 && (
                    <div className="px-2 py-8 text-center">
                        <p className="text-xs text-[var(--text-muted)]">{t('sidebar.no_conversations')}</p>
                        <p className="text-xs text-[var(--text-muted)] mt-1">{t('sidebar.start_new')}</p>
                    </div>
                )}

                {sessions.map((session) => (
                    <div
                        key={session.session_id}
                        className={`group relative flex items-center gap-3 rounded-2xl text-sm transition-all border ${currentSessionId === session.session_id
                                ? 'bg-white/5 border-white/10 text-white shadow-lg shadow-black/20'
                                : 'hover:bg-white/[0.03] border-transparent text-[var(--text-dim)] hover:text-white'
                            }`}
                    >
                        <button
                            type="button"
                            className="flex-1 flex items-center gap-3 p-3.5 bg-transparent border-none text-left text-inherit cursor-pointer"
                            onClick={() => selectSession(session.session_id)}
                        >
                            <Hash size={16} className={`flex-shrink-0 ${currentSessionId === session.session_id ? 'text-[var(--primary)]' : 'text-[var(--text-muted)]'}`} />
                            <span className="flex-1 truncate pr-6 font-medium" title={session.title}>
                                {session.title || t('chat.new_chat')}
                            </span>
                        </button>

                        <button
                            type="button"
                            className={`opacity-0 group-hover:opacity-100 p-1.5 rounded-lg hover:bg-red-500/20 hover:text-red-400 absolute right-2 transition-all bg-transparent border-none cursor-pointer ${currentSessionId === session.session_id ? 'text-red-400/50' : 'text-[var(--text-muted)]'
                                }`}
                            onClick={() => {
                                if (confirm('Delete this session?')) deleteSession(session.session_id);
                            }}
                        >
                            <Trash2 size={14} />
                        </button>
                    </div>
                ))}
            </div>

            {/* User Profile Area */}
            <div className="p-4 mt-auto">
                <div className="glass rounded-2xl p-3.5 flex items-center gap-3 border border-white/5 shadow-2xl">
                    <div className="w-9 h-9 rounded-xl bg-gradient-to-br from-[var(--primary)]/40 to-[var(--primary)]/10 flex items-center justify-center text-xs font-bold border border-[var(--primary)]/20">
                        {user?.username?.substring(0, 2).toUpperCase() || '??'}
                    </div>
                    <div className="flex-1 overflow-hidden min-w-0">
                        <div className="text-sm font-semibold truncate text-white">
                            {user?.display_name || user?.username || 'User'}
                        </div>
                        <div className="text-[10px] text-[var(--text-muted)] truncate uppercase tracking-wider font-medium">
                            {user?.role || 'Member'}
                        </div>
                    </div>
                    <button
                        onClick={handleLogout}
                        disabled={loggingOut}
                        className="p-2 rounded-xl hover:bg-red-500/10 text-[var(--text-muted)] hover:text-red-400 transition-all border border-transparent hover:border-red-500/20 disabled:opacity-50"
                        title="Sign out"
                    >
                        <LogOut size={15} />
                    </button>
                </div>
            </div>
            </div>{/* 内部コンテンツラッパー閉じ */}
        </div>
    );
};

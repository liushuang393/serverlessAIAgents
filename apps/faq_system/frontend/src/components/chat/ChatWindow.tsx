import React, { useState, useRef, useEffect } from 'react';
import { Send, Loader2, Sparkles, FileText, Database, Wrench, TrendingUp, MessageCircle, Settings } from 'lucide-react';
import { useChatStore, useServiceStatus } from '../../stores/chatStore';
import { MessageBubble } from './MessageBubble';
import { useI18n } from '../../i18n';
import { SettingsModal } from '../settings/SettingsPage';
import { useOutletContext } from 'react-router-dom';

interface LayoutContext {
    sidebarOpen: boolean;
}

export const ChatWindow = () => {
    const { t } = useI18n();
    const { sidebarOpen } = useOutletContext<LayoutContext>();
    const { messages, sendMessage, isStreaming, lastQueryType, runtimeStatus } = useChatStore();
    const { ragEnabled, sqlEnabled, startPolling, stopPolling } = useServiceStatus();
    const [input, setInput] = useState('');
    const [isSettingsOpen, setIsSettingsOpen] = useState(false);
    const messagesEndRef = useRef<HTMLDivElement>(null);
    const textareaRef = useRef<HTMLTextAreaElement>(null);

    const scrollToBottom = () => {
        messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
    };

    useEffect(() => {
        scrollToBottom();
    }, [messages, isStreaming]);

    /** マウント時にバックエンド状態のポーリングを開始し、アンマウント時に停止する */
    useEffect(() => {
        startPolling();
        return () => { stopPolling(); };
    }, [startPolling, stopPolling]);

    const handleSubmit = async (e?: React.SyntheticEvent) => {
        e?.preventDefault();
        if (!input.trim() || isStreaming) return;

        const msg = input;
        setInput('');
        if (textareaRef.current) {
            textareaRef.current.style.height = 'auto';
        }

        await sendMessage(msg);
    };

    const handleKeyDown = (e: React.KeyboardEvent) => {
        if (e.key === 'Enter' && !e.shiftKey) {
            e.preventDefault();
            handleSubmit();
        }
    };

    const quickActions = [
        { label: t('chat.quick.analyze_reports'), sub: t('chat.quick.analyze_reports_sub'), icon: FileText },
        { label: t('chat.quick.sql_query'), sub: t('chat.quick.sql_query_sub'), icon: Database },
        { label: t('chat.quick.tech_support'), sub: t('chat.quick.tech_support_sub'), icon: Wrench },
        { label: t('chat.quick.sales_strategy'), sub: t('chat.quick.sales_strategy_sub'), icon: TrendingUp },
    ];
    const canSend = input.trim().length > 0 && !isStreaming;
    const chatLaneStyle: React.CSSProperties = {
        width: '100%',
        maxWidth: '960px',
        marginInline: 'auto',
    };
    const sendIconClass = canSend
        ? 'text-sky-50 drop-shadow-[0_1px_6px_rgba(186,230,253,0.4)]'
        : 'text-[var(--text-muted)]';
    const hasMessages = messages.length > 0;
    const routeLabels: Record<string, string> = {
        faq: 'FAQ / RAG',
        sql: 'SQL 分析',
        hybrid: 'RAG + SQL',
        chat: '一般チャット',
        weather: '天気 API',
        external: '外部調査',
        sales_material: '営業資料',
        blocked: '安全拒否',
    };
    const latestRouteLabel = routeLabels[lastQueryType ?? ''] ?? '未判定';
    const runtimeText = isStreaming
        ? `実行中: ${runtimeStatus?.agent || 'agent'} / ${runtimeStatus?.message || '処理中...'}`
        : hasMessages
            ? `直近ルート: ${latestRouteLabel}`
            : 'システム準備完了';
    const knowledgeState = !ragEnabled ? 'OFF' : (['faq', 'hybrid'].includes(lastQueryType ?? '') ? '実行' : '待機');
    const dataState = !sqlEnabled ? 'OFF' : (['sql', 'hybrid'].includes(lastQueryType ?? '') ? '実行' : '待機');
    const safetyState = (lastQueryType ?? '') === 'blocked' ? '検知' : '正常';
    const stateColorClass = (state: string): string => {
        if (state === 'OFF') return 'text-[var(--text-muted)]';
        if (state === '実行' || state === '検知') return 'text-amber-300';
        if (state === '正常') return 'text-emerald-300';
        return 'text-cyan-300';
    };

    return (
        <div className="flex-1 flex flex-col relative h-full overflow-hidden"
            style={{ background: 'radial-gradient(ellipse at 50% 0%, hsl(220, 20%, 11%), var(--bg-main))' }}>

            {/* Top Bar / Header Info */}
            <div className={`w-full h-14 shrink-0 glass flex items-center justify-between pr-8 z-10 border-b border-white/5 ${sidebarOpen ? 'pl-[62px]' : 'pl-10'}`}>
                <div className="flex items-center gap-3">
                    <div className="w-7 h-7 rounded-lg bg-[var(--primary)]/10 flex items-center justify-center border border-[var(--primary)]/20">
                        <Sparkles className="text-[var(--primary)]" size={14} />
                    </div>
                    <div>
                        <h2 className="text-sm font-semibold text-white leading-tight">FAQ Agent</h2>
                        <div className="flex items-center gap-1.5">
                            <span className="w-1.5 h-1.5 rounded-full bg-emerald-500 animate-pulse" />
                            <span className="text-[10px] text-[var(--text-muted)] uppercase tracking-wider font-medium">{t('chat.online')}</span>
                        </div>
                    </div>
                </div>
                {/* 設定ボタン（右上） */}
                <button
                    onClick={() => setIsSettingsOpen(true)}
                    className="p-2 rounded-xl hover:bg-white/5 text-[var(--text-muted)] hover:text-white transition-all border border-transparent hover:border-white/10"
                    title={t('sidebar.settings')}
                >
                    <Settings size={16} />
                </button>
            </div>

            {/* 設定モーダル */}
            <SettingsModal isOpen={isSettingsOpen} onClose={() => setIsSettingsOpen(false)} />

            {/* Messages Area - 3列レイアウト（15px / 中央 / 15px） */}
            <div className="flex-1 min-h-0 overflow-y-auto w-full custom-scrollbar">
                <div className="grid w-full grid-cols-[15px_minmax(0,1fr)_15px]">
                    <div className="col-start-2">
                        <div style={chatLaneStyle} className="flex flex-col gap-8 pb-8">
                            {messages.length === 0 ? (
                                <div className="mt-16 flex flex-col items-center justify-center animate-in fade-in slide-in-from-bottom-4 duration-700">
                                    <div className="w-16 h-16 rounded-2xl glass flex items-center justify-center border border-white/10 shadow-2xl mb-6 relative">
                                        <MessageCircle className="text-[var(--primary)]" size={32} />
                                        <div className="absolute -top-1 -right-1 w-3 h-3 bg-[var(--primary)] rounded-full blur-sm opacity-50" />
                                    </div>

                                    <h1 className="text-3xl font-bold text-white mb-3 tracking-tight">{t('chat.welcome_title')}</h1>
                                    <p className="text-[var(--text-dim)] text-center max-w-sm mb-10 text-sm">
                                        {t('chat.welcome_subtitle')}
                                    </p>

                                    <div className="grid grid-cols-1 md:grid-cols-2 gap-3 w-full max-w-2xl">
                                        {quickActions.map((item) => (
                                            <button
                                                key={item.label}
                                                onClick={() => setInput(item.label)}
                                                className="glass p-4 rounded-2xl flex items-start gap-3.5 hover:bg-white/5 text-left transition-all group border border-white/5 hover:border-white/10"
                                            >
                                                <div className="w-9 h-9 rounded-xl bg-[var(--primary)]/10 flex items-center justify-center border border-[var(--primary)]/15 flex-shrink-0 group-hover:bg-[var(--primary)]/20 transition-colors">
                                                    <item.icon size={16} className="text-[var(--primary)]" />
                                                </div>
                                                <div>
                                                    <div className="text-sm font-semibold text-white group-hover:text-[var(--primary)] transition-colors">{item.label}</div>
                                                    <div className="text-xs text-[var(--text-muted)] mt-0.5">{item.sub}</div>
                                                </div>
                                            </button>
                                        ))}
                                    </div>
                                </div>
                            ) : (
                                <div className="flex flex-col gap-8">
                                    {messages.map((msg, idx) => (
                                        <MessageBubble key={msg.id ?? String(idx)} message={msg} />
                                    ))}
                                    <div ref={messagesEndRef} />
                                </div>
                            )}
                        </div>
                    </div>
                </div>
            </div>

            {/* Input Area - 3列レイアウト（15px / 中央 / 15px） */}
            <div className="shrink-0 w-full pt-6 pb-6 border-t border-white/5"
                style={{ background: 'linear-gradient(to top, var(--bg-main) 85%, rgba(0,0,0,0.0))' }}>
                <div className="grid w-full grid-cols-[15px_minmax(0,1fr)_15px]">
                    <div className="col-start-2">
                        <div style={chatLaneStyle} className="flex flex-col gap-3">
                            <div className="flex items-end gap-2 sm:gap-3">
                                <div className="flex-1 glass rounded-[20px] border border-white/10 px-4 py-2 shadow-2xl focus-within:border-[var(--primary)]/45 focus-within:shadow-[0_0_0_1px_rgba(94,234,212,0.25),0_10px_34px_rgba(0,0,0,0.45)] transition-all">
                                    <textarea
                                        ref={textareaRef}
                                        rows={1}
                                        value={input}
                                        onChange={(e) => {
                                            setInput(e.target.value);
                                            e.target.style.height = 'auto';
                                            e.target.style.height = `${Math.min(e.target.scrollHeight, 240)}px`;
                                        }}
                                        onKeyDown={handleKeyDown}
                                        placeholder={t('chat.ask_placeholder')}
                                        className="w-full bg-transparent border-none py-3 pr-2 text-white placeholder:text-[var(--text-muted)] focus:ring-0 resize-none max-h-[240px] custom-scrollbar text-[15px] leading-relaxed"
                                        style={{ height: '48px' }}
                                    />
                                </div>

                                <button
                                    onClick={() => handleSubmit()}
                                    disabled={!canSend}
                                    className={`h-12 w-12 sm:h-[52px] sm:w-[52px] rounded-2xl border mb-0.5 shrink-0 transition-all duration-200 ease-out focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-sky-300/70 ${canSend
                                        ? 'bg-gradient-to-br from-[#6ee7dc] via-[#3fd9d0] to-[#2cbec0] text-sky-50 border-white/40 shadow-[0_10px_28px_rgba(56,189,248,0.35)] hover:brightness-110 hover:-translate-y-0.5 active:translate-y-0 active:scale-[0.97]'
                                        : 'bg-white/[0.04] text-[var(--text-muted)] border-white/10 opacity-60 cursor-not-allowed'
                                        }`}
                                >
                                    {isStreaming ? (
                                        <Loader2 size={20} className="animate-spin text-sky-50" />
                                    ) : (
                                        <Send size={20} className={sendIconClass} />
                                    )}
                                </button>
                            </div>

                            <div className="flex items-center justify-between px-2">
                                <div className="flex items-center gap-4">
                                    <span className="text-[11px] flex items-center gap-1.5 font-medium tracking-wide text-[var(--text-dim)]">
                                        知識
                                        <span className={stateColorClass(knowledgeState)}>{knowledgeState}</span>
                                    </span>
                                    <span className="text-[11px] flex items-center gap-1.5 font-medium tracking-wide text-[var(--text-dim)]">
                                        データ
                                        <span className={stateColorClass(dataState)}>{dataState}</span>
                                    </span>
                                    <span className="text-[11px] flex items-center gap-1.5 font-medium tracking-wide text-[var(--text-dim)]">
                                        安全
                                        <span className={stateColorClass(safetyState)}>{safetyState}</span>
                                    </span>
                                </div>
                                <p className="text-[10px] text-[var(--text-muted)] font-medium tracking-wide truncate max-w-[62%] text-right">
                                    {runtimeText}
                                </p>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
};

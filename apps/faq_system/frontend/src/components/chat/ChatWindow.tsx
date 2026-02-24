import React, { useState, useRef, useEffect } from 'react';
import { Send, Loader2, Sparkles, FileText, Database, Wrench, TrendingUp, MessageCircle } from 'lucide-react';
import { useChatStore } from '../../stores/chatStore';
import { MessageBubble } from './MessageBubble';
import { useI18n } from '../../i18n';

export const ChatWindow = () => {
    const { t } = useI18n();
    const { messages, sendMessage, isStreaming } = useChatStore();
    const [input, setInput] = useState('');
    const messagesEndRef = useRef<HTMLDivElement>(null);
    const textareaRef = useRef<HTMLTextAreaElement>(null);

    const scrollToBottom = () => {
        messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
    };

    useEffect(() => {
        scrollToBottom();
    }, [messages, isStreaming]);

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

    return (
        <div className="flex-1 flex flex-col relative h-full overflow-hidden"
            style={{ background: 'radial-gradient(ellipse at 50% 0%, hsl(220, 20%, 11%), var(--bg-main))' }}>

            {/* Top Bar / Header Info */}
            <div className="w-full h-14 shrink-0 glass flex items-center justify-between px-8 z-10 border-b border-white/5">
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
            </div>

            {/* Messages Area */}
            <div className="flex-1 overflow-y-auto w-full custom-scrollbar">
                <div className="max-w-4xl mx-auto px-6 w-full flex flex-col gap-8 pb-40">
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

            {/* Input Area */}
            <div className="absolute bottom-0 left-0 w-full pt-10 pb-6 px-6" style={{ background: 'linear-gradient(to top, var(--bg-main) 60%, transparent)' }}>
                <div className="max-w-3xl mx-auto flex flex-col gap-3">
                    <div className="glass rounded-[20px] border border-white/10 p-2 pl-4 flex items-end gap-2 shadow-2xl focus-within:border-[var(--primary)]/40 transition-colors">
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
                            className="flex-1 bg-transparent border-none py-3 pr-2 text-white placeholder:text-[var(--text-muted)] focus:ring-0 resize-none max-h-[240px] custom-scrollbar text-[15px] leading-relaxed"
                            style={{ height: '48px' }}
                        />
                        <button
                            onClick={() => handleSubmit()}
                            disabled={!input.trim() || isStreaming}
                            className={`p-3 rounded-2xl transition-all mb-1 ${input.trim() && !isStreaming
                                ? 'bg-[var(--primary)] text-black shadow-[0_0_20px_var(--primary-glow)] hover:scale-105'
                                : 'text-[var(--text-muted)] bg-white/5 opacity-50 cursor-not-allowed'
                                }`}
                        >
                            {isStreaming ? <Loader2 size={20} className="animate-spin" /> : <Send size={20} />}
                        </button>
                    </div>

                    <div className="flex items-center justify-between px-2">
                        <div className="flex items-center gap-4">
                            <span className="text-[11px] text-[var(--text-muted)] flex items-center gap-1.5 font-medium uppercase tracking-wider">
                                <span className="w-1.5 h-1.5 rounded-full bg-white/20" />{' '}
                                RAG
                            </span>
                            <span className="text-[11px] text-[var(--text-muted)] flex items-center gap-1.5 font-medium uppercase tracking-wider">
                                <span className="w-1.5 h-1.5 rounded-full bg-white/20" />{' '}
                                SQL
                            </span>
                        </div>
                        <p className="text-[10px] text-[var(--text-muted)] font-medium tracking-wide">
                            {t('chat.shift_enter_hint')}
                        </p>
                    </div>
                </div>
            </div>
        </div>
    );
};

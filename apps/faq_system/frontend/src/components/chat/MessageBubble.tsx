import React from 'react';
import { User, Sparkles } from 'lucide-react';
import { MarkdownRenderer } from '../rich/MarkdownRenderer';
import type { ChatMessage } from '../../api/types';

interface MessageBubbleProps {
    message: ChatMessage;
}

export const MessageBubble: React.FC<MessageBubbleProps> = ({ message }) => {
    const isUser = message.role === 'user';

    return (
        <div className={`w-full flex ${isUser ? 'justify-end' : 'justify-start'} animate-in fade-in duration-500`}>
            <div className={`flex gap-4 max-w-[90%] md:max-w-[80%] ${isUser ? 'flex-row-reverse' : 'flex-row'}`}>

                {/* Avatar */}
                <div className="flex-shrink-0 mt-1">
                    <div className={`w-8 h-8 flex items-center justify-center rounded-lg border ${isUser
                            ? 'bg-white/5 border-white/10 text-white'
                            : 'bg-[var(--primary)]/10 border-[var(--primary)]/20 text-[var(--primary)]'
                        }`}>
                        {isUser ? <User size={16} /> : <Sparkles size={16} />}
                    </div>
                </div>

                {/* Content Container */}
                <div className={`flex flex-col gap-1.5 ${isUser ? 'items-end' : 'items-start'}`}>
                    <span className="text-[10px] font-semibold text-[var(--text-muted)] uppercase tracking-[0.12em] px-1">
                        {isUser ? 'You' : 'AI Assistant'}
                    </span>

                    <div className={`p-4 rounded-2xl ${isUser
                            ? 'bg-[var(--bg-card)] text-white border border-white/5 rounded-tr-sm shadow-lg'
                            : 'bg-transparent text-[var(--text-main)] leading-relaxed rounded-tl-sm'
                        }`}>
                        <div className="markdown-content">
                            {message.role === 'assistant' || message.role === 'system' ? (
                                <MarkdownRenderer content={message.content || (message.role === 'assistant' ? 'Thinking...' : '')} />
                            ) : (
                                <div className="whitespace-pre-wrap text-[15px]">{message.content}</div>
                            )}
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
};

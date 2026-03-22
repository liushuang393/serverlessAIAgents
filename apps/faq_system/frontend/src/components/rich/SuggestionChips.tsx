import React from 'react';
import { MessageCircle } from 'lucide-react';
import type { Suggestion } from '../../api/types';
import { useChatStore } from '../../stores/chatStore';

interface SuggestionChipsProps {
    suggestions: Suggestion[];
}

/**
 * フォローアップ提案チップ
 *
 * バックエンドが返す suggestions をクリック可能なボタンとして表示する。
 */
export const SuggestionChips: React.FC<SuggestionChipsProps> = ({ suggestions }) => {
    const sendMessage = useChatStore((s) => s.sendMessage);

    if (!suggestions || suggestions.length === 0) return null;

    return (
        <div className="mt-3 flex flex-wrap gap-2">
            {suggestions.map((s, i) => (
                <button
                    key={i}
                    onClick={() => void sendMessage(s.text)}
                    className="inline-flex items-center gap-1.5 px-3 py-1.5 rounded-full text-xs
                               bg-white/5 border border-white/10 text-[var(--text-muted)]
                               hover:bg-[var(--primary)]/10 hover:border-[var(--primary)]/30 hover:text-[var(--primary)]
                               transition-all duration-200 cursor-pointer"
                >
                    <MessageCircle size={12} />
                    {s.text}
                </button>
            ))}
        </div>
    );
};


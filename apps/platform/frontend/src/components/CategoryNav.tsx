import React from 'react';
import { useI18n } from '../i18n';

export type CategoryId = 'all' | 'core' | 'studio' | 'governance' | 'ops' | 'daily';

interface CategoryNavProps {
    activeCategory: CategoryId;
    onSelect: (category: CategoryId) => void;
}

export const CATEGORIES: { id: CategoryId; labelKey: string; icon: string }[] = [
    { id: 'all', labelKey: 'app_list.cat_all', icon: '🚀' },
    { id: 'core', labelKey: 'app_list.cat_core', icon: '🏢' },
    { id: 'studio', labelKey: 'app_list.cat_studio', icon: '🛠️' },
    { id: 'governance', labelKey: 'app_list.cat_governance', icon: '⚖️' },
    { id: 'ops', labelKey: 'app_list.cat_ops', icon: '🛡️' },
    { id: 'daily', labelKey: 'app_list.cat_daily', icon: '📒' }
];

export function CategoryNav({ activeCategory, onSelect }: CategoryNavProps) {
    const { t } = useI18n();

    return (
        <div className="flex flex-wrap gap-2 pb-2">
            {CATEGORIES.map((cat) => (
                <button
                    key={cat.id}
                    onClick={() => onSelect(cat.id)}
                    className={`px-4 py-2 rounded-xl text-sm font-medium transition-all flex items-center gap-2 border ${activeCategory === cat.id
                        ? 'bg-indigo-600 border-indigo-500 text-white shadow-lg shadow-indigo-500/20'
                        : 'bg-slate-900/50 border-slate-800 text-slate-400 hover:border-slate-700 hover:text-slate-200'
                        }`}
                >
                    <span className="text-base">{cat.icon}</span>
                    <span>{t(cat.labelKey)}</span>
                </button>
            ))}
        </div>
    );
}

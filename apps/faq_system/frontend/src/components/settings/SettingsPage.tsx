/**
 * 設定ページコンポーネント.
 *
 * 目的: 言語切替、ユーザープロフィール表示、システム情報表示
 * Sidebar の歯車アイコンから遷移する。
 */

import { ArrowLeft, Globe, User, Server, Shield } from 'lucide-react';
import { useNavigate } from 'react-router-dom';
import { LocaleSwitcher, useI18n } from '../../i18n';
import { useAuthStore } from '../../stores/authStore';

/** 設定ページ */
export const SettingsPage = () => {
    const { t } = useI18n();
    const navigate = useNavigate();
    const { user } = useAuthStore();

    return (
        <div className="flex-1 h-full overflow-y-auto custom-scrollbar">
            <div className="max-w-2xl mx-auto p-8 space-y-8">
                {/* ヘッダー */}
                <div className="flex items-center gap-4">
                    <button
                        onClick={() => navigate('/')}
                        className="p-2 rounded-xl hover:bg-white/5 text-[var(--text-muted)] hover:text-white transition-all"
                        aria-label={t('common.close')}
                    >
                        <ArrowLeft size={20} />
                    </button>
                    <h1 className="text-2xl font-bold text-white">{t('settings.title')}</h1>
                </div>

                {/* 言語設定セクション */}
                <section className="glass rounded-2xl p-6 border border-white/5 space-y-4">
                    <div className="flex items-center gap-3 mb-2">
                        <Globe size={18} className="text-[var(--primary)]" />
                        <h2 className="text-lg font-semibold text-white">{t('settings.language')}</h2>
                    </div>
                    <p className="text-sm text-[var(--text-muted)]">{t('settings.language_desc')}</p>
                    <LocaleSwitcher className="w-full bg-[var(--bg-main)] border border-white/10 rounded-xl px-4 py-3 text-sm text-white cursor-pointer focus:outline-none focus:border-[var(--primary)]/50 transition-colors" />
                </section>

                {/* ユーザー情報セクション */}
                <section className="glass rounded-2xl p-6 border border-white/5 space-y-4">
                    <div className="flex items-center gap-3 mb-2">
                        <User size={18} className="text-[var(--primary)]" />
                        <h2 className="text-lg font-semibold text-white">{t('settings.profile')}</h2>
                    </div>
                    <div className="grid grid-cols-2 gap-4">
                        <InfoRow label={t('settings.username')} value={user?.username ?? '-'} />
                        <InfoRow label={t('settings.display_name')} value={user?.display_name ?? '-'} />
                        <InfoRow label={t('settings.department')} value={user?.department ?? '-'} />
                        <InfoRow label={t('settings.role')} value={user?.role ?? '-'} />
                    </div>
                </section>

                {/* システム情報セクション */}
                <section className="glass rounded-2xl p-6 border border-white/5 space-y-4">
                    <div className="flex items-center gap-3 mb-2">
                        <Server size={18} className="text-[var(--primary)]" />
                        <h2 className="text-lg font-semibold text-white">{t('settings.system')}</h2>
                    </div>
                    <div className="grid grid-cols-2 gap-4">
                        <InfoRow label={t('settings.version')} value="2.0.0" />
                        <InfoRow label={t('settings.app_name')} value="FAQ Intelligence" />
                    </div>
                </section>

                {/* セキュリティセクション */}
                <section className="glass rounded-2xl p-6 border border-white/5 space-y-4">
                    <div className="flex items-center gap-3 mb-2">
                        <Shield size={18} className="text-[var(--primary)]" />
                        <h2 className="text-lg font-semibold text-white">{t('settings.security')}</h2>
                    </div>
                    <p className="text-sm text-[var(--text-muted)]">{t('settings.security_desc')}</p>
                </section>
            </div>
        </div>
    );
};

/** 情報表示行コンポーネント */
function InfoRow({ label, value }: Readonly<{ label: string; value: string }>) {
    return (
        <div className="p-3 bg-white/[0.02] rounded-xl border border-white/5">
            <div className="text-[10px] font-semibold text-[var(--text-muted)] uppercase tracking-wider mb-1">
                {label}
            </div>
            <div className="text-sm text-white font-medium truncate">{value}</div>
        </div>
    );
}


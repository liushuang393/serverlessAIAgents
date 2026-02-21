import React, { useState, useEffect } from 'react';
import { useAuthStore } from '../../stores/authStore';
import { authApi } from '../../api/auth';
import { useNavigate, useSearchParams } from 'react-router-dom';
import { LocaleSwitcher, useI18n } from '../../i18n';
import {
    ArrowRight,
    Eye,
    EyeOff,
    Loader2,
    LockKeyhole,
    MessageSquareText,
    ShieldCheck,
    Sparkles,
} from 'lucide-react';
import './LoginForm.css';

const getErrorMessage = (error: unknown, fallback: string): string => {
    if (error instanceof Error && error.message) {
        return error.message;
    }
    return fallback;
};

export const LoginForm = () => {
    const { t } = useI18n();
    const [username, setUsername] = useState('');
    const [password, setPassword] = useState('');
    const [showPassword, setShowPassword] = useState(false);
    const [error, setError] = useState('');
    const [isLoading, setIsLoading] = useState(false);
    const [mfaRequired, setMfaRequired] = useState(false);
    const [totpCode, setTotpCode] = useState('');
    const login = useAuthStore((state) => state.login);
    const isAuthenticated = useAuthStore((state) => state.isAuthenticated);
    const navigate = useNavigate();
    const [searchParams] = useSearchParams();

    /** ロケール変化時に再計算される信頼シグナルリスト */
    const trustSignals = [
        { key: 'knowledge', title: t('auth.trust.knowledge'), description: t('auth.trust.knowledge_desc') },
        { key: 'security',  title: t('auth.trust.security'),  description: t('auth.trust.security_desc') },
        { key: 'efficiency',title: t('auth.trust.efficiency'),description: t('auth.trust.efficiency_desc') },
    ];

    // Redirect if already authenticated
    useEffect(() => {
        if (isAuthenticated) {
            navigate('/', { replace: true });
        }
    }, [isAuthenticated, navigate]);

    // Show OAuth error from callback
    useEffect(() => {
        const oauthError = searchParams.get('error');
        if (oauthError) {
            setError(oauthError);
        }
    }, [searchParams]);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setIsLoading(true);
        setError('');

        try {
            const res = await authApi.login(username, password, mfaRequired ? totpCode : undefined);
            if (res.success && res.access_token && res.user) {
                login(res.access_token, res.user);
                navigate('/');
            } else if (res.message === "MFA_REQUIRED") {
                setMfaRequired(true);
            } else {
                setError(res.message || 'Authentication failed');
            }
        } catch (err: unknown) {
            setError(getErrorMessage(err, 'An unexpected error occurred'));
        } finally {
            setIsLoading(false);
        }
    };

    return (
        <div className="login-shell">
            <div className="login-shell__blob login-shell__blob--one" aria-hidden="true" />
            <div className="login-shell__blob login-shell__blob--two" aria-hidden="true" />
            <div className="login-shell__grid" aria-hidden="true" />

            <div className="login-layout">
                <aside className="login-brand-panel">
                    <div className="login-brand-badge">
                        <Sparkles size={14} />
                        {t('auth.brand_badge')}
                    </div>

                    <h2 className="login-brand-title">{t('auth.brand_title')}</h2>
                    <p className="login-brand-subtitle">{t('auth.brand_subtitle')}</p>

                    <div className="login-brand-signals">
                        {trustSignals.map((signal) => (
                            <article key={signal.key} className="login-signal-card">
                                <h3>{signal.title}</h3>
                                <p>{signal.description}</p>
                            </article>
                        ))}
                    </div>

                    <div className="login-brand-metrics">
                        <div>
                            <strong>99.9%</strong>
                            <span>{t('auth.metrics.uptime_label')}</span>
                        </div>
                        <div>
                            <strong>24/7</strong>
                            <span>{t('auth.metrics.availability_label')}</span>
                        </div>
                    </div>
                </aside>

                <section className="login-card card-shadow animate-in fade-in slide-in-from-bottom-4 duration-500">
                    {/* 言語切り替え */}
                    <div className="flex justify-end mb-3">
                        <LocaleSwitcher className="bg-transparent border border-white/20 rounded-lg px-2 py-1 text-xs text-slate-400 hover:border-white/40 focus:outline-none cursor-pointer" />
                    </div>
                    <div className="login-card-header">
                        <div className="login-icon-wrap">
                            {mfaRequired ? <ShieldCheck size={30} /> : <MessageSquareText size={30} />}
                            <div className="login-icon-status-dot" />
                        </div>
                        <div>
                            <h1 className="login-title">
                                {mfaRequired ? t('auth.mfa_title') : t('auth.welcome')}
                            </h1>
                            <p className="login-subtitle">
                                {mfaRequired ? t('auth.mfa_subtitle') : t('auth.subtitle')}
                            </p>
                        </div>
                    </div>

                    <form onSubmit={handleSubmit} className="login-form">
                        {error && (
                            <div className="login-error-banner">
                                <span className="login-error-dot" />
                                {error}
                            </div>
                        )}

                        {mfaRequired ? (
                            <div className="login-mfa-block">
                                <div className="login-mfa-badge">
                                    <LockKeyhole size={16} />
                                    {t('auth.mfa_shield')}
                                </div>
                                <input
                                    type="text"
                                    inputMode="numeric"
                                    value={totpCode}
                                    onChange={(e) => setTotpCode(e.target.value.replaceAll(/\D/g, ''))}
                                    className="login-input login-mfa-input w-full"
                                    placeholder="000000"
                                    maxLength={6}
                                    required
                                    autoFocus
                                    autoComplete="one-time-code"
                                />
                            </div>
                        ) : (
                            <>
                                <div className="login-field">
                                    <label htmlFor="login-username">{t('auth.username')}</label>
                                    <input
                                        id="login-username"
                                        type="text"
                                        placeholder={t('auth.username')}
                                        value={username}
                                        onChange={(e) => setUsername(e.target.value)}
                                        className="login-input w-full"
                                        required
                                        autoComplete="username"
                                        autoFocus
                                    />
                                </div>

                                <div className="login-field">
                                    <label htmlFor="login-password">{t('auth.password')}</label>
                                    <div className="login-input-wrap">
                                        <input
                                            id="login-password"
                                            type={showPassword ? 'text' : 'password'}
                                            placeholder={t('auth.password_placeholder')}
                                            value={password}
                                            onChange={(e) => setPassword(e.target.value)}
                                            className="login-input w-full pr-12"
                                            required
                                            autoComplete="current-password"
                                        />
                                        <button
                                            type="button"
                                            onClick={() => setShowPassword(!showPassword)}
                                            className="login-password-toggle"
                                            aria-label={showPassword ? t('auth.hide_password') : t('auth.show_password')}
                                        >
                                            {showPassword ? <EyeOff size={16} /> : <Eye size={16} />}
                                        </button>
                                    </div>
                                </div>
                            </>
                        )}

                        <button
                            type="submit"
                            disabled={isLoading}
                            className="btn-primary w-full py-4 text-lg mt-2 font-semibold login-submit-btn"
                        >
                            {isLoading ? (
                                <Loader2 className="animate-spin" size={24} />
                            ) : (
                                <>
                                    {mfaRequired ? t('auth.mfa_verify') : t('auth.sign_in')}
                                    <ArrowRight size={20} />
                                </>
                            )}
                        </button>

                        {!mfaRequired && (
                            <p className="login-register-hint">
                                {t('auth.register_hint')}{' '}
                                <button
                                    type="button"
                                    className="login-text-link"
                                    onClick={() => navigate('/register')}
                                >
                                    {t('auth.register')}
                                </button>
                            </p>
                        )}

                        {mfaRequired && (
                            <button
                                type="button"
                                onClick={() => { setMfaRequired(false); setTotpCode(''); setError(''); }}
                                className="login-ghost-link"
                            >
                                {t('auth.back_to_login')}
                            </button>
                        )}
                    </form>

                    {!mfaRequired && (
                        <div className="login-sso-section">
                            <div className="login-divider">
                                <span>{t('auth.sso')}</span>
                            </div>

                            <div className="login-sso-grid">
                                <a
                                    href="/api/auth/oauth2/google/authorize"
                                    className="login-sso-card"
                                    title={t('auth.sso_google')}
                                >
                                    <svg className="w-5 h-5" viewBox="0 0 24 24" aria-hidden="true">
                                        <path fill="#EA4335" d="M12 5.38c1.62 0 3.06.56 4.21 1.64l3.15-3.15C17.45 2.09 14.97 1 12 1 7.7 1 3.99 3.47 2.18 7.07l3.66 2.84c.87-2.6 3.3-4.53 6.16-4.53z" />
                                        <path fill="#4285F4" d="M22.56 12.25c0-.78-.07-1.53-.2-2.25H12v4.26h5.92c-.26 1.37-1.04 2.53-2.21 3.31v2.77h3.57c2.08-1.92 3.28-4.74 3.28-8.09z" />
                                        <path fill="#FBBC05" d="M5.84 14.09c-.22-.66-.35-1.36-.35-2.09s.13-1.43.35-2.09V7.07H2.18C1.43 8.55 1 10.22 1 12s.43 3.45 1.18 4.93l2.85-2.22.81-.62z" />
                                        <path fill="#34A853" d="M12 23c2.97 0 5.46-.98 7.28-2.66l-3.57-2.77c-.98.66-2.23 1.06-3.71 1.06-2.86 0-5.29-1.93-6.16-4.53H2.18v2.84C3.99 20.53 7.7 23 12 23z" />
                                    </svg>
                                    <span>{t('auth.sso_google')}</span>
                                </a>

                                <a
                                    href="/api/auth/oauth2/azure_ad/authorize"
                                    className="login-sso-card"
                                    title={t('auth.sso_microsoft')}
                                >
                                    <svg className="w-5 h-5" viewBox="0 0 21 21" aria-hidden="true">
                                        <rect x="1" y="1" width="9" height="9" fill="#F25022" />
                                        <rect x="11" y="1" width="9" height="9" fill="#7FBA00" />
                                        <rect x="1" y="11" width="9" height="9" fill="#00A4EF" />
                                        <rect x="11" y="11" width="9" height="9" fill="#FFB900" />
                                    </svg>
                                    <span>{t('auth.sso_microsoft')}</span>
                                </a>
                            </div>
                        </div>
                    )}
                </section>
            </div>
        </div>
    );
};

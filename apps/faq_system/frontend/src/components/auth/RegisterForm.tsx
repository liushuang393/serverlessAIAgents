import React, { useState } from 'react';
import { useAuthStore } from '../../stores/authStore';
import { useNavigate } from 'react-router-dom';
import { UserPlus, Loader2, AlertCircle, ArrowLeft } from 'lucide-react';
import { authApi } from '../../api/auth';
import type { RegisterRequest } from '../../api/types';
import { useI18n } from '../../i18n';

const getErrorMessage = (error: unknown, fallback: string): string => {
    if (error instanceof Error && error.message) {
        return error.message;
    }
    return fallback;
};

export const RegisterForm = () => {
    const { t } = useI18n();
    const [formData, setFormData] = useState<RegisterRequest>({
        username: '',
        password: '',
        display_name: '',
        department: '',
        position: '',
        email: '',
    });
    const [error, setError] = useState('');
    const [isLoading, setIsLoading] = useState(false);
    const setAuth = useAuthStore((state) => state.login);
    const navigate = useNavigate();

    const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        setFormData({
            ...formData,
            [e.target.name]: e.target.value,
        });
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setError('');
        setIsLoading(true);

        try {
            const response = await authApi.register(formData);
            if (response.success && response.user && response.access_token) {
                setAuth(response.access_token, response.user);
                navigate('/');
            } else {
                setError(response.message || t('register.error_fallback'));
            }
        } catch (err: unknown) {
            setError(getErrorMessage(err, t('register.error_occurred')));
        } finally {
            setIsLoading(false);
        }
    };

    return (
        <div className="min-h-screen w-full flex items-center justify-center p-6"
            style={{ background: 'radial-gradient(ellipse at 80% 100%, hsl(170, 30%, 12%) 0%, var(--bg-main) 60%)' }}>

            <div className="w-full max-w-[500px] glass card-shadow rounded-3xl p-10 flex flex-col gap-8 animate-in fade-in slide-in-from-bottom-4 duration-500">
                {/* Header */}
                <div className="flex flex-col items-center text-center gap-4">
                    <div className="w-16 h-16 rounded-2xl glass flex items-center justify-center border border-white/10 shadow-lg">
                        <UserPlus className="text-[var(--primary)]" size={32} />
                    </div>
                    <div>
                        <h1 className="text-3xl font-bold text-white mb-2 tracking-tight">{t('register.title')}</h1>
                        <p className="text-sm text-[var(--text-dim)]">{t('register.subtitle')}</p>
                    </div>
                </div>

                {/* Form */}
                <form className="flex flex-col gap-5" onSubmit={handleSubmit}>
                    {error && (
                        <div className="p-4 bg-red-500/10 border border-red-500/20 rounded-xl text-sm text-red-400 flex items-center gap-3">
                            <AlertCircle size={18} className="flex-shrink-0" />
                            {error}
                        </div>
                    )}

                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        <div className="flex flex-col gap-2">
                            <label className="text-xs font-semibold text-[var(--text-muted)] uppercase tracking-wider ml-1">{t('auth.username')}</label>
                            <input
                                name="username"
                                type="text"
                                placeholder="e.g. j.doe"
                                required
                                value={formData.username}
                                onChange={handleChange}
                                pattern="^[a-zA-Z0-9_.-]+$"
                                autoComplete="username"
                                autoFocus
                            />
                        </div>
                        <div className="flex flex-col gap-2">
                            <label className="text-xs font-semibold text-[var(--text-muted)] uppercase tracking-wider ml-1">{t('register.full_name')}</label>
                            <input
                                name="display_name"
                                type="text"
                                placeholder="John Doe"
                                required
                                value={formData.display_name}
                                onChange={handleChange}
                                autoComplete="name"
                            />
                        </div>
                    </div>

                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        <div className="flex flex-col gap-2">
                            <label className="text-xs font-semibold text-[var(--text-muted)] uppercase tracking-wider ml-1">{t('auth.email')}</label>
                            <input
                                name="email"
                                type="email"
                                placeholder="john@company.com"
                                value={formData.email || ''}
                                onChange={handleChange}
                                autoComplete="email"
                            />
                        </div>
                        <div className="flex flex-col gap-2">
                            <label className="text-xs font-semibold text-[var(--text-muted)] uppercase tracking-wider ml-1">{t('auth.password')}</label>
                            <input
                                name="password"
                                type="password"
                                placeholder={t('register.password_placeholder')}
                                required
                                minLength={8}
                                value={formData.password}
                                onChange={handleChange}
                                autoComplete="new-password"
                            />
                        </div>
                    </div>

                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        <div className="flex flex-col gap-2">
                            <label className="text-xs font-semibold text-[var(--text-muted)] uppercase tracking-wider ml-1">{t('register.department')}</label>
                            <input
                                name="department"
                                type="text"
                                placeholder="Engineering"
                                value={formData.department || ''}
                                onChange={handleChange}
                            />
                        </div>
                        <div className="flex flex-col gap-2">
                            <label className="text-xs font-semibold text-[var(--text-muted)] uppercase tracking-wider ml-1">{t('register.position')}</label>
                            <input
                                name="position"
                                type="text"
                                placeholder="Lead Developer"
                                value={formData.position || ''}
                                onChange={handleChange}
                            />
                        </div>
                    </div>

                    <button
                        type="submit"
                        disabled={isLoading}
                        className="btn-primary w-full py-4 text-lg mt-2 font-semibold"
                    >
                        {isLoading ? (
                            <Loader2 className="animate-spin" size={24} />
                        ) : (
                            <>
                                <UserPlus size={20} />
                                {t('register.submit')}
                            </>
                        )}
                    </button>

                    <div className="flex items-center justify-center mt-1">
                        <button
                            type="button"
                            onClick={() => navigate('/login')}
                            className="text-[var(--text-dim)] hover:text-white transition-colors flex items-center gap-2 text-sm bg-transparent border-none"
                        >
                            <ArrowLeft size={16} />
                            {t('register.already_have_account')} <span className="text-[var(--primary)] hover:underline font-medium">{t('register.sign_in')}</span>
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
};

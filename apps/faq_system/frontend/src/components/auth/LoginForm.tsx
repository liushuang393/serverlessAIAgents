import React, { useState, useEffect } from 'react';
import { useAuthStore } from '../../stores/authStore';
import { authApi } from '../../api/auth';
import { useNavigate, useSearchParams } from 'react-router-dom';
import { MessageSquare, Loader2, ArrowRight, ShieldCheck, Eye, EyeOff } from 'lucide-react';

export const LoginForm = () => {
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
        } catch (err: any) {
            setError(err.message || 'An unexpected error occurred');
        } finally {
            setIsLoading(false);
        }
    };

    return (
        <div className="min-h-screen w-full flex items-center justify-center p-6"
            style={{ background: 'radial-gradient(ellipse at 20% 0%, hsl(170, 30%, 12%) 0%, var(--bg-main) 60%)' }}>

            <div className="w-full max-w-[440px] glass card-shadow rounded-3xl p-10 flex flex-col gap-8 animate-in fade-in slide-in-from-bottom-4 duration-500">
                {/* Header */}
                <div className="flex flex-col items-center text-center gap-4">
                    <div className="w-16 h-16 rounded-2xl glass flex items-center justify-center border border-white/10 shadow-lg relative">
                        <MessageSquare className="text-[var(--primary)]" size={32} />
                        <div className="absolute -top-1 -right-1 w-3 h-3 bg-emerald-500 rounded-full border-2 border-[var(--bg-main)]" />
                    </div>
                    <div>
                        <h1 className="text-3xl font-bold text-white mb-2">
                            {mfaRequired ? "Security Verification" : "Welcome Back"}
                        </h1>
                        <p className="text-sm text-[var(--text-dim)]">
                            {mfaRequired
                                ? "Enter the 6-digit code from your authenticator app."
                                : "Access your enterprise FAQ & Intelligence platform."}
                        </p>
                    </div>
                </div>

                {/* Form */}
                <form onSubmit={handleSubmit} className="flex flex-col gap-6">
                    {error && (
                        <div className="p-4 bg-red-500/10 border border-red-500/20 rounded-xl text-sm text-red-400 flex items-center gap-3">
                            <span className="w-1.5 h-1.5 rounded-full bg-red-500 animate-pulse flex-shrink-0" />
                            {error}
                        </div>
                    )}

                    {!mfaRequired ? (
                        <>
                            <div className="flex flex-col gap-2">
                                <label className="text-xs font-semibold text-[var(--text-muted)] uppercase tracking-wider ml-1">Account</label>
                                <input
                                    type="text"
                                    placeholder="Username"
                                    value={username}
                                    onChange={(e) => setUsername(e.target.value)}
                                    className="w-full"
                                    required
                                    autoComplete="username"
                                    autoFocus
                                />
                            </div>

                            <div className="flex flex-col gap-2">
                                <label className="text-xs font-semibold text-[var(--text-muted)] uppercase tracking-wider ml-1">Password</label>
                                <div className="relative">
                                    <input
                                        type={showPassword ? 'text' : 'password'}
                                        placeholder="Enter password"
                                        value={password}
                                        onChange={(e) => setPassword(e.target.value)}
                                        className="w-full pr-12"
                                        required
                                        autoComplete="current-password"
                                    />
                                    <button
                                        type="button"
                                        onClick={() => setShowPassword(!showPassword)}
                                        className="absolute right-3 top-1/2 -translate-y-1/2 text-[var(--text-muted)] hover:text-white p-1"
                                        tabIndex={-1}
                                    >
                                        {showPassword ? <EyeOff size={16} /> : <Eye size={16} />}
                                    </button>
                                </div>
                            </div>
                        </>
                    ) : (
                        <div className="flex flex-col gap-4 items-center">
                            <div className="flex items-center gap-2 text-[var(--primary)] mb-2">
                                <ShieldCheck size={20} />
                                <span className="text-sm font-medium">MFA Shield Active</span>
                            </div>
                            <input
                                type="text"
                                inputMode="numeric"
                                value={totpCode}
                                onChange={(e) => setTotpCode(e.target.value.replace(/\D/g, ''))}
                                className="w-full text-center text-2xl tracking-[0.5em] font-mono py-4"
                                placeholder="000000"
                                maxLength={6}
                                required
                                autoFocus
                                autoComplete="one-time-code"
                            />
                        </div>
                    )}

                    <button
                        type="submit"
                        disabled={isLoading}
                        className="btn-primary w-full py-4 text-lg mt-2 font-semibold"
                    >
                        {isLoading ? (
                            <Loader2 className="animate-spin" size={24} />
                        ) : (
                            <>
                                {mfaRequired ? 'Verify & Enter' : 'Sign In'}
                                <ArrowRight size={20} />
                            </>
                        )}
                    </button>

                    {!mfaRequired && (
                        <p className="text-sm text-center text-[var(--text-dim)]">
                            Don't have an account?{' '}
                            <button
                                type="button"
                                className="text-[var(--primary)] hover:underline font-medium p-0 h-auto bg-transparent border-none"
                                onClick={() => navigate('/register')}
                            >
                                Register Now
                            </button>
                        </p>
                    )}

                    {mfaRequired && (
                        <button
                            type="button"
                            onClick={() => { setMfaRequired(false); setTotpCode(''); setError(''); }}
                            className="text-xs text-[var(--text-muted)] hover:text-white transition-colors bg-transparent border-none"
                        >
                            Return to sign in
                        </button>
                    )}
                </form>

                {/* SSO Section */}
                {!mfaRequired && (
                    <div className="flex flex-col gap-6">
                        <div className="relative flex items-center">
                            <div className="flex-grow border-t border-white/5"></div>
                            <span className="mx-4 text-[10px] font-bold text-[var(--text-muted)] uppercase tracking-[0.2em]">Enterprise SSO</span>
                            <div className="flex-grow border-t border-white/5"></div>
                        </div>

                        <div className="grid grid-cols-2 gap-4">
                            <a
                                href="/api/auth/oauth2/google/authorize"
                                className="glass hover:bg-white/5 p-4 rounded-2xl flex items-center justify-center gap-3 transition-all group"
                                title="Continue with Google"
                            >
                                <svg className="w-5 h-5 group-hover:scale-110 transition-transform" viewBox="0 0 24 24">
                                    <path fill="#EA4335" d="M12 5.38c1.62 0 3.06.56 4.21 1.64l3.15-3.15C17.45 2.09 14.97 1 12 1 7.7 1 3.99 3.47 2.18 7.07l3.66 2.84c.87-2.6 3.3-4.53 6.16-4.53z" />
                                    <path fill="#4285F4" d="M22.56 12.25c0-.78-.07-1.53-.2-2.25H12v4.26h5.92c-.26 1.37-1.04 2.53-2.21 3.31v2.77h3.57c2.08-1.92 3.28-4.74 3.28-8.09z" />
                                    <path fill="#FBBC05" d="M5.84 14.09c-.22-.66-.35-1.36-.35-2.09s.13-1.43.35-2.09V7.07H2.18C1.43 8.55 1 10.22 1 12s.43 3.45 1.18 4.93l2.85-2.22.81-.62z" />
                                    <path fill="#34A853" d="M12 23c2.97 0 5.46-.98 7.28-2.66l-3.57-2.77c-.98.66-2.23 1.06-3.71 1.06-2.86 0-5.29-1.93-6.16-4.53H2.18v2.84C3.99 20.53 7.7 23 12 23z" />
                                </svg>
                                <span className="text-xs text-[var(--text-dim)] group-hover:text-white transition-colors font-medium">Google</span>
                            </a>

                            <a
                                href="/api/auth/oauth2/azure_ad/authorize"
                                className="glass hover:bg-white/5 p-4 rounded-2xl flex items-center justify-center gap-3 transition-all group"
                                title="Continue with Microsoft"
                            >
                                <svg className="w-5 h-5 group-hover:scale-110 transition-transform" viewBox="0 0 21 21">
                                    <rect x="1" y="1" width="9" height="9" fill="#F25022" />
                                    <rect x="11" y="1" width="9" height="9" fill="#7FBA00" />
                                    <rect x="1" y="11" width="9" height="9" fill="#00A4EF" />
                                    <rect x="11" y="11" width="9" height="9" fill="#FFB900" />
                                </svg>
                                <span className="text-xs text-[var(--text-dim)] group-hover:text-white transition-colors font-medium">Microsoft</span>
                            </a>
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};

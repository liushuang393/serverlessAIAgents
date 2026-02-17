import React, { useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { useAuthStore } from '../../stores/authStore';
import { Loader2 } from 'lucide-react';

export const AuthGuard: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { isAuthenticated, isLoading, checkAuth } = useAuthStore();
    const navigate = useNavigate();

    useEffect(() => {
        checkAuth();
    }, [checkAuth]);

    useEffect(() => {
        if (!isLoading && !isAuthenticated) {
            navigate('/login');
        }
    }, [isLoading, isAuthenticated, navigate]);

    if (isLoading) {
        return (
            <div className="h-screen w-full flex flex-col items-center justify-center bg-[var(--bg-main)] text-white gap-4">
                <div className="w-12 h-12 rounded-2xl glass flex items-center justify-center border border-white/10">
                    <Loader2 className="animate-spin text-[var(--primary)]" size={24} />
                </div>
                <span className="text-xs text-[var(--text-muted)] uppercase tracking-widest font-semibold">Authenticating</span>
            </div>
        );
    }

    if (!isAuthenticated) {
        return null; // Will redirect
    }

    return <>{children}</>;
};

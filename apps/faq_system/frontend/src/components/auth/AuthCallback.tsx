import { useEffect } from 'react';
import { useNavigate, useSearchParams } from 'react-router-dom';
import { useAuthStore } from '../../stores/authStore';
import { authApi } from '../../api/auth';
import { Loader2 } from 'lucide-react';

export const AuthCallback = () => {
    const [searchParams] = useSearchParams();
    const navigate = useNavigate();
    const login = useAuthStore((state) => state.login);

    useEffect(() => {
        const accessToken = searchParams.get('access_token');
        // const sessionToken = searchParams.get('session_token'); // If we need it

        const processLogin = async () => {
            if (accessToken) {
                try {
                    localStorage.setItem('access_token', accessToken);
                    const res = await authApi.getMe();
                    // res is AuthResponse, we need res.user (UserInfo)
                    if (res.success && res.user) {
                        login(accessToken, res.user);
                        navigate('/');
                    } else {
                        throw new Error(res.message || "Failed to fetch user info");
                    }
                } catch (error) {
                    console.error("Failed to fetch user info", error);
                    navigate('/login?error=auth_failed');
                }
            } else {
                navigate('/login?error=no_token');
            }
        };

        processLogin();
    }, [searchParams, navigate, login]);

    return (
        <div className="min-h-screen flex items-center justify-center bg-[#343541] text-white">
            <div className="flex flex-col items-center gap-4">
                <Loader2 className="animate-spin text-green-500" size={48} />
                <p>Authenticating...</p>
            </div>
        </div>
    );
};

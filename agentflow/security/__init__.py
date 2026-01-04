# -*- coding: utf-8 -*-
"""AgentFlow セキュリティモジュール.

認証、認可、API Key 管理、レート制限を提供します。

機能:
- 認証ミドルウェア（JWT、API Key）
- 認可（RBAC、パーミッション）
- API Key 管理
- レート制限

使用例:
    >>> from agentflow.security import (
    ...     APIKeyManager,
    ...     RateLimiter,
    ...     create_auth_middleware,
    ... )
    >>>
    >>> # API Key 検証
    >>> key_manager = APIKeyManager()
    >>> is_valid = await key_manager.validate("sk-xxx")
    >>>
    >>> # レート制限
    >>> limiter = RateLimiter(requests_per_minute=60)
    >>> allowed = await limiter.check("user-123")
"""

from agentflow.security.api_key import (
    APIKeyManager,
    APIKey,
    APIKeyConfig,
    generate_api_key,
)
from agentflow.security.rate_limiter import (
    RateLimiter,
    RateLimitConfig,
    RateLimitExceeded,
)
from agentflow.security.auth_middleware import (
    AuthMiddleware,
    create_auth_middleware,
    JWTConfig,
    require_auth,
    require_permission,
)
from agentflow.security.rbac import (
    Permission,
    Role,
    RBACManager,
)

__all__ = [
    # API Key
    "APIKeyManager",
    "APIKey",
    "APIKeyConfig",
    "generate_api_key",
    # Rate Limiter
    "RateLimiter",
    "RateLimitConfig",
    "RateLimitExceeded",
    # Auth Middleware
    "AuthMiddleware",
    "create_auth_middleware",
    "JWTConfig",
    "require_auth",
    "require_permission",
    # RBAC
    "Permission",
    "Role",
    "RBACManager",
]


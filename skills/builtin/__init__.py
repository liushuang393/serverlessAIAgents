"""AgentFlow 内置 Skills - 生产级别的能力包.

提供数据库、支付、部署、认证等企业级功能的开箱即用实现。

内置 Skills:
- database-manager: 数据库统一管理（Supabase/Turso/PostgreSQL）
- stripe-payment: Stripe 支付集成
- deployment-manager: 部署管理（Vercel/Cloudflare）
- auth-provider: 认证集成（Supabase Auth/Clerk）

使用示例:
    ```python
    from agentflow.skills.builtin.database_manager import DatabaseManager, SupabaseConfig
    from agentflow.skills.builtin.stripe_payment import StripePayment, StripeConfig
    from agentflow.skills.builtin.deployment_manager import DeploymentManager, VercelConfig
    from agentflow.skills.builtin.auth_provider import AuthProvider, SupabaseAuthConfig
    ```
"""

__all__ = [
    "auth_provider",
    "database_manager",
    "deployment_manager",
    "stripe_payment",
]


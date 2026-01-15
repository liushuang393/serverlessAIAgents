# -*- coding: utf-8 -*-
"""AgentFlow セキュリティモジュール.

認証、認可、API Key 管理、レート制限、AI安全防護を提供します。

機能:
- 認証ミドルウェア（JWT、API Key）
- 認可（RBAC、パーミッション）
- API Key 管理
- レート制限
- AI弱点防護（幻覚検出、推理監視、データ脱敏）

使用例:
    >>> from agentflow.security import (
    ...     APIKeyManager,
    ...     RateLimiter,
    ...     create_auth_middleware,
    ...     HallucinationDetector,
    ...     ReasoningMonitor,
    ...     DataSanitizer,
    ... )
    >>>
    >>> # API Key 検証
    >>> key_manager = APIKeyManager()
    >>> is_valid = await key_manager.validate("sk-xxx")
    >>>
    >>> # レート制限
    >>> limiter = RateLimiter(requests_per_minute=60)
    >>> allowed = await limiter.check("user-123")
    >>>
    >>> # 幻覚検出
    >>> detector = HallucinationDetector()
    >>> result = await detector.check("GPT-4于2022年发布")
    >>>
    >>> # 推理監視
    >>> monitor = ReasoningMonitor("分析销售数据")
    >>> step_result = monitor.check_step(step)
    >>>
    >>> # データ脱敏
    >>> sanitizer = DataSanitizer()
    >>> safe_text = sanitizer.sanitize(user_input)
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
from agentflow.security.hallucination_detector import (
    HallucinationDetector,
    DetectionConfig,
    DetectionResult,
    Issue,
    IssueType,
    Severity,
)
from agentflow.security.reasoning_monitor import (
    ReasoningMonitor,
    MonitorConfig,
    MonitorResult,
    ReasoningStep,
    ReasoningState,
    DeviationType,
    DeviationReport,
)
from agentflow.security.data_sanitizer import (
    DataSanitizer,
    SanitizerConfig,
    SanitizationResult,
    ThreatDetection,
    ThreatType,
    PIIType,
)
from agentflow.security.ai_safety_guard import (
    AISafetyGuard,
    GuardConfig,
    InputCheckResult,
    OutputCheckResult,
    SafetyLevel,
)
from agentflow.security.safety_mixin import SafetyMixin

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
    # AI Safety - Hallucination Detection
    "HallucinationDetector",
    "DetectionConfig",
    "DetectionResult",
    "Issue",
    "IssueType",
    "Severity",
    # AI Safety - Reasoning Monitor
    "ReasoningMonitor",
    "MonitorConfig",
    "MonitorResult",
    "ReasoningStep",
    "ReasoningState",
    "DeviationType",
    "DeviationReport",
    # AI Safety - Data Sanitizer
    "DataSanitizer",
    "SanitizerConfig",
    "SanitizationResult",
    "ThreatDetection",
    "ThreatType",
    "PIIType",
    # AI Safety - Guard (Facade)
    "AISafetyGuard",
    "GuardConfig",
    "InputCheckResult",
    "OutputCheckResult",
    "SafetyLevel",
    # Safety Mixin (Apps統合用)
    "SafetyMixin",
]


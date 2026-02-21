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

from agentflow.security.ai_safety_guard import (
    AISafetyGuard,
    GuardConfig,
    InputCheckResult,
    OutputCheckResult,
    SafetyLevel,
)
from agentflow.security.api_key import (
    APIKey,
    APIKeyConfig,
    APIKeyManager,
    generate_api_key,
)
from agentflow.security.auth_middleware import (
    AuthMiddleware,
    JWTConfig,
    create_auth_middleware,
    require_auth,
    require_permission,
)
from agentflow.security.data_sanitizer import (
    DataSanitizer,
    PIIType,
    SanitizationResult,
    SanitizerConfig,
    ThreatDetection,
    ThreatType,
)
from agentflow.security.hallucination_detector import (
    DetectionConfig,
    DetectionResult,
    HallucinationDetector,
    Issue,
    IssueType,
    Severity,
)
from agentflow.security.output_contracts import (
    ContractRegistry,
    ContractValidator,
    Evidence,
    EvidenceRule,
    EvidenceType,
    FallbackAction,
    FieldSchema,
    OutputContract,
    ValidationResult,
)
from agentflow.security.rate_limiter import (
    RateLimitConfig,
    RateLimiter,
    RateLimitExceeded,
)
from agentflow.security.rbac import (
    Permission,
    RBACManager,
    Role,
)
from agentflow.security.reasoning_monitor import (
    DeviationReport,
    DeviationType,
    MonitorConfig,
    MonitorResult,
    ReasoningMonitor,
    ReasoningState,
    ReasoningStep,
)
from agentflow.security.safety_mixin import SafetyMixin


__all__ = [
    # AI Safety - Guard (Facade)
    "AISafetyGuard",
    "APIKey",
    "APIKeyConfig",
    # API Key
    "APIKeyManager",
    # Auth Middleware
    "AuthMiddleware",
    "ContractRegistry",
    "ContractValidator",
    # AI Safety - Data Sanitizer
    "DataSanitizer",
    "DetectionConfig",
    "DetectionResult",
    "DeviationReport",
    "DeviationType",
    "Evidence",
    "EvidenceRule",
    # L5: Output Contracts（構造化出力規約）
    "EvidenceType",
    "FallbackAction",
    "FieldSchema",
    "GuardConfig",
    # AI Safety - Hallucination Detection
    "HallucinationDetector",
    "InputCheckResult",
    "Issue",
    "IssueType",
    "JWTConfig",
    "MonitorConfig",
    "MonitorResult",
    "OutputCheckResult",
    "OutputContract",
    "PIIType",
    # RBAC
    "Permission",
    "RBACManager",
    "RateLimitConfig",
    "RateLimitExceeded",
    # Rate Limiter
    "RateLimiter",
    # AI Safety - Reasoning Monitor
    "ReasoningMonitor",
    "ReasoningState",
    "ReasoningStep",
    "Role",
    "SafetyLevel",
    # Safety Mixin (Apps統合用)
    "SafetyMixin",
    "SanitizationResult",
    "SanitizerConfig",
    "Severity",
    "ThreatDetection",
    "ThreatType",
    "ValidationResult",
    "create_auth_middleware",
    "generate_api_key",
    "require_auth",
    "require_permission",
]

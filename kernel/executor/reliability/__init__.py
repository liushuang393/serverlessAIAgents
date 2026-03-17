"""kernel.executor.reliability — 信頼性コンポーネント.

CircuitBreaker, Retry, RetryAdvisor, Reliability, RollbackManager を提供する。
"""

from kernel.executor.reliability.circuit_breaker import (
    CircuitBreaker,
    CircuitBreakerConfig,
    CircuitBreakerOpenError,
    CircuitState,
)
from kernel.executor.reliability.reliability import (
    get_circuit_breaker,
    reliable,
)
from kernel.executor.reliability.retry import RetryableAgent, RetryConfig
from kernel.executor.reliability.retry_advisor import (
    RetryAction,
    RetryAdvice,
    RetryAdvisor,
    RetryContext,
)
from kernel.executor.reliability.rollback_manager import (
    Checkpoint,
    CheckpointStatus,
    RetryStrategy,
    RollbackManager,
    RollbackResult,
)
from kernel.executor.reliability.rollback_manager import (
    RetryConfig as RollbackRetryConfig,
)


__all__ = [
    "Checkpoint",
    "CheckpointStatus",
    "CircuitBreaker",
    "CircuitBreakerConfig",
    "CircuitBreakerOpenError",
    "CircuitState",
    "RetryAction",
    "RetryAdvice",
    "RetryAdvisor",
    "RetryConfig",
    "RetryContext",
    "RetryStrategy",
    "RetryableAgent",
    "RollbackManager",
    "RollbackResult",
    "RollbackRetryConfig",
    "get_circuit_breaker",
    "reliable",
]

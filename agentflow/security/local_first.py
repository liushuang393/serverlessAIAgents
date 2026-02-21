"""Local-First execution policy for Agent OS.

Implements the principle that agents should execute locally first,
with explicit control over external access.
"""

from enum import Enum

from pydantic import BaseModel, Field


class ExecutionLocation(str, Enum):
    """Execution environment locations.

    Defines where code can be executed.
    """

    LOCAL = "local"
    """Execute on the local machine."""

    SANDBOX_LOCAL = "sandbox_local"
    """Execute in a local sandbox environment."""

    CLOUD_PROVIDER = "cloud_provider"
    """Execute on a cloud provider."""


class LocalFirstPolicy(BaseModel):
    """Local-first execution policy.

    Defines the constraints for agent execution, prioritizing
    local execution with explicit control over external access.

    Attributes:
        default_location: Default execution location
        allow_network: Whether network access is allowed at all
        network_whitelist: List of allowed external hosts
        network_loopback_only: Only allow loopback (localhost) network
        audit_all_external: Whether to audit all external access
    """

    default_location: ExecutionLocation = ExecutionLocation.LOCAL
    allow_network: bool = False
    network_whitelist: list[str] = Field(default_factory=list)
    network_loopback_only: bool = True
    audit_all_external: bool = True

    model_config = {"frozen": False}


class NetworkAccessDecision(BaseModel):
    """Result of a network access check.

    Indicates whether a network access is allowed and why.

    Attributes:
        allowed: Whether the access is allowed
        reason: Explanation of the decision
    """

    allowed: bool
    reason: str = ""

    model_config = {"frozen": False}


class LocalFirstEnforcer:
    """Enforces local-first execution policy.

    Checks network access requests against the policy and
    returns decisions.

    Example:
        policy = LocalFirstPolicy(network_loopback_only=True)
        enforcer = LocalFirstEnforcer(policy)
        decision = enforcer.check_network_access("api.example.com", 443, "api")
        if not decision.allowed:
            print(f"Blocked: {decision.reason}")
    """

    def __init__(self, policy: LocalFirstPolicy) -> None:
        """Initialize the enforcer.

        Args:
            policy: The local-first policy to enforce
        """
        self._policy = policy

    def check_network_access(self, host: str, port: int, purpose: str) -> NetworkAccessDecision:
        """Check if a network access is allowed.

        Args:
            host: The target host
            port: The target port
            purpose: Description of why the access is needed

        Returns:
            NetworkAccessDecision indicating if access is allowed
        """
        # Check loopback-only policy
        if self._policy.network_loopback_only:
            if host in ("127.0.0.1", "localhost", "::1"):
                return NetworkAccessDecision(
                    allowed=True,
                    reason="Loopback address allowed by policy",
                )
            return NetworkAccessDecision(
                allowed=False,
                reason="Network restricted to loopback only",
            )

        # Check if host is in whitelist
        if host in self._policy.network_whitelist:
            return NetworkAccessDecision(
                allowed=True,
                reason=f"Host {host} is in whitelist",
            )

        # Not in whitelist
        return NetworkAccessDecision(
            allowed=False,
            reason=f"Host {host} not in whitelist",
        )

    def get_policy(self) -> LocalFirstPolicy:
        """Get the current policy.

        Returns:
            The local-first policy
        """
        return self._policy

    def is_loopback_only(self) -> bool:
        """Check if policy restricts to loopback only.

        Returns:
            True if only loopback is allowed
        """
        return self._policy.network_loopback_only

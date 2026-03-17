"""infrastructure.security 後方互換shim → harness.guardrails."""

from harness.guardrails.safety_mixin import SafetyMixin

__all__ = ["SafetyMixin"]

import pytest
from agentflow.security.local_first import (
    LocalFirstPolicy,
    LocalFirstEnforcer,
    ExecutionLocation,
    NetworkAccessDecision,
)


def test_loopback_only_policy():
    policy = LocalFirstPolicy(network_loopback_only=True)
    enforcer = LocalFirstEnforcer(policy)

    # Loopback allowed
    decision = enforcer.check_network_access("127.0.0.1", 8080, "api")
    assert decision.allowed is True

    # External blocked
    decision = enforcer.check_network_access("api.external.com", 443, "api")
    assert decision.allowed is False


def test_localhost_allowed():
    policy = LocalFirstPolicy(network_loopback_only=True)
    enforcer = LocalFirstEnforcer(policy)

    decision = enforcer.check_network_access("localhost", 3000, "dev server")
    assert decision.allowed is True


def test_whitelist_hosts():
    policy = LocalFirstPolicy(
        network_loopback_only=False,
        network_whitelist=["api.openai.com", "api.anthropic.com"],
    )
    enforcer = LocalFirstEnforcer(policy)

    decision = enforcer.check_network_access("api.openai.com", 443, "llm")
    assert decision.allowed is True

    decision = enforcer.check_network_access("api.anthropic.com", 443, "llm")
    assert decision.allowed is True

    decision = enforcer.check_network_access("api.google.com", 443, "other")
    assert decision.allowed is False


def test_policy_defaults():
    policy = LocalFirstPolicy()
    assert policy.default_location == ExecutionLocation.LOCAL
    assert policy.allow_network is False
    assert policy.network_whitelist == []
    assert policy.network_loopback_only is True
    assert policy.audit_all_external is True


def test_execution_locations():
    assert ExecutionLocation.LOCAL == "local"
    assert ExecutionLocation.SANDBOX_LOCAL == "sandbox_local"
    assert ExecutionLocation.CLOUD_PROVIDER == "cloud_provider"


def test_network_access_decision():
    decision = NetworkAccessDecision(allowed=True, reason="Loopback allowed")
    assert decision.allowed is True
    assert decision.reason == "Loopback allowed"


def test_policy_with_network_allowed():
    policy = LocalFirstPolicy(
        allow_network=True,
        network_loopback_only=False,
        network_whitelist=["api.example.com"],
    )
    enforcer = LocalFirstEnforcer(policy)

    # Whitelisted host allowed
    decision = enforcer.check_network_access("api.example.com", 443, "api")
    assert decision.allowed is True


def test_decision_reason_message():
    policy = LocalFirstPolicy(network_loopback_only=True)
    enforcer = LocalFirstEnforcer(policy)

    decision = enforcer.check_network_access("external.com", 80, "test")
    assert decision.allowed is False
    assert "loopback" in decision.reason.lower()

"""shim: agentflow.routing.intent_router -> kernel."""
from kernel.router import IntentRouter  # noqa: F401

__all__ = ["IntentRouter"]

"""shared.rag.scope_resolver 後方互換shim → shared.scope.scope_resolver."""

from shared.scope.scope_resolver import *  # noqa: F403
from shared.scope.scope_resolver import (
    FALLBACK_ROLE_KB_MAP,
    CollectionTarget,
    ScopeResolver,
)


__all__ = ["FALLBACK_ROLE_KB_MAP", "CollectionTarget", "ScopeResolver"]

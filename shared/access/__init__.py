"""共有 access 公開 API."""

from shared.access.contracts import AccessContext
from shared.access.service import build_access_context

__all__ = ["AccessContext", "build_access_context"]

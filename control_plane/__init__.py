"""Layer 5 の control plane 公開 API.

`platform` というトップレベル名は Python 標準ライブラリと衝突するため、
実装パッケージ名は `control_plane` とする。
"""

from control_plane.discovery import DiscoveryService
from control_plane.lifecycle import LifecycleService
from control_plane.registry import AppRegistryService


__all__ = [
    "AppRegistryService",
    "DiscoveryService",
    "LifecycleService",
]

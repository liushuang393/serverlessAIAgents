"""IntentRouter - kernel/router からの再エクスポートshim.

本体は kernel/router/service.py に移行済み。
後方互換性のためこのモジュールからも全シンボルをインポート可能。
"""

from kernel.router.service import (
    Intent,
    IntentCategory,
    IntentRouter,
    RouterConfig,
)


__all__ = [
    "Intent",
    "IntentCategory",
    "IntentRouter",
    "RouterConfig",
]

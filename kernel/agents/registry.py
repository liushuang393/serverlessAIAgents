"""統一レジストリ基類 - 後方互換スタブ.

実体は shared/registry/base.py に移行済み。
既存コードの ``from kernel.agents.registry import Registry`` を壊さないよう
re-export する。
"""

from shared.registry.base import ProtocolRegistry, Registry


__all__ = ["ProtocolRegistry", "Registry"]

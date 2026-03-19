"""IConfigManager re-export shim.

正規配置: contracts/interfaces/config_manager.py
後方互換のため kernel からも import 可能にする。
"""

from contracts.interfaces.config_manager import IConfigManager


__all__ = ["IConfigManager"]

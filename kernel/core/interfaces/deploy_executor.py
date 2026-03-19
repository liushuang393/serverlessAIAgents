"""IDeployExecutor re-export shim.

正規配置: contracts/interfaces/deploy_executor.py
後方互換のため kernel からも import 可能にする。
"""

from contracts.interfaces.deploy_executor import IDeployExecutor


__all__ = ["IDeployExecutor"]

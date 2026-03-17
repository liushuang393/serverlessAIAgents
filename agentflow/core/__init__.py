"""agentflow.core shim（後方互換用）

agentflow.core.loader → kernel.core.schemas
agentflow.core.manager → kernel.agents
"""
import importlib as _il
import sys as _sys

# サブモジュールをsys.modulesに事前登録（patch互換のため）
_sys.modules.setdefault(
    "agentflow.core.loader",
    _il.import_module("kernel.core.schemas"),
)
_sys.modules.setdefault(
    "agentflow.core.manager",
    _il.import_module("kernel.agents"),
)

# kernel.coreの公開シンボルをre-export
from kernel.core import *  # noqa: F401,F403

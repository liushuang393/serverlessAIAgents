"""agentflow.core.loader → kernel.core.schemas shim（patch互換）"""
import importlib as _il
import sys as _sys
_real = _il.import_module("kernel.core.schemas")
_sys.modules[__name__] = _real

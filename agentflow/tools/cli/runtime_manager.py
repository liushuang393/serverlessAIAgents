"""runtime_manager shim → kernel.tools.cli.runtime_manager（後方互換用・patch互換）"""
import importlib as _il
import sys as _sys
_real = _il.import_module("kernel.tools.cli.runtime_manager")
_sys.modules[__name__] = _real

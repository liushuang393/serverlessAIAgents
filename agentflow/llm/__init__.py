"""__init__ shim → infrastructure.llm（後方互換用・patch互換）"""
import importlib as _il
import sys as _sys
_real = _il.import_module("infrastructure.llm")
_sys.modules[__name__] = _real

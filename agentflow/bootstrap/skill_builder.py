"""skill_builder shim → platform.bootstrap.skill_builder（後方互換用・patch互換）"""
import importlib as _il
import sys as _sys
_real = _il.import_module("platform.bootstrap.skill_builder")
_sys.modules[__name__] = _real

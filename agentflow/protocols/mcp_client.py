"""mcp_client shim → kernel.protocols.mcp_client（後方互換用・patch互換）"""
import importlib as _il
import sys as _sys
_real = _il.import_module("kernel.protocols.mcp_client")
_sys.modules[__name__] = _real

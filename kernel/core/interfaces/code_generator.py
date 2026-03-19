"""ICodeGenerator re-export shim.

正規配置: contracts/interfaces/code_generator.py
後方互換のため kernel からも import 可能にする。
"""

from contracts.interfaces.code_generator import ICodeGenerator


__all__ = ["ICodeGenerator"]

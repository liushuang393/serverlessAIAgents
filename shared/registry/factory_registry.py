"""差し替え可能コンポーネント向けの共通レジストリ（再エクスポート）。

型定義の正本は ``contracts.registry`` に統合済み。
後方互換のため、ここから re-export する。
"""

from __future__ import annotations

from contracts.registry import RegisteredComponent, ToggleableFactoryRegistry


__all__ = [
    "RegisteredComponent",
    "ToggleableFactoryRegistry",
]

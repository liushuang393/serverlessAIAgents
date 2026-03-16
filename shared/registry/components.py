"""差し替え可能コンポーネントの設定契約（再エクスポート）。

型定義の正本は ``contracts.base`` に統合済み。
後方互換のため、ここから re-export する。
"""

from __future__ import annotations

from contracts.base import ComponentSpec, ComponentToggle, LayerName

__all__ = [
    "ComponentSpec",
    "ComponentToggle",
    "LayerName",
]

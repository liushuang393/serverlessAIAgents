"""A2UI Protocol - Agent-Driven User Interface.

このモジュールは Google A2UI 規範に準拠した生成式 UI 機能を提供します：
- 宣言式コンポーネント定義
- ストリーミング UI 生成
- AG-UI との統合
- 富文本コンポーネント（Markdown、コード、表格、チャート）

設計原則：
- 安全：Agent は定義済みコンポーネントのみ使用可能
- 柔軟：カスタムコンポーネント拡張可能
- 統一：AG-UI を通じて配信

参考：
- Google A2UI Specification v0.8
- AG-UI Protocol
"""

from __future__ import annotations

import importlib
from typing import Any


# 遅延インポートマッピング（循環参照回避）
_LAZY_IMPORTS: dict[str, tuple[str, str]] = {
    # components
    "A2UIComponent": ("kernel.protocols.a2ui.components", "A2UIComponent"),
    "ButtonComponent": ("kernel.protocols.a2ui.components", "ButtonComponent"),
    "CardComponent": ("kernel.protocols.a2ui.components", "CardComponent"),
    "ComponentType": ("kernel.protocols.a2ui.components", "ComponentType"),
    "FormComponent": ("kernel.protocols.a2ui.components", "FormComponent"),
    "ImageComponent": ("kernel.protocols.a2ui.components", "ImageComponent"),
    "InputComponent": ("kernel.protocols.a2ui.components", "InputComponent"),
    "ListComponent": ("kernel.protocols.a2ui.components", "ListComponent"),
    "TextComponent": ("kernel.protocols.a2ui.components", "TextComponent"),
    # emitter
    "A2UIEmitter": ("kernel.protocols.a2ui.emitter", "A2UIEmitter"),
    # renderer（kernel.core.registry に依存するため遅延必須）
    "A2UIRenderer": ("kernel.protocols.a2ui.renderer", "A2UIRenderer"),
    "ComponentRegistry": ("kernel.protocols.a2ui.renderer", "ComponentRegistry"),
    # rich_content
    "Alert": ("kernel.protocols.a2ui.rich_content", "Alert"),
    "AlertType": ("kernel.protocols.a2ui.rich_content", "AlertType"),
    "ChartType": ("kernel.protocols.a2ui.rich_content", "ChartType"),
    "ChartView": ("kernel.protocols.a2ui.rich_content", "ChartView"),
    "Citation": ("kernel.protocols.a2ui.rich_content", "Citation"),
    "CodeBlock": ("kernel.protocols.a2ui.rich_content", "CodeBlock"),
    "CollapsibleSection": ("kernel.protocols.a2ui.rich_content", "CollapsibleSection"),
    "DataTable": ("kernel.protocols.a2ui.rich_content", "DataTable"),
    "Link": ("kernel.protocols.a2ui.rich_content", "Link"),
    "MarkdownContent": ("kernel.protocols.a2ui.rich_content", "MarkdownContent"),
    "Progress": ("kernel.protocols.a2ui.rich_content", "Progress"),
    "RichComponent": ("kernel.protocols.a2ui.rich_content", "RichComponent"),
    "RichComponentType": ("kernel.protocols.a2ui.rich_content", "RichComponentType"),
    "RichResponse": ("kernel.protocols.a2ui.rich_content", "RichResponse"),
    "Tabs": ("kernel.protocols.a2ui.rich_content", "Tabs"),
    "Timeline": ("kernel.protocols.a2ui.rich_content", "Timeline"),
}


def __getattr__(name: str) -> Any:
    """遅延インポートを実装.

    Args:
        name: インポートする属性名

    Returns:
        インポートされた属性

    Raises:
        AttributeError: 属性が見つからない場合
    """
    if name in _LAZY_IMPORTS:
        module_path, attr_name = _LAZY_IMPORTS[name]
        module = importlib.import_module(module_path)
        return getattr(module, attr_name)
    msg = f"module {__name__!r} has no attribute {name!r}"
    raise AttributeError(msg)


__all__ = list(_LAZY_IMPORTS.keys())

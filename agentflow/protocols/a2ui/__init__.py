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

from agentflow.protocols.a2ui.components import (
    A2UIComponent,
    ButtonComponent,
    CardComponent,
    ComponentType,
    FormComponent,
    ImageComponent,
    InputComponent,
    ListComponent,
    TextComponent,
)
from agentflow.protocols.a2ui.emitter import A2UIEmitter
from agentflow.protocols.a2ui.renderer import A2UIRenderer, ComponentRegistry

# 富文本コンポーネント（NEW）
from agentflow.protocols.a2ui.rich_content import (
    RichComponentType,
    ChartType,
    AlertType,
    RichComponent,
    MarkdownContent,
    CodeBlock,
    DataTable,
    ChartView,
    Citation,
    CollapsibleSection,
    Link,
    Progress,
    Alert,
    Tabs,
    Timeline,
    RichResponse,
)

__all__ = [
    # 基本コンポーネント
    "A2UIComponent",
    "ButtonComponent",
    "CardComponent",
    "ComponentType",
    "FormComponent",
    "ImageComponent",
    "InputComponent",
    "ListComponent",
    "TextComponent",
    # エミッター
    "A2UIEmitter",
    # レンダラー
    "A2UIRenderer",
    "ComponentRegistry",
    # 富文本コンポーネント（NEW）
    "RichComponentType",
    "ChartType",
    "AlertType",
    "RichComponent",
    "MarkdownContent",
    "CodeBlock",
    "DataTable",
    "ChartView",
    "Citation",
    "CollapsibleSection",
    "Link",
    "Progress",
    "Alert",
    "Tabs",
    "Timeline",
    "RichResponse",
]


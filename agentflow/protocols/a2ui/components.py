"""A2UI コンポーネント定義 - 宣言式 UI コンポーネント.

このモジュールは A2UI 規範に準拠したコンポーネントを定義します。
Agent はこれらのコンポーネントを組み合わせて UI を生成します。
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class ComponentType(str, Enum):
    """コンポーネント種別."""

    TEXT = "text"
    BUTTON = "button"
    INPUT = "input"
    CARD = "card"
    LIST = "list"
    IMAGE = "image"
    FORM = "form"
    CUSTOM = "custom"


@dataclass
class A2UIComponent:
    """A2UI コンポーネント基類.

    全コンポーネントの基底クラス。
    JSON シリアライズ可能な宣言式構造。

    Attributes:
        component_type: コンポーネント種別
        id: コンポーネント ID（オプション）
        props: プロパティ辞書
        children: 子コンポーネントリスト
        style: スタイル辞書
    """

    component_type: ComponentType
    id: str | None = None
    props: dict[str, Any] = field(default_factory=dict)
    children: list["A2UIComponent"] = field(default_factory=list)
    style: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換（JSON シリアライズ用）.

        Returns:
            コンポーネント辞書
        """
        result: dict[str, Any] = {
            "type": self.component_type.value,
            "props": self.props,
        }
        if self.id:
            result["id"] = self.id
        if self.children:
            result["children"] = [c.to_dict() for c in self.children]
        if self.style:
            result["style"] = self.style
        return result

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "A2UIComponent":
        """辞書から作成.

        Args:
            data: コンポーネント辞書

        Returns:
            A2UIComponent インスタンス
        """
        children = [cls.from_dict(c) for c in data.get("children", [])]
        return cls(
            component_type=ComponentType(data.get("type", "custom")),
            id=data.get("id"),
            props=data.get("props", {}),
            children=children,
            style=data.get("style", {}),
        )


@dataclass
class TextComponent(A2UIComponent):
    """テキストコンポーネント."""

    def __init__(self, content: str, **kwargs: Any) -> None:
        super().__init__(
            component_type=ComponentType.TEXT,
            props={"content": content, **kwargs},
        )


@dataclass
class ButtonComponent(A2UIComponent):
    """ボタンコンポーネント."""

    def __init__(self, label: str, action: str | None = None, **kwargs: Any) -> None:
        super().__init__(
            component_type=ComponentType.BUTTON,
            props={"label": label, "action": action, **kwargs},
        )


@dataclass
class InputComponent(A2UIComponent):
    """入力コンポーネント."""

    def __init__(
        self,
        name: str,
        input_type: str = "text",
        placeholder: str = "",
        **kwargs: Any,
    ) -> None:
        super().__init__(
            component_type=ComponentType.INPUT,
            props={"name": name, "type": input_type, "placeholder": placeholder, **kwargs},
        )


@dataclass
class CardComponent(A2UIComponent):
    """カードコンポーネント."""

    def __init__(
        self,
        title: str = "",
        children: list[A2UIComponent] | None = None,
        **kwargs: Any,
    ) -> None:
        super().__init__(
            component_type=ComponentType.CARD,
            props={"title": title, **kwargs},
            children=children or [],
        )


@dataclass
class ListComponent(A2UIComponent):
    """リストコンポーネント."""

    def __init__(self, items: list[A2UIComponent] | None = None, **kwargs: Any) -> None:
        super().__init__(
            component_type=ComponentType.LIST,
            props=kwargs,
            children=items or [],
        )


@dataclass
class ImageComponent(A2UIComponent):
    """画像コンポーネント."""

    def __init__(self, src: str, alt: str = "", **kwargs: Any) -> None:
        super().__init__(
            component_type=ComponentType.IMAGE,
            props={"src": src, "alt": alt, **kwargs},
        )


@dataclass
class FormComponent(A2UIComponent):
    """フォームコンポーネント."""

    def __init__(
        self,
        action: str,
        fields: list[A2UIComponent] | None = None,
        **kwargs: Any,
    ) -> None:
        super().__init__(
            component_type=ComponentType.FORM,
            props={"action": action, **kwargs},
            children=fields or [],
        )


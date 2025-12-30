"""A2UI レンダラー - コンポーネント描画基類.

このモジュールはコンポーネントを実際の UI に変換する基類を提供します。
各フレームワーク（React/Angular/Flutter）用に継承して使用します。
"""

import logging
from abc import ABC, abstractmethod
from typing import Any

from agentflow.core.registry import Registry
from agentflow.protocols.a2ui.components import A2UIComponent, ComponentType


class A2UIRenderer(ABC):
    """A2UI レンダラー基類.

    コンポーネントを具体的な UI 出力に変換します。
    各プラットフォーム用に継承してください。

    Example:
        >>> class HTMLRenderer(A2UIRenderer):
        ...     def render_text(self, component):
        ...         return f"<p>{component.props['content']}</p>"
    """

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._custom_renderers: dict[str, callable] = {}

    def render(self, component: A2UIComponent) -> Any:
        """コンポーネントを描画.

        Args:
            component: 描画対象コンポーネント

        Returns:
            描画結果（プラットフォーム依存）
        """
        renderer_method = self._get_renderer(component.component_type)
        return renderer_method(component)

    def _get_renderer(self, component_type: ComponentType) -> callable:
        """コンポーネント種別に応じたレンダラーを取得.

        Args:
            component_type: コンポーネント種別

        Returns:
            レンダラーメソッド
        """
        renderers = {
            ComponentType.TEXT: self.render_text,
            ComponentType.BUTTON: self.render_button,
            ComponentType.INPUT: self.render_input,
            ComponentType.CARD: self.render_card,
            ComponentType.LIST: self.render_list,
            ComponentType.IMAGE: self.render_image,
            ComponentType.FORM: self.render_form,
            ComponentType.CUSTOM: self.render_custom,
        }
        return renderers.get(component_type, self.render_unknown)

    @abstractmethod
    def render_text(self, component: A2UIComponent) -> Any:
        """テキストを描画."""
        pass

    @abstractmethod
    def render_button(self, component: A2UIComponent) -> Any:
        """ボタンを描画."""
        pass

    @abstractmethod
    def render_input(self, component: A2UIComponent) -> Any:
        """入力フィールドを描画."""
        pass

    @abstractmethod
    def render_card(self, component: A2UIComponent) -> Any:
        """カードを描画."""
        pass

    @abstractmethod
    def render_list(self, component: A2UIComponent) -> Any:
        """リストを描画."""
        pass

    @abstractmethod
    def render_image(self, component: A2UIComponent) -> Any:
        """画像を描画."""
        pass

    @abstractmethod
    def render_form(self, component: A2UIComponent) -> Any:
        """フォームを描画."""
        pass

    def render_custom(self, component: A2UIComponent) -> Any:
        """カスタムコンポーネントを描画.

        Args:
            component: カスタムコンポーネント

        Returns:
            描画結果
        """
        custom_type = component.props.get("custom_type", "unknown")
        if custom_type in self._custom_renderers:
            return self._custom_renderers[custom_type](component)
        return self.render_unknown(component)

    def render_unknown(self, component: A2UIComponent) -> Any:
        """未知のコンポーネントを描画.

        Args:
            component: 未知のコンポーネント

        Returns:
            描画結果（デフォルト：空文字列）
        """
        self._logger.warning(f"Unknown component type: {component.component_type}")
        return ""

    def register_custom_renderer(
        self,
        custom_type: str,
        renderer: callable,
    ) -> None:
        """カスタムレンダラーを登録.

        Args:
            custom_type: カスタムコンポーネント種別
            renderer: レンダラー関数
        """
        self._custom_renderers[custom_type] = renderer


class ComponentRegistry(Registry[type[A2UIComponent]]):
    """コンポーネントレジストリ - カスタムコンポーネント管理.

    Example:
        >>> registry = ComponentRegistry()
        >>> registry.register("chart", ChartComponent)
        >>> ChartClass = registry.get("chart")
    """

    _instance: "ComponentRegistry | None" = None

    def __new__(cls) -> "ComponentRegistry":
        """シングルトンパターン."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance


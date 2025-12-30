"""A2UI コンポーネントのテスト.

このテストは A2UI コンポーネントとエミッターをテストします。
"""

import pytest

from agentflow.protocols.a2ui import (
    A2UIComponent,
    A2UIEmitter,
    ButtonComponent,
    CardComponent,
    ComponentRegistry,
    ComponentType,
    FormComponent,
    ImageComponent,
    InputComponent,
    ListComponent,
    TextComponent,
)


class TestComponentType:
    """ComponentType 列挙のテスト."""

    def test_component_types(self) -> None:
        """全コンポーネント種別が定義されていることをテスト."""
        assert ComponentType.TEXT.value == "text"
        assert ComponentType.BUTTON.value == "button"
        assert ComponentType.INPUT.value == "input"
        assert ComponentType.CARD.value == "card"
        assert ComponentType.LIST.value == "list"
        assert ComponentType.IMAGE.value == "image"
        assert ComponentType.FORM.value == "form"
        assert ComponentType.CUSTOM.value == "custom"


class TestA2UIComponent:
    """A2UIComponent 基類のテスト."""

    def test_to_dict(self) -> None:
        """to_dict が正しく動作することをテスト."""
        component = A2UIComponent(
            component_type=ComponentType.TEXT,
            id="test-id",
            props={"content": "Hello"},
            style={"color": "red"},
        )
        result = component.to_dict()
        assert result["type"] == "text"
        assert result["id"] == "test-id"
        assert result["props"]["content"] == "Hello"
        assert result["style"]["color"] == "red"

    def test_to_dict_without_optional_fields(self) -> None:
        """オプションフィールドなしの to_dict をテスト."""
        component = A2UIComponent(component_type=ComponentType.TEXT, props={"content": "Hi"})
        result = component.to_dict()
        assert "id" not in result
        assert "children" not in result
        assert "style" not in result

    def test_from_dict(self) -> None:
        """from_dict が正しく動作することをテスト."""
        data = {
            "type": "button",
            "id": "btn-1",
            "props": {"label": "Click"},
            "children": [{"type": "text", "props": {"content": "Icon"}}],
        }
        component = A2UIComponent.from_dict(data)
        assert component.component_type == ComponentType.BUTTON
        assert component.id == "btn-1"
        assert component.props["label"] == "Click"
        assert len(component.children) == 1

    def test_from_dict_with_defaults(self) -> None:
        """デフォルト値で from_dict が動作することをテスト."""
        component = A2UIComponent.from_dict({})
        assert component.component_type == ComponentType.CUSTOM


class TestConcreteComponents:
    """具象コンポーネントのテスト."""

    def test_text_component(self) -> None:
        """TextComponent をテスト."""
        text = TextComponent("Hello World")
        assert text.component_type == ComponentType.TEXT
        assert text.props["content"] == "Hello World"

    def test_button_component(self) -> None:
        """ButtonComponent をテスト."""
        btn = ButtonComponent("Submit", action="submit_form")
        assert btn.component_type == ComponentType.BUTTON
        assert btn.props["label"] == "Submit"
        assert btn.props["action"] == "submit_form"

    def test_input_component(self) -> None:
        """InputComponent をテスト."""
        inp = InputComponent("email", input_type="email", placeholder="Enter email")
        assert inp.component_type == ComponentType.INPUT
        assert inp.props["name"] == "email"
        assert inp.props["type"] == "email"
        assert inp.props["placeholder"] == "Enter email"

    def test_card_component(self) -> None:
        """CardComponent をテスト."""
        card = CardComponent(
            title="Card Title",
            children=[TextComponent("Card content")],
        )
        assert card.component_type == ComponentType.CARD
        assert card.props["title"] == "Card Title"
        assert len(card.children) == 1

    def test_list_component(self) -> None:
        """ListComponent をテスト."""
        lst = ListComponent(items=[TextComponent("Item 1"), TextComponent("Item 2")])
        assert lst.component_type == ComponentType.LIST
        assert len(lst.children) == 2

    def test_image_component(self) -> None:
        """ImageComponent をテスト."""
        img = ImageComponent("https://example.com/image.png", alt="Example image")
        assert img.component_type == ComponentType.IMAGE
        assert img.props["src"] == "https://example.com/image.png"
        assert img.props["alt"] == "Example image"

    def test_form_component(self) -> None:
        """FormComponent をテスト."""
        form = FormComponent(
            action="submit",
            fields=[InputComponent("name"), InputComponent("email")],
        )
        assert form.component_type == ComponentType.FORM
        assert form.props["action"] == "submit"
        assert len(form.children) == 2


class TestComponentRegistry:
    """ComponentRegistry のテスト."""

    def test_register_and_get(self) -> None:
        """カスタムコンポーネントを登録・取得できることをテスト."""
        registry = ComponentRegistry()
        registry.register("custom_text", TextComponent)
        assert registry.get("custom_text") is TextComponent

    def test_list_all(self) -> None:
        """全登録コンポーネントを一覧できることをテスト."""
        registry = ComponentRegistry()
        registry.register("a", TextComponent)
        registry.register("b", ButtonComponent)
        all_items = registry.list_all()
        assert "a" in all_items
        assert "b" in all_items


class TestA2UIEmitter:
    """A2UIEmitter のテスト."""

    @pytest.fixture
    def emitter(self) -> A2UIEmitter:
        """テスト用エミッターを作成."""
        return A2UIEmitter()

    @pytest.mark.asyncio
    async def test_emit_component(self, emitter: A2UIEmitter) -> None:
        """コンポーネントを配信できることをテスト."""
        card = CardComponent("Test Card")
        await emitter.emit_component(card, surface_id="main")

        events = emitter.get_pending_events()
        assert len(events) == 1
        assert events[0]["type"] == "a2ui_component"
        assert events[0]["surface_id"] == "main"
        assert events[0]["component"]["type"] == "card"


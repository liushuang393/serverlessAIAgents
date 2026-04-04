"""プロトコルアダプタージェネレーター.

このモジュールは AgentMetadata から MCP/A2A/AG-UI プロトコルアダプターを
自動生成するファクトリークラスを提供します。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable

from kernel.agents.contracts import AgentDescriptor, descriptor_from_agent_metadata
from kernel.core.metadata import AgentMetadata, InputField, OutputField
from kernel.protocols.a2a_card import AgentCapabilities, AgentCard, AgentSkill


if TYPE_CHECKING:
    from kernel.core.engine import AgentFlowEngine
    from kernel.protocols.agui_emitter import AGUIEventEmitter

_logger = logging.getLogger(__name__)


@runtime_checkable
class ProtocolAdapterInterface(Protocol):
    """プロトコルアダプターインターフェース.

    各プロトコル（MCP / A2A / AG-UI）のアダプターが実装すべき Protocol。
    """

    def generate_tools(self, descriptor: AgentDescriptor) -> list[dict[str, Any]]:
        """descriptor からツール定義を生成.

        Args:
            descriptor: エージェント記述子

        Returns:
            ツール定義リスト
        """
        ...

    def generate_card(self, descriptor: AgentDescriptor) -> AgentCard:
        """descriptor から AgentCard を生成.

        Args:
            descriptor: エージェント記述子

        Returns:
            AgentCard
        """
        ...


class ProtocolAdapterRegistry:
    """プロトコルアダプターのレジストリ.

    プロトコル名でアダプターを登録・取得するディスパッチャー。

    Example:
        >>> registry = ProtocolAdapterRegistry()
        >>> registry.register("mcp", my_mcp_adapter)
        >>> adapter = registry.get("mcp")
    """

    def __init__(self) -> None:
        """初期化."""
        self._adapters: dict[str, ProtocolAdapterInterface] = {}

    def register(self, protocol_name: str, adapter: ProtocolAdapterInterface) -> None:
        """アダプターを登録.

        Args:
            protocol_name: プロトコル名（例: "mcp", "a2a", "agui"）
            adapter: アダプター実装
        """
        self._adapters[protocol_name] = adapter
        _logger.debug("プロトコルアダプター登録: %s", protocol_name)

    def get(self, protocol_name: str) -> ProtocolAdapterInterface | None:
        """アダプターを取得.

        Args:
            protocol_name: プロトコル名

        Returns:
            アダプターまたは None
        """
        return self._adapters.get(protocol_name)

    def list_protocols(self) -> list[str]:
        """登録済みプロトコル名一覧を取得.

        Returns:
            プロトコル名リスト
        """
        return list(self._adapters.keys())

    def unregister(self, protocol_name: str) -> bool:
        """アダプターを削除.

        Args:
            protocol_name: プロトコル名

        Returns:
            削除できたか
        """
        if protocol_name in self._adapters:
            del self._adapters[protocol_name]
            return True
        return False


# モジュールレベルのデフォルトレジストリインスタンス
_default_registry = ProtocolAdapterRegistry()


def get_default_protocol_adapter_registry() -> ProtocolAdapterRegistry:
    """デフォルトのプロトコルアダプターレジストリを取得.

    Returns:
        デフォルトレジストリ
    """
    return _default_registry


class ProtocolAdapter:
    """プロトコルアダプタージェネレーター.

    AgentMetadata から各プロトコルのアダプターを自動生成します。

    Example:
        >>> metadata = AgentMetadata(...)
        >>> mcp_tools = ProtocolAdapter.generate_mcp_tools(metadata)
        >>> a2a_card = ProtocolAdapter.generate_a2a_card(metadata)
        >>> emitter = ProtocolAdapter.wrap_flow_with_agui(engine, "flow-id", metadata)
    """

    @staticmethod
    def _field_to_json_schema(field: InputField | OutputField) -> dict[str, Any]:
        """InputField/OutputField を JSON Schema に変換.

        Args:
            field: 入力または出力フィールド

        Returns:
            JSON Schema 形式の辞書
        """
        schema: dict[str, Any] = {
            "type": field.type,
            "description": field.description if hasattr(field, "description") else "",
        }

        # InputField の場合の追加プロパティ
        if isinstance(field, InputField):
            if field.options:
                schema["enum"] = field.options
            if field.default is not None:
                schema["default"] = field.default
            if field.accept:
                schema["accept"] = field.accept

        # OutputField の場合の追加プロパティ
        if isinstance(field, OutputField) and field.output_schema:
            schema.update(field.output_schema)

        return schema

    @staticmethod
    def generate_mcp_tools(metadata: AgentMetadata) -> list[dict[str, Any]]:
        """MCP ツール定義を生成.

        AgentMetadata から MCP プロトコルのツール定義を生成します。

        Args:
            metadata: エージェントメタデータ

        Returns:
            MCP ツール定義のリスト

        Example:
            >>> tools = ProtocolAdapter.generate_mcp_tools(metadata)
            >>> print(tools[0]["name"])
            'process_text'
        """
        descriptor = ProtocolAdapter.build_descriptor_from_metadata(metadata)
        return ProtocolAdapter.generate_mcp_tools_from_descriptor(descriptor)

    @staticmethod
    def generate_a2a_card(metadata: AgentMetadata) -> AgentCard:
        """A2A AgentCard を生成.

        AgentMetadata から A2A プロトコルの AgentCard を生成します。

        Args:
            metadata: エージェントメタデータ

        Returns:
            AgentCard インスタンス

        Example:
            >>> card = ProtocolAdapter.generate_a2a_card(metadata)
            >>> print(card.name)
            'Text Processor'
        """
        descriptor = ProtocolAdapter.build_descriptor_from_metadata(metadata)
        return ProtocolAdapter.generate_a2a_card_from_descriptor(descriptor)

    @staticmethod
    def build_descriptor_from_metadata(metadata: AgentMetadata) -> AgentDescriptor:
        """metadata を内部 canonical descriptor に変換."""
        return descriptor_from_agent_metadata(metadata)

    @staticmethod
    def generate_mcp_tools_from_descriptor(descriptor: AgentDescriptor) -> list[dict[str, Any]]:
        """descriptor から MCP tool 定義を生成.

        レジストリに "mcp" アダプターが登録されている場合はそちらに委譲する。
        """
        # レジストリに登録済みのアダプターがあれば委譲
        registry_adapter = _default_registry.get("mcp")
        if registry_adapter is not None:
            return registry_adapter.generate_tools(descriptor)

        fallback_default = not descriptor.metadata.get("a2a_skills")
        tool_names = ProtocolAdapter._resolve_skill_names(descriptor) or [descriptor.agent_id.replace("-", "_")]
        return [
            {
                "name": tool_name.replace("-", "_"),
                "description": ProtocolAdapter._describe_skill(
                    descriptor,
                    tool_name,
                    fallback_default=fallback_default,
                ),
                "inputSchema": descriptor.input_schema,
            }
            for tool_name in tool_names
        ]

    @staticmethod
    def generate_a2a_card_from_descriptor(descriptor: AgentDescriptor) -> AgentCard:
        """descriptor から A2A card を生成.

        レジストリに "a2a" アダプターが登録されている場合はそちらに委譲する。
        """
        # レジストリに登録済みのアダプターがあれば委譲
        registry_adapter = _default_registry.get("a2a")
        if registry_adapter is not None:
            return registry_adapter.generate_card(descriptor)

        fallback_default = not descriptor.metadata.get("a2a_skills")
        skill_names = ProtocolAdapter._resolve_skill_names(descriptor)
        if not skill_names:
            skill_names = [descriptor.agent_id.replace("-", "_")]

        skills = [
            AgentSkill(
                name=skill_name.replace("-", "_"),
                description=ProtocolAdapter._describe_skill(
                    descriptor,
                    skill_name,
                    fallback_default=fallback_default,
                ),
                input_schema=descriptor.input_schema,
                output_schema=descriptor.output_schema,
            )
            for skill_name in skill_names
        ]
        metadata = dict(descriptor.metadata)
        metadata.setdefault("id", descriptor.agent_id)

        return AgentCard(
            name=descriptor.name,
            description=descriptor.description,
            version=descriptor.version,
            author=metadata.get("author"),
            skills=skills,
            capabilities=AgentCapabilities(
                streaming=descriptor.supports_streaming,
                push_notifications=False,
                state_transition_history=True,
            ),
            metadata=metadata,
        )

    @staticmethod
    def _resolve_skill_names(descriptor: AgentDescriptor) -> list[str]:
        """descriptor から公開 skill/tool 名を解決."""
        metadata_skills = descriptor.metadata.get("a2a_skills")
        if isinstance(metadata_skills, list):
            normalized = [str(item).strip() for item in metadata_skills if str(item).strip()]
            if normalized:
                return normalized

        category = descriptor.metadata.get("category")
        return [
            item
            for item in descriptor.capabilities
            if item and item != category and item not in {"specialist", "planner", "executor", "reactor", "reporter"}
        ]

    @staticmethod
    def _describe_skill(descriptor: AgentDescriptor, skill_name: str, *, fallback_default: bool) -> str:
        """skill/tool の説明を生成."""
        if fallback_default and skill_name.replace("-", "_") == descriptor.agent_id.replace("-", "_"):
            return descriptor.description
        return f"{descriptor.name} - {skill_name}"

    @staticmethod
    def wrap_flow_with_agui(
        engine: AgentFlowEngine,
        _flow_id: str,
        _metadata: AgentMetadata,
    ) -> AGUIEventEmitter:
        """AsyncFlow に AG-UI イベントエミッターを追加.

        AgentFlowEngine に AGUIEventEmitter を作成してアタッチします。

        Args:
            engine: AgentFlowEngine インスタンス
            _flow_id: フロー ID (未使用)
            _metadata: エージェントメタデータ (未使用)

        Returns:
            AGUIEventEmitter インスタンス

        Example:
            >>> engine = AgentFlowEngine()
            >>> emitter = ProtocolAdapter.wrap_flow_with_agui(engine, "flow-1", metadata)
            >>> async for event in emitter.stream_events():
            ...     print(event.event_type)
        """
        # Lazy import to avoid circular dependency
        from kernel.protocols.agui_emitter import AGUIEventEmitter

        return AGUIEventEmitter(engine)

"""プロトコルアダプタージェネレーター.

このモジュールは AgentMetadata から MCP/A2A/AG-UI プロトコルアダプターを
自動生成するファクトリークラスを提供します。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from kernel.agents.contracts import AgentDescriptor, descriptor_from_agent_metadata
from kernel.core.metadata import AgentMetadata, InputField, OutputField
from kernel.protocols.a2a_card import AgentCard, AgentSkill


if TYPE_CHECKING:
    from kernel.core.engine import AgentFlowEngine
    from kernel.protocols.agui_emitter import AGUIEventEmitter


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
        """descriptor から MCP tool 定義を生成."""
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
        """descriptor から A2A card を生成."""
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
            capabilities={
                "streaming": descriptor.supports_streaming,
                "push_notifications": False,
                "state_transition_history": True,
            },
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
            if item
            and item != category
            and item not in {"specialist", "planner", "executor", "reactor", "reporter"}
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

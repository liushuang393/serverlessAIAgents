"""プロトコルアダプタージェネレーター.

このモジュールは AgentMetadata から MCP/A2A/AG-UI プロトコルアダプターを
自動生成するファクトリークラスを提供します。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from agentflow.core.metadata import AgentMetadata, InputField, OutputField
from agentflow.protocols.a2a_card import AgentCard, AgentSkill


if TYPE_CHECKING:
    from agentflow.core.engine import AgentFlowEngine
    from agentflow.protocols.agui_emitter import AGUIEventEmitter


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
        tools: list[dict[str, Any]] = []

        # A2A スキルがある場合、それを MCP ツールとして公開
        if metadata.protocols.a2a and metadata.protocols.a2a.skills:
            for skill_name in metadata.protocols.a2a.skills:
                # 入力スキーマを生成
                input_schema: dict[str, Any] = {
                    "type": "object",
                    "properties": {},
                    "required": [],
                }

                for input_field in metadata.interfaces.inputs:
                    input_schema["properties"][input_field.name] = (
                        ProtocolAdapter._field_to_json_schema(input_field)
                    )
                    if input_field.required:
                        input_schema["required"].append(input_field.name)

                # ツール定義を作成
                tool = {
                    "name": skill_name,
                    "description": f"{metadata.meta.name} - {skill_name}",
                    "inputSchema": input_schema,
                }

                tools.append(tool)

        # メタデータにツールが定義されていない場合、デフォルトツールを作成
        if not tools:
            input_schema = {
                "type": "object",
                "properties": {},
                "required": [],
            }

            for input_field in metadata.interfaces.inputs:
                input_schema["properties"][input_field.name] = (
                    ProtocolAdapter._field_to_json_schema(input_field)
                )
                if input_field.required:
                    input_schema["required"].append(input_field.name)

            tool = {
                "name": metadata.meta.id.replace("-", "_"),
                "description": metadata.meta.description,
                "inputSchema": input_schema,
            }

            tools.append(tool)

        return tools

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
        # 入力スキーマを生成
        input_schema: dict[str, Any] = {
            "type": "object",
            "properties": {},
            "required": [],
        }

        for input_field in metadata.interfaces.inputs:
            input_schema["properties"][input_field.name] = ProtocolAdapter._field_to_json_schema(
                input_field
            )
            if input_field.required:
                input_schema["required"].append(input_field.name)

        # 出力スキーマを生成
        output_schema: dict[str, Any] = {
            "type": "object",
            "properties": {},
        }

        for output_field in metadata.interfaces.outputs:
            output_schema["properties"][output_field.name] = ProtocolAdapter._field_to_json_schema(
                output_field
            )

        # スキルリストを生成
        skills: list[AgentSkill] = []

        if metadata.protocols.a2a and metadata.protocols.a2a.skills:
            for skill_name in metadata.protocols.a2a.skills:
                skill = AgentSkill(
                    name=skill_name,
                    description=f"{metadata.meta.name} - {skill_name}",
                    input_schema=input_schema,
                    output_schema=output_schema,
                )
                skills.append(skill)
        else:
            # デフォルトスキルを作成
            skill = AgentSkill(
                name=metadata.meta.id.replace("-", "_"),
                description=metadata.meta.description,
                input_schema=input_schema,
                output_schema=output_schema,
            )
            skills.append(skill)

        # AgentCard を作成
        return AgentCard(
            name=metadata.meta.name,
            description=metadata.meta.description,
            version=metadata.meta.version,
            author=metadata.meta.author,
            skills=skills,
            metadata={
                "id": metadata.meta.id,
                "icon": metadata.meta.icon,
                "category": metadata.meta.category,
            },
        )

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
        from agentflow.protocols.agui_emitter import AGUIEventEmitter

        return AGUIEventEmitter(engine)

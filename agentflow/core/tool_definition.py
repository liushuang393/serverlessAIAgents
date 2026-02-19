"""統一ツール定義モデル.

すべてのツールソース（builtin、MCP、skill、dynamic）に対して
統一されたツール定義を提供するモジュール。

設計原則:
- 高度な抽象化: すべてのツールソースを統一インターフェースで表現
- 低結合: ソース固有の詳細はメタデータに格納
- 拡張性: 新しいソースタイプの追加が容易

使用例:
    >>> # MCPサーバーからツール定義を作成
    >>> tool = ToolDefinition.from_mcp(mcp_tool, server_name="filesystem")
    >>>
    >>> # Skillからツール定義を作成
    >>> tool = ToolDefinition.from_skill(skill_data)
    >>>
    >>> # MCP形式に変換（LLMに渡す用）
    >>> mcp_format = tool.to_mcp()
"""

from __future__ import annotations

from enum import Enum
from typing import Any

from pydantic import BaseModel, Field, field_validator


class ToolSource(str, Enum):
    """ツールのソースタイプ.

    ツールがどこから来たかを識別する列挙型。
    """

    BUILTIN = "builtin"  # @tool デコレータで定義された関数
    MCP = "mcp"  # MCPサーバーが提供するツール
    SKILL = "skill"  # Skills エンジンのスキル
    DYNAMIC = "dynamic"  # 実行時に動的生成されたツール


class ToolDefinition(BaseModel):
    """統一ツール定義モデル.

    すべてのソースからのツールに対して一貫したインターフェースを提供:
    - @tool デコレータで定義された関数 (builtin)
    - MCPサーバー (mcp)
    - Skills エンジン (skill)
    - 実行時生成ツール (dynamic)

    Attributes:
        uri: ユニークなツールURI (tool://{source}/{name})
        name: ツール名
        description: ツールの説明
        source: ツールのソースタイプ
        input_schema: 入力パラメータのJSONスキーマ
        output_schema: 出力のJSONスキーマ（オプション）
        metadata: ソース固有の追加メタデータ
    """

    uri: str = Field(..., description="ユニークなツールURI: tool://{source}/{name}")
    name: str = Field(..., description="ツール名")
    description: str = Field(..., description="ツールの説明")
    source: ToolSource = Field(..., description="ツールのソースタイプ")
    input_schema: dict[str, Any] = Field(
        default_factory=lambda: {"type": "object", "properties": {}},
        description="入力パラメータのJSONスキーマ",
    )
    output_schema: dict[str, Any] | None = Field(
        default=None,
        description="出力のJSONスキーマ（オプション）",
    )
    metadata: dict[str, Any] = Field(
        default_factory=dict,
        description="ソース固有の追加メタデータ",
    )

    @field_validator("uri")
    @classmethod
    def validate_uri(cls, v: str) -> str:
        """URIが tool:// スキームに従っているか検証.

        Args:
            v: 検証するURI文字列

        Returns:
            検証済みURI

        Raises:
            ValueError: URIが無効な場合
        """
        if not v.startswith("tool://"):
            msg = f"URIは 'tool://' で始まる必要があります。受信: {v}"
            raise ValueError(msg)
        return v

    @classmethod
    def from_mcp(
        cls,
        mcp_tool: dict[str, Any],
        server_name: str,
    ) -> ToolDefinition:
        """MCP ツール形式から ToolDefinition を作成.

        Args:
            mcp_tool: MCPツール定義（name、description、inputSchema を含む）
            server_name: ツールを提供するMCPサーバー名

        Returns:
            ToolDefinition インスタンス
        """
        name = mcp_tool["name"]
        return cls(
            uri=f"tool://mcp/{server_name}/{name}",
            name=name,
            description=mcp_tool.get("description", ""),
            source=ToolSource.MCP,
            input_schema=mcp_tool.get("inputSchema", {"type": "object", "properties": {}}),
            metadata={"server": server_name, "original": mcp_tool},
        )

    @classmethod
    def from_skill(cls, skill_data: dict[str, Any] | Any) -> ToolDefinition:
        """Skill 形式から ToolDefinition を作成.

        Args:
            skill_data: スキル定義（dict または Skill オブジェクト）

        Returns:
            ToolDefinition インスタンス
        """
        # Skill オブジェクトの場合は属性から取得
        if hasattr(skill_data, "metadata"):
            # agentflow.skills.base.Skill オブジェクト
            metadata = skill_data.metadata
            name = metadata.name
            description = metadata.description

            # input_schema があれば使用、なければデフォルト
            input_schema = getattr(metadata, "input_schema", None) or {
                "type": "object",
                "properties": {"query": {"type": "string", "description": "スキルへの入力"}},
            }

            # メタデータを収集
            extra_metadata = {
                "version": getattr(metadata, "version", None),
                "author": getattr(metadata, "author", None),
                "triggers": getattr(metadata, "triggers", []),
                "tags": getattr(metadata, "tags", []),
                "requirements": getattr(metadata, "requirements", []),
            }
        else:
            # dict の場合
            name = skill_data.get("name", "unknown")
            description = skill_data.get("description", "")
            input_schema = skill_data.get("input_schema") or {
                "type": "object",
                "properties": skill_data.get("parameters", {}),
            }
            extra_metadata = {
                k: v
                for k, v in skill_data.items()
                if k not in ("name", "description", "input_schema", "parameters")
            }

        return cls(
            uri=f"tool://skill/{name}",
            name=name,
            description=description,
            source=ToolSource.SKILL,
            input_schema=input_schema if input_schema else {},
            metadata=extra_metadata,
        )

    @classmethod
    def from_builtin(
        cls,
        name: str,
        description: str,
        input_schema: dict[str, Any],
    ) -> ToolDefinition:
        """@tool デコレータで定義された関数から ToolDefinition を作成.

        Args:
            name: 関数名
            description: 関数の docstring
            input_schema: 型ヒントから推論されたJSONスキーマ

        Returns:
            ToolDefinition インスタンス
        """
        return cls(
            uri=f"tool://builtin/{name}",
            name=name,
            description=description,
            source=ToolSource.BUILTIN,
            input_schema=input_schema,
        )

    def to_mcp(self) -> dict[str, Any]:
        """MCP ツール形式に変換.

        LLMに渡すためのMCP互換形式に変換。

        Returns:
            MCP ツール定義と互換性のある辞書
        """
        return {
            "name": self.name,
            "description": self.description,
            "inputSchema": self.input_schema,
        }

    def matches(self, query: str) -> float:
        """クエリに対する関連性スコアを計算.

        ツール名と説明に対してクエリをマッチングし、
        0.0〜1.0のスコアを返す。

        Args:
            query: 検索クエリ

        Returns:
            0.0〜1.0のスコア
        """
        query_lower = query.lower()
        score = 0.0

        # 名前にマッチ
        if query_lower in self.name.lower():
            score += 0.5

        # 説明にマッチ
        if query_lower in self.description.lower():
            score += 0.3

        # 入力スキーマのプロパティ名にマッチ
        props = self.input_schema.get("properties", {})
        for prop_name in props:
            if query_lower in prop_name.lower():
                score += 0.1
                break

        return min(score, 1.0)


__all__ = [
    "ToolDefinition",
    "ToolSource",
]

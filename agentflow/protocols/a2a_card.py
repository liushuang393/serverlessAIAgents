"""A2A AgentCard モデル.

このモジュールは A2A プロトコルの AgentCard を管理するための Pydantic モデルを提供します。
"""

from typing import Any

from pydantic import BaseModel, Field


class AgentSkill(BaseModel):
    """エージェントのスキル定義.

    Attributes:
        name: スキル名
        description: スキルの説明
        input_schema: 入力スキーマ (JSON Schema)
        output_schema: 出力スキーマ (JSON Schema)
    """

    name: str = Field(..., description="スキル名")
    description: str = Field(..., description="スキルの説明")
    input_schema: dict[str, Any] = Field(default_factory=dict, description="入力スキーマ")
    output_schema: dict[str, Any] = Field(default_factory=dict, description="出力スキーマ")


class AgentCard(BaseModel):
    """A2A AgentCard.

    Attributes:
        name: エージェント名
        description: エージェントの説明
        version: エージェントのバージョン
        author: エージェントの作成者
        skills: エージェントのスキルリスト
        metadata: 追加のメタデータ
    """

    name: str = Field(..., description="エージェント名")
    description: str = Field(..., description="エージェントの説明")
    version: str = Field(default="1.0.0", description="エージェントのバージョン")
    author: str | None = Field(default=None, description="エージェントの作成者")
    skills: list[AgentSkill] = Field(default_factory=list, description="エージェントのスキルリスト")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加のメタデータ")

    @classmethod
    def from_yaml(cls, yaml_data: dict[str, Any]) -> "AgentCard":
        """YAML データから AgentCard を作成.

        Args:
            yaml_data: agent.yaml から読み込んだデータ

        Returns:
            AgentCard インスタンス
        """
        skills = [
            AgentSkill(
                name=skill.get("name", ""),
                description=skill.get("description", ""),
                input_schema=skill.get("input_schema", {}),
                output_schema=skill.get("output_schema", {}),
            )
            for skill in yaml_data.get("skills", [])
        ]

        return cls(
            name=yaml_data.get("name", ""),
            description=yaml_data.get("description", ""),
            version=yaml_data.get("version", "1.0.0"),
            author=yaml_data.get("author"),
            skills=skills,
            metadata=yaml_data.get("metadata", {}),
        )

    def to_a2a_format(self) -> dict[str, Any]:
        """A2A プロトコル形式に変換.

        Returns:
            A2A AgentCard 形式の辞書
        """
        return {
            "name": self.name,
            "description": self.description,
            "version": self.version,
            "author": self.author,
            "skills": [
                {
                    "name": skill.name,
                    "description": skill.description,
                    "inputSchema": skill.input_schema,
                    "outputSchema": skill.output_schema,
                }
                for skill in self.skills
            ],
            "metadata": self.metadata,
        }

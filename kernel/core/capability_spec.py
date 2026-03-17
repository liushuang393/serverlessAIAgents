"""Agent能力仕様モデル — kernel 層.

Agent の能力を宣言し、タスク要件とのマッチングを行う。
agentflow/core/capability_spec.py から移行。
"""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field


class LLMRequirements(BaseModel):
    """LLM設定要件.

    Attributes:
        model: 推奨モデル名
        temperature: 温度パラメータ
        max_tokens: 最大トークン数
        additional: 追加設定
    """

    model: str | None = Field(default=None, description="推奨モデル名")
    temperature: float = Field(default=0.7, ge=0.0, le=2.0, description="温度パラメータ")
    max_tokens: int = Field(default=2000, gt=0, description="最大トークン数")
    additional: dict[str, Any] = Field(default_factory=dict, description="追加設定")


class CapabilityRequirement(BaseModel):
    """能力要件.

    Attributes:
        description: タスク説明
        required_tags: 必須タグ
        required_tools: 必須ツールURI
        min_confidence: 最小信頼度
        preferred_model: 推奨モデル
    """

    description: str = Field(..., description="タスク説明")
    required_tags: list[str] = Field(default_factory=list, description="必須タグ")
    required_tools: list[str] = Field(default_factory=list, description="必須ツールURI")
    min_confidence: float = Field(default=0.5, ge=0.0, le=1.0, description="最小信頼度")
    preferred_model: str | None = Field(default=None, description="推奨モデル")


class AgentCapabilitySpec(BaseModel):
    """Agent能力仕様.

    Attributes:
        id: ユニーク能力ID
        name: 人間可読な名前
        description: 能力の説明
        input_schema: 入力形式
        output_schema: 出力形式
        required_tools: 必要ツールURI
        llm_requirements: LLM設定要件
        tags: 発見用タグ
        confidence: 信頼度スコア
        metadata: 追加メタデータ
    """

    id: str = Field(..., description="ユニーク能力ID")
    name: str = Field(..., description="人間可読な名前")
    description: str = Field(..., description="能力の説明")
    input_schema: dict[str, Any] = Field(
        default_factory=lambda: {"type": "object", "properties": {}},
        description="入力形式",
    )
    output_schema: dict[str, Any] = Field(
        default_factory=lambda: {"type": "object", "properties": {}},
        description="出力形式",
    )
    required_tools: list[str] = Field(default_factory=list, description="必要ツールURI")
    llm_requirements: LLMRequirements = Field(default_factory=LLMRequirements, description="LLM設定要件")
    tags: list[str] = Field(default_factory=list, description="発見用タグ")
    confidence: float = Field(default=1.0, ge=0.0, le=1.0, description="信頼度スコア")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタデータ")

    def matches(self, requirement: CapabilityRequirement) -> float:
        """タスク要件に対するマッチスコアを計算.

        Args:
            requirement: タスク要件

        Returns:
            0.0〜1.0 のスコア
        """
        score = 0.0
        weights = {"description": 0.3, "tags": 0.4, "tools": 0.3}

        req_words = set(requirement.description.lower().split())
        cap_words = set(self.description.lower().split())
        cap_words.update(self.name.lower().split())
        overlap = len(req_words & cap_words)
        if req_words:
            score += weights["description"] * (overlap / len(req_words))

        if requirement.required_tags:
            matched_tags = sum(1 for t in requirement.required_tags if t in self.tags)
            score += weights["tags"] * (matched_tags / len(requirement.required_tags))
        else:
            score += weights["tags"] * 0.5

        if requirement.required_tools:
            matched_tools = sum(1 for t in requirement.required_tools if t in self.required_tools)
            score += weights["tools"] * (matched_tools / len(requirement.required_tools))
        else:
            score += weights["tools"] * 0.5

        return score * self.confidence

    @classmethod
    def from_agent_config(cls, config: dict[str, Any]) -> AgentCapabilitySpec:
        """Agent設定から能力仕様を作成.

        Args:
            config: Agent設定

        Returns:
            AgentCapabilitySpec インスタンス
        """
        name = config.get("name", "unknown")
        skills = config.get("skills", [])
        tools = config.get("tools", [])
        return cls(
            id=name.lower().replace(" ", ""),
            name=name,
            description=config.get("description", ""),
            required_tools=tools,
            tags=skills,
            metadata={"source": "agent_config", "original": config},
        )


__all__ = ["AgentCapabilitySpec", "CapabilityRequirement", "LLMRequirements"]


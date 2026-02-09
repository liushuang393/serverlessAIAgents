"""Agent能力仕様モデル.

Agentの能力を正式に宣言し、タスク要件とのマッチングを行うモジュール。

注意:
このモジュールは agentflow.patterns.adaptive_coordinator.AgentCapability とは
異なる目的で設計されている。adaptive_coordinator は実行時のパフォーマンス追跡に
フォーカスし、このモジュールはツール/LLM要件の宣言とタスクマッチングにフォーカス。

設計原則:
- 高度な抽象化: Agent能力を統一的に表現
- 低結合: 具体的なAgent実装に依存しない
- 拡張性: 新しい能力タイプの追加が容易

使用例:
    >>> # Agent能力を宣言
    >>> cap = AgentCapabilitySpec(
    ...     id="pdf_analysis",
    ...     name="PDF Analysis",
    ...     description="PDF文書を分析",
    ...     required_tools=["tool://mcp/filesystem/read_file"],
    ...     tags=["pdf", "analysis"],
    ... )
    >>>
    >>> # タスク要件とのマッチング
    >>> req = CapabilityRequirement(description="PDFを分析", required_tags=["pdf"])
    >>> score = cap.matches(req)
"""
from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field


class LLMRequirements(BaseModel):
    """LLM設定要件.

    能力が必要とするLLMの設定を宣言。

    Attributes:
        model: 推奨モデル名（Noneの場合は自動選択）
        temperature: 温度パラメータ
        max_tokens: 最大トークン数
        additional: 追加設定
    """

    model: str | None = Field(
        default=None,
        description="推奨モデル名（Noneの場合は自動選択）",
    )
    temperature: float = Field(
        default=0.7,
        ge=0.0,
        le=2.0,
        description="温度パラメータ",
    )
    max_tokens: int = Field(
        default=2000,
        gt=0,
        description="最大トークン数",
    )
    additional: dict[str, Any] = Field(
        default_factory=dict,
        description="追加設定",
    )


class CapabilityRequirement(BaseModel):
    """能力要件.

    タスクが必要とする能力を宣言するモデル。
    AgentCapabilitySpecとのマッチングに使用。

    Attributes:
        description: タスク説明
        required_tags: 必須タグ
        required_tools: 必須ツールURI
        min_confidence: 最小信頼度
        preferred_model: 推奨モデル
    """

    description: str = Field(..., description="タスク説明")
    required_tags: list[str] = Field(
        default_factory=list,
        description="必須タグ",
    )
    required_tools: list[str] = Field(
        default_factory=list,
        description="必須ツールURI",
    )
    min_confidence: float = Field(
        default=0.5,
        ge=0.0,
        le=1.0,
        description="最小信頼度",
    )
    preferred_model: str | None = Field(
        default=None,
        description="推奨モデル",
    )


class AgentCapabilitySpec(BaseModel):
    """Agent能力仕様.

    Agentの能力を正式に宣言するモデル。

    用途:
    - 能力ベースのAgent発見
    - タスク要件とのマッチング
    - 自動Agent生成の要件定義

    Attributes:
        id: ユニーク能力ID（小文字、バージョン付き推奨）
        name: 人間可読な名前
        description: 能力の説明
        input_schema: 期待される入力形式
        output_schema: 期待される出力形式
        required_tools: この能力が必要とするツールURI
        llm_requirements: LLM設定要件
        tags: 発見用タグ
        confidence: 信頼度スコア（学習された能力は低い）
        metadata: 追加メタデータ
    """

    id: str = Field(..., description="ユニーク能力ID（小文字、バージョン付き）")
    name: str = Field(..., description="人間可読な名前")
    description: str = Field(..., description="この能力が何をするか")
    input_schema: dict[str, Any] = Field(
        default_factory=lambda: {"type": "object", "properties": {}},
        description="期待される入力形式",
    )
    output_schema: dict[str, Any] = Field(
        default_factory=lambda: {"type": "object", "properties": {}},
        description="期待される出力形式",
    )
    required_tools: list[str] = Field(
        default_factory=list,
        description="この能力が必要とするツールURI",
    )
    llm_requirements: LLMRequirements = Field(
        default_factory=LLMRequirements,
        description="LLM設定要件",
    )
    tags: list[str] = Field(
        default_factory=list,
        description="発見用タグ",
    )
    confidence: float = Field(
        default=1.0,
        ge=0.0,
        le=1.0,
        description="信頼度スコア（学習された能力は低い）",
    )
    metadata: dict[str, Any] = Field(
        default_factory=dict,
        description="追加メタデータ",
    )

    def matches(self, requirement: CapabilityRequirement) -> float:
        """タスク要件に対するマッチスコアを計算.

        説明、タグ、ツールの一致度を考慮してスコアを計算。

        Args:
            requirement: マッチング対象のタスク要件

        Returns:
            0.0〜1.0のスコア
        """
        score = 0.0
        weights = {"description": 0.3, "tags": 0.4, "tools": 0.3}

        # 説明のマッチング
        req_words = set(requirement.description.lower().split())
        cap_words = set(self.description.lower().split())
        cap_words.update(self.name.lower().split())
        overlap = len(req_words & cap_words)
        if req_words:
            score += weights["description"] * (overlap / len(req_words))

        # タグのマッチング
        if requirement.required_tags:
            matched_tags = sum(1 for t in requirement.required_tags if t in self.tags)
            score += weights["tags"] * (matched_tags / len(requirement.required_tags))
        else:
            score += weights["tags"] * 0.5  # タグ要件がない場合は中立

        # ツールのマッチング
        if requirement.required_tools:
            matched_tools = sum(
                1 for t in requirement.required_tools if t in self.required_tools
            )
            score += weights["tools"] * (matched_tools / len(requirement.required_tools))
        else:
            score += weights["tools"] * 0.5  # ツール要件がない場合は中立

        # 信頼度を適用
        return score * self.confidence

    @classmethod
    def from_agent_config(cls, config: dict[str, Any]) -> AgentCapabilitySpec:
        """Agent設定から能力仕様を作成.

        @agent デコレータの設定から能力仕様を生成。

        Args:
            config: Agent設定（name、description、skills、tools を含む）

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


__all__ = [
    "AgentCapabilitySpec",
    "CapabilityRequirement",
    "LLMRequirements",
]

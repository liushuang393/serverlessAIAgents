"""Agentブループリント - 宣言式Agent定義.

YAML/JSON形式でAgentを宣言的に定義し、動的にインスタンス化。

設計原則:
- 宣言的: コードを書かずにAgent定義
- 検証可能: ブループリントの妥当性を事前検証
- 拡張可能: カスタムコンポーネントの追加

使用例:
    >>> from agentflow.core.blueprint import AgentBlueprint
    >>>
    >>> # YAMLからロード
    >>> blueprint = AgentBlueprint.from_yaml("agent.yaml")
    >>>
    >>> # Agentをインスタンス化
    >>> agent = await blueprint.to_agent(llm_client=my_llm)
    >>> result = await agent.run({"input": "..."})
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import yaml
from pydantic import BaseModel, Field, field_validator


class SkillConfig(BaseModel):
    """スキル設定.

    Attributes:
        name: スキル名
        enabled: 有効かどうか
        priority: 優先度
        config: 追加設定
    """

    name: str = Field(default="", description="スキル名")
    enabled: bool = Field(default=True)
    priority: int = Field(default=0, ge=0)
    config: dict[str, Any] = Field(default_factory=dict)


class ToolConfig(BaseModel):
    """ツール設定.

    Attributes:
        uri: ツールURI
        enabled: 有効かどうか
        constraints: 制約
    """

    uri: str = Field(default="", description="ツールURI")
    enabled: bool = Field(default=True)
    constraints: dict[str, Any] = Field(default_factory=dict)


class MemoryConfig(BaseModel):
    """メモリ設定.

    Attributes:
        type: メモリ種別
        enabled: 有効かどうか
        distillation: 蒸馏を有効化
        max_entries: 最大エントリ数
    """

    type: str = Field(default="standard", description="メモリ種別")
    enabled: bool = Field(default=True)
    distillation: bool = Field(default=False)
    max_entries: int = Field(default=1000, ge=0)


class SafetyConfig(BaseModel):
    """安全設定.

    Attributes:
        hallucination_check: 幻覚チェック
        pii_sanitization: PII脱敏
        injection_protection: 注入攻撃防護
        strict_mode: 厳格モード
    """

    hallucination_check: bool = Field(default=True)
    pii_sanitization: bool = Field(default=True)
    injection_protection: bool = Field(default=True)
    strict_mode: bool = Field(default=False)


class ConstraintsConfig(BaseModel):
    """制約設定.

    Attributes:
        allowed_tools: 許可ツールリスト
        blocked_operations: ブロック操作
        max_iterations: 最大イテレーション
        max_concurrent_requests: 最大同時リクエスト
        allowed_domains: 許可ドメイン
    """

    allowed_tools: list[str] = Field(default_factory=list)
    blocked_operations: list[str] = Field(default_factory=list)
    max_iterations: int = Field(default=10, ge=1)
    max_concurrent_requests: int = Field(default=5, ge=1)
    allowed_domains: list[str] = Field(default_factory=list)
    timeout_seconds: float = Field(default=60.0, ge=1.0)


class AgentBlueprintModel(BaseModel):
    """Agentブループリントモデル.

    YAMLファイルの構造を定義。

    Attributes:
        name: Agent名
        description: 説明
        version: バージョン
        skills: スキル設定
        tools: ツール設定
        memory: メモリ設定
        safety: 安全設定
        constraints: 制約設定
        metadata: メタデータ
    """

    name: str = Field(default="", description="Agent名")
    description: str = Field(default="", description="説明")
    version: str = Field(default="1.0.0")
    skills: list[SkillConfig | str] = Field(default_factory=list)
    tools: list[ToolConfig | str] = Field(default_factory=list)
    memory: MemoryConfig = Field(default_factory=MemoryConfig)
    safety: SafetyConfig = Field(default_factory=SafetyConfig)
    constraints: ConstraintsConfig = Field(default_factory=ConstraintsConfig)
    metadata: dict[str, Any] = Field(default_factory=dict)

    @field_validator("skills", mode="before")
    @classmethod
    def normalize_skills(cls, v: Any) -> list[SkillConfig | str]:
        """スキル設定を正規化."""
        if not v:
            return []
        result: list[SkillConfig | str] = []
        for item in v:
            if isinstance(item, str):
                result.append(item)
            elif isinstance(item, dict):
                result.append(SkillConfig(**item))
            else:
                result.append(item)
        return result

    @field_validator("tools", mode="before")
    @classmethod
    def normalize_tools(cls, v: Any) -> list[ToolConfig | str]:
        """ツール設定を正規化."""
        if not v:
            return []
        result: list[ToolConfig | str] = []
        for item in v:
            if isinstance(item, str):
                result.append(item)
            elif isinstance(item, dict):
                result.append(ToolConfig(**item))
            else:
                result.append(item)
        return result


@dataclass
class ValidationResult:
    """検証結果.

    Attributes:
        valid: 有効かどうか
        errors: エラーリスト
        warnings: 警告リスト
    """

    valid: bool = True
    errors: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)


class AgentBlueprint:
    """Agentブループリント.

    YAML/JSON形式でAgentを宣言的に定義。

    主な機能:
    - YAMLファイルからロード
    - 検証
    - Agentのインスタンス化
    - ブループリントの保存

    Example:
        >>> # YAMLからロード
        >>> blueprint = AgentBlueprint.from_yaml("agent.yaml")
        >>>
        >>> # 検証
        >>> result = blueprint.validate()
        >>> if not result.valid:
        ...     print(result.errors)
        >>>
        >>> # Agentをインスタンス化
        >>> agent = await blueprint.to_agent(llm_client=my_llm)
    """

    def __init__(self, model: AgentBlueprintModel) -> None:
        """初期化.

        Args:
            model: ブループリントモデル
        """
        self._model = model
        self._logger = logging.getLogger(__name__)

    @classmethod
    def from_yaml(cls, path: str | Path) -> AgentBlueprint:
        """YAMLファイルからロード.

        Args:
            path: YAMLファイルパス

        Returns:
            AgentBlueprint
        """
        path = Path(path)
        if not path.exists():
            msg = f"ブループリントファイルが見つかりません: {path}"
            raise FileNotFoundError(msg)

        with open(path, encoding="utf-8") as f:
            data = yaml.safe_load(f)

        model = AgentBlueprintModel(**data)
        return cls(model)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> AgentBlueprint:
        """辞書からロード.

        Args:
            data: ブループリントデータ

        Returns:
            AgentBlueprint
        """
        model = AgentBlueprintModel(**data)
        return cls(model)

    @classmethod
    def from_json(cls, json_string: str) -> AgentBlueprint:
        """JSON文字列からロード.

        Args:
            json_string: JSON文字列

        Returns:
            AgentBlueprint
        """
        import json

        data = json.loads(json_string)
        return cls.from_dict(data)

    def validate(self) -> ValidationResult:
        """ブループリントを検証.

        Returns:
            ValidationResult
        """
        result = ValidationResult()

        # 名前チェック
        if not self._model.name:
            result.errors.append("Agent名が指定されていません")
            result.valid = False

        # スキルチェック
        for skill in self._model.skills:
            skill_name = skill if isinstance(skill, str) else skill.name
            if not skill_name:
                result.warnings.append("空のスキル名があります")

        # ツールチェック
        for tool in self._model.tools:
            tool_uri = tool if isinstance(tool, str) else tool.uri
            if not tool_uri:
                result.warnings.append("空のツールURIがあります")

        # 制約チェック
        if self._model.constraints.max_iterations <= 0:
            result.errors.append("max_iterationsは1以上である必要があります")
            result.valid = False

        return result

    async def to_agent(
        self,
        llm_client: Any = None,
        tool_provider: Any = None,
    ) -> Any:
        """ブループリントからAgentをインスタンス化.

        Args:
            llm_client: LLMクライアント
            tool_provider: ツールプロバイダー

        Returns:
            Agentインスタンス
        """
        from agentflow.core.agent_block import AgentBlock

        # 動的Agentクラスを生成
        blueprint = self

        class DynamicAgent(AgentBlock):
            """動的生成Agent."""

            def __init__(self) -> None:
                super().__init__()
                self._blueprint = blueprint
                self._llm = llm_client
                self._tool_provider = tool_provider

            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                """実行."""
                # 制約を適用

                # ツールを呼び出し
                results: list[dict[str, Any]] = []

                for tool in self._blueprint._model.tools:
                    tool_uri = tool if isinstance(tool, str) else tool.uri
                    if not tool_uri:
                        continue

                    if self._tool_provider:
                        result = await self._tool_provider.call(tool_uri, input_data)
                        results.append(result.to_dict() if hasattr(result, "to_dict") else {"result": result})

                return {
                    "agent_name": self._blueprint._model.name,
                    "results": results,
                    "input": input_data,
                }

        return DynamicAgent()

    def to_yaml(self, path: str | Path) -> None:
        """YAMLファイルに保存.

        Args:
            path: 保存先パス
        """
        path = Path(path)
        data = self._model.model_dump()

        with open(path, "w", encoding="utf-8") as f:
            yaml.dump(data, f, allow_unicode=True, default_flow_style=False)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換.

        Returns:
            ブループリントデータ
        """
        return self._model.model_dump()

    @property
    def name(self) -> str:
        """Agent名."""
        return self._model.name

    @property
    def description(self) -> str:
        """説明."""
        return self._model.description

    @property
    def version(self) -> str:
        """バージョン."""
        return self._model.version

    @property
    def skills(self) -> list[SkillConfig | str]:
        """スキル設定."""
        return self._model.skills

    @property
    def tools(self) -> list[ToolConfig | str]:
        """ツール設定."""
        return self._model.tools

    @property
    def memory(self) -> MemoryConfig:
        """メモリ設定."""
        return self._model.memory

    @property
    def safety(self) -> SafetyConfig:
        """安全設定."""
        return self._model.safety

    @property
    def constraints(self) -> ConstraintsConfig:
        """制約設定."""
        return self._model.constraints

    def get_skill_names(self) -> list[str]:
        """スキル名リストを取得.

        Returns:
            スキル名リスト
        """
        return [skill if isinstance(skill, str) else skill.name for skill in self._model.skills]

    def get_tool_uris(self) -> list[str]:
        """ツールURIリストを取得.

        Returns:
            ツールURIリスト
        """
        return [tool if isinstance(tool, str) else tool.uri for tool in self._model.tools]

    def __repr__(self) -> str:
        """文字列表現."""
        return f"AgentBlueprint(name={self.name}, version={self.version})"


# エクスポート
__all__ = [
    "AgentBlueprint",
    "AgentBlueprintModel",
    "ConstraintsConfig",
    "MemoryConfig",
    "SafetyConfig",
    "SkillConfig",
    "ToolConfig",
    "ValidationResult",
]

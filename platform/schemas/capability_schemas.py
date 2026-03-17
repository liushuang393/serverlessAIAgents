"""Capability ontology schemas for Platform.

用語定義:
  - domain   : 能力の最上位分類 (knowledge / reasoning / interaction / ...)
  - task     : domain 内のアクション種別 (retrieval / analysis / notification / ...)
  - qualifier: task をさらに限定する成果物や手法 (rag / trend_detection / email / ...)

CapabilitySpec を使うと app_config.json で能力を 3 層構造で宣言できる。
レガシーのフラット文字列（"rag" / "analysis"）との後方互換を保つ。
"""

from __future__ import annotations

from pydantic import BaseModel, Field, field_validator


CAPABILITY_DOMAINS: tuple[str, ...] = (
    "knowledge",
    "reasoning",
    "interaction",
    "integration",
    "operations",
    "governance",
    "media",
)
"""Capability ontology domains (v1)."""


class CapabilitySpec(BaseModel):
    """3 層構造の能力宣言（app_config.json で構造体として記述可能）.

    レガシーのフラット文字列と共存でき、後方互換性を保つ。

    Attributes:
        domain  : 能力ドメイン（knowledge / reasoning / interaction 等）
        actions : ドメイン内で実行できるアクション（task 相当）リスト
        artifacts: 各アクションが生成・処理する成果物（qualifier 相当）リスト。
                   省略時はアクションのみで canonical ID を構築する。

    Examples:
        # シンプル（qualifier なし）
        {"domain": "knowledge", "actions": ["retrieval", "faq"]}
        → canonical IDs: knowledge.retrieval, knowledge.faq

        # 成果物付き（qualifier あり）
        {"domain": "knowledge", "actions": ["retrieval"], "artifacts": ["rag_answer", "summary"]}
        → canonical IDs: knowledge.retrieval.rag_answer, knowledge.retrieval.summary
    """

    domain: str = Field(..., min_length=1, description="能力ドメイン")
    actions: list[str] = Field(..., min_length=1, description="アクション（task）リスト")
    artifacts: list[str] = Field(default_factory=list, description="成果物（qualifier）リスト")

    @field_validator("domain", mode="before")
    @classmethod
    def normalize_domain(cls, v: str) -> str:
        """ドメイン文字列を小文字・trim 正規化する."""
        return str(v).strip().lower()

    @field_validator("actions", "artifacts", mode="before")
    @classmethod
    def normalize_string_list(cls, v: list[str]) -> list[str]:
        """文字列リストを小文字・trim 正規化し、空要素を除去する."""
        if not isinstance(v, list):
            return v  # Pydantic のデフォルトバリデーションに委譲
        return [s.strip().lower() for s in v if isinstance(s, str) and s.strip()]

    def to_canonical_ids(self) -> list[str]:
        """この CapabilitySpec を canonical capability ID のリストに展開する.

        Returns:
            canonical ID リスト。artifacts がある場合は
            ``domain.action.artifact`` 形式、ない場合は ``domain.action`` 形式。
        """
        ids: list[str] = []
        for action in self.actions:
            if self.artifacts:
                for artifact in self.artifacts:
                    ids.append(f"{self.domain}.{action}.{artifact}")
            else:
                ids.append(f"{self.domain}.{action}")
        return ids


class CanonicalCapability(BaseModel):
    """Canonical capability object."""

    id: str = Field(..., min_length=3, description="Canonical capability id")
    domain: str = Field(..., min_length=1, description="Capability domain")
    task: str = Field(..., min_length=1, description="Task segment")
    qualifier: str | None = Field(default=None, description="Optional qualifier segment")
    label: str = Field(..., min_length=1, description="Human readable label")
    aliases: list[str] = Field(default_factory=list, description="Observed legacy aliases")


class CapabilityAggregate(CanonicalCapability):
    """Aggregated capability stats across apps."""

    count: int = Field(default=0, ge=0, description="Observed count")
    apps: list[str] = Field(default_factory=list, description="Apps using this capability")
